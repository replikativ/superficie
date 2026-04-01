(ns superficie.parse.reader
  "superficie reader: recursive-descent parser.
   Transforms superficie tokens into Clojure forms."
  (:require [clojure.string :as str]
            [superficie.errors :as errors]
            [superficie.forms :as forms]
            [superficie.operators :as ops]
            [superficie.parse.expander :as expander]
            [superficie.parse.resolve :as resolve]))

;; Sentinel for #_ discard. Contract:
;; - Returned by `parse-form-base` (via `:discard`) when the parsed form was a #_ discard
;; - `parse-form` passes it through (skips `parse-path-chain` for sentinels)
;; - Use `discard-sentinel?` to check — never use `identical?` directly
;; - MUST be filtered by every caller of `parse-form` that collects forms:
;;   1. `parse-forms-until` — filters in its accumulation loop
;;   2. `parse-tokens` — filters in its top-level loop
;;   3. Any new callsite of `parse-form` must handle this sentinel
;; - The `:open-anon-fn` handler rejects it (single-expression body cannot be discarded)
(def ^:private discard-sentinel #?(:clj (Object.) :cljs #js {}))

(defn- discard-sentinel?
  "Returns true if v is the discard sentinel. Use this instead of
   (identical? discard-sentinel v) to keep the contract grep-able."
  [v]
  (identical? discard-sentinel v))

;; ---------------------------------------------------------------------------
;; Parser state
;; ---------------------------------------------------------------------------

(def ^:private ^:const max-depth 150)

(defn- make-parser
  ([tokens] (make-parser tokens nil nil))
  ([tokens opts] (make-parser tokens opts nil))
  ([tokens opts source]
   {:tokens tokens :pos (volatile! 0) :depth (volatile! 0)
    :opts opts :source source :sq-depth (volatile! 0)
    :sexp-mode      (volatile! false)  ; true inside '(...) quoted lists
    :no-block       (volatile! false)  ; true immediately inside 'form (suppresses block dispatch)
    :bracket-depth  (volatile! 0)}))   ; > 0 when inside [...] — suppresses block dispatch

(defn- peof? [{:keys [tokens pos]}]
  (>= @pos (count tokens)))

(defn- ppeek
  ([p] (ppeek p 0))
  ([{:keys [tokens pos]} offset]
   (let [i (+ @pos offset)]
     (when (< i (count tokens)) (nth tokens i)))))

(defn- padvance! [{:keys [pos]}] (vswap! pos inc))

(defn- plast-loc
  "Location of the last consumed token, or {} if none."
  [{:keys [tokens pos]}]
  (let [i (dec @pos)]
    (if (and (>= i 0) (< i (count tokens)))
      (select-keys (nth tokens i) [:line :col])
      {})))

(defn- error-data
  "Merge source into error data map so reader-error can attach :source-context."
  [p data]
  (cond-> data
    (:source p) (assoc :source (:source p))))

(def ^:private token-name
  "Human-readable names for token types."
  {:close-paren  ")"
   :close-bracket "]"
   :close-brace  "}"
   :open-paren   "("
   :open-bracket "["
   :open-brace   "{"
   :open-set     "#{"
   :open-anon-fn "#("
   :symbol       "symbol"
   :keyword      "keyword"
   :number       "number"
   :string       "string"
   :char         "character"
   :regex        "regex"
   :deref        "@"
   :meta         "^"
   :quote        "'"
   :unquote      "~"
   :unquote-splicing "~@"
   :var-quote    "#'"
   :discard      "#_"
   :tagged-literal "tagged literal"
   :reader-cond-start "reader conditional"
   :namespaced-map-start "namespaced map prefix"
   :syntax-quote "syntax-quote"})

(def ^:private closer-name
  "Human-readable descriptions for closing delimiters."
  {:close-paren  "closing )"
   :close-bracket "closing ]"
   :close-brace  "closing }"})

(def ^:private closer-context
  "What structure each closer terminates."
  {:close-paren  "call"
   :close-bracket "vector"
   :close-brace  "map/set"})

(defn- describe-token [tok]
  (let [typ (:type tok)
        n (get token-name typ (name typ))]
    (if (#{:symbol :keyword :number :string :char :regex} typ)
      (str n " " (:value tok))
      n)))

(defn- tok-type? [tok typ]
  (and tok (= (:type tok) typ)))

(declare parse-form parse-form-base parse-expr)

;; ---------------------------------------------------------------------------
;; Syntax-quote expansion — delegated to superficie.parse.expander
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Splice result marker for #?@ inside collections
;; ---------------------------------------------------------------------------

(defn- splice-result
  "Wrap matched forms for #?@ splicing into surrounding collection.
   In Clojure, #?@(:clj [2 3]) inside [1 ... 4] splices to [1 2 3 4].
   The marker is detected by parse-forms-until which splices instead of conj'ing."
  [forms]
  (with-meta (vec forms) {::splice true}))

(defn- splice-result?
  "Is this form a splice marker from #?@ evaluation?"
  [form]
  (and (vector? form) (some? (meta form)) (::splice (meta form))))

;; ---------------------------------------------------------------------------
;; Collections
;; ---------------------------------------------------------------------------

(def ^:private closer-types
  "Set of token types that are closing delimiters."
  #{:close-paren :close-bracket :close-brace})

(defn- parse-forms-until
  ([p end-type] (parse-forms-until p end-type nil))
  ([p end-type open-loc]
   (let [end-pred #(= end-type (:type %))]
     (loop [forms []]
       (when (peof? p)
         (let [ctx (get closer-context end-type "expression")
               closer (get closer-name end-type (name end-type))]
           (errors/reader-error
            (str "Unclosed " ctx " — expected " closer " but reached end of input")
            (error-data p (cond-> (assoc (plast-loc p) :incomplete true)
                            open-loc (assoc :secondary [{:line (:line open-loc) :col (:col open-loc) :label "opened here"}])
                            open-loc (assoc :hint (str "Add " (get token-name end-type (name end-type)) " to close this " ctx)))))))
       (let [tok (ppeek p)]
         (cond
           ;; Correct closer — done
           (end-pred tok)
           (do (padvance! p) forms)

           ;; Wrong closer — mismatched delimiter
           (and (closer-types (:type tok)) (not (end-pred tok)))
           (let [expected (get token-name end-type (name end-type))
                 actual (get token-name (:type tok) (name (:type tok)))
                 ctx (get closer-context end-type "expression")]
             (errors/reader-error
              (str "Mismatched delimiter — expected " expected " to close " ctx " but got " actual)
              (error-data p (cond-> (select-keys tok [:line :col])
                              open-loc (assoc :secondary [{:line (:line open-loc) :col (:col open-loc) :label "opened here"}])
                              open-loc (assoc :hint (str "Replace " actual " with " expected " to close this " ctx))))))

           ;; Normal form — parse and accumulate.
           ;; In sexp-mode (inside quoted lists) use parse-form (no infix).
           ;; Otherwise use parse-expr so infix works inside collections.
           :else
           (let [form (if @(:sexp-mode p) (parse-form p) (parse-expr p))]
             (cond
               (discard-sentinel? form) (recur forms)
               (splice-result? form) (recur (into forms form))
               :else (recur (conj forms form))))))))))

(defn- parse-vector [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; [
    (vswap! (:bracket-depth p) inc)
    (let [result (vec (parse-forms-until p :close-bracket loc))]
      (vswap! (:bracket-depth p) dec)
      result)))

(defn- parse-map [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; {
    (let [forms (parse-forms-until p :close-brace loc)]
      (when (odd? (count forms))
        (errors/reader-error (str "Map literal requires an even number of forms, but got " (count forms))
                             (error-data p (assoc loc :hint "Maps need key-value pairs — check for a missing key or value"))))
      (let [m (apply array-map forms)
            keys (take-nth 2 forms)]
        (when (not= (count m) (/ (count forms) 2))
          (let [dup (first (filter (fn [k] (> (count (filter #(= k %) keys)) 1)) keys))]
            (errors/reader-error (str "Duplicate key in map literal: " (pr-str dup))
                                 (error-data p loc))))
        m))))

(defn- parse-set [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; #{
    (let [forms (parse-forms-until p :close-brace loc)
          s (set forms)]
      (when (not= (count s) (count forms))
        (let [seen (volatile! #{})
              dup (first (filter (fn [x] (if (contains? @seen x) true (do (vswap! seen conj x) false))) forms))]
          (errors/reader-error (str "Duplicate element in set literal: " (pr-str dup))
                               (error-data p loc))))
      (with-meta s {:sup/order (vec forms)}))))

;; ---------------------------------------------------------------------------
;; Call: f(args...)
;; ---------------------------------------------------------------------------

(defn- parse-call-args [p]
  (let [loc (select-keys (ppeek p) [:line :col])]
    (padvance! p) ; (
    (parse-forms-until p :close-paren loc)))

;; ---------------------------------------------------------------------------
;; #() anonymous function — % param helpers
;; ---------------------------------------------------------------------------

;; percent-param-type is in forms.cljc (shared with printer)

(defn- find-percent-params
  "Walk form collecting % param types. Skips nested (fn ...) bodies."
  [form]
  (cond
    (symbol? form)
    (if-let [p (forms/percent-param-type form)] #{p} #{})

    (and (seq? form) (= 'fn (first form)))
    #{} ; don't recurse into nested fn / inner #()

    (seq? form)
    (reduce into #{} (map find-percent-params form))

    (vector? form)
    (reduce into #{} (map find-percent-params form))

    ;; AST node defrecords satisfy (map? x) — check before map?
    (forms/raw? form) #{} ; raw values (numbers, chars) contain no % params
    (forms/syntax-quote? form) (find-percent-params (:form form))
    (forms/unquote? form) (find-percent-params (:form form))
    (forms/unquote-splicing? form) (find-percent-params (:form form))

    (map? form)
    (reduce into #{} (mapcat (fn [[k v]] [(find-percent-params k) (find-percent-params v)]) form))

    (set? form)
    (reduce into #{} (map find-percent-params form))

    #?@(:clj [(tagged-literal? form)
              (find-percent-params (.-form form))])

    :else #{}))

(defn- normalize-bare-percent
  "Replace bare % with %1 in form. Skips nested (fn ...) bodies."
  [form]
  (cond
    (and (symbol? form) (= "%" (name form))) (symbol "%1")

    (and (seq? form) (= 'fn (first form)))
    form ; don't recurse into nested fn

    (seq? form)
    (apply list (map normalize-bare-percent form))

    (vector? form)
    (mapv normalize-bare-percent form)

    ;; AST node defrecords satisfy (map? x) — check before map?
    (forms/raw? form) form ; pass through unchanged
    (forms/syntax-quote? form) (forms/->SupSyntaxQuote (normalize-bare-percent (:form form)))
    (forms/unquote? form) (forms/->SupUnquote (normalize-bare-percent (:form form)))
    (forms/unquote-splicing? form) (forms/->SupUnquoteSplicing (normalize-bare-percent (:form form)))

    (map? form)
    (into {} (map (fn [[k v]] [(normalize-bare-percent k) (normalize-bare-percent v)]) form))

    (set? form)
    (set (map normalize-bare-percent form))

    #?@(:clj [(tagged-literal? form)
              (tagged-literal (.-tag form) (normalize-bare-percent (.-form form)))])

    :else form))

(defn- build-anon-fn-params
  "Build [%1 %2 ...] or [%1 & %&] param vector from collected param types."
  [param-set]
  (let [has-bare? (contains? param-set :bare)
        has-rest? (contains? param-set :rest)
        nums (filter number? param-set)
        max-n (if (seq nums) (apply max nums) (if has-bare? 1 0))]
    (cond-> (mapv #(symbol (str "%" %)) (range 1 (inc max-n)))
      has-rest? (into ['& (symbol "%&")]))))

;; ---------------------------------------------------------------------------
;; Main parse dispatch
;; ---------------------------------------------------------------------------

(defn- adjacent-open-paren?
  "Is the next token an ( with no preceding whitespace?
   Spacing is significant: f(x) is a call, f () is two separate forms."
  [p]
  (let [tok (ppeek p)]
    (and (tok-type? tok :open-paren)
         (not (:ws tok)))))

(defn- maybe-call
  "If next token is ( with no whitespace gap, parse call args and wrap."
  [p head]
  (if (adjacent-open-paren? p)
    (let [args (parse-call-args p)]
      (apply list head args))
    head))

(defn- parse-reader-cond-preserve
  "Parse #?(...) or #?@(...) in preserve mode — collect all branches,
   return a ReaderConditional object."
  [p loc splice?]
  (loop [pairs []]
    (cond
      (peof? p)
      (errors/reader-error (str "Unclosed reader conditional — expected ) but reached end of input")
                           (error-data p (cond-> (assoc loc :incomplete true)
                                           loc (assoc :secondary [{:line (:line loc) :col (:col loc) :label "opened here"}])
                                           loc (assoc :hint "Add ) to close this reader conditional"))))

      (tok-type? (ppeek p) :close-paren)
      (do (padvance! p)
          (forms/make-reader-conditional (apply list pairs) splice?))

      :else
      (let [key-tok (ppeek p)]
        (when-not (tok-type? key-tok :keyword)
          (errors/reader-error (str "Expected platform keyword in reader conditional, got " (describe-token key-tok))
                               (error-data p (select-keys key-tok [:line :col]))))
        (let [platform-key (keyword (subs (:value key-tok) 1))]
          (padvance! p)
          ;; Use parse-expr so infix expressions (a and b) work as reader-cond values
          (let [form (parse-expr p)]
            (recur (conj pairs platform-key form))))))))

(defn- parse-reader-cond-eval
  "Parse #?(...) or #?@(...) in evaluate mode — return matching platform's form."
  [p loc splice?]
  (let [platform #?(:clj :clj :cljs :cljs)]
    ;; Use discard-sentinel as the "no match" marker — nil can be a valid form value.
    (loop [matched discard-sentinel]
      (cond
        (peof? p)
        (errors/reader-error (str "Unclosed reader conditional — expected ) but reached end of input")
                             (error-data p (cond-> (assoc loc :incomplete true)
                                             loc (assoc :secondary [{:line (:line loc) :col (:col loc) :label "opened here"}])
                                             loc (assoc :hint "Add ) to close this reader conditional"))))

        (tok-type? (ppeek p) :close-paren)
        (do (padvance! p)
            (if (discard-sentinel? matched)
              discard-sentinel
              (if splice?
                (if (sequential? matched)
                  (splice-result matched)
                  (errors/reader-error
                   "Splicing reader conditional value must be a list or vector"
                   (error-data p loc)))
                (maybe-call p matched))))

        ;; Already matched — consume remaining forms permissively until ).
        ;; Matches Clojure's behavior: once a branch is selected, remaining
        ;; content is not validated for key-value pair structure. This handles
        ;; #_ read-through consuming a platform keyword as a branch value.
        (not (discard-sentinel? matched))
        (do (parse-form p)
            (recur matched))

        :else
        (let [key-tok (ppeek p)]
          (when-not (tok-type? key-tok :keyword)
            (errors/reader-error (str "Expected platform keyword in reader conditional, got " (describe-token key-tok))
                                 (error-data p (select-keys key-tok [:line :col]))))
          (let [platform-key (keyword (subs (:value key-tok) 1))]
            (padvance! p)
            ;; Use parse-expr so infix expressions (a and b) work as reader-cond values
            (let [form (parse-expr p)]
              (if (or (= platform-key platform) (= platform-key :default))
                (recur form)
                (recur matched)))))))))

;; ---------------------------------------------------------------------------
;; Surface syntax: infix operators (Pratt precedence-climbing)
;; ---------------------------------------------------------------------------

(defn- arrow-tok?
  "'=>' clause separator in cond/case."
  [tok]
  (and tok (= :symbol (:type tok)) (= "=>" (:value tok))))

(defn- infix-tok?
  "Is this token a surface infix operator (in *surface-index* from op registry)?
   Returns false when the operator is immediately followed by '(' — in that
   case it is a call head (e.g. *(x y)), not an infix operator.
   Returns false when the operator is followed by a close-delimiter or EOF —
   no right operand, so it's a standalone symbol (e.g. f([a] +) — '+' is a value).
   Returns false when the operator is followed by '=>' — in that position the
   operator is a case/cond/match arm pattern, not an infix continuation."
  ([tok] (infix-tok? tok nil))
  ([tok next-tok]
   (and tok
        (= :symbol (:type tok))
        (contains? @ops/*surface-index* (:value tok))
        ;; if adjacent to (, it's a call head, not infix
        (not (and next-tok
                  (= :open-paren (:type next-tok))
                  (not (:ws next-tok))))
        ;; if next is close-delimiter or EOF, no right operand — treat as symbol
        (not (or (nil? next-tok)
                 (tok-type? next-tok :close-paren)
                 (tok-type? next-tok :close-bracket)
                 (tok-type? next-tok :close-brace)))
        ;; if next is =>, this token is a case/cond arm pattern, not an infix op
        (not (arrow-tok? next-tok)))))

(defn- colon-tok?
  "Bare ':' keyword — block-start delimiter."
  [tok]
  (and tok (= :keyword (:type tok)) (= ":" (:value tok))))

(defn- bind-op?
  "':=' — let-binding sugar."
  [tok]
  (and tok (= :keyword (:type tok)) (= ":=" (:value tok))))

(defn- end-symbol?
  "Is this the 'end' block-close symbol?"
  [tok]
  (and tok (= :symbol (:type tok)) (= "end" (:value tok))))

(defn- block-terminator?
  "Tokens that can close a block body (else, else:, catch, catch:, finally, finally:, end)."
  [tok]
  (and tok (= :symbol (:type tok))
       (#{"end" "else" "else:" "catch" "catch:" "finally" "finally:"} (:value tok))))

(defn- sym-value? [tok s]
  (and tok (= :symbol (:type tok)) (= s (:value tok))))

;; ---------------------------------------------------------------------------
;; Surface syntax: block forms
;; ---------------------------------------------------------------------------

(defn- strip-trailing-colon
  "If sym-str ends with ':', strip it and return [name-str true], else [sym-str false]."
  [sym-str]
  (if (str/ends-with? sym-str ":")
    [(subs sym-str 0 (dec (count sym-str))) true]
    [sym-str false]))

(defn- strip-expr-colon
  "Strip trailing ':' from the rightmost symbol in an expression.
   Returns [stripped-form colon-found?].
   Handles: bare symbol 'x: → [x true]; compound '(and x y:) → [(and x y) true];
   unquote '~x: → [~x true]; keyword ':=>: → [:=> true]."
  [form]
  (cond
    (and (symbol? form) (str/ends-with? (name form) ":"))
    [(symbol (namespace form) (subs (name form) 0 (dec (count (name form))))) true]

    ;; Keyword ending with ':' — e.g. :=>: produced by tokenizer absorbing the if-block
    ;; colon into the preceding keyword. Strip the trailing colon.
    (and (keyword? form) (str/ends-with? (name form) ":"))
    [(keyword (namespace form) (subs (name form) 0 (dec (count (name form))))) true]

    (and (seq? form) (seq form))
    (let [args (vec form)
          [stripped colon?] (strip-expr-colon (peek args))]
      (if colon?
        [(apply list (conj (pop args) stripped)) true]
        [form false]))

    (forms/unquote? form)
    (let [[stripped-inner cin] (strip-expr-colon (:form form))]
      (if cin [(forms/->SupUnquote stripped-inner) true] [form false]))

    :else [form false]))

(defn- consume-colon!
  "Consume the block-start colon. Accepts both:
   - Already-consumed (colon was part of prior token, indicated by colon-in-prior?)
   - Standalone ':' as the next token.
   Errors if neither."
  [p colon-in-prior? context-msg loc]
  (when-not colon-in-prior?
    (when-not (colon-tok? (ppeek p))
      (errors/reader-error (str "Expected ':' to start " context-msg " block")
                           (error-data p loc)))
    (padvance! p)))

(declare wrap-body-lets)

(defn- parse-body
  "Parse a sequence of exprs until block-terminator? or EOF.
   Returns a vector of forms. Does NOT consume the terminator.
   Handles let-statement sugar: 'let x := expr' sentinels wrapped into (let [...] ...)."
  [p]
  (let [forms
        (loop [forms []]
          (let [tok (ppeek p)]
            (cond
              (or (nil? tok) (block-terminator? tok)) forms
              :else
              (let [form (parse-expr p)]
                (if (discard-sentinel? form)
                  (recur forms)
                  (recur (conj forms form)))))))]
    (wrap-body-lets forms)))

(defn- parse-defn-arities
  "Parse arity bodies for defn/defmacro: [params] body... repeated until 'end'.
   Returns vector of [params body...] vectors (one per arity)."
  [p]
  (loop [arities []]
    (cond
      (peof? p)
      (errors/reader-error "Unclosed defn block — expected 'end'"
                           (error-data p (assoc (plast-loc p) :incomplete true)))

      (end-symbol? (ppeek p))
      (do (padvance! p) arities)

      (or (tok-type? (ppeek p) :open-bracket)
          (tok-type? (ppeek p) :meta))
      (let [params (if (tok-type? (ppeek p) :open-bracket) (parse-vector p) (parse-form p))
            body   (parse-body p)]
        (recur (conj arities (into [params] body))))

      :else
      (errors/reader-error "Expected parameter vector '[...]' or 'end' in defn block"
                           (error-data p (select-keys (ppeek p) [:line :col]))))))

(defn- parse-defn-block
  "Parse: defn name [params]: body end
   Optionally: defn name \"docstring\" [params]: body end
   Optionally: defn name \"docstring\" {attr-map} [params]: body end
   name may be any form (symbol, #?(...) reader conditional, etc.)"
  [p form-sym tok]
  (let [name-sym (parse-form p)
        ;; Optional docstring
        docstring (when (tok-type? (ppeek p) :string)
                    (let [ds (:value (ppeek p))]
                      (padvance! p)
                      (resolve/resolve-string ds {})))
        ;; Optional attr-map
        attr-map (when (tok-type? (ppeek p) :open-brace)
                   (parse-form p))
        ;; Params vector (required; ^meta [params] also accepted)
        _ (when-not (or (tok-type? (ppeek p) :open-bracket)
                        (tok-type? (ppeek p) :meta))
            (errors/reader-error (str "Expected parameter vector '[...]' in " (clojure.core/name form-sym))
                                 (error-data p (select-keys (or (ppeek p) tok) [:line :col]))))
        params (if (tok-type? (ppeek p) :open-bracket) (parse-vector p) (parse-form p))
        _ (consume-colon! p false (clojure.core/name form-sym) (select-keys tok [:line :col]))
        body (parse-body p)
        _ (when-not (end-symbol? (ppeek p))
            (errors/reader-error (str "Expected 'end' to close " (clojure.core/name form-sym) " block")
                                 (error-data p (plast-loc p))))
        _ (padvance! p)]
    (apply list form-sym name-sym
           (concat (when docstring [docstring])
                   (when attr-map [attr-map])
                   [params]
                   body))))

(defn- parse-fn-block
  "Parse: fn [params]: body end
   Optionally: fn name [params]: body end"
  ([p form-sym tok] (parse-fn-block p form-sym tok false))
  ([p form-sym tok _colon-in-kw?]
   (let [;; Optional name: symbol before '[' or '^meta'
         name-sym (when (and (tok-type? (ppeek p) :symbol)
                             (not (tok-type? (ppeek p) :open-bracket))
                             (not (tok-type? (ppeek p) :meta)))
                    (let [n (symbol (:value (ppeek p)))]
                      (padvance! p) n))
         ;; Params vector (required; ^meta [params] also accepted)
         _ (when-not (or (tok-type? (ppeek p) :open-bracket)
                         (tok-type? (ppeek p) :meta))
             (errors/reader-error "Expected parameter vector '[...]' after fn"
                                  (error-data p (select-keys tok [:line :col]))))
         params (if (tok-type? (ppeek p) :open-bracket) (parse-vector p) (parse-form p))
         _ (consume-colon! p false "fn" (select-keys tok [:line :col]))
         body (parse-body p)
         _ (when-not (end-symbol? (ppeek p))
             (errors/reader-error "Expected 'end' to close fn block"
                                  (error-data p (plast-loc p))))
         _ (padvance! p)]
     (apply list form-sym (concat (when name-sym [name-sym]) [params] body)))))

(defn- parse-if-block
  "Parse: if cond: then-body [else: else-body] end
   Also handles when/when-not (no else branch)."
  [p form-sym tok]
  (let [raw-cond (parse-expr p)
        ;; Handle colon fused into the last symbol of the condition (e.g. 'flag:, 'x and y:)
        [cond-form colon-in-cond] (strip-expr-colon raw-cond)
        ;; Consume ':'
        _ (let [next-tok (ppeek p)]
            (cond
              ;; Next token is a word ending with ':' (fused, like "then:") — skip
              (and (tok-type? next-tok :symbol) (str/ends-with? (:value next-tok) ":"))
              (padvance! p)
              ;; Colon was already inside the condition expression
              colon-in-cond nil
              ;; Standalone ':' keyword
              :else
              (consume-colon! p false "if" (select-keys tok [:line :col]))))
        then-body (parse-body p)
        ;; Check for else:/else
        else-body (when (and (tok-type? (ppeek p) :symbol)
                             (#{"else" "else:"} (:value (ppeek p))))
                    (let [else-val (:value (ppeek p))]
                      (padvance! p) ; consume else/else:
                      (when (= "else" else-val)
                        (consume-colon! p false "else" (select-keys tok [:line :col])))
                      (parse-body p)))
        _ (when-not (end-symbol? (ppeek p))
            (errors/reader-error "Expected 'end' to close if block"
                                 (error-data p (plast-loc p))))
        _ (padvance! p)] ; consume end
    (if (= 'if form-sym)
      ;; Clojure's if takes exactly one then and one else — wrap multi-form bodies in do
      (let [then-expr (if (= 1 (count then-body))
                        (first then-body)
                        (apply list 'do then-body))
            else-expr (when else-body
                        (if (= 1 (count else-body))
                          (first else-body)
                          (apply list 'do else-body)))]
        (if else-body
          (list 'if cond-form then-expr else-expr)
          (list 'if cond-form then-expr)))
      ;; when/when-not: multi-body is fine; no else expected
      (apply list form-sym cond-form then-body))))

(defn- parse-let-block
  "Parse: let [bindings]: body end"
  [p form-sym tok]
  (when-not (tok-type? (ppeek p) :open-bracket)
    (errors/reader-error (str "Expected '[bindings]' after " (name form-sym))
                         (error-data p (select-keys tok [:line :col]))))
  (let [bindings (parse-vector p)
        _ (let [next-tok (ppeek p)]
            (if (and (tok-type? next-tok :symbol) (str/ends-with? (:value next-tok) ":"))
              (padvance! p)
              (consume-colon! p false "let" (select-keys tok [:line :col]))))
        body (parse-body p)
        _ (when-not (end-symbol? (ppeek p))
            (errors/reader-error "Expected 'end' to close let block"
                                 (error-data p (plast-loc p))))
        _ (padvance! p)]
    (apply list form-sym bindings body)))

(defn- parse-cond-block
  "Parse: cond: test1 => val1  test2 => val2  end
   Each arm: parse-form => parse-expr"
  ([p form-sym tok] (parse-cond-block p form-sym tok false))
  ([p _form-sym tok colon-in-kw?]
   (let [_ (consume-colon! p colon-in-kw? "cond" (select-keys tok [:line :col]))]
     (loop [clauses []]
       (cond
         (peof? p)
         (errors/reader-error "Unclosed cond block — expected 'end'"
                              (error-data p (assoc (plast-loc p) :incomplete true)))

         (end-symbol? (ppeek p))
         (do (padvance! p) (apply list 'cond (apply concat clauses)))

         :else
         (let [test-form (parse-expr p)
               _ (when-not (arrow-tok? (ppeek p))
                   (errors/reader-error "Expected '=>' in cond clause"
                                        (error-data p (plast-loc p))))
               _ (padvance! p)
               val-form (parse-expr p)]
           (recur (conj clauses [test-form val-form]))))))))

(defn- parse-case-block
  "Parse: case expr: val1 => result1  val2 => result2  [_ => default] end
   _ as pattern becomes the Clojure default (no test key).
   Each arm: parse-form => parse-expr"
  [p _form-sym tok]
  (let [raw-expr (parse-expr p)
        [expr colon-in-expr] (strip-expr-colon raw-expr)
        _ (consume-colon! p colon-in-expr "case" (select-keys tok [:line :col]))]
    (let [no-default ::no-case-default]
      (loop [clauses [] default no-default]
        (cond
          (peof? p)
          (errors/reader-error "Unclosed case block — expected 'end'"
                               (error-data p (assoc (plast-loc p) :incomplete true)))

          (end-symbol? (ppeek p))
          (do (padvance! p)
              (apply list 'case expr
                     (concat (apply concat clauses)
                             (when-not (identical? default no-default) [default]))))

          :else
          (if (arrow-tok? (ppeek p))
          ;; Bare => default (no pattern): "=> value"
            (do (padvance! p)
                (recur clauses (parse-expr p)))
            (let [raw-pat (parse-expr p)
                  _ (when-not (arrow-tok? (ppeek p))
                      (errors/reader-error "Expected '=>' in case clause"
                                           (error-data p (plast-loc p))))
                  _ (padvance! p)
                  result (parse-expr p)
                ;; '(a b c) in a case test means multi-dispatch list, not a quoted
                ;; form — strip the quote wrapper to get the bare list Clojure expects.
                  pat (if (and (seq? raw-pat) (= 'quote (first raw-pat)) (= 2 (count raw-pat)))
                        (second raw-pat)
                        raw-pat)]
              (recur (conj clauses [pat result]) default))))))))

(defn- parse-try-block
  "Parse: try: body [catch [ExType e]: handler]* [finally: cleanup] end"
  ([p form-sym tok] (parse-try-block p form-sym tok false))
  ([p _form-sym tok colon-in-kw?]
   (let [_ (consume-colon! p colon-in-kw? "try" (select-keys tok [:line :col]))
         body (parse-body p)]
     (loop [clauses []]
       (cond
         (peof? p)
         (errors/reader-error "Unclosed try block — expected 'end'"
                              (error-data p (assoc (plast-loc p) :incomplete true)))

         (end-symbol? (ppeek p))
         (do (padvance! p)
             (apply list 'try (concat body clauses)))

         (and (tok-type? (ppeek p) :symbol)
              (#{"catch" "catch:"} (:value (ppeek p))))
         (let [_ (padvance! p) ; consume catch/catch:
              ;; catch needs [ExType e] binding
               _ (when-not (tok-type? (ppeek p) :open-bracket)
                   (errors/reader-error "Expected '[ExceptionType binding]' after catch"
                                        (error-data p (select-keys (ppeek p) [:line :col]))))
               binding (parse-vector p) ; [ExType e]
               _ (consume-colon! p false "catch" (select-keys tok [:line :col]))
               catch-body (parse-body p)]
           (recur (conj clauses (apply list 'catch (concat binding catch-body)))))

         (and (tok-type? (ppeek p) :symbol)
              (#{"finally" "finally:"} (:value (ppeek p))))
         (let [finally-val (:value (ppeek p))
               _ (padvance! p) ; consume finally/finally:
               colon-in-finally? (str/ends-with? finally-val ":")
               _ (consume-colon! p colon-in-finally? "finally" (select-keys tok [:line :col]))
               finally-body (parse-body p)]
           (recur (conj clauses (apply list 'finally finally-body))))

         :else
         (errors/reader-error "Expected 'catch', 'finally', or 'end' in try block"
                              (error-data p (select-keys (ppeek p) [:line :col]))))))))

(defn- parse-for-block
  "Parse: for [bindings]: body end  (also doseq, loop, dotimes)"
  [p form-sym tok]
  (when-not (tok-type? (ppeek p) :open-bracket)
    (errors/reader-error (str "Expected '[bindings]' after " (name form-sym))
                         (error-data p (select-keys tok [:line :col]))))
  (let [bindings (parse-vector p)
        _ (let [next-tok (ppeek p)]
            (if (and (tok-type? next-tok :symbol) (str/ends-with? (:value next-tok) ":"))
              (padvance! p)
              (consume-colon! p false (name form-sym) (select-keys tok [:line :col]))))
        body (parse-body p)
        _ (when-not (end-symbol? (ppeek p))
            (errors/reader-error (str "Expected 'end' to close " (name form-sym) " block")
                                 (error-data p (plast-loc p))))
        _ (padvance! p)]
    (apply list form-sym bindings body)))

;; ---------------------------------------------------------------------------
;; def / defonce block
;; ---------------------------------------------------------------------------

(defn- parse-def-block
  "Parse: def [^meta] name [\"docstring\"]: value
   Maps to: (def name docstring? value)
   name is read as a raw token to avoid interop transformation of qualified names.
   ^meta before the name is handled by parse-form, which attaches metadata to the symbol."
  [p form-sym tok]
  (let [[name-sym colon-in-name]
        (cond
          (tok-type? (ppeek p) :meta)
          ;; ^meta sym — parse-form reads ^ + meta-map/key + symbol as one unit,
          ;; returning the symbol with Clojure metadata attached.
          ;; The printer emits name: (no space before colon), so the scanner
          ;; tokenizes it as a single symbol with trailing ':'. Strip it here.
          (let [raw (parse-form p)
                sym-str (when (symbol? raw) (name raw))
                [stripped cin] (if sym-str (strip-trailing-colon sym-str) [nil false])
                name-sym (if (and cin (symbol? raw))
                           (vary-meta (symbol (namespace raw) stripped) merge (meta raw))
                           raw)]
            [name-sym cin])

          (tok-type? (ppeek p) :unquote)
          ;; ~expr — unquote in a syntax-quote context (e.g. `def ~name: val,
          ;; or `def ~'publics: val where the quoted symbol has a fused colon).
          ;; Use strip-expr-colon to handle all variants: plain ~sym:, ~'sym:, etc.
          (let [uform (parse-form p)]
            (if (forms/unquote? uform)
              (let [[stripped cin] (strip-expr-colon (:form uform))]
                [(forms/->SupUnquote stripped) cin])
              [uform false]))

          :else
          ;; bare symbol — may have trailing ':' attached (def name: value shorthand)
          (do (when-not (tok-type? (ppeek p) :symbol)
                (errors/reader-error (str "Expected symbol name after " (name form-sym))
                                     (error-data p (select-keys tok [:line :col]))))
              (let [name-tok (ppeek p)
                    _        (padvance! p)
                    [name-str cin] (strip-trailing-colon (:value name-tok))]
                [(symbol name-str) cin])))
        ;; Optional docstring (only when colon not already consumed)
        [docstring colon-in-doc]
        (if (and (not colon-in-name) (tok-type? (ppeek p) :string))
          (let [d (parse-expr p)] (strip-expr-colon d))
          [nil false])
        _ (consume-colon! p (or colon-in-name colon-in-doc)
                          (name form-sym) (select-keys tok [:line :col]))
        value (parse-expr p)]
    (if docstring
      (list form-sym name-sym docstring value)
      (list form-sym name-sym value))))

;; ---------------------------------------------------------------------------
;; ns block
;; ---------------------------------------------------------------------------

(defn- ns-sub-clause?
  "True when tok is a symbol ending with ':' that opens an ns sub-clause
   (e.g. require:, import:, use:, refer-clojure:, gen-class:, load:).
   'end:' is excluded — it closes the ns block."
  [tok]
  (and (tok-type? tok :symbol)
       (str/ends-with? (:value tok) ":")
       (not= "end:" (:value tok))))

(defn- parse-ns-clause-items
  "Read all items for a single ns sub-clause.
   Stops at the next sub-clause keyword, 'end', or EOF.
   Each item is a full expression (vector, symbol, keyword, etc.)."
  [p]
  (loop [items []]
    (if (or (peof? p)
            (end-symbol? (ppeek p))
            (ns-sub-clause? (ppeek p)))
      items
      (recur (conj items (parse-expr p))))))

(defn- parse-ns-block
  "Parse: ns name [\"docstring\"] [{attr-map}]:
     require: [lib :as alias] ...
     import:  ClassName ...
   end

   Maps to: (ns name docstring? attr-map? (:require ...) (:import ...) ...)"
  [p form-sym tok]
  ;; 1. Namespace name — dotted symbols (e.g. myapp.core) are safe:
  ;;    interop transforms only fire for '.-' or adjacent '('.
  (let [raw-name (parse-expr p)
        [name-sym colon-in-name] (strip-expr-colon raw-name)
        ;; 2. Optional docstring
        [docstring colon-after-doc]
        (if (and (not colon-in-name) (tok-type? (ppeek p) :string))
          (let [d (parse-expr p)] (strip-expr-colon d))
          [nil false])
        ;; 3. Optional attr-map
        [attr-map colon-after-attr]
        (if (and (not colon-in-name) (not colon-after-doc)
                 (tok-type? (ppeek p) :open-brace))
          (let [m (parse-expr p)] [m false])
          [nil false])
        ;; 4. Consume block-opening colon
        _ (consume-colon! p (or colon-in-name colon-after-doc colon-after-attr)
                          "ns" (select-keys tok [:line :col]))]
    ;; 5. Parse sub-clauses
    (loop [clauses []]
      (cond
        (peof? p)
        (errors/reader-error "Unclosed ns block — expected 'end'"
                             (error-data p (assoc (plast-loc p) :incomplete true)))

        (end-symbol? (ppeek p))
        (do (padvance! p)
            (apply list form-sym name-sym
                   (concat (when docstring [docstring])
                           (when attr-map [attr-map])
                           clauses)))

        (ns-sub-clause? (ppeek p))
        (let [clause-tok (ppeek p)
              raw-name   (:value clause-tok)
              clause-kw  (keyword (subs raw-name 0 (dec (count raw-name))))
              _          (padvance! p)
              items      (parse-ns-clause-items p)
              ;; Consume 'end' if the printer emitted one to terminate the sub-clause
              _          (when (end-symbol? (ppeek p)) (padvance! p))
              ;; :import vectors [pkg Class ...] → lists (pkg Class ...) for Clojure ns
              items      (if (= clause-kw :import)
                           (mapv (fn [item] (if (vector? item) (apply list item) item)) items)
                           items)]
          (recur (conj clauses (apply list clause-kw items))))

        ;; #?(:clj (:import ...)) or #?@(:clj [...]) as a ns clause
        (tok-type? (ppeek p) :reader-cond-start)
        (recur (conj clauses (parse-form p)))

        :else
        (errors/reader-error
         (str "Expected sub-clause keyword (require:, import:, etc.) or 'end' in ns block, got: "
              (:value (ppeek p)))
         (error-data p (select-keys (ppeek p) [:line :col])))))))

;; ---------------------------------------------------------------------------
;; defmethod block
;; ---------------------------------------------------------------------------

(defn- parse-defmethod-block
  "Parse: defmethod multi-name dispatch-val [params]: body end
   Maps to: (defmethod multi-name dispatch-val [params] body...)
   multi-name may be any form (symbol, #?(...) reader conditional, etc.)"
  [p form-sym tok]
  (let [name-sym     (parse-form p)
        dispatch-val  (parse-expr p)
        _             (when-not (tok-type? (ppeek p) :open-bracket)
                        (errors/reader-error "Expected '[params]' in defmethod"
                                             (error-data p (select-keys tok [:line :col]))))
        params        (parse-vector p)
        _             (consume-colon! p false "defmethod" (select-keys tok [:line :col]))
        body          (parse-body p)
        _             (when-not (end-symbol? (ppeek p))
                        (errors/reader-error "Expected 'end' to close defmethod block"
                                             (error-data p (plast-loc p))))
        _             (padvance! p)]
    (apply list form-sym name-sym dispatch-val params body)))

;; ---------------------------------------------------------------------------
;; defprotocol block
;; ---------------------------------------------------------------------------

(defn- parse-defprotocol-method-sig
  "Parse one protocol method signature: name [params]... ['docstring']
   Returns: (name [params] ... 'docstring'?)"
  [p]
  (let [name-sym  (symbol (:value (ppeek p)))
        _          (padvance! p)
        arities    (loop [arities []]
                     (if (tok-type? (ppeek p) :open-bracket)
                       (recur (conj arities (parse-vector p)))
                       arities))
        docstring  (when (tok-type? (ppeek p) :string)
                     (let [ds (:value (ppeek p))]
                       (padvance! p)
                       (resolve/resolve-string ds {})))]
    (apply list name-sym (concat arities (when docstring [docstring])))))

(defn- parse-defprotocol-block
  "Parse: defprotocol Name ['docstring'] [{attr-map}]:
     method-name [params]...
   end
   Maps to: (defprotocol Name 'docstring'? attr-map? (method [params]) ...)"
  [p form-sym tok]
  (when-not (or (tok-type? (ppeek p) :symbol)
                (tok-type? (ppeek p) :meta)
                (tok-type? (ppeek p) :unquote))
    (errors/reader-error "Expected protocol name after defprotocol"
                         (error-data p (select-keys tok [:line :col]))))
  (let [raw-name     (parse-expr p)
        [name-sym colon-in-name] (strip-expr-colon raw-name)
        docstring    (when (and (not colon-in-name) (tok-type? (ppeek p) :string))
                       (parse-expr p))
        attr-map     (when (and (not colon-in-name) (tok-type? (ppeek p) :open-brace))
                       (parse-form p))
        _            (consume-colon! p colon-in-name "defprotocol" (select-keys tok [:line :col]))]
    (loop [methods []]
      (cond
        (peof? p)
        (errors/reader-error "Unclosed defprotocol block — expected 'end'"
                             (error-data p (assoc (plast-loc p) :incomplete true)))

        (end-symbol? (ppeek p))
        (do (padvance! p)
            (apply list form-sym name-sym
                   (concat (when docstring [docstring])
                           (when attr-map  [attr-map])
                           methods)))

        ;; Method spec: symbol followed by [
        (and (tok-type? (ppeek p) :symbol)
             (tok-type? (ppeek p 1) :open-bracket))
        (recur (conj methods (parse-defprotocol-method-sig p)))

        ;; #?(:clj (method-name [this]) :cljs nil)
        (tok-type? (ppeek p) :reader-cond-start)
        (recur (conj methods (parse-form p)))

        :else
        (errors/reader-error (str "Expected method signature or 'end' in defprotocol, got: "
                                  (:value (ppeek p)))
                             (error-data p (select-keys (ppeek p) [:line :col])))))))

;; ---------------------------------------------------------------------------
;; Protocol implementation body (defrecord/deftype/reify/proxy)
;; ---------------------------------------------------------------------------

(defn- parse-protocol-method-impl
  "Parse one method implementation: name [params]: body end
   Returns: (name [params] body...)
   name may be any form (symbol, #?(...) reader conditional, etc.)"
  [p tok]
  (let [name-sym (parse-form p)
        _         (when-not (tok-type? (ppeek p) :open-bracket)
                    (errors/reader-error "Expected '[params]' in method implementation"
                                         (error-data p (select-keys tok [:line :col]))))
        params    (parse-vector p)
        _         (consume-colon! p false "method" (select-keys tok [:line :col]))
        body      (parse-body p)
        _         (when-not (end-symbol? (ppeek p))
                    (errors/reader-error "Expected 'end' to close method body"
                                         (error-data p (plast-loc p))))
        _         (padvance! p)]
    (apply list name-sym params body)))

(defn- parse-protocol-impl-body
  "Parse the body of defrecord/deftype/reify/proxy.
   Items are interface/protocol names and method implementations.
   Uses unified form dispatch: parse-form, then check for '[' to decide if method.
   Any form (symbol, ^meta symbol, #?(...) RC) is accepted as a name."
  [p tok]
  (loop [items []]
    (cond
      (peof? p)
      (errors/reader-error "Unclosed block — expected 'end'"
                           (error-data p (assoc (plast-loc p) :incomplete true)))

      ;; 'end' followed by '[' is a method named 'end', not the block terminator.
      ;; (e.g. proxy method: (end [] body) → printed as 'end []: body end')
      (and (end-symbol? (ppeek p))
           (not (tok-type? (ppeek p 1) :open-bracket)))
      items

      :else
      (let [item (parse-form p)]
        (if (tok-type? (ppeek p) :open-bracket)
          ;; next is '[' → method implementation
          (let [params (parse-vector p)
                _ (consume-colon! p false "method" (select-keys tok [:line :col]))
                body (parse-body p)
                _ (when-not (end-symbol? (ppeek p))
                    (errors/reader-error "Expected 'end' to close method body"
                                         (error-data p (plast-loc p))))
                _ (padvance! p)]
            (recur (conj items (apply list item params body))))
          ;; no '[' → interface/protocol name
          (recur (conj items item)))))))

(defn- parse-defrecord-block
  "Parse: defrecord Name [fields]: protocol-impl-body end
   Maps to: (defrecord Name [fields] Interface (method [params] body) ...)
   Name and [fields] may be any form (symbol/vector or #?(...) reader conditional)."
  [p form-sym tok]
  (let [name-sym  (parse-form p)
        fields    (parse-form p)
        _          (consume-colon! p false (name form-sym) (select-keys tok [:line :col]))
        items     (parse-protocol-impl-body p tok)
        _          (when-not (end-symbol? (ppeek p))
                     (errors/reader-error (str "Expected 'end' to close " (name form-sym) " block")
                                          (error-data p (plast-loc p))))
        _          (padvance! p)]
    (apply list form-sym name-sym fields items)))

(defn- parse-reify-block
  "Parse: reify: interface-and-method-impls end
   Maps to: (reify Interface (method [params] body) ...)"
  ([p form-sym tok] (parse-reify-block p form-sym tok false))
  ([p form-sym tok colon-in-kw?]
   (consume-colon! p colon-in-kw? "reify" (select-keys tok [:line :col]))
   (let [items (parse-protocol-impl-body p tok)
         _      (when-not (end-symbol? (ppeek p))
                  (errors/reader-error "Expected 'end' to close reify block"
                                       (error-data p (plast-loc p))))
         _      (padvance! p)]
     (apply list form-sym items))))

(defn- parse-proxy-block
  "Parse: proxy [bases] [ctor-args]: method-impls end
   Maps to: (proxy [bases] [ctor-args] (method [params] body) ...)"
  [p form-sym tok]
  (when-not (tok-type? (ppeek p) :open-bracket)
    (errors/reader-error "Expected '[bases]' in proxy"
                         (error-data p (select-keys tok [:line :col]))))
  (let [bases     (parse-vector p)
        _          (when-not (tok-type? (ppeek p) :open-bracket)
                     (errors/reader-error "Expected '[ctor-args]' in proxy"
                                          (error-data p (select-keys tok [:line :col]))))
        ctor-args (parse-vector p)
        _          (consume-colon! p false "proxy" (select-keys tok [:line :col]))
        items     (parse-protocol-impl-body p tok)
        _          (when-not (end-symbol? (ppeek p))
                     (errors/reader-error "Expected 'end' to close proxy block"
                                          (error-data p (plast-loc p))))
        _          (padvance! p)]
    (apply list form-sym bases ctor-args items)))

(def block-dispatch
  "Map: qualified-symbol → block-kind keyword.
   Extended at runtime by register-block! (eval.cljc) and :superficie/role metadata.
   Consulted AFTER namespace resolution — keys are fully-qualified symbols."
  (atom {'clojure.core/defn       :defn-block
         'clojure.core/defn-      :defn-block
         'clojure.core/defmacro   :defn-block
         'clojure.core/fn         :fn-block
         'clojure.core/fn*        :fn-block
         'clojure.core/if         :if-block
         'clojure.core/when       :when-block
         'clojure.core/when-not   :when-block
         'clojure.core/let        :let-block
         'clojure.core/letfn      :let-block
         'clojure.core/binding    :let-block
         'clojure.core/with-open  :let-block
         'clojure.core/with-redefs :let-block
         'clojure.core/cond       :cond-block
         'clojure.core/case       :case-block
         'clojure.core.match/match :match-block
         'clojure.core/try        :try-block
         'clojure.core/for        :for-block
         'clojure.core/doseq      :for-block
         'clojure.core/loop       :for-block
         'clojure.core/dotimes    :for-block
         'clojure.core/ns         :ns-block
         'clojure.core/def        :def-block
         'clojure.core/defonce    :def-block
         'clojure.core/defmulti   :def-block
         'clojure.core/defmethod   :defmethod-block
         'clojure.core/defprotocol :defprotocol-block
         'clojure.core/defrecord   :defrecord-block
         'clojure.core/deftype     :defrecord-block
         'clojure.core/reify       :reify-block
         'clojure.core/proxy       :proxy-block}))

(def ^:private block-name->sym
  "Static fallback: bare name string → best-guess qualified symbol.
   Used when no :resolve-sym hook is available (CLJS, static analysis)."
  {"defn"        'clojure.core/defn
   "defn-"       'clojure.core/defn-
   "defmacro"    'clojure.core/defmacro
   "fn"          'clojure.core/fn
   "fn*"         'clojure.core/fn*
   "if"          'clojure.core/if
   "when"        'clojure.core/when
   "when-not"    'clojure.core/when-not
   "let"         'clojure.core/let
   "letfn"       'clojure.core/letfn
   "binding"     'clojure.core/binding
   "with-open"   'clojure.core/with-open
   "with-redefs" 'clojure.core/with-redefs
   "cond"        'clojure.core/cond
   "case"        'clojure.core/case
   "match"       'clojure.core.match/match
   "try"         'clojure.core/try
   "for"         'clojure.core/for
   "doseq"       'clojure.core/doseq
   "loop"        'clojure.core/loop
   "dotimes"     'clojure.core/dotimes
   "ns"          'clojure.core/ns
   "def"         'clojure.core/def
   "defonce"     'clojure.core/defonce
   "defmulti"    'clojure.core/defmulti
   "defmethod"   'clojure.core/defmethod
   "defprotocol" 'clojure.core/defprotocol
   "defrecord"   'clojure.core/defrecord
   "deftype"     'clojure.core/deftype
   "reify"       'clojure.core/reify
   "proxy"       'clojure.core/proxy})

(defn- resolve-block-qsym
  "Resolve a bare name string to a qualified symbol for block dispatch.
   Primary: :resolve-sym from opts (namespace-aware, JVM/SCI).
   Fallback: static block-name->sym table."
  [p base-str]
  (let [resolve-sym (:resolve-sym (:opts p))]
    (or (when resolve-sym (resolve-sym base-str))
        (get block-name->sym base-str))))

(defn- has-colon-before-close?
  "Scan ahead (without consuming) to find ':' at bracket-nesting depth 0
   before any close-delimiter or block-terminator.
   Used to distinguish block-form 'if'/'when'/'case' from symbol uses
   inside vectors or other delimited contexts: '[if a]' has no colon
   at depth 0 before ']', so 'if' is a symbol there."
  [p]
  (loop [i 0 depth 0]
    (let [tok (ppeek p i)]
      (cond
        (nil? tok) false
        (zero? depth)
        (cond
          (colon-tok? tok) true
          (and (tok-type? tok :symbol) (str/ends-with? (:value tok) ":")) true
          (tok-type? tok :close-paren) false
          (tok-type? tok :close-bracket) false
          (tok-type? tok :close-brace) false
          ;; 'end' at depth 0: if followed by ':' it is used in a condition
          ;; (e.g. 'if i >= end :'); otherwise it is a block terminator → stop.
          (end-symbol? tok) (colon-tok? (ppeek p (inc i)))
          (or (tok-type? tok :open-paren)
              (tok-type? tok :open-bracket)
              (tok-type? tok :open-brace)
              (tok-type? tok :open-set)) (recur (inc i) (inc depth))
          :else (recur (inc i) depth))
        :else
        (cond
          (or (tok-type? tok :open-paren)
              (tok-type? tok :open-bracket)
              (tok-type? tok :open-brace)
              (tok-type? tok :open-set)) (recur (inc i) (inc depth))
          (or (tok-type? tok :close-paren)
              (tok-type? tok :close-bracket)
              (tok-type? tok :close-brace)) (recur (inc i) (dec depth))
          :else (recur (inc i) depth))))))

;; Block kinds whose header may be empty — a bare ':' immediately after the keyword is valid.
;; All other registered block kinds require at least one header token before ':'.
(def ^:private empty-header-blocks
  #{:cond-block :try-block :reify-block :fn-block})

;; Block kinds whose header MUST start with a '[...]' binding vector.
;; For these, we require both (a) next token is '[' and (b) a ':' follows after the ']'.
;; This prevents block-keyword variable names (e.g. 'binding', 'for', 'let') from
;; triggering block parsing when used as ordinary function arguments.
(def ^:private bracket-first-blocks
  #{:let-block :for-block})

(defn- block-kind->start?
  "True if the current token stream looks like a valid block start for block-kind.

   All recognition is based on has-colon-before-close?, which scans through nested
   brackets and returns true only if a ':' at depth 0 exists before any close-delimiter.
   This prevents false positives like 'binding [:var :sym])' triggering :let-block
   (no ':' follows the ']').

   Special cases checked first:
   1. colon-in-tok?  — keyword was tokenized as 'keyword:' — always a block start
   2. bare ':' as first token: valid only for empty-header-blocks (no mandatory header)
      — prevents block keywords used as variables from stealing an outer ':'.
   3. '=>' as first token: never a block start (cond/case arm separator)
   4. def-block: first token must be name-like (symbol/^meta/~unquote)
   5. All other: has-colon-before-close? handles both '[...]' and plain headers."
  [p block-kind colon-in-tok?]
  (let [next (ppeek p)]
    (or colon-in-tok?
        (when (and next
                   (not (tok-type? next :close-paren))
                   (not (tok-type? next :close-bracket))
                   (not (tok-type? next :close-brace))
                   (not (arrow-tok? next)))
          (cond
            ;; Bare ':' is valid only for blocks with no mandatory header
            (colon-tok? next)
            (or (contains? empty-header-blocks block-kind) (nil? block-kind))

            ;; def/defonce/defmulti: require a name-like first token
            (= :def-block block-kind)
            (and (or (tok-type? next :symbol)
                     (tok-type? next :meta)
                     (tok-type? next :unquote))
                 (has-colon-before-close? p))

            ;; let/for/loop/binding/with-open etc.: header MUST start with '[...]'.
            ;; Additionally require colon AFTER the matching ']' to avoid triggering
            ;; when the keyword is used as a variable: 'binding [:v :s])' has no ':' after ']'.
            (contains? bracket-first-blocks block-kind)
            (and (tok-type? next :open-bracket)
                 (has-colon-before-close? p))

            ;; fn/fn*: params vector '[...]' must immediately follow (with optional name/meta).
            ;; Prevents fn* used as a local variable from triggering fn block detection:
            ;;   fn* and args* :   → next is 'and' (infix operator) → not a fn block
            ;;   fn* [p1 p2]:      → next is '[' → is a fn block
            ;;   fn* name [p]:     → next is symbol, peek+1 is '[' → is a fn block
            (= :fn-block block-kind)
            (and (or (tok-type? next :open-bracket)
                     (tok-type? next :meta)
                     (and (tok-type? next :symbol)
                          (let [nt2 (ppeek p 1)]
                            (or (tok-type? nt2 :open-bracket)
                                (tok-type? nt2 :meta)))))
                 (has-colon-before-close? p))

            ;; ns block: first token must look like a namespace name (or its metadata), not an operator.
            ;; Additionally, no call-parens '(' may appear at depth 0 before the ':' —
            ;; a genuine ns form has only a name + optional docstring/attr-map before ':'.
            ;; Prevents 'ns' used as a local variable from triggering ns block detection:
            ;;   ns = table or ...         → next is '=' (infix operator) → rejected
            ;;   ns symbol(str(fm))...:    → '(' at depth 0 before ':' → rejected
            ;;   ns my.namespace:          → non-operator symbol, no '(' before ':' → allowed
            ;;   ns ^:no-doc my.ns:        → next is '^' (meta), no '(' before ':' → allowed
            (= :ns-block block-kind)
            (and (or (tok-type? next :meta)
                     (and (tok-type? next :symbol)
                          (not (contains? @ops/*surface-index* (:value next)))))
                 ;; scan: find ':' at depth 0 without encountering '(' at depth 0 first
                 (loop [i 0 depth 0]
                   (let [t (ppeek p i)]
                     (cond
                       (nil? t) false
                       (zero? depth)
                       (cond
                         (colon-tok? t) true
                         (and (tok-type? t :symbol) (str/ends-with? (:value t) ":")) true
                         (tok-type? t :open-paren)    false  ; call before colon → not ns
                         (tok-type? t :close-paren)   false
                         (tok-type? t :close-bracket) false
                         (tok-type? t :close-brace)   false
                         (end-symbol? t)              false
                         (or (tok-type? t :open-bracket)
                             (tok-type? t :open-brace)
                             (tok-type? t :open-set))  (recur (inc i) (inc depth))
                         :else (recur (inc i) depth))
                       :else
                       (cond
                         (or (tok-type? t :open-paren)
                             (tok-type? t :open-bracket)
                             (tok-type? t :open-brace)
                             (tok-type? t :open-set)) (recur (inc i) (inc depth))
                         (or (tok-type? t :close-paren)
                             (tok-type? t :close-bracket)
                             (tok-type? t :close-brace)) (recur (inc i) (dec depth))
                         :else (recur (inc i) depth))))))

            ;; All other block kinds: colon before any close-delimiter.
            ;; has-colon-before-close? handles nested brackets, so
            ;; 'defn f [params]:' → true   (colon after name + params)
            ;; 'if cond :' → true           (colon in condition position)
            ;; '(ns-sym :)' → false         (in empty-header-blocks; handled above)
            :else
            (has-colon-before-close? p))))))

(defn- block-start?
  "True when the parser state after consuming a block keyword looks like a block form.
   Resolves the name to a qualified sym, looks up block-kind, delegates to block-kind->start?."
  [p kw-str colon-in-tok?]
  (let [qsym (resolve-block-qsym p kw-str)]
    (block-kind->start? p (when qsym (get @block-dispatch qsym)) colon-in-tok?)))

(defn- parse-match-block
  "Parse: match expr: pat1 => result1  pat2 => result2  [_ => default] end
   Compiles to (form-sym expr pat1 result1 pat2 result2 [:else default]).
   _ as pattern becomes :else (catch-all).
   Each arm: parse-form => parse-expr"
  [p form-sym tok]
  (let [raw-expr (parse-expr p)
        [expr colon-in-expr] (strip-expr-colon raw-expr)
        _ (consume-colon! p colon-in-expr "match" (select-keys tok [:line :col]))]
    (loop [clauses []]
      (cond
        (peof? p)
        (errors/reader-error "Unclosed match block — expected 'end'"
                             (error-data p (assoc (plast-loc p) :incomplete true)))

        (end-symbol? (ppeek p))
        (do (padvance! p)
            (apply list form-sym expr (apply concat clauses)))

        :else
        (let [pat-form (parse-expr p)
              _ (when-not (arrow-tok? (ppeek p))
                  (errors/reader-error "Expected '=>' in match clause"
                                       (error-data p (plast-loc p))))
              _ (padvance! p)
              result-form (parse-expr p)
              pat-key (if (= '_ pat-form) :else pat-form)]
          (recur (conj clauses [pat-key result-form])))))))

(defn- parse-block-by-kind
  "Dispatch block parsing given an explicit block-kind and resolved form symbol.
   Used by both parse-block (built-in dispatch) and the :resolve-var hook path."
  [p block-kind form-sym tok colon-in-kw?]
  (case block-kind
    :defn-block  (parse-defn-block  p form-sym tok)
    :fn-block    (parse-fn-block    p form-sym tok colon-in-kw?)
    :if-block    (parse-if-block    p form-sym tok)
    :when-block  (parse-if-block    p form-sym tok)
    :let-block   (parse-let-block   p form-sym tok)
    :cond-block  (parse-cond-block  p form-sym tok colon-in-kw?)
    :case-block  (parse-case-block  p form-sym tok)
    :match-block      (parse-match-block      p form-sym tok)
    :try-block        (parse-try-block        p form-sym tok colon-in-kw?)
    :for-block        (parse-for-block        p form-sym tok)
    :ns-block         (parse-ns-block         p form-sym tok)
    :def-block        (parse-def-block        p form-sym tok)
    :defmethod-block  (parse-defmethod-block  p form-sym tok)
    :defprotocol-block (parse-defprotocol-block p form-sym tok)
    :defrecord-block  (parse-defrecord-block  p form-sym tok)
    :reify-block      (parse-reify-block      p form-sym tok colon-in-kw?)
    :proxy-block      (parse-proxy-block      p form-sym tok)
    ;; Unknown block kind — fall through to call
    (maybe-call p form-sym)))

(defn- parse-block
  "Dispatch to the right block parser. Resolves kw-str to a qualified symbol,
   looks up block-kind, and calls parse-block-by-kind."
  ([p kw-str tok] (parse-block p kw-str tok false))
  ([p kw-str tok colon-in-kw?]
   (let [qsym (or (resolve-block-qsym p kw-str) (symbol kw-str))
         kind (get @block-dispatch qsym)]
     (parse-block-by-kind p kind (symbol (name qsym)) tok colon-in-kw?))))

;; ---------------------------------------------------------------------------
;; Surface syntax: let-statement sugar
;; ---------------------------------------------------------------------------

(def ^:private let-stmt-tag ::let-stmt)

(defn- let-stmt? [form]
  (and (vector? form) (= let-stmt-tag (first form))))

(defn- wrap-body-lets
  "Collect consecutive ::let-stmt sentinels into (let [...] body...).
   Called by parse-body; declared above for forward reference."
  [forms]
  (loop [remaining (vec forms) bindings []]
    (if (empty? remaining)
      (if (empty? bindings)
        []
        [(apply list 'let bindings)])
      (let [f (first remaining)]
        (if (let-stmt? f)
          (recur (subvec remaining 1) (into bindings (rest f)))
          (if (empty? bindings)
            remaining
            [(apply list 'let bindings (vec remaining))]))))))

;; ---------------------------------------------------------------------------
;; Surface syntax: infix pratt-climbing
;; ---------------------------------------------------------------------------

(defn- comparison-op?
  "True when sym (qualified or unqualified) is a registered comparison operator."
  [sym]
  (when (symbol? sym)
    (boolean
     (or (:comparison (get @ops/*op-registry* sym))
         (when (nil? (namespace sym))
           (:comparison (get @ops/*op-registry* (symbol "clojure.core" (name sym)))))))))

(defn- build-binary
  "Combine left op right, handling:
   - Variadic flattening:  (+ a b) + c → (+ a b c)
   - Comparison chains:    (< a b) < c → (and (< a b) (< b c))
   - Chain extension: (and ... (< b c)) < d → (and ... (< b c) (< c d))
   qsym is the qualified registry key; output forms use (symbol (name qsym))."
  [qsym entry left right]
  (let [op (symbol (name qsym))]
    (cond
      ;; Variadic flattening: (+ a b) + c → (+ a b c)
      (and (:variadic entry)
           (seq? left) (= (first left) op))
      (apply list op (concat (rest left) [right]))

      ;; Start a comparison chain: (< a b) < c → (and (< a b) (< b c))
      (and (:comparison entry)
           (seq? left) (comparison-op? (first left)) (= 3 (count left)))
      (let [mid (nth left 2)]
        (list 'and left (list op mid right)))

      ;; Extend comparison chain: (and ... (< b c)) < d → (and ... (< b c) (< c d))
      (and (:comparison entry)
           (seq? left) (= 'and (first left))
           (let [lc (last left)]
             (and (seq? lc) (comparison-op? (first lc)) (= 3 (count lc)))))
      (let [last-cmp (last left)
            mid      (nth last-cmp 2)]
        (apply list 'and (concat (rest left) [(list op mid right)])))

      :else
      (list op left right))))

(defn- pratt-climb
  "Pratt precedence-climbing. Consumes infix operators while prec >= min-prec.
   Resolves operator surface strings to qualified symbols via :resolve-sym opts
   (namespace-aware, JVM only) or falls back to the static *surface-index*.
   Produces forms with fully-qualified operator heads."
  [p left min-prec]
  (if @(:sexp-mode p)
    left ; no infix inside quoted lists
    (loop [left left]
      (let [tok      (ppeek p)
            next-tok (ppeek p 1)]
        (if-not (infix-tok? tok next-tok)
          left
          (let [op-str      (:value tok)
                resolve-sym (:resolve-sym (:opts p))
                ;; Primary: namespace-aware resolution via :resolve-sym hook
                ;; Secondary: static surface-index fallback
                qsym        (or (when resolve-sym
                                  (let [q (resolve-sym op-str)]
                                    (when (and q (get @ops/*op-registry* q)) q)))
                                (get @ops/*surface-index* op-str))
                entry       (when qsym (get @ops/*op-registry* qsym))
                prec        (or (:prec entry) 0)]
            (if (< prec min-prec)
              left
              (do
                (padvance! p)
                (let [right-min-prec (if (= :right (:assoc entry)) prec (inc prec))
                      right          (parse-expr p right-min-prec)
                      result         (if-let [expander (:expander entry)]
                                       (expander left right)
                                       (build-binary qsym entry left right))]
                  (recur result))))))))))

(defn parse-expr
  "Parse a full surface expression: a form plus any infix operators."
  ([p]         (pratt-climb p (parse-form p) 0))
  ([p min-prec] (pratt-climb p (parse-form p) min-prec)))

;; ---------------------------------------------------------------------------
;; Surface syntax: let x := expr sugar
;; ---------------------------------------------------------------------------

(defn- maybe-parse-let-stmt
  "After parsing a symbol, check for ':=' — if found, parse as let-statement.
   Returns a ::let-stmt vector sentinel, or the bare symbol if no ':='.
   Only fires for unqualified, non-operator symbols — quoted operator values like
   '= and qualified symbols like clojure.core/= in map literals must not be treated
   as let-binding variables."
  [p sym]
  (if (and (bind-op? (ppeek p))
           (symbol? sym)
           (nil? (namespace sym))
           (not (contains? @ops/*surface-index* (name sym))))
    (do (padvance! p) ; consume :=
        (let [val (parse-expr p)]
          [let-stmt-tag sym val]))
    sym))

;; ---------------------------------------------------------------------------

(defn- parse-form-base
  "Parse a single superficie form."
  [p]
  (let [tok (ppeek p)]
    (when-not tok
      (errors/reader-error "Unexpected end of input — expected a form" (error-data p (assoc (plast-loc p) :incomplete true))))
    (case (:type tok)
      :symbol
      (let [s (:value tok)]
        (padvance! p)
        (case s
          "nil" nil
          "true" true
          "false" false
          ;; Constructor: new ClassName(args) → (ClassName. args)
          ;; Requires ClassName immediately followed by ( — two-token lookahead
          ;; prevents consuming plain symbols like in [entry old new diff state]
          "new"
          (let [class-tok (ppeek p)
                paren-tok (ppeek p 1)]
            (if (and (tok-type? class-tok :symbol)
                     (tok-type? paren-tok :open-paren)
                     (not (:ws paren-tok)))
              (do
                (padvance! p)
                (let [class-sym (symbol (str (:value class-tok) "."))
                      args (parse-call-args p)]
                  (apply list class-sym args)))
              ;; fallback: 'new' as a regular symbol
              (maybe-parse-let-stmt p (maybe-call p 'new))))
          ;; block keywords not followed by adjacent ( → block form
          ;; strip trailing colon (e.g. "defn:" → "defn") for dispatch
          ;; Special case: "let x := expr" sugar — detected before block dispatch
          (let [[base-str colon-in-tok?] (strip-trailing-colon s)
                dot-idx (str/last-index-of s ".")]
            (cond
              ;; Interop field access: obj.-field → (.-field obj)
              ;; Matches symbols like "point.-x" where ".-" appears after a prefix
              (and (some? dot-idx) (pos? dot-idx)
                   (str/includes? s ".-")
                   (not (str/starts-with? s ".-")))
              (let [obj-str (subs s 0 (str/last-index-of s ".-"))
                    field-str (subs s (str/last-index-of s ".-"))]
                (list (symbol field-str) (symbol obj-str)))

              ;; Interop method call: obj.method(args) → (.method obj args)
              ;; Only when adjacent to ( and symbol contains internal dot (not starting/ending with dot)
              (and (some? dot-idx) (pos? dot-idx)
                   (not (str/ends-with? s "."))
                   (not (str/starts-with? s "."))
                   (not (str/includes? s "/"))
                   (adjacent-open-paren? p))
              (let [obj-str (subs s 0 dot-idx)
                    method-str (str "." (subs s (inc dot-idx)))
                    args (parse-call-args p)]
                (apply list (symbol method-str) (symbol obj-str) args))

              ;; let-stmt sugar: let varname := expr
              (and (= "let" base-str)
                   (not (adjacent-open-paren? p))
                   (tok-type? (ppeek p) :symbol)
                   (bind-op? (ppeek p 1)))
              (let [var-sym (symbol (:value (ppeek p)))]
                (padvance! p) ; consume the variable name
                (maybe-parse-let-stmt p var-sym))

              :else
              ;; Block dispatch: resolve bare name to qualified symbol via
              ;; :resolve-sym hook (namespace-aware) or static block-name->sym
              ;; fallback. :no-block and sexp-mode suppress in quoted contexts.
              (let [suppress? (or (adjacent-open-paren? p)
                                  @(:no-block p)
                                  @(:sexp-mode p))
                    qsym      (when-not suppress? (resolve-block-qsym p base-str))
                    kind      (when qsym (get @block-dispatch qsym))]
                (if (and kind (block-kind->start? p kind colon-in-tok?))
                  (parse-block-by-kind p kind (symbol (name qsym)) tok colon-in-tok?)
                  (maybe-parse-let-stmt p (maybe-call p (symbol s)))))))))

      :keyword
      (let [v (:value tok)]
        (padvance! p)
        (if (str/starts-with? v "::")
          ;; Auto-resolve keywords — resolve at read time if resolver available,
          ;; otherwise defer to eval time via read-string wrapper
          (let [resolve-kw (:resolve-keyword (:opts p))]
            (maybe-call p (resolve/resolve-auto-keyword v (select-keys tok [:line :col]) resolve-kw)))
          (let [s (subs v 1)
                i (str/index-of s "/")]
            (maybe-call p (if (and (some? i) (pos? i))
                            (keyword (subs s 0 i) (subs s (inc i)))
                            (keyword s))))))

      :number
      (do (padvance! p)
          (resolve/resolve-number (:value tok) (select-keys tok [:line :col])))

      :string
      (do (padvance! p)
          (resolve/resolve-string (:value tok) (select-keys tok [:line :col])))

      :char
      (do (padvance! p)
          (resolve/resolve-char (:value tok) (select-keys tok [:line :col])))

      :regex
      (do (padvance! p)
          (resolve/resolve-regex (:value tok) (select-keys tok [:line :col])))

      :open-paren
      (let [loc (select-keys tok [:line :col])]
        (padvance! p)
        (cond
          ;; Empty list ()
          (tok-type? (ppeek p) :close-paren)
          (do (padvance! p) (list))

          ;; sexp-mode: inside '(...) — create a quoted list
          @(:sexp-mode p)
          (apply list (parse-forms-until p :close-paren loc))

          ;; Normal mode: grouping parens for infix — (a + b) * c
          :else
          (let [inner (parse-expr p)]
            (if (tok-type? (ppeek p) :close-paren)
              (do (padvance! p) inner)
              (errors/reader-error
               "Parenthesized group must contain a single expression — use f(args) for calls"
               (error-data p loc))))))

      :open-bracket (maybe-call p (parse-vector p))
      :open-brace (maybe-call p (parse-map p))
      :open-set (maybe-call p (parse-set p))

      :deref
      (do (padvance! p)
          (let [inner (parse-form p)]
            (when (discard-sentinel? inner)
              (errors/reader-error "Deref target was discarded by #_ — nothing to dereference"
                                   (error-data p (select-keys tok [:line :col]))))
            (with-meta (list 'clojure.core/deref inner) {:sup/sugar true})))

      :meta
      (do (padvance! p)
          (let [m (parse-form p)
                _ (when (discard-sentinel? m)
                    (errors/reader-error "Metadata value was discarded by #_ — nothing to attach as metadata"
                                         (error-data p (select-keys tok [:line :col]))))
                target (parse-form p)
                _ (when (discard-sentinel? target)
                    (errors/reader-error "Metadata target was discarded by #_ — nothing to attach metadata to"
                                         (error-data p (select-keys tok [:line :col]))))]
            (let [entry (cond
                          (keyword? m) {m true}
                          (symbol? m)  {:tag m}
                          (map? m)     m
                          :else
                          (errors/reader-error
                           (str "Metadata must be a keyword, symbol, or map — got " (pr-str m))
                           (error-data p (select-keys tok [:line :col]))))
                  chain (conj (or (:sup/meta-chain (meta target)) []) entry)]
              (vary-meta target merge entry {:sup/meta-chain chain}))))

      :quote
      ;; ' quotes the next superficie form.
      ;; '(a b c) — enter sexp-mode so inner (...) creates lists instead of errors.
      ;; 'f(x) quotes the call (f x), '() quotes the empty list.
      ;; For 'sym (non-list), set :no-block so 'if/'when/'defn don't start blocks.
      (do (padvance! p)
          (let [sexp? (tok-type? (ppeek p) :open-paren)
                _ (when sexp? (vreset! (:sexp-mode p) true))
                _ (when-not sexp? (vreset! (:no-block p) true))
                inner (try (parse-form p)
                           (finally
                             (when sexp? (vreset! (:sexp-mode p) false))
                             (when-not sexp? (vreset! (:no-block p) false))))]
            (when (discard-sentinel? inner)
              (errors/reader-error "Quote target was discarded by #_ — nothing to quote"
                                   (error-data p (select-keys tok [:line :col]))))
            (with-meta (list 'quote inner) {:sup/sugar true})))

      :syntax-quote
      ;; ` — parse next form with sup rules, preserve as AST node.
      ;; Expansion to seq/concat/list happens at eval time, not read time.
      (do (padvance! p)
          (vswap! (:sq-depth p) inc)
          (let [form (try (parse-form p)
                          (finally (vswap! (:sq-depth p) dec)))]
            (when (discard-sentinel? form)
              (errors/reader-error "Syntax-quote target was discarded by #_ — nothing to syntax-quote"
                                   (error-data p (select-keys tok [:line :col]))))
            (forms/->SupSyntaxQuote form)))

      :unquote
      (if (pos? @(:sq-depth p))
        (do (padvance! p)
            (let [inner (parse-form p)]
              (when (discard-sentinel? inner)
                (errors/reader-error "Unquote target was discarded by #_ — nothing to unquote"
                                     (error-data p (select-keys tok [:line :col]))))
              (forms/->SupUnquote inner)))
        (errors/reader-error "Unquote (~) outside syntax-quote — ~ only has meaning inside `"
                             (error-data p (select-keys tok [:line :col]))))

      :unquote-splicing
      (if (pos? @(:sq-depth p))
        (do (padvance! p)
            (let [inner (parse-form p)]
              (when (discard-sentinel? inner)
                (errors/reader-error "Unquote-splicing target was discarded by #_ — nothing to unquote-splice"
                                     (error-data p (select-keys tok [:line :col]))))
              (forms/->SupUnquoteSplicing inner)))
        (errors/reader-error "Unquote-splicing (~@) outside syntax-quote — ~@ only has meaning inside `"
                             (error-data p (select-keys tok [:line :col]))))

      :var-quote
      (do (padvance! p)
          (let [inner (parse-form p)]
            (when (discard-sentinel? inner)
              (errors/reader-error "Var-quote target was discarded by #_ — nothing to reference"
                                   (error-data p (select-keys tok [:line :col]))))
            (with-meta (list 'var inner) {:sup/sugar true})))

      :discard
      ;; #_ discards the next form and, if a non-boundary form follows,
      ;; returns it as this expression's value — so prefix operators
      ;; (@, ^, #', #()) transparently skip over #_-discarded forms.
      ;; At a boundary (EOF / closing delimiter), returns discard-sentinel
      ;; so callers that accumulate forms skip the gap.
      ;; #_ #_ chains work because each outer #_ discards the return
      ;; value of the inner #_ (itself a form or sentinel) and recurses.
      (do (padvance! p)
          (when (peof? p)
            (errors/reader-error "Missing form after #_ — expected a form to discard"
                                 (error-data p (assoc (select-keys tok [:line :col]) :incomplete true))))
          (parse-form p) ; parse and discard
          (let [nxt (ppeek p)]
            (if (or (nil? nxt)
                    (#{:close-paren :close-bracket :close-brace} (:type nxt)))
              discard-sentinel
              (parse-form p))))

      :tagged-literal
      (let [tag (symbol (subs (:value tok) 1))]
        (padvance! p)
        (let [data (parse-form p)]
          (when (discard-sentinel? data)
            (errors/reader-error (str "Tagged literal #" tag " value was discarded by #_ — tagged literal requires a value")
                                 (error-data p (select-keys tok [:line :col]))))
          (resolve/resolve-tagged-literal tag data (select-keys tok [:line :col]))))

      :namespaced-map-start
      ;; #:ns{...} — parse map natively, apply namespace prefix to bare keys
      (let [prefix (:value tok) ; e.g. "#:user" or "#::foo"
            ns-str (subs prefix 2) ; strip "#:"
            auto? (str/starts-with? ns-str ":")
            ns-name (if auto? (subs ns-str 1) ns-str)]
        (padvance! p)
        (when-not (tok-type? (ppeek p) :open-brace)
          (errors/reader-error (str "Expected { after " prefix)
                               (error-data p (select-keys tok [:line :col]))))
        (let [m (parse-map p) ; parse the {map} directly, no maybe-call
              apply-ns (fn [k]
                         (if (and (keyword? k) (nil? (namespace k)))
                           (keyword ns-name (name k))
                           k))
              nsed (into {} (map (fn [[k v]] [(apply-ns k) v]) m))]
          (let [tagged (vary-meta nsed assoc :sup/ns (if auto? (str ":" ns-name) ns-name))]
            (maybe-call p tagged))))

      :open-anon-fn
      ;; #() — parse body as sup, collect % params, emit (fn [params] body)
      (do (padvance! p)
          (let [body (parse-form p)
                _ (when (discard-sentinel? body)
                    (errors/reader-error "#() body was discarded — #() requires a non-discarded expression"
                                         (error-data p (select-keys tok [:line :col]))))
                nxt (ppeek p)]
            (cond
              (nil? nxt)
              (errors/reader-error "Unterminated #() — expected closing )"
                                   (error-data p (assoc (select-keys tok [:line :col]) :incomplete true)))

              (not (tok-type? nxt :close-paren))
              (errors/reader-error "#() body must be a single expression — use fn(args...) for multiple expressions"
                                   (error-data p (assoc (select-keys nxt [:line :col])
                                                        :secondary [{:line (:line tok) :col (:col tok) :label "#( opened here"}]))))
            (padvance! p)
            (let [params (find-percent-params body)
                  _ (when (contains? params 0)
                      (errors/reader-error "%0 is not a valid parameter — use %1 or % for the first argument"
                                           (error-data p (select-keys tok [:line :col]))))
                  param-vec (build-anon-fn-params params)
                  body' (normalize-bare-percent body)]
              (with-meta (list 'fn param-vec body') {:sup/sugar true}))))

      :reader-cond-start
      ;; #?(...) or #?@(...) — parse natively.
      ;; With :read-cond :preserve, returns a ReaderConditional object.
      ;; Otherwise (default), evaluates and returns the matching platform's form.
      (let [prefix (:value tok)
            splice? (= prefix "#?@")]
        (padvance! p)
        (when-not (tok-type? (ppeek p) :open-paren)
          (errors/reader-error (str "Expected ( after " prefix)
                               (error-data p (select-keys tok [:line :col]))))
        (let [loc (select-keys (ppeek p) [:line :col])]
          (padvance! p) ; consume (
          (if (= :preserve (:read-cond (:opts p)))
            (parse-reader-cond-preserve p loc splice?)
            (parse-reader-cond-eval p loc splice?))))

      ;; default
      (errors/reader-error (str "Unexpected " (describe-token tok))
                           (error-data p (select-keys tok [:line :col]))))))

(defn- metadatable?
  "Can this value carry Clojure metadata?"
  [x]
  #?(:clj  (instance? clojure.lang.IObj x)
     :cljs (implements? IWithMeta x)))

(defn- attach-ws
  "Attach :ws metadata from a token to a form, if the form supports metadata."
  [form ws]
  (if (and ws (metadatable? form))
    (vary-meta form assoc :ws ws)
    form))

(defn- adjacent-dot-method?
  "True if next token is an adjacent symbol starting with '.' but not '.-'.
   Used for postfix method chaining: obj.method(args)."
  [p]
  (let [tok (ppeek p)]
    (and (tok-type? tok :symbol)
         (not (:ws tok))
         (let [v (:value tok)]
           (and (str/starts-with? v ".") (not (str/starts-with? v ".-")) (> (count v) 1))))))

(defn- adjacent-dot-field?
  "True if next token is an adjacent symbol starting with '.-'.
   Used for postfix field access: obj.-field."
  [p]
  (let [tok (ppeek p)]
    (and (tok-type? tok :symbol)
         (not (:ws tok))
         (str/starts-with? (:value tok) ".-"))))

(defn- parse-call-chain
  "After parsing a form, check for chained call openers and postfix interop.
   Handles: f(x)(y)(z), obj.method(args), obj.-field chaining.
   Skipped for discard sentinels, splice results, and sexp-mode.
   Requires adjacent tokens (no whitespace)."
  [p form]
  (cond
    (or (discard-sentinel? form) (splice-result? form) @(:sexp-mode p))
    form

    ;; f(x)(y) → ((f x) y) chaining
    (adjacent-open-paren? p)
    (let [args (parse-call-args p)]
      (recur p (apply list form args)))

    ;; postfix method: form.method(args) → (.method form args)
    (adjacent-dot-method? p)
    (let [method-sym (symbol (:value (ppeek p)))]
      (padvance! p)
      (let [args (if (adjacent-open-paren? p) (parse-call-args p) [])]
        (recur p (apply list method-sym form args))))

    ;; postfix field: form.-field → (.-field form)
    (adjacent-dot-field? p)
    (let [field-sym (symbol (:value (ppeek p)))]
      (padvance! p)
      (recur p (list field-sym form)))

    :else form))

(defn- parse-form
  "Parse a single surface form.
   Attaches :ws (leading whitespace) and :line/:column source location
   as metadata on forms that support it."
  [p]
  (let [start-tok (ppeek p)
        ws        (:ws start-tok)
        depth     (vswap! (:depth p) inc)]
    (try
      (when (> depth max-depth)
        (errors/reader-error (str "Maximum nesting depth (" max-depth ") exceeded — input is too deeply nested")
                             (error-data p (merge {:depth depth} (when-let [tok (ppeek p)]
                                                                   (select-keys tok [:line :col]))))))
      (let [form  (parse-form-base p)
            form' (parse-call-chain p form)
            form' (attach-ws form' ws)]
        ;; Attach source location to any form that supports metadata.
        ;; Uses :line (1-indexed) and :column (1-indexed) to match
        ;; clojure.lang.LispReader and tooling conventions.
        (if (and start-tok
                 (not (discard-sentinel? form'))
                 #?(:clj  (instance? clojure.lang.IMeta form')
                    :cljs (satisfies? IMeta form')))
          (vary-meta form' merge
                     {:line   (:line start-tok)
                      :column (:col  start-tok)})
          form'))
      (finally
        (vswap! (:depth p) dec)))))

;; ---------------------------------------------------------------------------
;; Syntax-quote expansion — re-exported from parse.expander for compatibility
;; ---------------------------------------------------------------------------

(def expand-syntax-quotes
  "Walk a form tree and expand all AST nodes into plain Clojure forms.
   Delegates to superficie.parse.expander/expand-syntax-quotes."
  expander/expand-syntax-quotes)

(def expand-forms
  "Expand all syntax-quote nodes in a vector of forms. For eval pipelines.
   Delegates to superficie.parse.expander/expand-forms."
  expander/expand-forms)

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn parse-tokens
  "Parse pre-tokenized, pre-grouped tokens into Clojure forms.
   Used by the pipeline; most callers should use superficie.core/sup->forms instead."
  ([tokens] (parse-tokens tokens nil nil))
  ([tokens opts] (parse-tokens tokens opts nil))
  ([tokens opts source]
   (let [p (make-parser tokens opts source)
         trailing (:trailing-ws (meta tokens))
         raw-forms (loop [forms []]
                     (if (peof? p)
                       forms
                       (let [form (parse-expr p)]
                         (cond
                           (discard-sentinel? form) (recur forms)
                           (splice-result? form) (recur (into forms form))
                           :else (recur (conj forms form))))))]
     (cond-> (wrap-body-lets raw-forms)
       trailing (with-meta {:trailing-ws trailing})))))

;; ---------------------------------------------------------------------------
;; Streaming API — form-by-form reading
;; ---------------------------------------------------------------------------

(defn streaming-parser
  "Create a parser for form-by-form reading via parse-next! / parser-done?.
   Used by the runtime to interleave parsing and evaluation."
  ([tokens] (make-parser tokens nil nil))
  ([tokens opts] (make-parser tokens opts nil))
  ([tokens opts source] (make-parser tokens opts source)))

(defn parser-done?
  "Returns true when the parser has consumed all tokens."
  [p]
  (peof? p))

(defn parse-next!
  "Read one top-level form from the streaming parser.
   Returns the form, or the discard sentinel for #_ forms (check with discarded?)."
  [p]
  (parse-expr p))

(defn discarded?
  "True if the value returned by parse-next! was a #_ discarded form."
  [v]
  (discard-sentinel? v))
