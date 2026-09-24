(ns superficie.emit.printer
  "Form printer: Clojure forms → superficie or Clojure text.
   Mode :sup (default) emits head(args...) call syntax.
   Mode :clj emits (head args...) S-expression syntax with reader sugar."
  (:require [clojure.string :as str]
            [superficie.forms :as forms]
            [superficie.operators :as ops]
            [superficie.shapes :as shapes]))

;; ---------------------------------------------------------------------------
;; Forward declaration
;; ---------------------------------------------------------------------------

(declare print-form)

(defn- reader-anon-param
  "For a parameter the Clojure reader generates for #(…) — p1__123#, p2__123#,
   rest__123# — the % symbol it stands for; else nil. Only the #() reader makes
   these names, so this identifies an anonymous-fn literal exactly."
  [sym]
  (when (symbol? sym)
    (let [n (name sym)]
      (if-let [[_ i] (re-matches #"p(\d+)__\d+#" n)]
        (symbol (str "%" i))
        (when (re-matches #"rest__\d+#" n) '%&)))))

(defn- rename-syms
  "form with every symbol s replaced by (f s), inside collections and the
   syntax-quote records alike; metadata is kept."
  [f form]
  (let [walk #(rename-syms f %)
        keep-meta (fn [y] (if (meta form) (with-meta y (meta form)) y))]
    (cond
      (symbol? form) (f form)
      (record? form) (reduce-kv (fn [r k v] (assoc r k (walk v))) form form)
      (seq? form) (keep-meta (apply list (map walk form)))
      (vector? form) (keep-meta (mapv walk form))
      (map? form) (keep-meta (into {} (map (fn [[k v]] [(walk k) (walk v)])) form))
      (set? form) (keep-meta (into #{} (map walk) form))
      :else form)))

(defn clj-anon-fn
  "If form is a (fn* [p1__N# …] body) the Clojure reader made from #(…), the
   same function written with % parameters, as superficie's reader builds it:
   (fn [%1 …] body[%1 …]), tagged :sup/sugar. Else nil."
  [form]
  (when (and (seq? form) (= 'fn* (first form)) (= 3 (count form))
             (vector? (second form)))
    (let [params (second form)
          named (remove #{'&} params)]
      (when (and (seq named) (every? reader-anon-param named))
        (let [rename (into {} (map (fn [p] [p (reader-anon-param p)]) named))]
          (with-meta (list 'fn (mapv #(get rename % %) params)
                           (rename-syms #(get rename % %) (nth form 2)))
            {:sup/sugar true}))))))

;; ---------------------------------------------------------------------------
;; Mode — :sup or :clj
;; ---------------------------------------------------------------------------

(def ^:dynamic *mode* :sup)

;; ---------------------------------------------------------------------------
;; Print helpers
;; ---------------------------------------------------------------------------

(def ^:dynamic *arrow-ok*
  "True while printing a call argument directly: there a lambda may print as
   `x -> body`, which a comma or ')' ends. Reset for anything nested."
  false)

(def ^:dynamic *arrow-here*
  "*arrow-ok* as it was for the form being printed now."
  false)

(def ^:dynamic *stmt-ok*
  "True while printing a body statement directly: there a store prints bare,
   `x[i] <- v`. Reset for anything nested."
  false)

(def ^:dynamic *stmt-here*
  "*stmt-ok* as it was for the form being printed now."
  false)

(defn- print-args
  "Print function arguments. Superficie uses familiar comma separators;
   Clojure output retains conventional whitespace separators."
  [forms]
  (str/join (if (= *mode* :sup) ", " " ")
            (map #(binding [*arrow-ok* (= *mode* :sup)] (print-form %)) forms)))

(defn- percent-param?
  "Is sym a % parameter symbol (%1, %2, %&)?"
  [sym]
  (some? (forms/percent-param-type sym)))

(defn- max-percent-n
  "Find the max numbered %N param index referenced in a form body.
   Returns max N found (0 if none). Ignores %& (rest params).
   Skips nested (fn ...) bodies — their % params are scoped to the inner fn."
  [form]
  (cond
    (symbol? form)
    (let [p (forms/percent-param-type form)]
      (if (number? p) p 0))
    (and (seq? form) (= 'fn (first form))) 0
    (seq? form) (reduce max 0 (map max-percent-n form))
    (vector? form) (reduce max 0 (map max-percent-n form))
    ;; AST node defrecords satisfy (map? x) — check before map?
    (forms/raw? form) 0
    (forms/syntax-quote? form) (max-percent-n (:form form))
    (forms/unquote? form) (max-percent-n (:form form))
    (forms/unquote-splicing? form) (max-percent-n (:form form))
    (map? form) (reduce max 0 (mapcat (fn [[k v]] [(max-percent-n k) (max-percent-n v)]) form))
    (set? form) (reduce max 0 (map max-percent-n form))
    (tagged-literal? form) (max-percent-n (.-form form))
    :else 0))

;; ---------------------------------------------------------------------------
;; #() shorthand detection
;; ---------------------------------------------------------------------------

(defn- anon-fn-shorthand?
  "Can (fn [params] body) be printed as #(body)?
   Only when :sup/sugar tagged by reader (i.e., originally written as #())."
  [form]
  (and (:sup/sugar (meta form))
       (seq? form)
       (= 'fn (first form))
       (= 3 (count form))
       (vector? (second form))))

;; ---------------------------------------------------------------------------
;; Surface syntax: infix and block emission (sup mode)
;; ---------------------------------------------------------------------------

(def ^:dynamic *indent* "")

(def block-dispatch
  "Extensible map: qualified-symbol → block-kind keyword.
   Extended at runtime (e.g. by register-block! after eval).
   Keyed by fully-qualified symbols; block-kind-for handles unqualified fallback."
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
         'clojure.core/ns         :ns-block
         'clojure.core/def        :def-block
         'clojure.core/defonce    :def-block
         'clojure.core/defmulti   :def-block
         'clojure.core/try        :try-block
         'clojure.core/for        :for-block
         'clojure.core/doseq      :for-block
         'clojure.core/loop       :for-block
         'clojure.core/dotimes    :for-block
         'clojure.core/defmethod   :defmethod-block
         'clojure.core/defprotocol :defprotocol-block
         'clojure.core/defrecord   :defrecord-block
         'clojure.core/deftype     :defrecord-block
         'clojure.core/reify       :reify-block
         'clojure.core/proxy       :proxy-block}))

(def ^:dynamic *width*
  "Target line width, bound by the pretty-printer. nil (plain printing)
   keeps every binding vector on one line."
  nil)

(def ^:dynamic *dotted-calls*
  "True while printing inside a block whose shape has :dotted-calls: there
   A.b(x) reads as the plain call (A.b x), so dotted heads print unwrapped and
   Java method calls must use the explicit .b(A, x) form."
  false)

(def ^:dynamic *match-arms*
  "True inside a block whose shape has :match-arms: a match whose clauses are
   [pattern body] vectors (ansatz's pattern form) prints them as `| pattern => body`
   arms instead of core.match's `pattern => result` pairs."
  false)

(def ^:dynamic *index-sym*
  "{:get f :set g} inside the body of a block whose shape has :index:
   (aget x i) prints as x[i], (aset x i v) as x[i] <- v. nil elsewhere."
  nil)

(def ^:dynamic *in-quote*
  "True while printing the target of a quote. The reader parses no block
   syntax inside '..., so neither may the printer."
  false)

(defn block-kind-for
  "Return the block-kind for a symbol head, or nil (always nil inside a quote).
   Resolves the head like the reader does: first through the file's ns context
   (aliases and refers, see superficie.shapes/*ns-context*), then as a
   clojure.core name. Heads with a registered shape get :shape-block."
  [head]
  (when (and (symbol? head) (not *in-quote*))
    (if-let [q (if (and *match-arms* (= 'match head))
                 ;; inside an ansatz block, match is ansatz's pattern match; the
                 ;; reader reads a bare `match x:` block with | arms
                 'clojure.core.match/match
                 (shapes/resolve-static head))]
      (or (get @block-dispatch q)
          (when (shapes/shape-for q) :shape-block))
      (or (get @block-dispatch head)
          (when (nil? (namespace head))
            (get @block-dispatch (symbol "clojure.core" (name head))))))))

(defn- infix-entry
  "Return the *op-registry* entry for head if it prints as an infix operator,
   else nil. Only bare heads qualify: the reader builds `a * b` with the bare
   symbol *, so (clojure.core/* a b) — which raster writes on purpose where *
   is its own generic function — must stay the call clojure.core/*(a, b)."
  [head]
  (when (and (symbol? head) (nil? (namespace head)))
    (let [entry (or (get @ops/*op-registry* head)
                    (get @ops/*op-registry* (symbol "clojure.core" (name head))))]
      (when (= :infix (:kind entry)) entry))))

(def ^:private block-terminator-syms
  "Bare symbols that the reader's parse-body treats as block terminators.
   These cannot appear as top-level expressions in a block body."
  #{'else 'end 'catch 'finally})

(declare print-form)

(declare operator-arg?)

(defn- join-forms
  "Print forms separated by spaces. After a bare `new` symbol use a comma:
   `[new List(x)]` would read as the constructor call `new List(x)`."
  [forms]
  (let [strs (mapv print-form forms)]
    (apply str (map-indexed (fn [i s]
                              (cond (zero? i) s
                                    (or (= 'new (nth forms (dec i)))
                                        ;; `[a, +, b]` — a comma keeps + from reading as infix
                                        (operator-arg? (nth forms i))
                                        (operator-arg? (nth forms (dec i))))
                                    (str ", " s)
                                    :else (str " " s)))
                            strs))))

(defn print-docstring
  "Print a docstring with its line breaks written out instead of as \\n
   escapes, so multi-line documentation reads as it does in source."
  [s]
  (if (str/includes? s "\n")
    (str "\""
         (str/join "\n" (map #(let [q (pr-str %)] (subs q 1 (dec (count q))))
                             (str/split s #"\n" -1)))
         "\"")
    (pr-str s)))

(declare block-kind-for)

(def ^:private reserved-words
  #{"nil" "true" "false" "end" "else" "catch" "finally" "new" "let" "fn" "do"})

(defn- fusable-trailing-symbol?
  "Can the ':' be written directly after the final word of expr-str?
   The tokenizer fuses `x:` into one symbol and every block header strips it
   again, but only a plain symbol survives that: not a number, keyword, literal
   lookalike, reserved or block word, dotted interop name, operator, or a word
   behind a sigil (' @ # ~ ^) or metadata."
  [expr-str]
  (when-let [[_ _ word] (re-find #"(^|[\s(\[{,])([A-Za-z_*!?<>=+$%&|][A-Za-z0-9_*!?<>=+$%&|'#/-]*|-[A-Za-z_*!?<>=+$%&|][A-Za-z0-9_*!?<>=+$%&|'#/-]*)$"
                                 expr-str)]
    (and (not (str/includes? word "."))
         (not (str/ends-with? word "/"))
         (not (contains? reserved-words word))
         (not (contains? @ops/*surface-index* word))
         ;; any word that can open a block, however it resolves
         (nil? (block-kind-for (symbol word)))
         (not-any? #(= word (name %)) (keys @block-dispatch))
         (not (shapes/shaped-name? word))
         (not (re-find #"\^\S*\s+$" (subs expr-str 0 (- (count expr-str) (count word))))))))

(defn- colon-sep
  "Append the block-opening ':' to a header. Written as `x:` after a plain
   symbol, otherwise separated by a space when it would fuse with the last
   token (e.g. `n > 0 :`, `:- :`)."
  [expr-str]
  (cond
    (fusable-trailing-symbol? expr-str) (str expr-str ":")
    (re-find #"[a-zA-Z0-9_!\?\*\-]$" expr-str) (str expr-str " :")
    :else (str expr-str ":")))

(defn- infix-print-arg
  "Print an infix operand; parenthesize when its op has lower precedence than threshold."
  [threshold arg]
  (let [inner-prec (when (and (seq? arg) (symbol? (first arg)))
                     (:prec (infix-entry (first arg))))]
    (if (and inner-prec (< inner-prec threshold))
      (str "(" (print-form arg) ")")
      (print-form arg))))

(defn- infix-operand-strs
  "The printed operands of an infix expression, each parenthesized exactly as
   the reader needs to rebuild the same form."
  [entry args head]
  (let [prec       (:prec entry 0)
        left-assoc? (not= :right (:assoc entry))
        ;; Right operands of left-associative ops need parens at equal precedence
        ;; to preserve left-to-right evaluation order.
        right-threshold (if left-assoc? (inc prec) prec)
        ;; comparisons chain (a < b < c reads as (and (< a b) (< b c))), so an
        ;; operand at comparison level needs parens on either side
        left-threshold (if (:comparison entry) (inc prec) prec)]
    (cons (let [a (first args)]
            ;; (* (* a b) c): the reader flattens a * b * c into (* a b c),
            ;; so a nested left operand of the same variadic op is grouped
            (if (and (:variadic entry) head (seq? a) (= head (first a)))
              (str "(" (print-form a) ")")
              (infix-print-arg left-threshold a)))
          (map #(infix-print-arg right-threshold %) (rest args)))))

(defn- print-infix [entry op-str args & [head]]
  (str/join (str " " op-str " ") (infix-operand-strs entry args head)))

(def ^:dynamic *body-form-printer*
  "When bound (by the pretty-printer) to (fn [form col] text), prints each
   block-body line width-aware; otherwise body lines are printed flat."
  nil)

(defn- print-body
  "Print forms as indented body lines (each on its own line)."
  [forms]
  (let [inner (str *indent* "  ")]
    (binding [*indent* inner]
      (str/join "\n" (map #(str inner (binding [*stmt-ok* true]
                                        (if *body-form-printer*
                                          (*body-form-printer* % (count inner))
                                          (print-form %))))
                          forms)))))

(defn- print-defn-block [head args]
  (let [head-str (clojure.core/name head)
        name-sym (first args)
        rest1    (rest args)
        [docstring rest2] (if (string? (first rest1))
                            [(first rest1) (rest rest1)]
                            [nil rest1])
        ;; Optional attr-map (metadata map) between docstring and params
        [attr-map rest3] (if (map? (first rest2))
                           [(first rest2) (rest rest2)]
                           [nil rest2])]
    (cond
      (and (seq rest3) (seq? (first rest3)) (vector? (first (first rest3))))
      ;; multi-arity: fall back to call syntax for roundtrippability.
      ;; Block syntax can't distinguish arity boundaries on re-parse.
      (str head-str "(" (print-args args) ")")
      ;; single arity with vector params (normal case, including ^Hint [params])
      ;; Exception: if name is "=>" the block-kind->start? arrow guard prevents detection → call syntax
      (and (seq rest3) (vector? (first rest3))
           (not (and (symbol? name-sym) (= "=>" (clojure.core/name name-sym)))))
      (let [[params & body] rest3
            multi-doc? (and docstring
                            (or (str/includes? docstring "\n")
                                (and *width*
                                     (> (+ (count *indent*) (count head-str) (count (print-form name-sym))
                                           (count (print-docstring docstring)) (count (print-form params)) 4)
                                        *width*))))
            cont (str "\n" *indent* "    ")
            ;; a multi-line docstring and the rest of the header go on hanging-indented
            ;; continuation lines (the reader reads a defn header across lines)
            doc-part (cond multi-doc? (str cont (print-docstring docstring))
                           docstring (str " " (print-docstring docstring))
                           :else "")
            attr-part (if attr-map (str " " (print-form attr-map)) "")
            params-sep (if multi-doc? cont " ")]
        (str head-str " " (print-form name-sym) doc-part attr-part params-sep (print-form params) ":\n"
             (print-body (vec body)) "\n"
             *indent* "end"))
      ;; params is not a vector — fall back to call syntax (e.g. fn used as local variable)
      :else
      (str head-str "(" (print-args args) ")"))))

(declare print-fn-block*)

(defn- arrow-lambda
  "`x -> body` / `(x, y) -> body` for (fn [params] body) in a call argument, when
   the parameters are plain names and the body stays on one line; else nil."
  [head args]
  (when (and *arrow-here* (= 'fn head) (= 2 (count args)) (vector? (first args)))
    (let [[params body] args]
      (when (and (every? #(and (symbol? %) (nil? (namespace %))
                               ;; a type hint (^long x) keeps the fn form
                               (empty? (forms/strip-internal-meta (meta %)))
                               (not= '& %) (not (contains? @ops/*surface-index* (name %))))
                         params)
                 (empty? (forms/strip-internal-meta (meta params))))
        (let [b (print-form body)]
          (when-not (str/includes? b "\n")
            (str (if (= 1 (count params))
                   (str (first params))
                   (str "(" (str/join ", " params) ")"))
                 " -> " b)))))))

(defn- print-fn-block [head args]
  (or (arrow-lambda head args) (print-fn-block* head args)))

(defn- print-fn-block* [head args]
  (let [head-str (clojure.core/name head)
        [maybe-name & rest1] args
        [name-sym rest2] (if (symbol? maybe-name)
                           [maybe-name rest1]
                           [nil (cons maybe-name rest1)])
        prefix (if name-sym (str head-str " " name-sym) head-str)]
    (cond
      (and (seq rest2) (seq? (first rest2)) (vector? (first (first rest2))))
      ;; multi-arity: fall back to call syntax for roundtrippability
      (str head-str "(" (print-args args) ")")
      ;; single arity with vector params
      (and (seq rest2) (vector? (first rest2)))
      (let [[params & body] rest2]
        (if (and (= 1 (count body))
                 ;; only a body that stays on one line reads well inline
                 (not (str/includes? (print-form (first body)) "\n")))
          ;; single-expression body: inline, delimited by `end` so it stays
          ;; unambiguous on one line (no indentation-sensitivity needed).
          (str prefix " " (print-form params) ": " (print-form (first body)) " end")
          (str prefix " " (print-form params) ":\n"
               (print-body (vec body)) "\n"
               *indent* "end")))
      ;; params not a vector — fall back to call syntax (fn used as local variable)
      :else
      (str head-str "(" (print-args args) ")"))))

(defn- print-if-block [args]
  (let [[cond-form then-form & more] args
        else-form (first more)]
    ;; Fall back to generic call syntax when then/else is a reserved block keyword
    ;; (e.g. a local variable named 'else' or 'end') — such symbols would be
    ;; misread as block terminators inside parse-body.
    (if (or (contains? block-terminator-syms then-form)
            (contains? block-terminator-syms else-form))
      (str "if(" (print-args args) ")")
      (if (seq more)
        (str "if " (colon-sep (print-form cond-form)) "\n"
             (print-body [then-form]) "\n"
             *indent* "else:\n"
             (print-body [else-form]) "\n"
             *indent* "end")
        (str "if " (colon-sep (print-form cond-form)) "\n"
             (print-body [then-form]) "\n"
             *indent* "end")))))

(defn- print-when-block [head args]
  (let [[cond-form & body] args]
    (str (clojure.core/name head) " " (colon-sep (print-form cond-form)) "\n"
         (print-body (vec body)) "\n"
         *indent* "end")))

(defn- print-binding-vec
  "Print a binding vector with a comma between each name-value pair.
   [x 1 y 2] → [x 1, y 2]    [x xs :when p y ys] → [x xs, :when p, y ys]
   A vector that would overflow the line (under the pretty-printer), or that
   holds a multi-line value such as an `if` block, puts one pair per line,
   aligned after '[' (head-str is the block word before it); a block value is
   indented from the column where the value starts."
  ([bindings] (print-binding-vec bindings nil))
  ([bindings head-str]
   (if (even? (count bindings))
     (let [kvs (partition 2 bindings)
           sep (fn [k v] (if (or (operator-arg? k) (operator-arg? v)) ", " " "))
           ;; `[rem mod(x, y)]` would read as infix — hence the comma separator
           flat-pairs (map (fn [[k v]] (str (print-form k) (sep k v) (print-form v))) kvs)
           flat (str "[" (str/join ", " flat-pairs) "]")
           start (+ (count *indent*) (count (or head-str "")) 2)
           multi-line-value? (str/includes? flat "\n")]
       (if (and head-str (> (count kvs) 0)
                (or multi-line-value?
                    (and *width* (> (count kvs) 1) (> (+ start (count flat)) *width*))))
         (let [pairs (map (fn [[k v]]
                            (let [ks (print-form k)
                                  s (sep k v)
                                  vcol (+ start (count ks) (count s))]
                              (str ks s (binding [*indent* (apply str (repeat vcol " "))]
                                          ;; width-aware under the pretty-printer
                                          (if *body-form-printer*
                                            (*body-form-printer* v vcol)
                                            (print-form v))))))
                          kvs)]
           (str "[" (str/join (str ",\n" (apply str (repeat start " "))) pairs) "]"))
         flat))
     (str "[" (join-forms bindings) "]"))))

(defn- print-let-block [head args]
  (let [[bindings & body] args]
    (if-not (vector? bindings)
      ;; Non-vector bindings (e.g. ~bindings SupUnquote) — can't use block syntax.
      (str (clojure.core/name head) "(" (print-args args) ")")
      (let [bvec (if (= head 'letfn)
                   (print-form bindings)
                   (print-binding-vec bindings (clojure.core/name head)))]
        (str (clojure.core/name head) " " bvec ":\n"
             (print-body (vec body)) "\n"
             *indent* "end")))))

(defn- print-cond-block [args]
  (if (odd? (count args))
    ;; Odd arg count — likely contains a ~@ splice with unknown runtime length.
    ;; partition 2 would silently drop the last element, so fall back to call syntax.
    (str "cond(" (print-args args) ")")
    (let [pairs (partition 2 args)
          inner (str *indent* "  ")]
      (str "cond:\n"
           (binding [*indent* inner]
             (str/join "\n"
                       (map (fn [[test val]]
                              (str inner (print-form test) " => " (print-form val)))
                            pairs)))
           "\n" *indent* "end"))))

(defn- print-case-test
  "Print a case dispatch value safely.
   - Seq multi-dispatch lists (e.g. (defn defn- defmacro)) → quoted '(...)
   - Block keyword symbols (e.g. if, when, let) → quoted 'if etc.
   This prevents the parser from treating case test values as block-form starts."
  [test]
  (cond
    (and (seq? test) (seq test))
    (str "'" (binding [*mode* :clj]
               (str "(" (join-forms test) ")")))
    (and (symbol? test) (some? (block-kind-for test)))
    (str "'" test)
    :else
    (print-form test)))

(defn- print-case-block [args]
  (let [[expr & clauses] args
        has-default? (odd? (count clauses))
        pairs (partition 2 (if has-default? (butlast clauses) clauses))
        default (when has-default? (last clauses))
        inner (str *indent* "  ")]
    (str "case " (colon-sep (print-form expr)) "\n"
         (binding [*indent* inner]
           (str/join "\n"
                     (map (fn [[test val]]
                            (str inner (print-case-test test) " => " (print-form val)))
                          pairs)))
         (when has-default?
           (binding [*indent* inner]
             (str "\n" inner "=> " (print-form default))))
         "\n" *indent* "end")))

(defn- print-try-block [args]
  (let [body    (take-while #(not (and (seq? %) (#{'catch 'finally} (first %)))) args)
        clauses (drop (count body) args)]
    (str "try:\n"
         (print-body (vec body)) "\n"
         (str/join "\n"
                   (keep (fn [clause]
                           (case (first clause)
                             catch
                             (let [[_ ex-type binding & handler] clause]
                               (str *indent* "catch [" (print-form ex-type) " " (print-form binding) "]:\n"
                                    (print-body (vec handler))))
                             finally
                             (let [[_ & finally-body] clause]
                               (str *indent* "finally:\n"
                                    (print-body (vec finally-body))))
                             nil))
                         clauses))
         "\n" *indent* "end")))

(defn- print-for-block [head args]
  (let [[bindings & body] args]
    (if-not (vector? bindings)
      ;; Non-vector bindings (e.g. ~@spliced) — can't use block syntax.
      (str (clojure.core/name head) "(" (print-args args) ")")
      (str (clojure.core/name head) " " (print-binding-vec bindings (clojure.core/name head)) ":\n"
           (print-body (vec body)) "\n"
           *indent* "end"))))

(defn- print-def-block [head args]
  (let [[name-sym & rest1] args
        [docstring rest2]  (if (string? (first rest1))
                             [(first rest1) (rest rest1)]
                             [nil rest1])
        ;; Reader-conditional names can't be block-detected by the reader — use call syntax
        name-is-rc? (forms/sup-reader-conditional? name-sym)]
    (if (or name-is-rc? (not= 1 (count rest2)))
      ;; Complex form or reader-conditional name — call syntax
      (str (clojure.core/name head) "(" (print-args args) ")")
      (str (clojure.core/name head) " " (print-form name-sym)
           (when docstring (str " " (print-docstring docstring)))
           ": " (print-form (first rest2))))))

(defn- print-ns-block [args]
  (let [[name-sym & rest1] args
        [docstring rest2]  (if (string? (first rest1))
                             [(first rest1) (rest rest1)]
                             [nil rest1])
        [attr-map clauses] (if (map? (first rest2))
                             [(first rest2) (rest rest2)]
                             [nil rest2])
        inner (str *indent* "  ")]
    (str "ns " (print-form name-sym)
         (when docstring (str " " (print-docstring docstring)))
         (when attr-map  (str " " (print-form attr-map)))
         ":\n"
         (binding [*indent* inner]
           (str/join "\n"
                     (map (fn [clause]
                            ;; Reader-conditional ns clauses (#?(:clj (:import ...)))
                            ;; can't be expressed as keyword sub-clauses — fall back
                            ;; to print-form which emits them as #?(...) expressions.
                            (if (forms/sup-reader-conditional? clause)
                              (str inner (print-form clause))
                              (let [kw   (first clause)
                                    items (rest clause)]
                                (str inner (name kw) ":\n"
                                     (str/join "\n"
                                               (map #(str inner "  "
                                                          ;; :import list specs (java.pkg Class ...)
                                                          ;; would print as java.pkg(Class ...) which
                                                          ;; the reader misreads as (.pkg java Class ...).
                                                          ;; Emit as [java.pkg Class ...] (vector) instead;
                                                          ;; the reader converts import vectors back to lists.
                                                          (if (and (= kw :import) (seq? %))
                                                            (str "[" (join-forms %) "]")
                                                            (print-form %)))
                                                    items))
                                     "\n" inner "end"))))
                          clauses)))
         "\n" *indent* "end")))

(defn- print-match-block [head args]
  (let [[expr & clauses] args
        pairs (partition 2 clauses)
        inner (str *indent* "  ")]
    (if (and *match-arms* (seq clauses)
             (every? #(and (vector? %) (= 2 (count %))) clauses))
      ;; pattern-form match (ansatz): each clause [pattern body] as an arm
      (str (clojure.core/name head) " " (colon-sep (print-form expr)) "\n"
           (binding [*indent* inner]
             (str/join "\n" (map (fn [[pat body]]
                                   (str inner "| " (print-form pat) " => " (print-form body)))
                                 clauses)))
           "\n" *indent* "end")
      (if *match-arms*
      ;; ansatz's explicit form (match x T R (ctor [fields] body) …): not core.match
      ;; pairs, which would misrepresent it — call syntax
        (str (clojure.core/name head) "(" (print-args args) ")")
        (str (clojure.core/name head) " " (colon-sep (print-form expr)) "\n"
             (binding [*indent* inner]
               (str/join "\n"
                         (map (fn [[pat result]]
                            ;; :else catch-all → print as bare _ wildcard
                                (let [pat-str (if (= :else pat) "_" (print-form pat))]
                                  (str inner pat-str " => " (print-form result))))
                              pairs)))
             "\n" *indent* "end")))))

(defn- print-defmethod-block [head args]
  (let [[name-sym dispatch-val params & body] args]
    (str (clojure.core/name head) " " (print-form name-sym)
         " " (print-form dispatch-val)
         " " (print-form params) ":\n"
         (print-body (vec body)) "\n"
         *indent* "end")))

(defn- print-defprotocol-method-sig [method]
  (let [[name-sym & rest] method
        arities   (take-while vector? rest)
        docstring (first (drop-while vector? rest))]
    (str (print-form name-sym) " "
         (str/join " " (map print-form arities))
         (when docstring (str " " (print-docstring docstring))))))

(defn- print-defprotocol-block [head args]
  (let [[name-sym & rest1] args
        [docstring rest2] (if (string? (first rest1))
                            [(first rest1) (rest rest1)]
                            [nil rest1])
        [attr-map methods] (if (map? (first rest2))
                             [(first rest2) (rest rest2)]
                             [nil rest2])
        inner (str *indent* "  ")]
    (str (clojure.core/name head) " " (print-form name-sym)
         (when docstring (str " " (print-docstring docstring)))
         (when attr-map  (str " " (print-form attr-map)))
         ":\n"
         (binding [*indent* inner]
           (str/join "\n"
                     (map (fn [m]
                            (if (forms/sup-reader-conditional? m)
                              (str inner (print-form m))
                              (str inner (print-defprotocol-method-sig m))))
                          methods)))
         "\n" *indent* "end")))

(defn- print-method-impl [method]
  ;; Called with *indent* already set to the method indentation level by the caller.
  ;; print-body adds one more level (2 spaces) for the body lines.
  (let [[name-sym params & body] method]
    (str (print-form name-sym) " " (print-form params) ":\n"
         (print-body (vec body)) "\n"
         *indent* "end")))

(defn- print-protocol-impl-items [items]
  (let [inner (str *indent* "  ")]
    (binding [*indent* inner]
      (str/join "\n"
                (map (fn [item]
                       (str inner
                            (cond
                              (forms/sup-reader-conditional? item) (print-form item)
                              (symbol? item)                        (print-form item)
                              :else                                 (print-method-impl item))))
                     items)))))

(defn- print-defrecord-block [head args]
  (let [[name-sym fields & items] args]
    (if (empty? items)
      ;; a record with no protocol implementations: one line
      (str (clojure.core/name head) " " (print-form name-sym) " " (print-form fields) ": end")
      (str (clojure.core/name head) " " (print-form name-sym)
           " " (print-form fields) ":\n"
           (print-protocol-impl-items items) "\n"
           *indent* "end"))))

(defn- print-reify-block [head args]
  (str (clojure.core/name head) ":\n"
       (print-protocol-impl-items args) "\n"
       *indent* "end"))

(defn- print-proxy-block [head args]
  (let [[bases ctor-args & methods] args]
    (str (clojure.core/name head)
         " " (print-form bases) " " (print-form ctor-args) ":\n"
         (print-protocol-impl-items methods) "\n"
         *indent* "end")))

(defn- depth0-space?
  "Does printed text contain whitespace outside brackets and string literals?
   Such a form is not a single token group and must be parenthesized when it
   stands in a block header."
  [s]
  (loop [i 0 depth 0 in-str? false]
    (if (>= i (count s))
      false
      (let [c (nth s i)]
        (cond
          in-str? (case c
                    \\ (recur (+ i 2) depth true)
                    \" (recur (inc i) depth false)
                    (recur (inc i) depth true))
          (= c \\) (recur (+ i 2) depth false) ; char literal, e.g. \space
          (= c \") (recur (inc i) depth true)
          (#{\( \[ \{} c) (recur (inc i) (inc depth) false)
          (#{\) \] \}} c) (recur (inc i) (dec depth) false)
          (and (zero? depth) (#{\space \tab \newline} c)) true
          :else (recur (inc i) depth false))))))

(defn- print-header-item
  "Print one block-header form so the reader parses it back as exactly one form."
  [form]
  (let [s (if (string? form) (print-docstring form) (print-form form))]
    (if (and (seq? form) (depth0-space? s))
      (str "(" s ")")
      s)))

(defn- param-groups
  "Group a parameter vector into `name :- Type` triples and single params."
  [v]
  (loop [xs (seq v) groups []]
    (if-not xs
      groups
      (if (= :- (second xs))
        (recur (nthnext xs 3) (conj groups (take 3 xs)))
        (recur (next xs) (conj groups [(first xs)]))))))

(defn- print-header-vector
  "Print a header vector starting at column col. Under the pretty-printer, a
   vector that would overflow puts one parameter group per line, aligned after
   '['. The line breaks stay inside the brackets, which the reader allows."
  [v col]
  (let [flat (print-form v)
        groups (param-groups v)]
    (if (and *width* (> (count groups) 1) (> (+ col (count flat)) *width*)
             ;; a user ^meta prefix would precede '[' — keep those flat
             (empty? (forms/strip-internal-meta (meta v))))
      ;; align after '[' unless that is past mid-line (e.g. after a docstring);
      ;; then use a hanging indent below the head
      (let [align (if (< (inc col) (quot *width* 2)) (inc col) (+ (count *indent*) 4))]
        (str "[" (str/join (str ",\n" (apply str (repeat align " ")))
                           (map #(join-forms %) groups))
             "]"))
      flat)))

(defn- operator-sym? [x]
  (and (symbol? x)
       (or (= '=> x)
           (contains? @ops/*surface-index* (str x)))))

(defn- first-header-item-ok?
  "Mirror of the reader's shape-start? check on the first header token."
  [shape x]
  (let [wrap (first shape)]
    (cond
      (and (vector? wrap) (= :wrap? (first wrap)) (= (second wrap) x)) true
      (= :name (shapes/first-slot shape))
      (or (and (symbol? x) (not (operator-sym? x)))
          (forms/unquote? x))
      :else (not (operator-sym? x)))))

(defn- wrapped-call-head?
  "Must a call head be parenthesized so `(head)(args)` reads back as a call?
   @x(args) reads as @(x args) and ~x(args) as ~(x(args)); a dotted symbol
   `A.b(args)` reads as the method call (.b A args)."
  [head]
  (or (and (seq? head) (= 'clojure.core/deref (first head)))
      ;; #'f(args) reads as (var (f args))
      (and (seq? head) (= 'var (first head)) (= 2 (count head)))
      (forms/unquote? head)
      (and (symbol? head)
           (not *dotted-calls*)
           (nil? (namespace head))
           (let [n (str head)]
             (and (str/includes? n ".")
                  (not (str/starts-with? n "."))
                  (not (str/ends-with? n ".")))))))

(defn infix-form?
  "Would form print as an infix expression (`a + b`)?"
  [form]
  (and (seq? form) (symbol? (first form))
       (let [e (infix-entry (first form))
             n (count (rest form))]
         (boolean (and e (if (:variadic e) (>= n 2) (= n 2)))))))

(defn index-form?
  "Is form (aget x i …) inside a block whose index function is aget?"
  [form]
  (and (= *mode* :sup) *index-sym* (seq? form) (= (:get *index-sym*) (first form))
       (>= (count form) 3)))

(defn store-form?
  "Is form (aset x i … v) inside a block whose index store function is aset?"
  [form]
  (and (= *mode* :sup) (:set *index-sym*) (seq? form) (= (:set *index-sym*) (first form))
       (>= (count form) 4)))

(defn store-parts
  "For a store form: [target-string value]; the target is x[i, …]."
  [form]
  [(print-form (apply list (:get *index-sym*) (butlast (rest form))))
   (last form)])

(defn store-bare?
  "May the store being printed now go without parentheses? Only as a body
   statement or a call argument, where nothing can continue it."
  []
  (or *stmt-here* *arrow-here*))

(defn infix-parts
  "For a form that prints as infix: [operator-string operand-strings], with the
   operands parenthesized as print-form would. The pretty-printer uses it to
   break a long expression before its operators; the reader continues an infix
   expression across a line break."
  [form]
  (when (infix-form? form)
    (let [head (first form)
          e (infix-entry head)]
      [(or (:str e) (name head)) (vec (infix-operand-strs e (rest form) head))])))

(defn infix-prec
  "Precedence of a form that prints as infix, else nil."
  [form]
  (when (infix-form? form) (:prec (infix-entry (first form)))))

(defn quoted-as-sexp?
  "Must the target of a quote print as an S-expression? ' binds one form, so
   a rendering like `x / 2` or `(A.b)(x)` would quote only its first part;
   the reader reads '(...) as a plain list, which is exact."
  [inner s]
  (and (seq? inner) (seq inner)
       (or (not (symbol? (first inner)))   ; a data list: '(1 2 3)
           (str/starts-with? s "(")
           (depth0-space? s))))

(defn print-quoted
  "Print the target of a quote (without the ')."
  [inner]
  (let [s (binding [*in-quote* true] (print-form inner))]
    (if (quoted-as-sexp? inner s)
      (binding [*mode* :clj] (print-form inner))
      s)))

(defn operator-arg?
  "Is x an operator symbol? As a call argument it needs care: `f(a, +, b)`
   must not read as the infix `a + b`."
  [x]
  (and (symbol? x)
       (or (contains? @ops/*surface-index* (name x))
           (some? (infix-entry x)))))

(defn call-head-str
  "The text written before '(' for a call with this head."
  [head]
  (if (wrapped-call-head? head)
    (str "(" (print-form head) ")")
    (print-form head)))

(declare print-shape-block*)

(defn- print-shape-block
  "Print a macro with a registered shape as a block:
     head header-form...: body... end
   Falls back to call syntax whenever the block would not read back to the
   same form (see superficie.shapes/split-verified)."
  [head args]
  (let [q (or (shapes/resolve-static head) head)
        outer *dotted-calls*]
    ;; the option applies only if the form prints as a block: the reader
    ;; switches it on when it parses the block, never for call syntax
    (binding [*dotted-calls* (or outer (boolean (:dotted-calls (shapes/shape-options q))))
              *match-arms* (or *match-arms* (boolean (:match-arms (shapes/shape-options q))))]
      (print-shape-block* head args outer (or (shapes/index-fns (shapes/shape-options q)) *index-sym*)))))

(defn- print-shape-block*
  "Indexing (body-index) applies in the body only: a header is names,
   parameters and types, where `name[x]` would be ambiguous."
  [head args outer-dotted body-index]
  (let [shape (shapes/shape-for (or (shapes/resolve-static head) head))
        [header body :as split] (when shape (shapes/split-verified shape args))
        head-str (str head)
        ;; print header items left to right, tracking the column so a long
        ;; parameter vector can break inside its brackets
        print-items (fn [items col]
                      (loop [items items col col out []]
                        (if-let [x (first items)]
                          (let [s (if (vector? x) (print-header-vector x col) (print-header-item x))
                                last-line (peek (str/split s #"\n" -1))
                                col' (if (str/includes? s "\n")
                                       (+ (count last-line) 1)
                                       (+ col (count s) 1))]
                            (recur (rest items) col' (conj out s)))
                          out)))
        header-strs (binding [*index-sym* nil]
                      (print-items header (+ (count *indent*) (count head-str) 1)))
        ;; a multi-line docstring goes on its own continuation line, with the rest
        ;; of the header on the next one — Python's hanging indent, deeper than the body
        flat-len (+ (count *indent*) (count head-str) 1
                    (count (str/join " " header-strs)))
        doc-idx (first (keep-indexed (fn [i x]
                                       (when (and (string? x)
                                                  (or (str/includes? x "\n")
                                                      (and *width* (> flat-len *width*))))
                                         i))
                                     header))
        cont-indent (str *indent* "    ")
        call (fn [] (binding [*dotted-calls* outer-dotted]
                      (str head-str "(" (print-args args) ")")))]
    (if (or (nil? split)
            (and (shapes/requires-header? shape) (empty? header))
            ;; the reader recognizes the block by its first header token
            (not (first-header-item-ok? shape (first header)))
            ;; header must stay on the head's line (breaks only inside a
            ;; parameter vector); terminators would close the block
            (some #(str/includes? % "\n")
                  (map (fn [x s] (if (or (vector? x) (string? x)) "" s)) header header-strs))
            (some #(contains? block-terminator-syms %) (concat header body))
            (some #{(keyword "=")} header))
      (call)
      (let [line (cond
                   doc-idx
                   (let [before (subvec (vec header-strs) 0 doc-idx)
                         doc (nth header-strs doc-idx)
                         after (binding [*index-sym* nil]
                                 (print-items (subvec (vec header) (inc doc-idx)) (count cont-indent)))]
                     (str head-str (when (seq before) (str " " (str/join " " before)))
                          "\n" cont-indent (if (seq after) doc (colon-sep doc))
                          (when (seq after)
                            (str "\n" cont-indent (colon-sep (str/join " " after))))))
                   (seq header)
                   (str head-str " " (colon-sep (str/join " " header-strs)))
                   :else
                   (str head-str ":"))]
        (if (seq body)
          (str line "\n" (binding [*index-sym* body-index] (print-body (vec body))) "\n" *indent* "end")
          (str line " end"))))))

(defn- print-block-form [head args]
  (case (block-kind-for head)
    :shape-block (print-shape-block head args)
    :defn-block  (print-defn-block head args)
    :fn-block    (print-fn-block head args)
    :if-block    (print-if-block args)
    :when-block  (print-when-block head args)
    :let-block   (print-let-block head args)
    :cond-block  (print-cond-block args)
    :case-block  (print-case-block args)
    :match-block       (print-match-block head args)
    :def-block         (print-def-block head args)
    :ns-block          (print-ns-block args)
    :try-block         (print-try-block args)
    :for-block         (print-for-block head args)
    :defmethod-block   (print-defmethod-block head args)
    :defprotocol-block (print-defprotocol-block head args)
    :defrecord-block   (print-defrecord-block head args)
    :reify-block       (print-reify-block head args)
    :proxy-block       (print-proxy-block head args)
    ;; fallback for unknown kind: generic call syntax
    (str (clojure.core/name head) "(" (print-args args) ")")))

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

(declare print-form*)

(defn print-form
  "Print a single Clojure form as sup text."
  [form]
  (binding [*arrow-here* *arrow-ok*
            *arrow-ok* false
            *stmt-here* *stmt-ok*
            *stmt-ok* false]
    (print-form* form)))

(defn- print-form*
  [form]
  (cond
    ;; metadata prefix: ^:key, ^Type, or ^{map} — emit before the form
    ;; Filter out :line/:column/:file added by Clojure's compiler/reader
    (and (some? form)
         #?(:clj (instance? clojure.lang.IMeta form)
            :cljs (satisfies? IMeta form))
         (some? (meta form))
         (seq (forms/strip-internal-meta (meta form))))
    (let [chain (:sup/meta-chain (meta form))
          stripped (with-meta form nil)
          emit-one (fn [m]
                     (cond
                       (and (= 1 (count m))
                            (keyword? (key (first m)))
                            (true? (val (first m))))
                       (str "^" (print-form (key (first m))))
                       (and (= 1 (count m))
                            (contains? m :tag)
                            (symbol? (:tag m)))
                       (str "^" (print-form (:tag m)))
                       :else
                       (str "^" (print-form m))))
          ;; For infix expressions as meta targets, force call syntax to avoid
          ;; ambiguity: ^Meta 1 + 1 would apply ^Meta to only 1, not the
          ;; whole expression. Instead emit ^Meta +(1 1).
          print-stripped (fn []
                           (if (and (= *mode* :sup)
                                    (seq? stripped)
                                    (symbol? (first stripped))
                                    (let [e (infix-entry (first stripped))
                                          n (count (rest stripped))]
                                      (and e (if (:variadic e) (>= n 2) (= n 2)))))
                             (str (print-form (first stripped)) "(" (print-args (rest stripped)) ")")
                             (print-form stripped)))]
      (if chain
        (str (str/join " " (map emit-one (reverse chain))) " " (print-stripped))
        (let [m (forms/strip-internal-meta (meta form))]
          (str (emit-one m) " " (print-stripped)))))

    ;; raw value wrapper — emit original source text
    (forms/raw? form) (:raw form)

    ;; nil
    (nil? form) "nil"

    ;; boolean
    (boolean? form) (str form)

    ;; Deferred auto-resolve keywords: (clojure.core/read-string "::foo") → ::foo
    (forms/deferred-auto-keyword? form)
    (forms/deferred-auto-keyword-raw form)

    ;; empty list
    (and (seq? form) (empty? form))
    "()"

    ;; Literal heads: nil, true, false. The reader reads a literal followed by an
    ;; adjacent '(' as a call, so (nil ys) — e.g. a match clause — prints as nil(ys).
    (and (= *mode* :sup) (seq? form) (seq form) (contains? #{nil true false} (first form)))
    (str (pr-str (first form)) "(" (print-args (rest form)) ")")

    ;; sequences — calls and reader sugar
    (seq? form)
    (let [head (first form)]
      (cond
        (anon-fn-shorthand? form)
        ;; a single parameter is written %, as Clojure writes #(* % %)
        (let [body (nth form 2)
              body (if (#{['%1] ['%1 '& '%&]} (second form))
                     ;; (not a #() literal here: the reader would rewrite the quoted %)
                     (rename-syms (fn [s] (if (= '%1 s) (symbol "%") s)) body)
                     body)]
          (str "#(" (print-form body) ")"))

        ;; #(…) from Clojure source: the reader's fn* with p1__N# parameters
        (and (= *mode* :sup) (clj-anon-fn form))
        (print-form (clj-anon-fn form))

        ;; @deref — always use shorthand (both 'x and (quote x) from Clojure source round-trip)
        (= head 'clojure.core/deref)
        (str "@" (print-form (second form)))

        ;; 'quote — always use shorthand ('x and (quote x) are semantically identical)
        (and (= head 'quote) (= 2 (count form)))
        (str "'" (print-quoted (second form)))

        ;; #'var — always use shorthand, but when (var X) is the head of a call
        ;; use (#'X)(args) to prevent #'X(args) being read as (var (X args))
        (and (= head 'var) (= 2 (count form)))
        (str "#'" (print-form (second form)))

        ;; (var X) as call head: ((var X) args) → (#'X)(args)
        (and (= *mode* :sup) (seq? head) (= 'var (first head)) (= 2 (count head))
             (seq (rest form)))
        (str "(#'" (print-form (second head)) ")(" (print-args (rest form)) ")")

        ;; sup mode: x[i] <- v for the block's index store function (raster's aset)
        (store-form? form)
        (let [[target v] (store-parts form)
              s (str target " <- " (print-form v))]
          (if (store-bare?) s (str "(" s ")")))

        ;; sup mode: x[i, j] for the block's index function (raster's aget)
        (index-form? form)
        (let [[_ target & idx] form
              t (print-form target)]
          (str (if (or (depth0-space? t)
                       (and (seq? target) (not (str/ends-with? t ")"))))
                 (str "(" t ")")
                 t)
               "[" (print-args idx) "]"))

        ;; sup mode: block forms (defn, if, let, fn, etc.)
        (and (= *mode* :sup) (symbol? head) (some? (block-kind-for head)))
        (print-block-form head (rest form))

        ;; sup mode: Java instance method call — (.method obj args) → obj.method(args)
        ;; Falls back to .method(obj args) when obj ends with a number token (to avoid
        ;; "1.method" being tokenized as a malformed number literal).
        (and (= *mode* :sup) (symbol? head)
             (let [n (str head)]
               ;; .method pattern: starts with ., not .-, length > 1, and second char
               ;; is not another dot — this excludes the .. double-dot chaining operator
               (and (str/starts-with? n ".") (not (str/starts-with? n ".-")) (> (count n) 1)
                    (not= \. (nth n 1))))
             (seq (rest form)))
        (let [method-name (subs (str head) 1)
              [obj & method-args] (rest form)
              obj-str (print-form obj)
              ;; Safe to use obj.method() notation only when obj-str is a simple expression:
              ;; - not ending with a number literal (would look like "1.method")
              ;; - not namespace-qualified (contains '/')
              ;; - not prefixed with ~, @, ' operators
              ;; - no spaces (complex expressions like "or(a, b)" are ambiguous in arg position:
              ;;   "f(or(a, b).method())" would be parsed as "f(or(a, (.method b)))")
              ;; - not a boolean/nil literal: "true.setDaemon()" reads "true" as Symbol, not Boolean
              last-word (last (str/split (str/trim obj-str) #"\s+"))
              safe? (and (not *dotted-calls*) ; obj.m(x) would read as the call (obj.m x)
                         (not (re-matches #"-?[0-9].*" last-word))
                         (not (str/includes? obj-str "/"))
                         (not (re-matches #"[~@'].*" obj-str))
                         (not (str/includes? obj-str " "))
                         (not (str/includes? obj-str ".-"))
                         (not (#{"true" "false" "nil"} obj-str)))]
          (if safe?
            (str obj-str "." method-name "(" (print-args method-args) ")")
            (str "." method-name "(" obj-str (when (seq method-args) (str ", " (print-args method-args))) ")")))

        ;; sup mode: Java field access — (.-field obj) → obj.-field
        ;; Same safety check for number-ending obj expressions.
        (and (= *mode* :sup) (symbol? head)
             (str/starts-with? (str head) ".-") (= 1 (count (rest form))))
        (let [field-sym-str (str head)          ;; e.g. ".-uuid"
              field-name    (subs field-sym-str 1) ;; e.g. "-uuid" (dot removed for safe concat)
              obj-str (print-form (second form))
              last-word (last (str/split (str/trim obj-str) #"\s+"))
              safe? (and (not (re-matches #"-?[0-9].*" last-word))
                         (not (str/includes? obj-str "/"))
                         (not (re-matches #"[~@'].*" obj-str))
                         (not (str/includes? obj-str " "))
                         (not (str/includes? obj-str ".-")))]
          (if safe?
            (str obj-str "." field-name)
            ;; Unsafe: use function-call notation .-field(obj) which round-trips cleanly
            (str field-sym-str "(" obj-str ")")))

        ;; sup mode: Java constructor — (ClassName. args) → new ClassName(args)
        ;; Excludes .. (double-dot chaining operator) which also ends with "."
        (and (= *mode* :sup) (symbol? head)
             (let [n (str head)]
               (and (str/ends-with? n ".") (not= n ".")
                    ;; Prefix (class name) must not start with "." — excludes ".."
                    (not (str/starts-with? (subs n 0 (dec (count n))) ".")))))
        (let [class-name (subs (str head) 0 (dec (count (str head))))]
          (str "new " class-name "(" (print-args (rest form)) ")"))

        ;; sup mode: (new ClassName args...) → new ClassName(args...)
        (and (= *mode* :sup) (= head 'new) (seq (rest form)) (symbol? (second form)))
        (let [class-name (second form)]
          (str "new " class-name "(" (print-args (drop 2 form)) ")"))

        ;; sup mode: infix operators — unified via infix-entry
        ;; variadic ops emit for 2+ args; non-variadic only for exactly 2 args
        (and (= *mode* :sup)
             (let [e (infix-entry head)
                   n (count (rest form))]
               (and e (if (:variadic e) (>= n 2) (= n 2)))))
        (let [e      (infix-entry head)
              op-str (or (:str e) (name head))]
          (print-infix e op-str (rest form) head))

        ;; call: sup emits head(args...), clj emits (head args...)
        :else
        (cond
          (= *mode* :clj)
          (str "(" (print-form head) (when (seq (rest form)) (str " " (print-args (rest form)))) ")")

          ;; Deref/unquote-then-call: ((deref x) args) → (@x)(args)
          ;;                         ((~x) args) → (~x)(args)
          ;; @x(args) parses as @(x args) — wrong order.
          ;; ~x(args) parses as ~(x(args)) — wrong order.
          ;; (head)(args) works: parse-call-chain sees adjacent ( after the group.
          ;; Dotted-symbol call head: (clojure.lang.MapEntry k) → (clojure.lang.MapEntry)(k)
          ;; Without parens, the reader's obj.method() rule would split on the last dot.
          (wrapped-call-head? head)
          (str (call-head-str head) "(" (print-args (rest form)) ")")

          :else
          ;; Arguments are comma-separated; an operator preceded by a comma is
          ;; never infix, so f(a, +, b) keeps + as the symbol it is.
          (str (print-form head) "(" (print-args (rest form)) ")"))))

    ;; syntax-quote / unquote / unquote-splicing AST nodes
    ;; Must be before map? because these are defrecords (satisfy map?)
    (forms/syntax-quote? form)
    (let [inner (:form form)
          ;; When the top-level inner form is an infix expression, surface
          ;; syntax `` `~a + ~b `` only syntax-quotes ~a (the first form).
          ;; Use explicit call notation `` `+(~a ~b) `` so the entire
          ;; expression stays inside the syntax-quote context.
          inner-str
          (if (and (= *mode* :sup) (seq? inner) (seq inner)
                   (symbol? (first inner))
                   (let [e (infix-entry (first inner))
                         n (count (rest inner))]
                     (and e (if (:variadic e) (>= n 2) (= n 2)))))
            (let [head (first inner)
                  e    (infix-entry head)
                  ;; Use the surface string (e.g. "|>" for ->>), not the
                  ;; Clojure symbol name, so the call head roundtrips.
                  op-str (or (:str e) (name head))]
              (str op-str "(" (print-args (rest inner)) ")"))
            (print-form inner))]
      (str "`" inner-str))

    (forms/unquote? form)
    (str "~" (print-form (:form form)))

    (forms/unquote-splicing? form)
    (str "~@" (print-form (:form form)))

    ;; reader conditional — walk inner forms with sup syntax
    ;; Must be before map? because CLJS SupReaderConditional is a defrecord (satisfies map?)
    (forms/sup-reader-conditional? form)
    (let [prefix (if (forms/rc-splicing? form) "#?@(" "#?(")
          pairs (partition 2 (forms/rc-form form))
          body (str/join " " (mapcat (fn [[k v]] [(print-form k) (print-form v)]) pairs))]
      (str prefix body ")"))

    ;; vector
    (vector? form)
    (str "[" (join-forms form) "]")

    ;; map — reconstruct #:ns{} when :sup/ns metadata present
    (map? form)
    (if-let [ns-str (:sup/ns (meta form))]
      (let [strip-ns (fn [k]
                       (if (and (keyword? k) (= (namespace k) (if (str/starts-with? ns-str ":") (subs ns-str 1) ns-str)))
                         (keyword (name k))
                         k))
            body (str/join ", " (map (fn [[k v]] (str (print-form (strip-ns k)) (if (or (= 'new k) (operator-arg? v)) ", " " ") (print-form v))) form))]
        (str "#:" ns-str "{" body "}"))
      (let [entries (vec form)]
        (str "{"
             (str/join ", "
                       (map
                        (fn [[k v]]
                          (str (print-form k)
                               ;; a comma keeps an operator value from reading as infix
                               (if (or (= 'new k) (operator-arg? v)) ", " " ")
                               (print-form v)))
                        entries))
             "}")))

    ;; set — use :sup/order for insertion-order output when available
    (set? form)
    (let [elements (or (:sup/order (meta form)) (seq form))]
      (str "#{" (join-forms elements) "}"))

    ;; symbol
    (symbol? form) (str form)

    ;; keyword
    (keyword? form)
    (if (namespace form)
      (str ":" (namespace form) "/" (name form))
      (str ":" (name form)))

    ;; string
    (string? form) (pr-str form)

    ;; regex — escape bare quotes in the pattern.
    ;; Match escape sequences (\.) atomically so \\" is parsed as
    ;; (escaped-backslash)(bare-quote), not (backslash)(escaped-quote).
    (instance? #?(:clj java.util.regex.Pattern :cljs js/RegExp) form)
    (let [raw #?(:clj (.pattern ^java.util.regex.Pattern form) :cljs (.-source form))]
      (str "#\"" (str/replace raw #"\\.|\"" (fn [m] (if (= m "\"") "\\\"" m))) "\""))

    ;; char (JVM/Babashka only — ClojureScript has no char type)
    #?@(:clj [(char? form)
              (let [named {(char 10) "newline" (char 13) "return" (char 9) "tab"
                           (char 32) "space" (char 8) "backspace" (char 12) "formfeed"}]
                (if-let [n (get named form)]
                  (str \\ n)
                  (str \\ form)))])

    ;; number — preserve BigDecimal M and BigInt N suffixes, symbolic values
    #?@(:clj [(decimal? form) (str form "M")
              (instance? clojure.lang.BigInt form) (str form "N")
              (instance? java.math.BigInteger form) (str form "N")])
    (and (number? form)
         #?(:clj (Double/isNaN (double form))
            :cljs (js/isNaN form)))
    "##NaN"
    (and (number? form)
         #?(:clj (Double/isInfinite (double form))
            :cljs (and (not (js/isFinite form)) (not (js/isNaN form)))))
    (if (pos? (double form)) "##Inf" "##-Inf")
    (number? form) (str form)

    (tagged-literal? form)
    (str "#" (.-tag form) " " (print-form (.-form form)))

    ;; fallback
    :else (pr-str form)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn map-in-ns-context
  "Map f over top-level forms in order, tracking the static namespace context:
   after an (ns ...) form, later forms resolve heads through its aliases and
   refers, mirroring what the reader does."
  [f forms]
  (binding [shapes/*ns-context* shapes/*ns-context*]
    (mapv (fn [form]
            (let [r (f form)]
              (when-let [ctx (shapes/ns-context form shapes/*ns-context*)]
                (set! shapes/*ns-context* ctx))
              r))
          forms)))

(defn print-sup-string
  "Print Clojure forms as sup text."
  [forms]
  (str/join "\n\n" (map-in-ns-context print-form forms)))

(defn print-clj-string
  "Print Clojure forms as Clojure text with reader sugar ('quote, @deref, #'var)."
  [forms]
  (binding [*mode* :clj]
    (str/join "\n\n" (map print-form forms))))
