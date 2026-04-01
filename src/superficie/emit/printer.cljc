(ns superficie.emit.printer
  "Form printer: Clojure forms → superficie or Clojure text.
   Mode :sup (default) emits head(args...) call syntax.
   Mode :clj emits (head args...) S-expression syntax with reader sugar."
  (:require [clojure.string :as str]
            [superficie.forms :as forms]
            [superficie.operators :as ops]))

;; ---------------------------------------------------------------------------
;; Forward declaration
;; ---------------------------------------------------------------------------

(declare print-form)

;; ---------------------------------------------------------------------------
;; Mode — :sup or :clj
;; ---------------------------------------------------------------------------

(def ^:dynamic *mode* :sup)


;; ---------------------------------------------------------------------------
;; Print helpers
;; ---------------------------------------------------------------------------

(defn- print-args
  "Print a sequence of forms separated by spaces."
  [forms]
  (str/join " " (map #(print-form %) forms)))

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

(defn- block-kind-for
  "Return the block-kind for a symbol head, or nil.
   Handles both qualified ('clojure.core/defn) and unqualified ('defn) heads.
   For unqualified heads, falls back to trying clojure.core/ qualification."
  [head]
  (when (symbol? head)
    (or (get @block-dispatch head)
        (when (nil? (namespace head))
          (get @block-dispatch (symbol "clojure.core" (name head)))))))

(defn- infix-entry
  "Return the *op-registry* entry for head if it is an infix operator, else nil.
   Handles both qualified ('clojure.core/+) and unqualified ('+) heads."
  [head]
  (when (symbol? head)
    (let [entry (or (get @ops/*op-registry* head)
                    (when (nil? (namespace head))
                      (get @ops/*op-registry* (symbol "clojure.core" (name head)))))]
      (when (= :infix (:kind entry)) entry))))

(defn- qualify-if-operator
  "When sym would be tokenized as an infix operator, return its fully-qualified
   name string so the reader does not treat it as infix in value positions."
  [sym]
  (if-let [qsym (get @ops/*surface-index* (name sym))]
    (str qsym)   ; e.g. "clojure.core/+" — not in *surface-index* as a token
    (str sym)))

(def ^:private block-terminator-syms
  "Bare symbols that the reader's parse-body treats as block terminators.
   These cannot appear as top-level expressions in a block body."
  #{'else 'end 'catch 'finally})

(declare print-form)

(defn- colon-sep
  "Append ':' to expr-str, adding a space if the string ends with a symbol
   character that would fuse with ':' in the tokenizer (e.g. 'x:' → 'x :')."
  [expr-str]
  (str expr-str (if (re-find #"[a-zA-Z0-9_!\?\*\-]$" expr-str) " :" ":")))

(defn- infix-print-arg
  "Print an infix operand; parenthesize when its op has lower precedence than threshold."
  [threshold arg]
  (let [inner-prec (when (and (seq? arg) (symbol? (first arg)))
                     (:prec (infix-entry (first arg))))]
    (if (and inner-prec (< inner-prec threshold))
      (str "(" (print-form arg) ")")
      (print-form arg))))

(defn- print-infix [entry op-str args]
  (let [prec       (:prec entry 0)
        left-assoc? (not= :right (:assoc entry))
        ;; Right operands of left-associative ops need parens at equal precedence
        ;; to preserve left-to-right evaluation order.
        right-threshold (if left-assoc? (inc prec) prec)]
    (str/join (str " " op-str " ")
              (cons (infix-print-arg prec (first args))
                    (map #(infix-print-arg right-threshold %) (rest args))))))

(defn- print-body
  "Print forms as indented body lines (each on its own line)."
  [forms]
  (let [inner (str *indent* "  ")]
    (binding [*indent* inner]
      (str/join "\n" (map #(str inner (print-form %)) forms)))))

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
      (str head-str "(" (print-form name-sym)
           (when docstring (str " " (pr-str docstring)))
           (when attr-map (str " " (print-form attr-map)))
           " " (str/join " " (map print-form rest3))
           ")")
      ;; single arity with vector params (normal case, including ^Hint [params])
      ;; Exception: if name is "=>" the block-kind->start? arrow guard prevents detection → call syntax
      (and (seq rest3) (vector? (first rest3))
           (not (and (symbol? name-sym) (= "=>" (clojure.core/name name-sym)))))
      (let [[params & body] rest3
            doc-part (if docstring (str " " (pr-str docstring)) "")
            attr-part (if attr-map (str " " (print-form attr-map)) "")]
        (str head-str " " (print-form name-sym) doc-part attr-part " " (print-form params) ":\n"
             (print-body (vec body)) "\n"
             *indent* "end"))
      ;; params is not a vector — fall back to call syntax (e.g. fn used as local variable)
      :else
      (str head-str "(" (print-form name-sym)
           (when docstring (str " " (pr-str docstring)))
           (when attr-map (str " " (print-form attr-map)))
           (when (seq rest3) (str " " (str/join " " (map print-form rest3))))
           ")"))))

(defn- print-fn-block [head args]
  (let [head-str (clojure.core/name head)
        [maybe-name & rest1] args
        [name-sym rest2] (if (symbol? maybe-name)
                           [maybe-name rest1]
                           [nil (cons maybe-name rest1)])
        prefix (if name-sym (str head-str " " name-sym) head-str)]
    (cond
      (and (seq rest2) (seq? (first rest2)) (vector? (first (first rest2))))
      ;; multi-arity: fall back to call syntax for roundtrippability
      (str head-str "(" (when name-sym (str name-sym " "))
           (str/join " " (map print-form rest2))
           ")")
      ;; single arity with vector params
      (and (seq rest2) (vector? (first rest2)))
      (let [[params & body] rest2]
        (str prefix " " (print-form params) ":\n"
             (print-body (vec body)) "\n"
             *indent* "end"))
      ;; params not a vector — fall back to call syntax (fn used as local variable)
      :else
      (str head-str "(" (when name-sym (str name-sym " "))
           (str/join " " (map print-form rest2))
           ")"))))

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
   [x 1 y 2] → [x 1, y 2]    [x xs :when p y ys] → [x xs, :when p, y ys]"
  [bindings]
  (if (even? (count bindings))
    (str "["
         (str/join ", "
                   (map (fn [[k v]] (str (print-form k) " " (print-form v)))
                        (partition 2 bindings)))
         "]")
    (str "[" (str/join " " (map print-form bindings)) "]")))

(defn- print-let-block [head args]
  (let [[bindings & body] args]
    (if-not (vector? bindings)
      ;; Non-vector bindings (e.g. ~bindings SupUnquote) — can't use block syntax.
      (str (clojure.core/name head) "(" (print-args args) ")")
      (let [bvec (if (= head 'letfn)
                   (print-form bindings)
                   (print-binding-vec bindings))]
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
               (str "(" (str/join " " (map print-form test)) ")")))
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
      (str (clojure.core/name head) " " (print-binding-vec bindings) ":\n"
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
           (when docstring (str " " (pr-str docstring)))
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
         (when docstring (str " " (pr-str docstring)))
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
                                                            (str "[" (str/join " " (map print-form %)) "]")
                                                            (print-form %)))
                                                    items))
                                     "\n" inner "end"))))
                          clauses)))
         "\n" *indent* "end")))

(defn- print-match-block [head args]
  (let [[expr & clauses] args
        pairs (partition 2 clauses)
        inner (str *indent* "  ")]
    (str (clojure.core/name head) " " (colon-sep (print-form expr)) "\n"
         (binding [*indent* inner]
           (str/join "\n"
                     (map (fn [[pat result]]
                            ;; :else catch-all → print as bare _ wildcard
                            (let [pat-str (if (= :else pat) "_" (print-form pat))]
                              (str inner pat-str " => " (print-form result))))
                          pairs)))
         "\n" *indent* "end")))

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
         (when docstring (str " " (pr-str docstring))))))

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
         (when docstring (str " " (pr-str docstring)))
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
    (str (clojure.core/name head) " " (print-form name-sym)
         " " (print-form fields) ":\n"
         (print-protocol-impl-items items) "\n"
         *indent* "end")))

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

(defn- print-block-form [head args]
  (case (block-kind-for head)
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

(defn print-form
  "Print a single Clojure form as sup text."
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

    ;; Non-callable heads: nil, true, false are resolved as literals by the reader,
    ;; not as symbols — they cannot be call heads in sup syntax.
    ;; In :clj mode, fall through to generic S-expression printing.
    (and (= *mode* :sup) (seq? form) (seq form) (contains? #{nil true false} (first form)))
    (throw (ex-info (str "Cannot print list with " (pr-str (first form))
                         " as head — not representable in sup syntax")
                    {:form form}))

    ;; sequences — calls and reader sugar
    (seq? form)
    (let [head (first form)]
      (cond
        (anon-fn-shorthand? form)
        (str "#(" (print-form (nth form 2)) ")")

        ;; @deref — always use shorthand (both 'x and (quote x) from Clojure source round-trip)
        (= head 'clojure.core/deref)
        (str "@" (print-form (second form)))

        ;; 'quote — always use shorthand ('x and (quote x) are semantically identical)
        (and (= head 'quote) (= 2 (count form)))
        (str "'" (print-form (second form)))

        ;; #'var — always use shorthand, but when (var X) is the head of a call
        ;; use (#'X)(args) to prevent #'X(args) being read as (var (X args))
        (and (= head 'var) (= 2 (count form)))
        (str "#'" (print-form (second form)))

        ;; (var X) as call head: ((var X) args) → (#'X)(args)
        (and (= *mode* :sup) (seq? head) (= 'var (first head)) (= 2 (count head))
             (seq (rest form)))
        (str "(#'" (print-form (second head)) ")(" (print-args (rest form)) ")")

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
              ;; - no spaces (complex expressions like "or(a b)" are ambiguous in arg position:
              ;;   "f(or(a b).method())" would be parsed as "f(or(a (.method b)))")
              ;; - not a boolean/nil literal: "true.setDaemon()" reads "true" as Symbol, not Boolean
              last-word (last (str/split (str/trim obj-str) #"\s+"))
              safe? (and (not (re-matches #"-?[0-9].*" last-word))
                         (not (str/includes? obj-str "/"))
                         (not (re-matches #"[~@'].*" obj-str))
                         (not (str/includes? obj-str " "))
                         (not (str/includes? obj-str ".-"))
                         (not (#{"true" "false" "nil"} obj-str)))]
          (if safe?
            (str obj-str "." method-name "(" (print-args method-args) ")")
            (str "." method-name "(" obj-str (when (seq method-args) (str " " (print-args method-args))) ")")))

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
          (print-infix e op-str (rest form)))

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
          (or (and (seq? head) (= 'clojure.core/deref (first head)))
              (forms/unquote? head))
          (str "(" (print-form head) ")(" (print-args (rest form)) ")")

          ;; Dotted-symbol call head: (clojure.lang.MapEntry k) → (clojure.lang.MapEntry)(k)
          ;; Without parens, the reader's obj.method() rule would split on the last dot.
          ;; Paren-wrapping the head prevents that: (Head)(args) is parsed via call-chain.
          (and (symbol? head)
               (nil? (namespace head))
               (let [n (str head)]
                 (and (str/includes? n ".")
                      (not (str/starts-with? n "."))
                      (not (str/ends-with? n ".")))))
          (str "(" (str head) ")(" (print-args (rest form)) ")")

          :else
          ;; Qualify operator-symbol args in non-first/non-last positions to avoid
          ;; infix consumption: f(a + b) is (f (+ a b)), but f(a clojure.core/+ b) is (f a + b).
          (let [args (vec (rest form))
                n    (count args)]
            (str (print-form head)
                 "("
                 (str/join " "
                           (map-indexed
                             (fn [i arg]
                               (if (and (> i 0) (< i (dec n))
                                        (symbol? arg)
                                        (contains? @ops/*surface-index* (name arg)))
                                 (qualify-if-operator arg)
                                 (print-form arg)))
                             args))
                 ")")))))

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
    (str "[" (str/join " " (map print-form form)) "]")

    ;; map — reconstruct #:ns{} when :sup/ns metadata present
    (map? form)
    (if-let [ns-str (:sup/ns (meta form))]
      (let [strip-ns (fn [k]
                        (if (and (keyword? k) (= (namespace k) (if (str/starts-with? ns-str ":") (subs ns-str 1) ns-str)))
                          (keyword (name k))
                          k))
            body (str/join ", " (map (fn [[k v]] (str (print-form (strip-ns k)) " " (print-form v))) form))]
        (str "#:" ns-str "{" body "}"))
      (let [entries (vec form)
            n       (count entries)]
        (str "{"
             (str/join ", "
                       (map-indexed
                         (fn [i [k v]]
                           (str (print-form k) " "
                                ;; Qualify operator-symbol values in non-last entry positions:
                                ;; {k / k2 v2} → {k clojure.core// k2 v2} avoids infix parse.
                                (if (and (< i (dec n))
                                         (= *mode* :sup)
                                         (symbol? v)
                                         (contains? @ops/*surface-index* (name v)))
                                  (qualify-if-operator v)
                                  (print-form v))))
                         entries))
             "}")))

    ;; set — use :sup/order for insertion-order output when available
    (set? form)
    (let [elements (or (:sup/order (meta form)) (seq form))]
      (str "#{" (str/join " " (map print-form elements)) "}"))

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

(defn print-sup-string
  "Print Clojure forms as sup text."
  [forms]
  (str/join "\n\n" (map print-form forms)))

(defn print-clj-string
  "Print Clojure forms as Clojure text with reader sugar ('quote, @deref, #'var)."
  [forms]
  (binding [*mode* :clj]
    (str/join "\n\n" (map print-form forms))))
