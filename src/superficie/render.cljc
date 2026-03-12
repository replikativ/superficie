(ns superficie.render
  "Renders Clojure S-expressions as superficie surface syntax.
   This is the Clojure -> .sup direction."
  (:require [rewrite-clj.zip :as z]
            [rewrite-clj.node :as node]
            [clojure.string :as str]
            [superficie.opaque :as opaque]
            [superficie.resolve :as resolve]
            [superficie.parse :as parse]))

(defn- regex? [x]
  #?(:clj (instance? java.util.regex.Pattern x)
     :cljs (regexp? x)))

;; --- Precedence levels ---
;; Higher number = tighter binding

(def ^:private precedence
  "Precedence levels for operators, matching the grammar."
  {'-> 10, '->> 10, 'some-> 10, 'some->> 10  ;; pipe
   'or 20
   'and 30
   '< 40, '> 40, '<= 40, '>= 40, '= 40, 'not= 40  ;; comparison
   '+ 50, '- 50  ;; additive
   '* 60, '/ 60, 'mod 60, 'rem 60  ;; multiplicative
   'not 70  ;; unary
   })

(defn- op-precedence [sym]
  (get precedence sym 0))

;; --- Helpers ---

(defn- indent
  "Indent a multi-line string by n spaces."
  [n s]
  (let [pad (apply str (repeat n \space))]
    (->> (str/split-lines s)
         (map #(if (str/blank? %) % (str pad %)))
         (str/join "\n"))))

(defn- comma-sep
  "Join rendered expressions with \", \"."
  [exprs]
  (str/join ", " exprs))

;; --- Core rendering ---

(declare render-form render-data render-node render-top-node)

(defn- render-children
  "Render a sequence of sexpr values."
  [ctx exprs]
  (mapv #(render-form ctx %) exprs))

(defn- render-call
  "Default: render as function call f(a, b, c)."
  [ctx f args]
  (str (render-form ctx f) "(" (comma-sep (render-children ctx args)) ")"))

(defn- wrap-if-lower
  "Wrap rendered string in parens if the sub-expression has lower precedence
   than the parent context."
  [rendered sub-prec parent-prec]
  (if (and (pos? parent-prec) (pos? sub-prec) (< sub-prec parent-prec))
    (str "(" rendered ")")
    rendered))

(defn- expr-precedence
  "Return the precedence of a form, or 0 if it's not an infix/logical expression."
  [ctx form]
  (if (and (sequential? form) (seq form) (symbol? (first form)))
    (let [head (first form)
          role (resolve/resolve-role ctx head)]
      (case role
        :infix-op (op-precedence (symbol (name head)))
        :logical-op (op-precedence (symbol (name head)))
        0))
    0))

(defn- render-infix
  "Render as infix: a op b op c. Wraps sub-expressions that have lower precedence."
  [ctx op args]
  (let [op-str (name op)  ;; use bare name, not namespace-qualified
        my-prec (op-precedence (symbol op-str))]
    (if (= 1 (count args))
      ;; Unary
      (str op-str (render-form ctx (first args)))
      ;; N-ary — wrap children with lower precedence
      (str/join (str " " op-str " ")
                (mapv (fn [arg]
                        (wrap-if-lower (render-form ctx arg)
                                       (expr-precedence ctx arg)
                                       my-prec))
                      args)))))

(defn- render-threading
  "Render -> as .> and ->> as |>."
  [ctx op initial steps]
  (let [pipe (case (name op)
               ("->" "some->") ".>"
               ("->>" "some->>") "|>")
        rendered-init (render-form ctx initial)
        rendered-steps (for [step steps]
                         (if (sequential? step)
                           (let [[f & args] step]
                             (cond
                               ;; Constructor in threading: (File. x) -> new File(x)
                               (and (symbol? f) (str/ends-with? (str f) "."))
                               (let [class-name (subs (str f) 0 (dec (count (str f))))]
                                 (str "new " class-name
                                      "(" (comma-sep (render-children ctx args)) ")"))

                               ;; Java method in thread-first: (.method arg) -> .method(arg)
                               (and (symbol? f) (str/starts-with? (str f) "."))
                               (let [method-name (subs (str f) 1)]
                                 (str "." method-name
                                      "(" (comma-sep (render-children ctx args)) ")"))

                               :else
                               (str (render-form ctx f)
                                    "(" (comma-sep (render-children ctx args)) ")")))
                           ;; Bare symbol step — check for constructor
                           (if (and (symbol? step) (str/ends-with? (str step) "."))
                             (let [class-name (subs (str step) 0 (dec (count (str step))))]
                               (str "new " class-name "()"))
                             (render-form ctx step))))]
    (str rendered-init "\n"
         (->> rendered-steps
              (map #(str "  " pipe " " %))
              (str/join "\n")))))

(defn- render-params
  "Render a parameter vector, handling & for varargs."
  [ctx params]
  (comma-sep
   (loop [ps (seq params) result []]
     (if (nil? ps)
       result
       (let [p (first ps)]
         (if (= '& p)
           (recur (nnext ps)
                  (conj result (str "& " (render-form ctx (second ps)))))
           (recur (next ps)
                  (conj result (render-form ctx p)))))))))

(defn- render-bindings
  "Render a binding vector [x 1 y 2] as x := 1, y := 2."
  [ctx bindings]
  (let [pairs (partition 2 bindings)]
    (->> pairs
         (map (fn [[k v]]
                (str (render-form ctx k) " := " (render-form ctx v))))
         (str/join ", "))))

(defn- render-body-flat
  "Render a body, flattening tail let forms into let-statements.
   A tail (let [x 1 y 2] body...) becomes:
     let x := 1
     let y := 2
     body..."
  [ctx body]
  (let [last-expr (last body)
        prefix (butlast body)]
    (if (and (sequential? last-expr)
             (symbol? (first last-expr))
             (= 'let (first last-expr)))
      ;; Don't flatten binding/with-open/with-redefs, only let
      (str (when (seq prefix)
             (str (str/join "\n" (render-children ctx prefix)) "\n"))
           (let [[_ bindings & let-body] last-expr]
             (str (->> (partition 2 bindings)
                       (map (fn [[k v]]
                              (str "let " (render-form ctx k) " := " (render-form ctx v))))
                       (str/join "\n"))
                  "\n"
                  (render-body-flat ctx let-body))))
      ;; No tail let — render normally
      (str/join "\n" (render-children ctx body)))))

(defn- render-defn
  "Render defn/defn-/defmacro."
  [ctx form-name parts]
  (let [kw (str form-name)
        ;; Parse out docstring and skip attr-map (metadata on the name symbol
        ;; is already handled by render-form)
        [name-sym & rest-parts] parts
        [docstring rest-parts] (if (string? (first rest-parts))
                                 [(first rest-parts) (rest rest-parts)]
                                 [nil rest-parts])
        rest-parts (if (map? (first rest-parts))
                     (rest rest-parts)   ;; skip attr-map
                     rest-parts)
        ;; Single arity: (defn foo [x] body)
        ;; Multi arity: (defn foo ([x] body1) ([x y] body2))
        multi-arity? (and (list? (first rest-parts))
                          (vector? (ffirst rest-parts)))]
    (if multi-arity?
      ;; Multi-arity
      (let [arities (for [arity rest-parts]
                      (let [[args & body] arity]
                        (str "  (" (render-params ctx args) "):\n"
                             (indent 4 (render-body-flat ctx body)))))]
        (str (when docstring
               (str (->> (str/split-lines docstring)
                         (map #(str ";; " %))
                         (str/join "\n"))
                    "\n"))
             kw " " (render-form ctx name-sym) "\n"
             (str/join "\n" arities)
             "\nend"))
      ;; Single arity
      (let [[args & body] rest-parts]
        (str (when docstring
               (str (->> (str/split-lines docstring)
                         (map #(str ";; " %))
                         (str/join "\n"))
                    "\n"))
             kw " " (render-form ctx name-sym)
             "(" (render-params ctx args) "):\n"
             (indent 2 (render-body-flat ctx body))
             "\nend")))))

(defn- render-if
  "Render if/if-let/if-some."
  [ctx form-name args]
  (let [has-bindings? (#{"if-let" "if-some"} (str form-name))
        [test then else has-else?] (if has-bindings?
                                     (let [[bindings & body] args]
                                       [bindings (first body) (second body) (>= (count body) 2)])
                                     [(first args) (second args) (nth args 2 nil) (>= (count args) 3)])
        test-str (if has-bindings?
                   (render-bindings ctx test)
                   (render-form ctx test))]
    (str (str form-name) " " test-str ":\n"
         (indent 2 (render-body-flat ctx [then]))
         (when has-else?
           (str "\nelse:\n"
                (indent 2 (render-body-flat ctx [else]))))
         "\nend")))

(defn- render-when
  "Render when/when-not and binding variants."
  [ctx form-name args]
  (let [has-bindings? (#{"when-let" "when-some" "when-first"} (str form-name))
        [test & body] args
        test-str (if has-bindings?
                   (render-bindings ctx test)
                   (render-form ctx test))]
    (str (str form-name) " " test-str ":\n"
         (indent 2 (render-body-flat ctx body))
         "\nend")))

(defn- render-let
  "Render let/binding/with-open/with-redefs."
  [ctx form-name [bindings & body]]
  (str (str form-name) " " (render-bindings ctx bindings) ":\n"
       (indent 2 (render-body-flat ctx body))
       "\nend"))

(defn- render-cond
  "Render cond."
  [ctx _form-name pairs]
  (let [clauses (partition 2 pairs)]
    (str "cond:\n"
         (->> clauses
              (map (fn [[test expr]]
                     (str "  " (render-form ctx test) " => "
                          (render-form ctx expr))))
              (str/join "\n"))
         "\nend")))

(defn- render-case
  "Render case."
  [ctx _form-name [expr & clauses]]
  (let [has-default? (odd? (count clauses))
        default (when has-default? (last clauses))
        clause-items (if has-default? (butlast clauses) clauses)
        pairs (partition 2 clause-items)]
    (str "case " (render-form ctx expr) ":\n"
         (->> pairs
              (map (fn [[test expr]]
                     (str "  " (render-form ctx test) " => "
                          (render-form ctx expr))))
              (str/join "\n"))
         (when default
           (str "\n  else => " (render-form ctx default)))
         "\nend")))

(defn- render-do
  "Render do block."
  [ctx _form-name body]
  (str "do:\n"
       (indent 2 (render-body-flat ctx body))
       "\nend"))

(defn- render-fn
  "Render anonymous fn."
  [ctx _form-name parts]
  (let [;; (fn name? [args] body) or (fn name? ([args] body) ...)
        [maybe-name & rest-parts] parts
        [fn-name rest-parts] (if (symbol? maybe-name)
                               [maybe-name rest-parts]
                               [nil parts])
        multi-arity? (and (list? (first rest-parts))
                          (vector? (ffirst rest-parts)))]
    (if multi-arity?
      (let [arities (for [arity rest-parts]
                      (let [[args & body] arity]
                        (str "  (" (render-params ctx args) "):\n"
                             (indent 4 (render-body-flat ctx body)))))]
        (str "fn" (when fn-name (str " " fn-name)) "\n"
             (str/join "\n" arities)
             "\nend"))
      (let [[args & body] rest-parts]
        (str "fn"
             (when fn-name (str " " fn-name))
             "(" (render-params ctx args) "):\n"
             (indent 2 (render-body-flat ctx body))
             "\nend")))))

(defn- render-loop
  "Render loop/recur."
  [ctx _form-name [bindings & body]]
  (if (empty? bindings)
    (str "loop:\n"
         (indent 2 (render-body-flat ctx body))
         "\nend")
    (str "loop " (render-bindings ctx bindings) ":\n"
         (indent 2 (render-body-flat ctx body))
         "\nend")))

(defn- render-try
  "Render try/catch/finally."
  [ctx _form-name body]
  (str "try:\n"
       (->> body
            (map (fn [form]
                   (if (and (sequential? form) (= 'catch (first form)))
                     (let [[_ exc-type binding & catch-body] form]
                       (str "catch " (render-form ctx exc-type) " "
                            (render-form ctx binding) ":\n"
                            (indent 2 (render-body-flat ctx catch-body))))
                     (if (and (sequential? form) (= 'finally (first form)))
                       (str "finally:\n"
                            (indent 2 (render-body-flat ctx (rest form))))
                       (str "  " (render-form ctx form))))))
            (str/join "\n"))
       "\nend"))

(defn- render-for
  "Render for/doseq/dotimes."
  [ctx form-name [bindings & body]]
  ;; bindings can have :when, :let, :while modifiers
  ;; For now, render binding pairs and pass modifiers through
  (let [parts (loop [remaining bindings
                     result []]
                (if (empty? remaining)
                  result
                  (let [[a b & rest] remaining]
                    (cond
                      (keyword? a)
                      (recur rest (conj result (str (name a) " " (render-form ctx b))))

                      :else
                      (recur rest (conj result (str (render-form ctx a) " in "
                                                    (render-form ctx b))))))))]
    (str (str form-name) " " (str/join ", " parts) ":\n"
         (indent 2 (render-body-flat ctx body))
         "\nend")))

(defn- render-def
  "Render def/defonce."
  [ctx form-name [sym & rest]]
  (let [[docstring val] (if (and (string? (first rest)) (second rest))
                          [(first rest) (second rest)]
                          [nil (first rest)])]
    (str (when docstring
           (str (->> (str/split-lines docstring)
                     (map #(str ";; " %))
                     (str/join "\n"))
                "\n"))
         (str form-name) " " (render-form ctx sym) " := " (render-form ctx val))))

(defn- render-ns-form
  "Render ns form. Keep it mostly literal but with cleaner formatting."
  [_ctx [_ ns-name & clauses]]
  (str "ns " ns-name "\n"
       (->> clauses
            (map #(str "  " (pr-str %)))
            (str/join "\n"))
       "\nend"))

(defn- render-interop-method
  "Render (.method obj args) as obj.method(args)."
  [ctx method-sym args]
  (let [method-name (subs (str method-sym) 1) ;; strip leading dot
        [obj & method-args] args]
    (str (render-form ctx obj) "." method-name
         "(" (comma-sep (render-children ctx method-args)) ")")))

(defn- render-interop-field
  "Render (.-field obj) as obj.-field to distinguish from dotted symbols."
  [ctx field-sym [obj]]
  (let [field-name (subs (str field-sym) 2)] ;; strip leading .-
    (str (render-form ctx obj) ".-" field-name)))

(defn- render-new
  "Render (ClassName. args) or (new ClassName args)."
  [ctx class-sym args]
  (let [class-name (let [s (str class-sym)]
                     (if (str/ends-with? s ".")
                       (subs s 0 (dec (count s)))
                       s))]
    (str "new " class-name "(" (comma-sep (render-children ctx args)) ")")))

(defn- render-condp
  "Render condp."
  [ctx _form-name [pred expr & clauses]]
  (let [has-default? (odd? (count clauses))
        default (when has-default? (last clauses))
        clause-items (if has-default? (butlast clauses) clauses)
        pairs (partition 2 clause-items)]
    (str "condp " (render-form ctx pred) " " (render-form ctx expr) ":\n"
         (->> pairs
              (map (fn [[test val]]
                     (str "  " (render-form ctx test) " => " (render-form ctx val))))
              (str/join "\n"))
         (when default
           (str "\n  else => " (render-form ctx default)))
         "\nend")))

;; --- Reserved word set ---

(def ^:private reserved-words
  #{"defn" "defn-" "fn" "fn-" "defmacro" "defonce" "def"
    "if-let" "if-some" "if-not" "if"
    "when-not" "when-let" "when-some" "when-first" "when"
    "with-open" "with-redefs"
    "let" "binding"
    "loop" "cond" "condp" "case"
    "do" "try" "catch" "finally"
    "doseq" "dotimes" "for" "while"
    "end" "else" "new" "not" "throw" "recur"
    "ns" "nil" "true" "false"
    "and" "or" "mod" "rem" "in"})

(defn- escape-symbol
  "Backtick-escape a symbol if it's a reserved word or not parseable as-is."
  [s]
  (let [n (str s)]
    (if (or (reserved-words n)
            ;; Symbols that clash with superficie syntax
            (str/starts-with? n ".")   ;; .method, .-field, ., ..
            (str/starts-with? n "@")   ;; @deref
            ;; Single minus is ambiguous with unary negation in surface syntax
            (= n "-"))
      (str "`" s "`")
      n)))

(defn render-data
  "Render a Clojure form as literal data (no infix, no special forms).
   Used for quoted content where the form is data, not code."
  [form]
  (cond
    (nil? form) "nil"
    (boolean? form) (str form)
    (number? form) (str form)
    (string? form) (pr-str form)
    (keyword? form) (str form)
    (char? form) (pr-str form)
    (regex? form) (str "#\"" form "\"")
    (symbol? form) (escape-symbol form)
    (vector? form) (str "[" (str/join ", " (map render-data form)) "]")
    (map? form) (str "{" (str/join ", " (map (fn [[k v]] (str (render-data k) " " (render-data v))) form)) "}")
    (set? form) (str "#{" (str/join ", " (map render-data form)) "}")
    (seq? form) (str "(" (str/join " " (map render-data form)) ")")
    :else (str form)))

;; --- Main dispatch ---

(defn render-form
  "Render a single Clojure form (sexpr) to superficie syntax."
  [ctx form]
  (try
    (cond
      (opaque/opaque-form? form) (opaque/opaque-raw form)

    ;; Nil, booleans, numbers, strings, keywords, regexes
      (nil? form) "nil"
      (boolean? form) (str form)
      (number? form) (str form)
      (string? form) (pr-str form)
      (keyword? form) (str form)
      (char? form) (pr-str form)
      (regex? form) (str "#\"" form "\"")

    ;; Symbols (with optional metadata)
      (symbol? form)
      (let [m (meta form)
            prefix (cond
                     (nil? m) ""
                   ;; ^:private shorthand
                     (and (= 1 (count m)) (:private m)) "^:private "
                   ;; ^:dynamic shorthand
                     (and (= 1 (count m)) (:dynamic m)) "^:dynamic "
                   ;; ^Type shorthand
                     (and (= 1 (count m)) (:tag m))
                     (let [tag (:tag m)]
                       (if (string? tag)
                         (str "^" (pr-str tag) " ")
                         (str "^" tag " ")))
                   ;; General metadata map
                     (seq m) (str "^" (pr-str m) " ")
                     :else "")]
        (str prefix (escape-symbol form)))

    ;; Vectors: [a, b, c] — handle & for destructuring
      (vector? form)
      (let [;; Handle & in vectors (destructuring)
            rendered (loop [items (seq form) result []]
                       (if (nil? items)
                         result
                         (let [item (first items)]
                           (if (= '& item)
                             (recur (nnext items)
                                    (conj result (str "& " (render-form ctx (second items)))))
                             (recur (next items)
                                    (conj result (render-form ctx item)))))))]
        (str "[" (comma-sep rendered) "]"))

    ;; Maps: {k1: v1, k2: v2}
      (map? form)
      (str "{"
           (->> form
                (map (fn [[k v]]
                       (str (render-form ctx k) " " (render-form ctx v))))
                (str/join ", "))
           "}")

    ;; Sets: #{a, b, c}
      (set? form) (str "#{" (comma-sep (render-children ctx (seq form))) "}")

    ;; Lists / S-expressions — the main dispatch
      (sequential? form)
      (if (empty? form)
        "()"
        (let [[head & args] form]
          (cond
          ;; Keyword as callable: (:key m) → :key(m)
            (keyword? head)
            (render-call ctx head args)

          ;; Not a symbol head — collections/fns as callables: #{:a}(x), {:k v}(:k), [1 2](0)
            (not (symbol? head))
            (cond
              (or (set? head) (map? head) (vector? head))
              (str (render-form ctx head) "(" (comma-sep (render-children ctx args)) ")")
            ;; Complex callable (call-expr result, etc.) — wrap in parens then call: (expr)(args)
              :else
              (str "(" (render-form ctx head) ")" "(" (comma-sep (render-children ctx args)) ")"))

          ;; Java interop: (.method obj args...)
            (and (str/starts-with? (str head) ".")
                 (not (str/starts-with? (str head) ".."))
                 (not= head '.))
            (if (str/starts-with? (str head) ".-")
              (render-interop-field ctx head args)
              (render-interop-method ctx head args))

          ;; Constructor: (ClassName. args...)
            (str/ends-with? (str head) ".")
            (render-new ctx head args)

            :else
          ;; Role-based dispatch
            (let [role (resolve/resolve-role ctx head)]
              (case role
                :infix-op    (render-infix ctx head args)
                :logical-op  (render-infix ctx head args)
                :threading   (render-threading ctx head (first args) (rest args))

                :not-form    (let [arg (first args)
                                   rendered (render-form ctx arg)
                                   arg-prec (expr-precedence ctx arg)]
                               (str "not " (wrap-if-lower rendered arg-prec (op-precedence 'not))))

                :deref-form  (str "@" (render-form ctx (first args)))
                :ns-form     (render-ns-form ctx form)

                :defn-form   (render-defn ctx head args)
                :def-form    (render-def ctx head args)

                :if-form     (render-if ctx head args)
                :if-bind-form (render-if ctx head args)
                :when-form   (render-when ctx head args)
                :when-bind-form (render-when ctx head args)
                :let-form    (render-let ctx head args)
                :loop-form   (render-loop ctx head args)
                :cond-form   (render-cond ctx head args)
                :case-form   (render-case ctx head args)
                :condp-form  (render-condp ctx head args)
                :do-form     (render-do ctx head args)
                :fn-form     (render-fn ctx head args)
                :try-form    (render-try ctx head args)
                :for-form    (render-for ctx head args)

              ;; No role — check special syntax
                (cond
                ;; recur
                  (= head 'recur)
                  (str "recur(" (comma-sep (render-children ctx args)) ")")

                ;; throw
                  (= head 'throw)
                  (str "throw " (render-form ctx (first args)))

                ;; new
                  (= head 'new)
                  (render-new ctx (first args) (rest args))

                ;; comment → s-expression passthrough (contains arbitrary code)
                  (= head 'comment)
                  (str "(comment " (str/join " " (map pr-str args)) ")")

                ;; quote → 'form (render content as data, not code)
                  (= head 'quote)
                  (let [quoted (first args)]
                    (if (seq? quoted)
                    ;; Quoted list → sexp passthrough so we don't mangle data
                      (str "'(" (str/join " " (map render-data quoted)) ")")
                    ;; Vectors, maps, sets, symbols → normal rendering
                      (str "'" (render-form ctx quoted))))

                ;; var → #'sym
                  (= head 'var)
                  (str "#'" (first args))

                ;; Default: function call
                  :else
                  (render-call ctx head args)))))))

      :else (pr-str form))
    (catch #?(:clj Exception :cljs :default) _
      ;; Fallback for forms we can't render (e.g. expanded syntax-quote internals)
      (pr-str form))))

;; --- Node-level rendering (for metadata) ---

(defn- render-meta-node
  "Render a :meta node, preserving the metadata prefix."
  [ctx meta-node]
  (let [children (->> (node/children meta-node)
                      (remove #(#{:whitespace :newline :comma} (node/tag %))))
        meta-val (first children)
        target (second children)]
    (str "^" (node/string meta-val) " " (render-top-node ctx target))))

(defn- sexp-passthrough
  "Emit a node as raw Clojure sexp, collapsing blank lines to avoid
   the surface syntax grammar treating them as top-level separators."
  [n]
  (str/replace (node/string n) #"\n\n+" "\n"))

(defn- render-top-node
  "Render a rewrite-clj node, handling metadata and reader macros at node level."
  [ctx n]
  (case (node/tag n)
    :meta (render-meta-node ctx n)
    ;; Reader macros: pass through literally
    (:quote :syntax-quote :unquote :unquote-splicing :var :fn :deref)
    (sexp-passthrough n)
    ;; Discard (#_): skip — these are dead code and can't roundtrip through
    ;; the surface grammar (since #_ can appear anywhere in Clojure).
    ;; Inline #_ within forms is handled by sexp passthrough of the parent.
    :uneval nil
    ;; Reader macros (#?, #?@, tagged literals): preserve literally
    :reader-macro (node/string n)
    ;; Regular forms — preserve opaque descendants exactly while still
    ;; rendering the surrounding structure in superficie syntax.
    (let [fallback (sexp-passthrough n)
          {:keys [source mapping]} (opaque/preprocess-source fallback {:include-comment? true
                                                                       :include-uneval? true})]
      (try
        (let [sexpr (node/sexpr (z/node (z/of-string source {:track-position? true})))
              restored (opaque/restore-opaque-forms sexpr mapping)
              rendered (render-form ctx restored)]
          ;; Validate roundtrip: if the rendered form doesn't parse back,
          ;; fall back to sexp passthrough for correctness.
          (parse/parse-string rendered)
          rendered)
        (catch #?(:clj Exception :cljs :default) _
          fallback)))))

;; --- Top-level API ---

(defn render-string
  "Render a Clojure source string to superficie syntax.
   Preserves line positions from the original source.
   Optional opts map:
     :runtime-roles - map of qualified-symbol -> role from superficie.runtime"
  ([source] (render-string source nil))
  ([source opts]
   (let [ctx (resolve/build-context source opts)
         zloc (z/of-string source {:track-position? true})
         forms (loop [loc zloc
                      result []]
                 (if (nil? loc)
                   result
                   (let [[row _col] (z/position loc)]
                     (recur (z/right loc)
                            (conj result {:node (z/node loc) :line row})))))
         rendered (->> forms
                       (mapv #(assoc % :text (render-top-node ctx (:node %))))
                       (filterv :text))]
    ;; Build output with line-number alignment
    ;; Ensure at least one blank line between top-level forms (grammar requires it)
     (loop [items rendered
            current-line 1
            lines []
            first? true]
       (if (empty? items)
         (str/join "\n" lines)
         (let [{:keys [text line]} (first items)
               text-lines (str/split-lines text)
              ;; Add blank lines to reach the target line
              ;; Ensure at least 2 newlines (= 1 blank line) between forms
               padding (if first?
                         (max 0 (- line current-line))
                         (max 2 (- line current-line)))
               padded-lines (into lines (repeat padding ""))
              ;; Add the rendered text lines
               new-lines (into padded-lines text-lines)
               new-current (+ line (count text-lines))]
           (recur (rest items) new-current new-lines false)))))))
