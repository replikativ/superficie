(ns superficie.parse
  "Parses .sup surface syntax into Clojure S-expressions.
   This is the .sup -> Clojure direction."
  (:require [instaparse.core :as insta #?(:cljs :refer-macros :clj :refer) [defparser]]
            #?(:clj [clojure.java.io :as io])
            #?(:cljs [cljs.reader :as cljs-reader])
            [clojure.string :as str]))

#?(:clj (def parser (insta/parser (io/resource "superficie.ebnf")))
   :cljs (defparser parser "resources/superficie.ebnf"))

(defn- read-sym
  "Read a string as a Clojure value using the platform reader."
  [s]
  #?(:clj (read-string s)
     :cljs (cljs-reader/read-string s)))

(defn parse-raw
  "Parse a .sup string into a raw parse tree."
  [input]
  (parser input))

;; === S-expression reconstruction ===

(defn- reconstruct-sexp-node
  "Reconstruct an S-expression string from sexp parse nodes."
  [node]
  (cond
    (string? node) node
    (vector? node)
    (case (first node)
      :sexp (str "(" (str/join " " (map reconstruct-sexp-node (rest node))) ")")
      :sexp-vec (str "[" (str/join " " (map reconstruct-sexp-node (rest node))) "]")
      :sexp-map (str "{" (str/join " " (map reconstruct-sexp-node (rest node))) "}")
      :sexp-set (str "#{" (str/join " " (map reconstruct-sexp-node (rest node))) "}")
      :sexp-anon-fn (str "#(" (str/join " " (map reconstruct-sexp-node (rest node))) ")")
      :sexp-string (second node)
      :sexp-regex  (second node)
      :sexp-atom (second node)
      :sexp-comment ""  ; strip comments — they'd eat the rest of line when read-string linearises
      (str node))
    :else (str node)))

(defn- read-sexp
  "Read a reconstructed S-expression string."
  [& nodes]
  (let [s (str "(" (str/join " " (map reconstruct-sexp-node nodes)) ")")]
    (read-sym s)))

(defn- read-sexp-anon-fn
  "Read a reconstructed #(...) anonymous function string."
  [& nodes]
  (let [s (str "#(" (str/join " " (map reconstruct-sexp-node nodes)) ")")]
    (read-sym s)))

;; === Number parsing ===

(defn- parse-number [s]
  (cond
    (re-matches #"0[xX][0-9a-fA-F]+" s) #?(:clj (Long/parseLong (subs s 2) 16)
                                           :cljs (js/parseInt (subs s 2) 16))
    (str/ends-with? s "N") #?(:clj (bigint (subs s 0 (dec (count s))))
                              :cljs (js/parseInt (subs s 0 (dec (count s)))))
    (str/ends-with? s "M") #?(:clj (bigdec (subs s 0 (dec (count s))))
                              :cljs (js/parseFloat (subs s 0 (dec (count s)))))
    (str/includes? s "/") (let [[n d] (str/split s #"/")]
                            (/ #?(:clj (Long/parseLong n) :cljs (js/parseInt n))
                               #?(:clj (Long/parseLong d) :cljs (js/parseInt d))))
    (str/includes? s ".") #?(:clj (Double/parseDouble s) :cljs (js/parseFloat s))
    :else #?(:clj (Long/parseLong s) :cljs (js/parseInt s))))

;; === Core transform map ===
;; insta/transform walks bottom-up, so children are already transformed
;; when the parent rule fires.

(def ^:private xform
  {:program       (fn [& forms] (vec forms))

   ;; Atoms — use platform reader for symbol parsing
   :symbol          (fn [s] (if (string? s) (read-sym s) s))
   :escaped-symbol  (fn [s] (read-sym s))
   :dotted-symbol   (fn [s] (read-sym s))
   :dotted-ns-symbol (fn [s] (read-sym s))
   :operator-symbol (fn [s] (read-sym s))
   :number        (fn [s] (parse-number s))
   :string        (fn [s] (read-sym s))
   :keyword       (fn [s] (read-sym s))
   :regex         (fn [s] (re-pattern (subs s 2 (dec (count s)))))
   :char-literal  (fn [s] (read-sym s))
   :nil-literal   (constantly nil)
   :bool-literal  (fn [s] (= s "true"))

   ;; Collections
   :vector-literal (fn [& elems]
                     (vec (mapcat #(if (and (seq? %) (= '& (first %)))
                                     %
                                     [%])
                                  elems)))
   :map-literal    (fn [& entries] (apply hash-map (mapcat identity entries)))
   :map-entry      (fn [k v] [k v])
   :set-literal    (fn [& elems] (set elems))

   ;; Operators (keep as strings for now, parent rules handle them)
   :pipe-op    identity
   :pipe-method (fn [ident args]
                  ;; Returns the method call form for thread-first
                  ;; The symbol will be .method, and args vector is the method args
                  (apply list (symbol (str "." ident)) args))
   :comp-op    identity
   :add-op     identity
   :mul-op     identity

   ;; Expression rules
   :pipe-expr  (fn [& parts]
                 ;; parts: init pipe-op step pipe-op step ...
                 (if (= 1 (count parts))
                   (first parts)
                   (let [init (first parts)
                         pairs (partition 2 (rest parts))
                         op-sym (fn [op] (case op "|>" '->> ".>" '->))
                         ;; Group consecutive same-op steps into flat threading forms
                         ;; For mixed ops, nest them
                         steps (mapv (fn [[op step]]
                                       [(op-sym op)
                                        (if (and (sequential? step) (not (keyword? (first step))))
                                          step
                                          (list step))])
                                     pairs)
                         ;; Check if all ops are the same
                         first-op (first (first steps))
                         all-same? (every? #(= first-op (first %)) steps)]
                     (if all-same?
                       ;; Flat threading: (->> init (step1) (step2) ...)
                       (apply list first-op init (map second steps))
                       ;; Mixed: nest binary
                       (reduce (fn [acc [op step]]
                                 (list op acc step))
                               init steps)))))

   :or-expr    (fn [& parts]
                 (if (= 1 (count parts))
                   (first parts)
                   (apply list 'or parts)))

   :and-expr   (fn [& parts]
                 (if (= 1 (count parts))
                   (first parts)
                   (apply list 'and parts)))

   :comp-expr  (fn
                 ([x] x)
                 ([left op right] (list (symbol op) left right)))

   :add-expr   (fn [& parts]
                 ;; parts: operand op operand op operand ...
                 (if (= 1 (count parts))
                   (first parts)
                   (let [first-operand (first parts)
                         pairs (partition 2 (rest parts))]
                     (reduce (fn [acc [op operand]]
                               (list (symbol op) acc operand))
                             first-operand pairs))))

   :mul-expr   (fn [& parts]
                 (if (= 1 (count parts))
                   (first parts)
                   (let [first-operand (first parts)
                         pairs (partition 2 (rest parts))]
                     (reduce (fn [acc [op operand]]
                               (list (symbol op) acc operand))
                             first-operand pairs))))

   ;; Unary
   :not-expr    (fn [x] (list 'not x))
   :neg-expr    (fn [x] (if (number? x) (- x) (list '- x)))
   :deref-expr  (fn [x] (list 'deref x))
   :throw-expr  (fn [x] (list 'throw x))
   :recur-expr  (fn [args] (apply list 'recur args))

   ;; Postfix
   :postfix-expr (fn [base & ops]
                   (reduce (fn [acc op]
                             (if (fn? op) (op acc) acc))
                           base ops))
   :method-call  (fn [ident args]
                   ;; Returns a function that takes the object
                   (fn [obj] (apply list (symbol (str "." ident)) obj args)))
   :field-access (fn [ident]
                   (fn [obj] (list (symbol (str ".-" ident)) obj)))
   :ident        identity

   ;; Primary
   :paren-expr identity

   ;; New
   :new-expr   (fn [class-sym args]
                 (apply list (symbol (str class-sym ".")) args))
   :class-name (fn [s] (symbol s))

   ;; Function call — call-args returns a vec, call-expr splices it
   :call-expr  (fn [f args] (apply list f args))
   :call-args  (fn [& args] (vec args))

   ;; Definitions
   :def-form   (fn [kw sym val] (list (symbol kw) sym val))
   :def-kw     identity
   :defn-form  (fn [kw sym & rest]
                 (let [form-sym (case kw "defn" 'defn "defn-" 'defn- "defmacro" 'defmacro)]
                   (apply list form-sym sym rest)))
   :defn-kw    identity

   :multi-arity (fn [params & body]
                  (apply list params body))

   :params     (fn [& ps] (vec (mapcat #(if (and (seq? %) (= '& (first %)))
                                          %
                                          [%])
                                       ps)))
   :amp-param  (fn [sym] (list '& sym))

   ;; Anonymous fn
   :anon-fn    (fn [& parts]
                 ;; parts might start with fn-name or params/multi-arity
                 (apply list 'fn parts))
   :fn-name    (fn [s] (symbol s))

   ;; Block expressions
   :if-expr    (fn [kw & parts]
                 (apply list (symbol kw) parts))
   :if-kw      identity
   :if-bind-kw identity

   :when-expr  (fn [kw & parts]
                 (apply list (symbol kw) parts))
   :when-kw    identity
   :when-bind-kw identity

   :let-expr   (fn [kw bindings & body]
                 (apply list (symbol kw) bindings body))
   :let-kw     identity

   :loop-expr  (fn [& parts]
                 (if (vector? (first parts))
                   ;; Has bindings
                   (apply list 'loop (first parts) (rest parts))
                   ;; No bindings (loop : body end)
                   (apply list 'loop [] parts)))

   :cond-expr  (fn [& clauses]
                 (apply list 'cond (mapcat identity clauses)))
   :cond-clause (fn [test val] [test val])

   :condp-expr (fn [pred expr & parts]
                 ;; parts = cond-clause pairs + optional else value
                 (let [clauses (filter vector? parts)
                       else-val (first (filter #(not (vector? %)) parts))]
                   (apply list 'condp pred expr
                          (concat (mapcat identity clauses)
                                  (when else-val [else-val])))))

   :case-expr  (fn [dispatch & parts]
                 ;; parts = clause pairs + optional else value
                 (let [clauses (filter vector? parts)
                       else-val (first (filter #(not (vector? %)) parts))]
                   (apply list 'case dispatch
                          (concat (mapcat identity clauses)
                                  (when else-val [else-val])))))
   :case-clause (fn [test val] [test val])

   :for-expr   (fn [kw bindings & body]
                 (apply list (symbol kw) bindings body))
   :for-kw     identity
   :for-bindings (fn [& bindings] (vec (mapcat identity bindings)))
   :for-binding  (fn [& parts] parts)
   :for-modifier (fn [s] (keyword s))

   :do-expr    (fn [& body] (apply list 'do body))

   :try-expr   (fn [& parts]
                 (apply list 'try parts))
   :catch-clause   (fn [exc-type binding & body]
                     (apply list 'catch exc-type binding body))
   :finally-clause (fn [& body]
                     (apply list 'finally body))

   ;; Bindings
   :bindings   (fn [& pairs] (vec (mapcat identity pairs)))
   :binding    (fn [target val] [target val])

   ;; NS form
   :ns-form    (fn [sym & clauses]
                 (apply list 'ns sym clauses))
   :sexp         read-sexp
   :sexp-anon-fn read-sexp-anon-fn
   :sexp-string  identity
   :sexp-regex   identity
   :sexp-comment (constantly "")

   ;; Reader macros
   :quote-form (fn [x] (list 'quote x))
   :minus-symbol (fn [_] '-)
   :var-form   (fn [sym] (list 'var sym))

   ;; Metadata
   :meta-symbol identity
   :metadata    (fn [meta-val target]
                  (let [m (cond
                            (keyword? meta-val) {meta-val true}
                            (symbol? meta-val)  {:tag meta-val}
                            (string? meta-val)  {:tag meta-val}
                            (map? meta-val)     meta-val
                            :else               {})]
                    (if #?(:clj (instance? clojure.lang.IObj target)
                           :cljs (implements? IWithMeta target))
                      (with-meta target m)
                      target)))

   ;; Top-level
   :top-form   identity})

;; === Public API ===

(defn- strip-leading-comments
  "Remove leading comment-only lines from input so the program grammar can start.
   Comment lines like ';; ...' that precede a top-level form are not valid top-forms,
   but they appear in rendered output when render-defn includes docstring as comments.
   Only strips comments at the very start of the file, not throughout."
  [input]
  (str/replace input #"\A([ \t]*;;[^\n]*\n)+" ""))

(defn parse-string
  "Parse a .sup string and return Clojure S-expressions."
  [input]
  (let [clean (strip-leading-comments input)
        tree (parse-raw clean)]
    (if (insta/failure? tree)
      (throw (ex-info "Parse error" {:failure (insta/get-failure tree)
                                     :input clean}))
      (let [result (insta/transform xform tree)]
        (if (and (vector? result) (= 1 (count result)))
          (first result)
          result)))))

#?(:clj
   (defn parse-file
     "Parse a .sup file and return Clojure S-expressions."
     [path]
     (parse-string (slurp path))))
