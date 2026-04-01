(ns superficie.parse.expander
  "Syntax-quote expansion: SupSyntaxQuote AST nodes → plain Clojure forms.
   Called by runtime paths before eval. Tooling paths use the AST nodes directly."
  (:require [superficie.errors :as errors]
            [superficie.forms :as forms]))

;; ---------------------------------------------------------------------------
;; Symbol qualification inside syntax-quote
;; ---------------------------------------------------------------------------

(defn- sq-qualify
  "Qualify a symbol for syntax-quote. Uses :resolve-symbol from opts when
   available; otherwise returns the symbol as-is (best effort without ns context)."
  [sym opts]
  (if-let [resolver (:resolve-symbol opts)]
    (resolver sym)
    sym))

;; ---------------------------------------------------------------------------
;; Auto-gensym  (foo# → foo__NNN__auto__)
;; ---------------------------------------------------------------------------

(def ^:private ^:dynamic *gsenv* nil)

(defn- auto-gensym [sym]
  (let [n (name sym)]
    (if-not (clojure.string/ends-with? n "#")
      sym
      (let [base (subs n 0 (dec (count n)))]
        (or (get @*gsenv* sym)
            (let [gs (symbol (str base "__" (gensym) "__auto__"))]
              (vswap! *gsenv* assoc sym gs)
              gs))))))

;; ---------------------------------------------------------------------------
;; Core expansion  (mirrors Clojure's LispReader/SyntaxQuoteReader)
;; ---------------------------------------------------------------------------

(declare expand-sq)

(defn- expand-items
  "Expand a sequence of items for use inside concat. Each item becomes either
   a splice or a (list ...) wrapper. Must be eager (mapv) so that auto-gensym
   is called while *gsenv* is still bound."
  [items opts loc]
  (mapv (fn [item]
          (cond
            (forms/unquote-splicing? item) (:form item)
            (forms/unquote? item)          (list 'clojure.core/list (:form item))
            :else                          (list 'clojure.core/list (expand-sq item opts loc))))
        items))

(defn expand-sq
  "Expand a form inside syntax-quote into a Clojure expression that, when
   evaluated, produces the quoted result. Mirrors SyntaxQuoteReader semantics."
  [form opts loc]
  (cond
    (forms/unquote? form)
    (:form form)

    (forms/unquote-splicing? form)
    (errors/reader-error "Unquote-splicing (~@) not in collection" loc)

    (symbol? form)
    (list 'quote (auto-gensym (sq-qualify form opts)))

    (seq? form)
    (if (empty? form)
      (list 'clojure.core/list)
      (list 'clojure.core/seq
            (cons 'clojure.core/concat (expand-items form opts loc))))

    (vector? form)
    (list 'clojure.core/apply 'clojure.core/vector
          (cons 'clojure.core/concat (expand-items form opts loc)))

    ;; SupRaw — unwrap to plain value
    (forms/raw? form) (:value form)

    ;; Nested syntax-quote
    (forms/syntax-quote? form)
    (binding [*gsenv* (volatile! {})]
      (expand-sq (:form form) opts loc))

    (map? form)
    (let [pairs (mapcat (fn [[k v]]
                          [(list 'clojure.core/list (expand-sq k opts loc))
                           (list 'clojure.core/list (expand-sq v opts loc))])
                        form)]
      (list 'clojure.core/apply 'clojure.core/hash-map
            (cons 'clojure.core/concat pairs)))

    (set? form)
    (list 'clojure.core/apply 'clojure.core/hash-set
          (cons 'clojure.core/concat (expand-items (seq form) opts loc)))

    ;; Keyword, number, string, nil, boolean — self-quoting
    :else form))

;; ---------------------------------------------------------------------------
;; Tree walk — expand all AST nodes
;; ---------------------------------------------------------------------------

(defn expand-syntax-quotes
  "Walk form and expand all SupSyntaxQuote nodes into plain Clojure forms.
   Also unwraps SupRaw values. Called by runtime paths before eval."
  ([form] (expand-syntax-quotes form nil))
  ([form opts]
   (cond
     (forms/raw? form)
     (:value form)

     (forms/syntax-quote? form)
     (binding [*gsenv* (volatile! {})]
       (expand-sq (:form form) opts
                  (select-keys (meta form) [:line :col])))

     ;; Unquote/unquote-splicing outside syntax-quote: walk the inner form
     ;; (they'll be caught as errors if they end up in expand-sq)
     (forms/unquote? form)
     (forms/->SupUnquote (expand-syntax-quotes (:form form) opts))

     (forms/unquote-splicing? form)
     (forms/->SupUnquoteSplicing (expand-syntax-quotes (:form form) opts))

     (seq? form)
     (with-meta (apply list (map #(expand-syntax-quotes % opts) form)) (meta form))

     (vector? form)
     (with-meta (mapv #(expand-syntax-quotes % opts) form) (meta form))

     (map? form)
     (with-meta (into {} (map (fn [[k v]]
                                [(expand-syntax-quotes k opts)
                                 (expand-syntax-quotes v opts)])
                              form))
                (meta form))

     (set? form)
     (with-meta (set (map #(expand-syntax-quotes % opts) form)) (meta form))

     :else form)))

(defn expand-forms
  "Expand syntax-quote nodes in a vector of forms. For eval pipelines."
  ([forms] (expand-forms forms nil))
  ([forms opts] (mapv #(expand-syntax-quotes % opts) forms)))
