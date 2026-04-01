(ns superficie.core
  "superficie public API: read and print superficie (.sup) syntax.

   Three tracks:
     text-to-form:  sup->forms, forms->sup (all platforms)
     form-to-text:  forms->clj (all platforms), clj->forms (JVM only)
     text-to-text:  sup->clj (all platforms), clj->sup (JVM only)

   Pipeline:
     superficie.pipeline/run — full ctx->ctx pipeline with intermediate state"
  (:require [superficie.emit.printer :as printer]
            [superficie.emit.pprint :as pprint]
            [superficie.forms :as forms]
            [superficie.parse.expander :as expander]
            [superficie.pipeline :as pipeline]
            #?(:clj  [clojure.tools.reader :as tr])
            #?(:clj  [clojure.tools.reader.reader-types :as tr-types])
            #?(:cljs [cljs.tools.reader :as cljs-reader])
            #?(:cljs [cljs.tools.reader.reader-types :refer [string-push-back-reader]])))

;; ---------------------------------------------------------------------------
;; Text-to-form track
;; ---------------------------------------------------------------------------

(defn sup->forms
  "Read a superficie source string, return a vector of Clojure forms.
   opts map:
     :resolve-keyword — fn that resolves auto-resolve keyword strings (\"::foo\")
                        to keywords at read time. Required on CLJS.
     :read-cond       — :preserve to return ReaderConditional objects instead of
                        evaluating for the current platform."
  ([s] (:forms (pipeline/run s)))
  ([s opts] (:forms (pipeline/run s opts))))

(defn forms->sup
  "Print Clojure forms as a superficie source string (single-line per form)."
  [forms]
  (printer/print-sup-string forms))

(defn pprint-sup
  "Pretty-print Clojure forms as superficie source (multi-line, indented).
   opts: {:width 80}"
  ([forms] (pprint/pprint-forms forms))
  ([forms opts] (pprint/pprint-forms forms opts)))

;; ---------------------------------------------------------------------------
;; Form-to-text track
;; ---------------------------------------------------------------------------

(defn forms->clj
  "Print Clojure forms as a Clojure source string."
  [forms]
  (printer/print-clj-string (expander/expand-forms forms)))

#?(:clj  (def ^:private eof-sentinel (Object.))
   :cljs (def ^:private eof-sentinel #js {}))

#?(:clj
   (defn- normalize-syntax-quote
     "Walk a form produced by clojure.tools.reader (with read-syntax-quote
      overridden to produce plain lists) and convert:
        (clojure.core/syntax-quote x)    → SupSyntaxQuote{:form x}
        (clojure.core/unquote x)         → SupUnquote{:form x}
        (clojure.core/unquote-splicing x) → SupUnquoteSplicing{:form x}
      so the printer can emit ` ~ ~@ notation faithfully."
     [form]
     (cond
       (and (seq? form) (= 'clojure.core/syntax-quote (first form)))
       (forms/->SupSyntaxQuote (normalize-syntax-quote (second form)))

       (and (seq? form) (= 'clojure.core/unquote (first form)))
       (forms/->SupUnquote (normalize-syntax-quote (second form)))

       (and (seq? form) (= 'clojure.core/unquote-splicing (first form)))
       (forms/->SupUnquoteSplicing (normalize-syntax-quote (second form)))

       (seq? form)
       (apply list (map normalize-syntax-quote form))

       (vector? form)
       (vec (map normalize-syntax-quote form))

       (map? form)
       (into {} (map (fn [[k v]] [(normalize-syntax-quote k)
                                  (normalize-syntax-quote v)]) form))

       :else form)))

(defn clj->forms
  "Read a Clojure source string, return a vector of forms.
   On JVM: uses clojure.tools.reader with syntax-quote preserved as AST nodes
           (SupSyntaxQuote / SupUnquote / SupUnquoteSplicing) rather than expanded.
   On CLJS: uses cljs.tools.reader with :read-cond :preserve."
  [clj-src]
  #?(:clj
     (let [read* @#'tr/read*
           rdr   (tr-types/string-push-back-reader clj-src)]
       (with-redefs [clojure.tools.reader/read-syntax-quote
                     (fn [rdr _backquote opts pending-forms]
                       (list 'clojure.core/syntax-quote
                             (read* rdr true nil opts pending-forms)))]
         (loop [forms []]
           (let [form (try
                        (tr/read {:read-cond :preserve :eof eof-sentinel} rdr)
                        (catch Exception e
                          (throw (ex-info (str "Clojure read error: " (ex-message e)) {} e))))]
             (if (identical? form eof-sentinel)
               (mapv normalize-syntax-quote forms)
               (recur (conj forms form)))))))
     :cljs
     ;; CLJS cannot intercept read-syntax-quote (private, advanced-compiled).
     ;; Instead: bind resolve-symbol to identity (no qualification), read
     ;; normally, then reverse the cljs.core/sequence+concat expansion back
     ;; to SupSyntaxQuote / SupUnquote / SupUnquoteSplicing AST nodes.
     ;;
     ;; CLJS expansion patterns (from cljs.tools.reader/syntax-quote-coll):
     ;;   list  → (cljs.core/sequence (cljs.core/concat <elems>...))
     ;;   vec   → (cljs.core/vec (cljs.core/sequence (cljs.core/concat <elems>...)))
     ;;   empty → (cljs.core/list)
     ;; Inside concat, each element is one of:
     ;;   (clojure.core/list x) — x is the element value (x is unquoted if bare sym)
     ;;   bare-form              — was ~@something (unquote-splicing)
     ;;
     ;; Within (clojure.core/list x):
     ;;   x = (quote sym) → literal symbol
     ;;   x = primitive   → literal value
     ;;   x = (cljs.core/sequence …) or (cljs.core/vec …) → literal nested form
     ;;   x = anything else → was ~x (SupUnquote)
     (let [rdr (string-push-back-reader clj-src)]
       (letfn [(sq-list? [f]
                 (and (seq? f)
                      (= 'cljs.core/sequence (first f))
                      (seq? (second f))
                      (= 'cljs.core/concat (first (second f)))))
               (sq-vec? [f]
                 (and (seq? f) (= 'cljs.core/vec (first f)) (sq-list? (second f))))
               (sq-empty-list? [f]
                 (and (seq? f) (= 'cljs.core/list (first f)) (= 1 (count f))))
               (concat-args [sq-list]
                 (rest (second sq-list)))
               ;; Reverse one element inside a syntax-quoted collection.
               ;; Returns the plain Clojure value (symbols, SupUnquote, etc.)
               ;; — NOT wrapped in SupSyntaxQuote.
               (sq-item [x]
                 (cond
                   ;; (quote sym) — literal symbol preserved as-is
                   (and (seq? x) (= 'quote (first x)))
                   (second x)
                   ;; primitives — literal value
                   (or (keyword? x) (number? x) (string? x) (nil? x) (boolean? x))
                   x
                   ;; nested list expansion — reconstruct the plain list
                   (sq-list? x)
                   (apply list (map sq-concat-elem (concat-args x)))
                   ;; nested vec expansion — reconstruct the plain vector
                   (sq-vec? x)
                   (vec (map sq-concat-elem (concat-args (second x))))
                   ;; empty list
                   (sq-empty-list? x)
                   '()
                   ;; anything else was ~x (SupUnquote)
                   :else
                   (forms/->SupUnquote (denorm x))))
               ;; Process one argument to cljs.core/concat.
               (sq-concat-elem [concat-arg]
                 (if (and (seq? concat-arg)
                          (= 'clojure.core/list (first concat-arg)))
                   ;; (clojure.core/list x) — single element
                   (sq-item (second concat-arg))
                   ;; bare form — was ~@something (SupUnquoteSplicing)
                   (forms/->SupUnquoteSplicing (denorm concat-arg))))
               ;; Top-level reverse: detect SQ expansion or recurse.
               (denorm [form]
                 (cond
                   (sq-vec? form)
                   (forms/->SupSyntaxQuote
                    (vec (map sq-concat-elem (concat-args (second form)))))
                   (sq-list? form)
                   (forms/->SupSyntaxQuote
                    (apply list (map sq-concat-elem (concat-args form))))
                   (sq-empty-list? form)
                   (forms/->SupSyntaxQuote '())
                   (seq? form)
                   (apply list (map denorm form))
                   (vector? form)
                   (mapv denorm form)
                   (map? form)
                   (into {} (map (fn [[k v]] [(denorm k) (denorm v)]) form))
                   :else form))]
         (binding [cljs-reader/resolve-symbol (fn [s] s)
                   cljs-reader/*default-data-reader-fn* tagged-literal]
           (loop [forms []]
             (let [form (cljs-reader/read
                         {:read-cond :preserve :eof eof-sentinel} rdr)]
               (if (identical? form eof-sentinel)
                 (mapv denorm forms)
                 (recur (conj forms form))))))))))

;; ---------------------------------------------------------------------------
;; Text-to-text track
;; ---------------------------------------------------------------------------

(defn sup->clj
  "Convert a superficie source string to a Clojure source string.
   Reader conditionals are preserved (not evaluated for current platform).
   opts: same as sup->forms."
  ([sup-src] (forms->clj (sup->forms sup-src {:read-cond :preserve})))
  ([sup-src opts] (forms->clj (sup->forms sup-src (merge {:read-cond :preserve} opts)))))

(defn clj->sup
  "Convert a Clojure source string to superficie syntax."
  [clj-src]
  (forms->sup (clj->forms clj-src)))

;; ---------------------------------------------------------------------------
;; Pipeline access
;; ---------------------------------------------------------------------------

(defn run-pipeline
  "Run the full pipeline: source → scan → group → parse.
   Returns a context map with :source, :opts, :raw-tokens, :tokens,
   :shrubbery, :group-errors, and :forms."
  ([source] (pipeline/run source))
  ([source opts] (pipeline/run source opts)))
