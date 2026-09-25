(ns superficie.core
  "superficie public API: read and print superficie (.sup) syntax.

   Three tracks:
     text-to-form:  sup->forms, forms->sup (all platforms)
     form-to-text:  forms->clj (all platforms), clj->forms (JVM only)
     text-to-text:  sup->clj (all platforms), clj->sup (JVM only)

   Pipeline:
     superficie.pipeline/run — full ctx->ctx pipeline with intermediate state"
  (:require [superficie.emit.printer :as printer]
            [superficie.shapes :as shapes]
            [superficie.emit.pprint :as pprint]
            [superficie.forms :as forms]
            [superficie.parse.expander :as expander]
            [superficie.pipeline :as pipeline]
            #?(:cljs [superficie.parse.resolve :as resolve])
            #?(:cljs [clojure.string :as str])
            #?(:clj  [clojure.tools.reader :as tr])
            #?(:clj  [clojure.tools.reader.reader-types :as tr-types])
            #?(:cljs [cljs.tools.reader :as cljs-reader])
            #?(:cljs [cljs.tools.reader.reader-types :refer [indexing-push-back-reader]])))

;; ---------------------------------------------------------------------------
;; Text-to-form track
;; ---------------------------------------------------------------------------

(declare context-from-source)

(defn sup->forms
  "Read a superficie source string, return a vector of Clojure forms.
   opts map:
     :resolve-keyword — fn that resolves auto-resolve keyword strings (\"::foo\")
                        to keywords at read time. Required on CLJS.
     :read-cond       — :preserve to return ReaderConditional objects instead of
                        evaluating for the current platform.
     :context         — Clojure ns/require forms the snippet assumes (as for clj->sup)."
  ([s] (:forms (pipeline/run s)))
  ([s opts]
   (:forms (pipeline/run s (cond-> (dissoc opts :context)
                             (:context opts)
                             (assoc :ns-context (context-from-source (:context opts))))))))

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
       (with-meta (apply list (map normalize-syntax-quote form)) (meta form))

       (vector? form)
       (with-meta (vec (map normalize-syntax-quote form)) (meta form))

       (map? form)
       (with-meta (into {} (map (fn [[k v]] [(normalize-syntax-quote k)
                                             (normalize-syntax-quote v)]) form))
         (meta form))

       :else form)))

#?(:cljs
   (defn- tag-js-numbers
     "Clojure source with each number literal that JS would not print back as
      written (4.0, 1e3, 0xFF, 1/2, 2N, 1.5M, 2r101) wrapped as
      #superficie/num \"4.0\": the reader has no hook for a number's text, and
      JS reads 4.0 as 4. Character literals likewise become #superficie/char.
      Strings, comments and regexes are copied."
     [src]
     (let [n (count src)
           terminator? #(or (re-find #"[\s,;\"@^`~()\[\]{}\\]" %) false)
           sb (js/Array.)]
       (loop [i 0]
         (if (>= i n)
           (.join sb "")
           (let [c (.charAt src i)]
             (cond
               ;; string or regex: copy through the closing quote
               (or (= c "\"") (and (= c "#") (= "\"" (.charAt src (inc i)))))
               (let [start i
                     i (if (= c "#") (+ i 2) (inc i))
                     end (loop [j i]
                           (cond (>= j n) n
                                 (= "\\" (.charAt src j)) (recur (+ j 2))
                                 (= "\"" (.charAt src j)) (inc j)
                                 :else (recur (inc j))))]
                 (.push sb (subs src start end))
                 (recur end))

               ;; comment: copy to the end of the line
               (= c ";")
               (let [end (or (str/index-of src "\n" i) n)]
                 (.push sb (subs src i end))
                 (recur end))

               ;; character literal (\a, \newline, \( ...) — JS reads \a as the
               ;; string "a", so it is tagged to print as written
               (= c "\\")
               (let [end (min n (loop [j (+ i 2)]
                                  (if (and (< j n) (re-find #"[A-Za-z0-9]" (.charAt src j)))
                                    (recur (inc j))
                                    j)))]
                 (.push sb (str "#superficie/char " (pr-str (subs src i end))))
                 (recur end))

               ;; a token: read to its terminator; a numeric one may need a tag
               (not (terminator? c))
               (let [end (loop [j i]
                           (if (and (< j n) (not (terminator? (.charAt src j))))
                             (recur (inc j))
                             j))
                     tok (subs src i end)]
                 (.push sb (if (and (re-find #"^[+-]?\d" tok)
                                    ;; 017 is octal; a long integer loses digits
                                    (not= (str (js/parseInt tok 10)) (str/replace tok #"^\+" ""))
                                    (not= (str (js/parseFloat tok)) tok))
                             (str "#superficie/num \"" tok "\"")
                             tok))
                 (recur end))

               :else
               (do (.push sb c)
                   (recur (inc i))))))))))

#?(:cljs
   (defn- read-tagged-char [raw]
     (resolve/resolve-char raw {})))

#?(:cljs
   (defn- read-tagged-number [raw]
     (let [v (resolve/resolve-number raw {})]
       (if (forms/raw? v) v (forms/->SupRaw v raw)))))

(defn clj->forms
  "Read a Clojure source string, return a vector of forms.
   On JVM: uses clojure.tools.reader with syntax-quote preserved as AST nodes
           (SupSyntaxQuote / SupUnquote / SupUnquoteSplicing) rather than expanded.
   On CLJS: uses cljs.tools.reader with :read-cond :preserve."
  [clj-src]
  #?(:clj
     (let [read* @#'tr/read*
           rdr   (tr-types/indexing-push-back-reader clj-src)]
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
     (let [rdr (indexing-push-back-reader (tag-js-numbers clj-src))]
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
                   ;; primitives — literal value (a number literal kept as written too)
                   (or (keyword? x) (number? x) (string? x) (nil? x) (boolean? x)
                       (forms/raw? x))
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
                   ;; a number literal kept as written (#superficie/num) — a record,
                   ;; not a map to rebuild
                   (forms/raw? form) form
                   ;; inside a preserved #?(...) the tag is not read: read it here
                   (and (tagged-literal? form) (= 'superficie/num (:tag form)))
                   (read-tagged-number (:form form))
                   (and (tagged-literal? form) (= 'superficie/char (:tag form)))
                   (read-tagged-char (:form form))
                   ;; a reader conditional is a record too: keep its type
                   (forms/sup-reader-conditional? form)
                   (forms/make-reader-conditional (denorm (forms/rc-form form))
                                                  (forms/rc-splicing? form))

                   (sq-vec? form)
                   (forms/->SupSyntaxQuote
                    (with-meta (vec (map sq-concat-elem (concat-args (second form))))
                      (meta form)))
                   (sq-list? form)
                   (forms/->SupSyntaxQuote
                    (with-meta (apply list (map sq-concat-elem (concat-args form)))
                      (meta form)))
                   (sq-empty-list? form)
                   (forms/->SupSyntaxQuote '())
                   (seq? form)
                   (with-meta (apply list (map denorm form)) (meta form))
                   (vector? form)
                   (with-meta (mapv denorm form) (meta form))
                   (map? form)
                   (with-meta (into {} (map (fn [[k v]] [(denorm k) (denorm v)]) form))
                     (meta form))
                   :else form))]
         (binding [cljs-reader/resolve-symbol (fn [s] s)
                   cljs-reader/*data-readers* {'superficie/num read-tagged-number
                                               'superficie/char read-tagged-char}
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

(defn context-from-source
  "The namespace context (aliases and refers) established by the ns and
   top-level require forms of a Clojure source string."
  [src]
  (reduce (fn [ctx form] (or (shapes/ns-context form ctx) ctx))
          nil
          (clj->forms src)))

(defn clj->sup
  "Convert a Clojure source string to superficie syntax.
   Uses width-aware pretty-printing (default 80 columns).
   opts: {:width 80
          :context \"(require '[raster.core :refer [deftm]])\"} — ns/require forms
   the snippet assumes but does not contain, so a fragment of a file renders
   its library macros as blocks (a deftm, a spin, an a/theorem)."
  ([clj-src] (clj->sup clj-src nil))
  ([clj-src opts]
   (binding [shapes/*ns-context* (if-let [c (:context opts)]
                                   (context-from-source c)
                                   shapes/*ns-context*)]
     (pprint-sup (clj->forms clj-src) opts))))

;; ---------------------------------------------------------------------------
;; Pipeline access
;; ---------------------------------------------------------------------------

(defn run-pipeline
  "Run the full pipeline: source → scan → group → parse.
   Returns a context map with :source, :opts, :raw-tokens, :tokens,
   :shrubbery, :group-errors, and :forms."
  ([source] (pipeline/run source))
  ([source opts] (pipeline/run source opts)))
