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
            [clojure.string :as str]
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

;; ---------------------------------------------------------------------------
;; Comments of Clojure source
;;
;; The Clojure reader discards ; comments. To keep them, the source is scanned
;; for comments and each one is attached to a form (see attach-comments) as the
;; :ws metadata the superficie reader gives a form its leading comments, which
;; the pretty-printer writes above the form. A comment at the end of a line
;; therefore moves onto its own line above the code it ended.
;; ---------------------------------------------------------------------------

(defn- scan-comments
  "The ; comments of Clojure source src, in order, as maps
   {:line :column :text} (1-based, as the indexing reader counts).
   Strings, regexes and character literals (\\; \\\") are skipped."
  [src]
  (let [n (count src)]
    (loop [i 0 line 1 col 1 out []]
      (if (>= i n)
        out
        (let [c (nth src i)]
          (cond
            (= c \newline) (recur (inc i) (inc line) 1 out)

            ;; string or regex: skip to the closing quote, counting lines
            (or (= c \") (and (= c \#) (< (inc i) n) (= \" (nth src (inc i)))))
            (let [j0 (if (= c \#) (+ i 2) (inc i))
                  [j line col] (loop [j j0 line line col (+ col (- j0 i))]
                                 (cond (>= j n) [j line col]
                                       (= \\ (nth src j)) (if (and (< (inc j) n) (= \newline (nth src (inc j))))
                                                            (recur (+ j 2) (inc line) 1)
                                                            (recur (+ j 2) line (+ col 2)))
                                       (= \" (nth src j)) [(inc j) line (inc col)]
                                       (= \newline (nth src j)) (recur (inc j) (inc line) 1)
                                       :else (recur (inc j) line (inc col))))]
              (recur j line col out))

            ;; character literal: \; and \" are not a comment or a string
            (= c \\) (recur (min n (+ i 2)) line (+ col 2) out)

            (= c \;)
            (let [end (loop [j i] (if (or (>= j n) (= \newline (nth src j))) j (recur (inc j))))
                  text (str/trimr (subs src i end))]
              (recur end line (+ col (- end i)) (conj out {:line line :column col :text text})))

            :else (recur (inc i) line (inc col) out)))))))

(defn- positioned
  "[line column] of key-l/key-c in form's own metadata, or nil."
  [form key-l key-c]
  (when #?(:clj (instance? clojure.lang.IMeta form) :cljs (satisfies? IMeta form))
    (let [m (meta form)
          l (get m key-l)
          c (get m key-c)]
      (when (and l c) [l c]))))

(defn- wrapper-parts
  "The parts of a form without a position of its own: a reader-macro
   wrapper such as (clojure.core/deref x) or a syntax-quote record."
  [form]
  (cond
    (and (record? form) (contains? form :form)) [(:form form)]
    (and (seq? form) (not (record? form))) form
    :else nil))

(defn- form-start
  "[line column] where form starts, or nil. A reader-macro wrapper such as
   (clojure.core/deref x) starts where its first positioned part does."
  [form]
  (or (positioned form :line :column)
      (some form-start (wrapper-parts form))))

(defn- form-end
  "[line column] just past the end of form, or nil."
  [form]
  (or (positioned form :end-line :end-column)
      (some form-end (reverse (wrapper-parts form)))))

(defn- pos<
  [[l1 c1] [l2 c2]]
  (or (< l1 l2) (and (= l1 l2) (< c1 c2))))

(defn- add-comments
  "Append comments cs to form's leading :ws comments."
  [form cs]
  (if (seq cs)
    (vary-meta form (fn [m]
                      (assoc m
                             :ws (str/join "\n" (cond->> (map :text cs)
                                                  (:ws m) (cons (:ws m))))
                             :ws-line (or (:ws-line m) (:line (first cs))))))
    form))

(defn- comment-kids
  "The forms inside x a comment can attach to, in reading order, or nil
   when x is not a collection."
  [x]
  (cond
    (record? x) (when (contains? x :form) [(:form x)])
    (map? x) (mapcat identity x)
    (coll? x) (seq x)
    :else nil))

(defn- with-comment-kids
  "x rebuilt from kids, as comment-kids returned them."
  [x kids]
  (cond
    (record? x) (assoc x :form (first kids))
    (seq? x) (with-meta (apply list kids) (meta x))
    (vector? x) (with-meta (vec kids) (meta x))
    (map? x) (with-meta (into (empty x) (map vec (partition 2 kids))) (meta x))
    (set? x) (with-meta (into (empty x) kids) (meta x))))

(defn- can-hold-comments? [x]
  #?(:clj (instance? clojure.lang.IObj x) :cljs (satisfies? IWithMeta x)))

(defn- add-eol-comments
  "Attach cs, comments at the end of `line`, to the innermost form of x
   that ends on that line: the comment is about that code, and the
   pretty-printer writes it above the nearest enclosing form it lays out."
  [x line cs]
  (let [kids (vec (comment-kids x))
        i (last (keep-indexed (fn [i k]
                                (when (and (can-hold-comments? k)
                                           (= line (first (form-end k))))
                                  i))
                              kids))]
    (if i
      (with-comment-kids x (update kids i add-eol-comments line cs))
      (add-comments x cs))))

(defn- attach-comments
  "Attach comments (from scan-comments) to forms, as the :ws metadata
   (comment lines) and :ws-line (line of the first) the pretty-printer writes
   above a form. Within each collection, a comment at the end of a line goes
   onto the innermost form ending on that line (add-eol-comments); any other
   comment goes onto the next element after it, or, when none follows, onto
   the collection itself. Elements without a position (numbers, keywords,
   strings) take none. Comments after the last top-level form go onto the
   returned vector as :trailing-ws / :trailing-ws-line."
  [forms comments]
  (let [pending (volatile! (seq comments))
        take-before (fn [pos]
                      (loop [acc []]
                        (let [c (first @pending)]
                          (if (and c pos (or (= ::end pos) (pos< [(:line c) (:column c)] pos)))
                            (do (vswap! pending next) (recur (conj acc c)))
                            acc))))
        split-eol (fn [acc prev cs]
                    ;; cs: comments before the next element; one on the line
                    ;; where the previous element ends is about that element
                    (let [prev-end (when prev (form-end (nth acc prev)))
                          [eol others] (if prev-end
                                         (split-with #(= (:line %) (first prev-end)) cs)
                                         [nil cs])]
                      [(cond-> acc (seq eol) (update prev add-eol-comments (first prev-end) eol))
                       others]))]
    (letfn [;; Distribute the comments before `end` (::end: all of them;
            ;; nil, for a collection without a position: none after its
            ;; last element) among kids; return [kids' leftover-comments].
            (walk-kids [kids end]
              (loop [kids (seq kids) acc [] prev nil carried []]
                (if kids
                  (if-let [start (let [k (first kids)]
                                   (when (can-hold-comments? k) (form-start k)))]
                    (let [[acc others] (split-eol acc prev (take-before start))
                          k' (walk (add-comments (first kids) (concat carried others)))]
                      (recur (next kids) (conj acc k') (count acc) []))
                    (recur (next kids) (conj acc (walk (first kids))) prev carried))
                  (let [[acc others] (split-eol acc prev (take-before end))]
                    [acc (vec (concat carried others))]))))
            ;; Rebuild x with the comments inside it attached.
            (walk [x]
              (if-let [kids (when (can-hold-comments? x) (comment-kids x))]
                (let [[kids' left] (walk-kids kids (form-end x))]
                  (add-comments (with-comment-kids x kids') left))
                x))]
      (let [[out left] (walk-kids forms ::end)]
        (cond-> out
          (seq left) (with-meta {:trailing-ws (str/join "\n" (map :text left))
                                 :trailing-ws-line (:line (first left))}))))))

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

(declare read-clj-forms)

(defn clj->forms
  "Read a Clojure source string, return a vector of forms.
   On JVM: uses clojure.tools.reader with syntax-quote preserved as AST nodes
           (SupSyntaxQuote / SupUnquote / SupUnquoteSplicing) rather than expanded.
   On CLJS: uses cljs.tools.reader with :read-cond :preserve.
   opts: {:comments true} attaches the source's ; comments to the forms
         (see attach-comments), for the pretty-printer to keep."
  ([clj-src] (clj->forms clj-src nil))
  ([clj-src opts]
   (if (:comments opts)
     (attach-comments (clj->forms clj-src nil)
                      (scan-comments #?(:clj clj-src :cljs (tag-js-numbers clj-src))))
     (read-clj-forms clj-src))))

(defn- read-clj-forms
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
   its library macros as blocks (a deftm, a spin, an a/theorem).
   :comments false drops the source's ; comments; by default each is kept,
   written on its own line above the form that follows it."
  ([clj-src] (clj->sup clj-src nil))
  ([clj-src opts]
   (binding [shapes/*ns-context* (if-let [c (:context opts)]
                                   (context-from-source c)
                                   shapes/*ns-context*)]
     (pprint-sup (clj->forms clj-src {:comments (not (false? (:comments opts)))}) opts))))

;; ---------------------------------------------------------------------------
;; Pipeline access
;; ---------------------------------------------------------------------------

(defn run-pipeline
  "Run the full pipeline: source → scan → group → parse.
   Returns a context map with :source, :opts, :raw-tokens, :tokens,
   :shrubbery, :group-errors, and :forms."
  ([source] (pipeline/run source))
  ([source opts] (pipeline/run source opts)))
