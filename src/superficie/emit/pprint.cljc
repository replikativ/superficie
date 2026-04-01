(ns superficie.emit.pprint
  "Pretty-printer: Clojure forms → idiomatic multi-line superficie text.
   Width-aware — uses indented parenthesized form for multi-line calls."
  (:require [superficie.emit.printer :as printer]
            [superficie.forms :as forms]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private default-width 80)
(def ^:private indent-step 2)

;; ---------------------------------------------------------------------------
;; Core: width-aware recursive formatter
;; ---------------------------------------------------------------------------

(declare pp)

(defn- flat
  "Single-line representation of a form (delegates to existing printer)."
  [form]
  (printer/print-form form))

(defn- indent-str
  "String of n spaces."
  [n]
  (apply str (repeat n \space)))

(defn- call?
  "Is form a non-empty seq (i.e., a call)?"
  [form]
  (and (seq? form) (seq form)))

;; ---------------------------------------------------------------------------
;; Call formatting — keeps leading args on head line when appropriate
;; ---------------------------------------------------------------------------

(def ^:private head-line-args
  "How many args to keep on the first line with the head.
   nil means no special treatment (default: break all to body)."
  {'def 1, 'def- 1,
   'defn 1, 'defn- 1, 'defmacro 1, 'defmulti 1, 'defmethod 2,
   'defprotocol 1, 'defrecord 1, 'deftype 1,
   'fn 0,
   'let 0, 'loop 0, 'binding 0, 'doseq 0, 'for 0,
   'if 1, 'if-not 1, 'if-let 0, 'if-some 0,
   'when 1, 'when-not 1, 'when-let 0, 'when-some 0, 'when-first 0,
   'cond 0, 'condp 2, 'case 1, 'cond-> 1, 'cond->> 1,
   'try 0, 'catch 2, 'finally 0,
   'do 0,
   'ns 1,
   '-> 1, '->> 1, 'some-> 1, 'some->> 1, 'as-> 2,
   'deftest 1, 'testing 1, 'is 0, 'are 0})

(defn- pp-call-smart
  "Pretty-print a call, keeping leading args with the head when appropriate."
  [form col width]
  (let [head (first form)
        args (rest form)
        head-str (flat head)
        flat-str (flat form)]
    (cond
      ;; No args
      (empty? args)
      (str head-str "()")

      ;; Fits flat
      (<= (+ col (count flat-str)) width)
      flat-str

      ;; Multi-line — check if we should keep some args on the head line
      :else
      (let [n-head-args (get head-line-args head)
            ;; Split into head-line args and body args
            [head-args body-args]
            (if (and n-head-args (pos? n-head-args) (> (count args) n-head-args))
              [(take n-head-args args) (drop n-head-args args)]
              [nil args])

            inner-col (+ col indent-step)
            inner-indent (indent-str inner-col)]

        (if head-args
          ;; Some args stay on the head line — but only if they fit
          (let [head-args-str (str/join " " (map flat head-args))
                first-line (str head-str "(" head-args-str)]
            (if (<= (+ col (count first-line)) width)
              ;; Head-line args fit: head(name [params]\n  body)
              (let [pp-body (map #(pp % inner-col width) body-args)
                    body (str/join (str "\n" inner-indent) pp-body)]
                (str first-line "\n"
                     inner-indent body ")"))
              ;; Head-line args don't fit: fall back to all-in-body
              (let [pp-args (map #(pp % inner-col width) (concat head-args body-args))
                    body (str/join (str "\n" inner-indent) pp-args)]
                (str head-str "(\n"
                     inner-indent body ")"))))

          ;; All args in body
          (let [pp-args (map #(pp % inner-col width) body-args)
                body (str/join (str "\n" inner-indent) pp-args)]
            (str head-str "(\n"
                 inner-indent body ")")))))))

;; ---------------------------------------------------------------------------
;; Collection formatting
;; ---------------------------------------------------------------------------

(defn- pp-vec
  "Pretty-print a vector."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (+ col indent-step)
            inner-indent (indent-str inner-col)
            outer-indent (indent-str col)
            elems (map #(pp % inner-col width) form)]
        (str "[\n"
             inner-indent (str/join (str "\n" inner-indent) elems) "\n"
             outer-indent "]")))))

(defn- pp-map
  "Pretty-print a map."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (+ col indent-step)
            inner-indent (indent-str inner-col)
            outer-indent (indent-str col)
            entries (map (fn [[k v]]
                           (let [pp-k (pp k inner-col width)
                                 ;; Value column: after the key's last line + space.
                                 ;; Single-line keys lack indent (it's added by the
                                 ;; join), so we add inner-col. Multi-line keys have
                                 ;; indentation baked into the last line by pp.
                                 last-line (peek (str/split-lines pp-k))
                                 multi-line? (not= last-line pp-k)
                                 val-col (if multi-line?
                                           (+ (count last-line) 1)
                                           (+ inner-col (count last-line) 1))]
                             (str pp-k " " (pp v val-col width))))
                         form)]
        (str "{\n"
             inner-indent (str/join (str "\n" inner-indent) entries) "\n"
             outer-indent "}")))))

(defn- pp-set
  "Pretty-print a set."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (+ col indent-step)
            inner-indent (indent-str inner-col)
            outer-indent (indent-str col)
            elems (map #(pp % inner-col width) form)]
        (str "#{\n"
             inner-indent (str/join (str "\n" inner-indent) elems) "\n"
             outer-indent "}")))))

;; ---------------------------------------------------------------------------
;; Comment extraction from :ws metadata
;; ---------------------------------------------------------------------------

(defn- extract-comments
  "Extract comment lines from a :ws metadata string.
   Returns a vector of comment strings (with leading ; intact), or nil."
  [ws]
  (when ws
    (let [lines (str/split-lines ws)]
      (not-empty (filterv #(re-find #"^\s*;" %) lines)))))

(defn- form-comments
  "Get comment lines from a form's :ws metadata, or nil."
  [form]
  (when (and (some? form)
             #?(:clj  (instance? clojure.lang.IMeta form)
                :cljs (satisfies? IMeta form))
             (meta form))
    (extract-comments (:ws (meta form)))))

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

;; ---------------------------------------------------------------------------
;; Main dispatch
;; ---------------------------------------------------------------------------

(defn- pp-meta-prefix
  "If form has user metadata (excluding :line/:column/:file/:ws), return
   the prefix string (e.g. \"^:private\") and the stripped form. Otherwise nil."
  [form]
  (when (and (some? form)
             #?(:clj  (instance? clojure.lang.IMeta form)
                :cljs (satisfies? IMeta form))
             (some? (meta form))
             (seq (forms/strip-internal-meta (meta form))))
    (let [m (forms/strip-internal-meta (meta form))
          prefix (cond
                   (and (= 1 (count m))
                        (keyword? (key (first m)))
                        (true? (val (first m))))
                   (str "^" (flat (key (first m))))
                   (and (= 1 (count m))
                        (contains? m :tag)
                        (symbol? (:tag m)))
                   (str "^" (flat (:tag m)))
                   :else
                   (str "^" (flat m)))]
      {:prefix prefix :stripped (with-meta form nil)})))

(defn- pp
  "Pretty-print a form at the given column and width."
  [form col width]
  (let [comments (form-comments form)
        indent (indent-str col)
        meta-info (pp-meta-prefix form)
        formatted (cond
                    ;; Metadata prefix — emit before the form, recurse on stripped
                    meta-info
                    (let [{:keys [prefix stripped]} meta-info
                          prefix-len (inc (count prefix))
                          inner (pp stripped (+ col prefix-len) width)]
                      (str prefix " " inner))

                    ;; Deferred auto-resolve keywords — must check before call?
                    ;; since the deferred form (clojure.core/read-string "::foo")
                    ;; satisfies call? but should emit ::foo, not a call.
                    (forms/deferred-auto-keyword? form)
                    (forms/deferred-auto-keyword-raw form)

                    ;; Quote — prefix sugar (only when tagged by reader)
                    (and (call? form) (= 'quote (first form)) (:sup/sugar (meta form)))
                    (str "'" (pp (second form) (inc col) width))

                    ;; @deref — prefix sugar (only when tagged by reader)
                    (and (call? form) (= 'clojure.core/deref (first form)) (:sup/sugar (meta form)))
                    (str "@" (pp (second form) (inc col) width))

                    ;; #'var — prefix sugar (only when tagged by reader)
                    (and (call? form) (= 'var (first form)) (:sup/sugar (meta form)))
                    (str "#'" (flat (second form)))

                    ;; Calls — the main case
                    (call? form)
                    (pp-call-smart form col width)

                    ;; Syntax-quote / unquote / unquote-splicing AST nodes
                    (forms/syntax-quote? form)
                    (str "`" (pp (:form form) (inc col) width))

                    (forms/unquote? form)
                    (str "~" (pp (:form form) (inc col) width))

                    (forms/unquote-splicing? form)
                    (str "~@" (pp (:form form) (+ col 2) width))

                    ;; AST node defrecords satisfy (map? x) — delegate to flat
                    ;; Must be before vector?/map? to avoid mishandling
                    (forms/raw? form) (flat form)
                    (forms/sup-reader-conditional? form) (flat form)

                    ;; Collections
                    (vector? form) (pp-vec form col width)
                    (map? form)    (pp-map form col width)
                    (set? form)    (pp-set form col width)

                    ;; Everything else — flat (primitives, empty list, etc.)
                    :else (flat form))]
    (if comments
      ;; First comment line: no indent (caller provides it via join/concat).
      ;; Subsequent comment lines: indent to current column.
      ;; All lines: strip original whitespace — pprint re-indents.
      (let [stripped (map str/triml comments)
            indented (cons (first stripped)
                           (map #(str indent %) (rest stripped)))]
        (str (str/join "\n" indented) "\n" indent formatted))
      formatted)))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn pprint-form
  "Pretty-print a single Clojure form as sup text.
   Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([form] (pprint-form form nil))
  ([form opts]
   (let [width (or (:width opts) default-width)]
     (pp form 0 width))))

(defn pprint-forms
  "Pretty-print a sequence of Clojure forms as sup text,
   separated by blank lines. Preserves comments from :ws metadata.
   opts: {:width 80}"
  ([forms] (pprint-forms forms nil))
  ([forms opts]
   (let [trailing-ws (:trailing-ws (meta forms))
         trailing-comments (when trailing-ws
                             (extract-comments trailing-ws))
         body (str/join "\n\n" (map #(pprint-form % opts) forms))]
     (if trailing-comments
       (str body "\n\n" (str/join "\n" trailing-comments))
       body))))
