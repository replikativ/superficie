(ns superficie.emit.pprint
  "Pretty-printer: Clojure forms → idiomatic multi-line superficie text.
   Two strategies, applied consistently to vectors, maps, sets, and call args:
     1. Line-hints: when :line metadata from the reader is available,
        break where the original Clojure source broke.
     2. Fill: when no :line metadata, pack items onto lines up to width."
  (:require [superficie.emit.printer :as printer]
            [superficie.forms :as forms]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Configuration
;; ---------------------------------------------------------------------------

(def ^:private default-width 80)
(def ^:private indent-step 2)

(defn- block-form?
  "True if head is a block form (defn, if, let, etc.) that the printer
   handles with block syntax."
  [head]
  (when (symbol? head)
    (or (get @printer/block-dispatch head)
        (when (nil? (namespace head))
          (get @printer/block-dispatch (symbol "clojure.core" (name head)))))))

;; ---------------------------------------------------------------------------
;; Core helpers
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
;; Unified line-hint + fill rendering
;; ---------------------------------------------------------------------------

(defn- elem-line
  "Get the :line metadata from a form, or nil.
   For reader-macro wrappers (deref, quote, var) that lack :line,
   checks the inner form."
  [form]
  (or (when (and (some? form)
                 #?(:clj  (instance? clojure.lang.IMeta form)
                    :cljs (satisfies? IMeta form))
                 (meta form))
        (:line (meta form)))
      ;; Reader macro wrappers: (clojure.core/deref x), (quote x), (var x)
      ;; These don't get :line from the indexing reader, but their inner form does.
      (when (and (seq? form) (seq form)
                 (#{'clojure.core/deref 'quote 'var} (first form))
                 (second form))
        (let [inner (second form)]
          (when (and (some? inner)
                     #?(:clj  (instance? clojure.lang.IMeta inner)
                        :cljs (satisfies? IMeta inner))
                     (meta inner))
            (:line (meta inner)))))))

(defn- has-line-meta?
  "True if any item in the sequence has :line metadata."
  [items]
  (some elem-line items))

(defn- assign-lines
  "For each item, determine its effective line number.
   Items with :line metadata keep it. Items without (keywords, numbers,
   strings) are assigned to the next known line — this puts clause-leading
   keywords like :where on the same line as the clause body that follows."
  [items]
  (let [lines (mapv elem-line items)
        n (count items)
        ;; Forward: next known line for each position
        next-known (loop [i (dec n) nk nil result (vec (repeat n nil))]
                     (if (neg? i)
                       result
                       (let [cur (or (nth lines i) nk)]
                         (recur (dec i) cur (assoc result i cur)))))
        ;; Backward: previous known line for each position
        prev-known (loop [i 0 pk nil result (vec (repeat n nil))]
                     (if (>= i n)
                       result
                       (let [cur (or (nth lines i) pk)]
                         (recur (inc i) cur (assoc result i cur)))))]
    (mapv (fn [i]
            (or (nth lines i) (nth next-known i) (nth prev-known i)))
          (range n))))

(defn- group-by-line
  "Partition indices into groups of consecutive items on the same line."
  [effective-lines]
  (when (seq effective-lines)
    (reduce (fn [groups [i line]]
              (if (and (seq groups) (= line (:line (peek groups))))
                (update-in groups [(dec (count groups)) :indices] conj i)
                (conj groups {:line line :indices [i]})))
            []
            (map-indexed vector effective-lines))))

(defn- render-line-groups
  "Render items grouped by original source lines.
   pp-item-fn: (fn [item col width] -> string)"
  [items inner-col inner-indent width pp-item-fn]
  (let [effective-lines (assign-lines items)
        groups (group-by-line effective-lines)]
    (->> groups
         (map (fn [{:keys [indices]}]
                (str/join " " (map #(pp-item-fn (nth items %) inner-col width) indices))))
         (str/join (str "\n" inner-indent)))))

(defn- render-fill
  "Pack rendered strings onto lines up to width."
  [rendered-strs inner-col inner-indent width]
  (loop [remaining rendered-strs
         lines []
         current-line ""
         cur-col inner-col]
    (if (empty? remaining)
      (let [all-lines (if (seq current-line) (conj lines current-line) lines)]
        (str/join (str "\n" inner-indent) all-lines))
      (let [s (first remaining)
            need-space? (pos? (count current-line))
            new-col (+ cur-col (if need-space? 1 0) (count s))]
        (if (and need-space? (> new-col width))
          (recur remaining (conj lines current-line) "" inner-col)
          (recur (rest remaining)
                 lines
                 (if need-space? (str current-line " " s) s)
                 new-col))))))

(defn- render-items
  "Render a sequence of items using line-hints when available, fill otherwise.
   pp-item-fn: (fn [item col width] -> string)
   line-src-fn: (fn [item] -> form-to-check-for-line-meta)"
  [items inner-col inner-indent width pp-item-fn line-src-fn]
  (let [line-sources (mapv line-src-fn items)]
    (if (has-line-meta? line-sources)
      ;; Line-hint mode: group by original source line
      (let [effective-lines (assign-lines line-sources)
            groups (group-by-line effective-lines)]
        (->> groups
             (map (fn [{:keys [indices]}]
                    (str/join " " (map #(pp-item-fn (nth items %) inner-col width) indices))))
             (str/join (str "\n" inner-indent))))
      ;; Fill mode: pack to width
      (let [rendered (mapv #(pp-item-fn % inner-col width) items)]
        (render-fill rendered inner-col inner-indent width)))))

;; ---------------------------------------------------------------------------
;; Collection formatting
;; ---------------------------------------------------------------------------

(defn- pp-vec
  "Pretty-print a vector."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (inc col)
            inner-indent (indent-str inner-col)
            items (vec form)]
        (str "["
             (render-items items inner-col inner-indent width
                           (fn [item c w] (pp item c w))
                           identity)
             "]")))))

(defn- pp-map-entry
  "Render a single map entry as \"key value\"."
  [entry col width]
  (let [[k v] entry
        pp-k (pp k col width)
        last-line (peek (str/split-lines pp-k))
        multi-line? (not= last-line pp-k)
        val-col (if multi-line?
                  (+ (count last-line) 1)
                  (+ col (count last-line) 1))]
    (str pp-k " " (pp v val-col width))))

(defn- pp-map
  "Pretty-print a map."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (inc col)
            inner-indent (indent-str inner-col)
            entries (vec form)]
        (str "{"
             (render-items entries inner-col inner-indent width
                           (fn [entry c w] (pp-map-entry entry c w))
                           ;; Line source: use whichever of key/value has :line
                           (fn [[k v]] (if (elem-line v) v (if (elem-line k) k v))))
             "}")))))

(defn- pp-set
  "Pretty-print a set."
  [form col width]
  (let [flat-str (flat form)]
    (if (<= (+ col (count flat-str)) width)
      flat-str
      (let [inner-col (+ col 2)  ; after #{
            inner-indent (indent-str inner-col)
            items (vec form)]
        (str "#{"
             (render-items items inner-col inner-indent width
                           (fn [item c w] (pp item c w))
                           identity)
             "}")))))

;; ---------------------------------------------------------------------------
;; Call formatting
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
  "Pretty-print a call, keeping leading args with the head when appropriate.
   Uses line-hints for arg placement when available, fill otherwise."
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

      ;; Multi-line
      :else
      (let [n-head-args (get head-line-args head)
            [head-args body-args]
            (if (and n-head-args (pos? n-head-args) (> (count args) n-head-args))
              [(take n-head-args args) (drop n-head-args args)]
              [nil args])

            inner-col (+ col indent-step)
            inner-indent (indent-str inner-col)
            all-args (vec (if head-args (concat head-args body-args) body-args))]

        (if (and head-args
                 (<= (+ col (count head-str) 1
                        (count (str/join " " (map flat head-args))))
                     width))
          ;; Head-line args fit on the first line
          (let [head-args-str (str/join " " (map flat head-args))
                first-line (str head-str "(" head-args-str)
                body-vec (vec body-args)]
            (str first-line "\n" inner-indent
                 (render-items body-vec inner-col inner-indent width
                               (fn [item c w] (pp item c w))
                               identity)
                 ")"))

          ;; All args in body — use unified line-hint/fill strategy
          (str head-str "(\n" inner-indent
               (render-items all-args inner-col inner-indent width
                             (fn [item c w] (pp item c w))
                             identity)
               ")"))))))

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

                    ;; @deref — always use shorthand
                    (and (call? form) (= 'clojure.core/deref (first form)) (= 2 (count form)))
                    (str "@" (pp (second form) (inc col) width))

                    ;; 'quote — always use shorthand
                    (and (call? form) (= 'quote (first form)) (= 2 (count form)))
                    (str "'" (pp (second form) (inc col) width))

                    ;; #'var — always use shorthand
                    (and (call? form) (= 'var (first form)) (= 2 (count form)))
                    (str "#'" (flat (second form)))

                    ;; Block forms (defn, if, let, etc.) — delegate to printer
                    ;; which already produces correct multi-line block syntax
                    (and (call? form) (symbol? (first form)) (block-form? (first form)))
                    (flat form)

                    ;; Non-block calls — width-aware formatting
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

                    ;; Everything else ��� flat (primitives, empty list, etc.)
                    :else (flat form))]
    (if comments
      ;; First comment line: no indent (caller provides it via join/concat).
      ;; Subsequent comment lines: indent to current column.
      ;; All lines: strip original whitespace ��� pprint re-indents.
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
