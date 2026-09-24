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
  "True if head is a block form (defn, if, let, a shaped macro, etc.) that
   the printer handles with block syntax."
  [head]
  (some? (printer/block-kind-for head)))

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

(defn- elem-col
  "Get the :column metadata from a form, or nil.
   For reader-macro wrappers (deref, quote, var), checks the inner form."
  [form]
  (or (when (and (some? form)
                 #?(:clj  (instance? clojure.lang.IMeta form)
                    :cljs (satisfies? IMeta form))
                 (meta form))
        (:column (meta form)))
      (when (and (seq? form) (seq form)
                 (#{'clojure.core/deref 'quote 'var} (first form))
                 (second form))
        (let [inner (second form)]
          (when (and (some? inner)
                     #?(:clj  (instance? clojure.lang.IMeta inner)
                        :cljs (satisfies? IMeta inner))
                     (meta inner))
            (:column (meta inner)))))))

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

(defn- compute-alignment-col
  "Compute the output column for a line group based on column alignment.
   If a previous multi-item group established an alignment column, single-item
   groups whose source column matches get indented to the same relative offset.
   Returns the column to render at, or inner-col as default."
  [indices items inner-col align-col-src align-col-out]
  (if (and align-col-src align-col-out (= 1 (count indices)))
    (let [item (nth items (first indices))
          src-col (elem-col item)]
      (if (and src-col (= src-col align-col-src))
        align-col-out
        inner-col))
    inner-col))

(defn- render-items
  "Render a sequence of items using line-hints when available, fill otherwise.
   pp-item-fn: (fn [item col width] -> string)
   line-src-fn: (fn [item] -> form-to-check-for-line-meta)
   container-col: (optional) the :column of the enclosing container in source,
                  used to compute relative alignment for sub-groups."
  ([items inner-col inner-indent width pp-item-fn line-src-fn]
   (render-items items inner-col inner-indent width pp-item-fn line-src-fn nil nil))
  ([items inner-col inner-indent width pp-item-fn line-src-fn container-col]
   (render-items items inner-col inner-indent width pp-item-fn line-src-fn container-col nil))
  ([items inner-col inner-indent width pp-item-fn line-src-fn container-col item-sep]
   (let [line-sources (mapv line-src-fn items)
         last-idx (dec (count items))
         ;; item-sep (e.g. ",") is appended after every item except the last,
         ;; so vertical maps keep their commas the flat printer already emits.
         sep-for (fn [idx s] (if (and item-sep (not= idx last-idx)) (str s item-sep) s))]
     (if (has-line-meta? line-sources)
       ;; Line-hint mode: group by original source line, with column alignment
       (let [effective-lines (assign-lines line-sources)
             groups (group-by-line effective-lines)]
         (loop [remaining groups
                result []
                align-col-src nil   ; source column of the alignment anchor
                align-col-out nil]  ; output column of the alignment anchor
           (if (empty? remaining)
             (str/join "\n" result)
             (let [{:keys [indices]} (first remaining)
                   ;; Detect alignment: multi-item group establishes anchor from
                   ;; the last item's source column
                   new-anchor? (and container-col (> (count indices) 1))
                   last-item (nth items (peek indices))
                   last-src-col (elem-col last-item)
                   ;; Compute render column for this group
                   render-col (compute-alignment-col
                               indices items inner-col
                               align-col-src align-col-out)
                   render-indent (indent-str render-col)
                   rendered (mapv #(sep-for % (pp-item-fn (nth items %) render-col width)) indices)
                   ;; Source line hints should not override the requested width.
                   ;; Re-fill an overlong same-line group while retaining explicit
                   ;; source breaks between groups.
                   line-str (if (and (> (count rendered) 1)
                                     (not-any? #(str/includes? % "\n") rendered))
                              (render-fill rendered render-col render-indent width)
                              (str/join " " rendered))
                   prefixed (if (seq result)
                              (str render-indent line-str)
                              line-str)
                   ;; If this group is multi-item and has a last-item column,
                   ;; compute the output column of that last item for alignment
                   new-align-src (if (and new-anchor? last-src-col)
                                   last-src-col
                                   align-col-src)
                   new-align-out (if (and new-anchor? last-src-col container-col)
                                   ;; relative offset: how far the last item was
                                   ;; from container start, applied to inner-col
                                   (+ inner-col (- last-src-col container-col 1))
                                   align-col-out)]
               (recur (rest remaining) (conj result prefixed)
                      new-align-src new-align-out)))))
       ;; Fill mode: pack to width
       (let [rendered (mapv (fn [idx item]
                              (sep-for idx (pp-item-fn item inner-col width)))
                            (range (count items))
                            items)]
         (render-fill rendered inner-col inner-indent width))))))

;; ---------------------------------------------------------------------------
;; Collection formatting
;; ---------------------------------------------------------------------------

(defn- pp-elem
  "Pretty-print a collection element. A bare `new` is followed by a comma so
   `[new List(x)]` cannot read as the constructor call `new List(x)`."
  [item col width]
  (if (= 'new item) "new," (pp item col width)))

(defn- pp-vec
  "Pretty-print a vector."
  [form col width]
  (let [flat-str (flat form)]
    (if (or (<= (+ col (count flat-str)) width)
            ;; operator elements need the flat printer's commas
            (some printer/operator-arg? form))
      flat-str
      (let [inner-col (inc col)
            inner-indent (indent-str inner-col)
            items (vec form)
            src-col (when (meta form) (:column (meta form)))]
        (str "["
             (render-items items inner-col inner-indent width
                           pp-elem
                           identity
                           src-col)
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
    (str pp-k (if (= 'new k) ", " " ") (pp v val-col width))))

(defn- pp-map
  "Pretty-print a map."
  [form col width]
  (let [flat-str (flat form)]
    (if (or (<= (+ col (count flat-str)) width)
            (some printer/operator-arg? (mapcat identity form)))
      flat-str
      (let [inner-col (inc col)
            inner-indent (indent-str inner-col)
            entries (vec form)]
        (str "{"
             (render-items entries inner-col inner-indent width
                           (fn [entry c w] (pp-map-entry entry c w))
                           ;; Line source: use whichever of key/value has :line
                           (fn [[k v]] (if (elem-line v) v (if (elem-line k) k v)))
                           nil ",")
             "}")))))

(defn- pp-set
  "Pretty-print a set."
  [form col width]
  (let [flat-str (flat form)]
    (if (or (<= (+ col (count flat-str)) width)
            ;; operator elements need the flat printer's commas
            (some printer/operator-arg? form))
      flat-str
      (let [inner-col (+ col 2)  ; after #{
            inner-indent (indent-str inner-col)
            items (vec form)]
        (str "#{"
             (render-items items inner-col inner-indent width
                           pp-elem
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

(def ^:private def-heads
  #{'def 'defonce 'defmulti
    'clojure.core/def 'clojure.core/defonce 'clojure.core/defmulti})

(defn- pp-simple-def
  "Render the simple def block shape with a width-aware value."
  [form col width]
  (let [[head name-sym & rest1] form
        [docstring rest2] (if (string? (first rest1))
                            [(first rest1) (rest rest1)]
                            [nil rest1])]
    (when (= 1 (count rest2))
      (let [flat-str (flat form)]
        (if (<= (+ col (count flat-str)) width)
          flat-str
          (let [prefix (str (name head) " " (flat name-sym)
                            (when docstring (str " " (printer/print-docstring docstring)))
                            ": ")]
            (str prefix (pp (first rest2) (+ col (count prefix)) width))))))))

(defn- infix-chain
  "The operands and operators of an infix expression as one chain of
   [operator operand] pieces (the first operator is nil). A left operand at the
   same precedence printed without parens — (+ a b) under - in a + b - c — is
   part of the same visual chain and is expanded into it."
  [form]
  (let [[op strs] (printer/infix-parts form)
        a0 (second form)
        s0 (first strs)
        head (if (and (= (printer/infix-prec a0) (printer/infix-prec form))
                      (not (str/starts-with? s0 "(")))
               (infix-chain a0)
               [[nil s0]])]
    (into head (map (fn [s] [op s]) (rest strs)))))

(defn- pp-infix
  "Pretty-print an infix expression that does not fit: break before operators,
   operands aligned at the first one's column (col). nil when an operand spans
   lines, so the caller falls back."
  [form col width]
  (when (printer/infix-parts form)
    (let [chain (infix-chain form)]
      (when (not-any? (fn [[_ x]] (str/includes? x "\n")) chain)
        (let [indent (indent-str col)]
          (loop [[[op x] & more] (rest chain)
                 line (second (first chain))
                 lines []]
            (if-not x
              (str/join (str "\n" indent) (conj lines line))
              (let [piece (str op " " x)]
                (if (> (+ col (count line) 1 (count piece)) width)
                  (recur more piece (conj lines line))
                  (recur more (str line " " piece) lines))))))))))

(defn- pp-call-smart
  "Pretty-print a call, keeping leading args with the head when appropriate.
   Uses line-hints for arg placement when available, fill otherwise."
  [form col width]
  (let [head (first form)
        args (rest form)
        head-str (printer/call-head-str head)
        flat-str (flat form)]
    (cond
      ;; No args
      (empty? args)
      (str head-str "()")

      ;; Fits flat — or has an operator-symbol argument, which only the flat
      ;; printer knows how to protect from being read as infix
      (or (<= (+ col (count flat-str)) width)
          (some printer/operator-arg? args))
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

        (cond
          ;; Head-line args fit on the first line
          (and head-args
               (<= (+ col (count head-str) 1
                      (count (str/join ", " (map flat head-args))))
                   width))
          (let [head-args-str (str/join ", " (map flat head-args))
                first-line (str head-str "(" head-args-str)
                body-vec (vec body-args)]
            (str first-line ",\n" inner-indent
                 (render-items body-vec inner-col inner-indent width
                               (fn [item c w] (pp item c w))
                               identity nil ",")
                 ")"))

          ;; Unknown calls (not in head-line-args): try first arg on head line
          (nil? n-head-args)
          (let [first-arg-str (pp (first all-args) (+ col (count head-str) 1) width)
                first-line (first (str/split-lines first-arg-str))
                first-line-len (+ col (count head-str) 1 (count first-line))]
            (if (<= first-line-len width)
              ;; First arg (or its first line) fits on head line
              (if (= 1 (count all-args))
                (let [a (first all-args)]
                  (if (and (coll? a) (not (seq? a)) (str/includes? first-arg-str "\n"))
                    ;; A single multi-line collection reads far cleaner on its
                    ;; own line at a shallow indent than staircased after the
                    ;; call head. Re-render it at inner-col.
                    (str head-str "(\n" inner-indent (pp a inner-col width) ")")
                    (str head-str "(" first-arg-str ")")))
                (let [rest-args (vec (rest all-args))]
                  (str head-str "(" first-arg-str ",\n" inner-indent
                       (render-items rest-args inner-col inner-indent width
                                     (fn [item c w] (pp item c w))
                                     identity nil ",")
                       ")")))
              ;; First arg doesn't fit — all in body
              (str head-str "(\n" inner-indent
                   (render-items all-args inner-col inner-indent width
                                 (fn [item c w] (pp item c w))
                                 identity nil ",")
                   ")")))

          ;; Known calls with 0 head-line-args — all in body
          :else
          (str head-str "(\n" inner-indent
               (render-items all-args inner-col inner-indent width
                             (fn [item c w] (pp item c w))
                             identity nil ",")
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
                    (if (printer/quoted-as-sexp? (second form) (subs (flat form) 1))
                      (flat form)
                      (str "'" (binding [printer/*in-quote* true]
                                 (pp (second form) (inc col) width))))

                    ;; #'var — always use shorthand
                    (and (call? form) (= 'var (first form)) (= 2 (count form)))
                    (str "#'" (flat (second form)))

                    ;; Block forms (defn, if, let, etc.) — delegate to printer
                    ;; which already produces correct multi-line block syntax.
                    ;; Bind *indent* to the current column so the block body and
                    ;; its `end` retain the enclosing indentation instead of
                    ;; collapsing to column 0 when nested in an argument list.
                    (and (call? form) (contains? def-heads (first form))
                         (pp-simple-def form col width))
                    (pp-simple-def form col width)

                    (and (call? form) (symbol? (first form)) (block-form? (first form)))
                    (binding [printer/*indent* (indent-str col)] (flat form))

                    ;; Infix that does not fit — break before operators
                    (and (call? form) (printer/infix-form? form)
                         (> (+ col (count (flat form))) width))
                    (or (pp-infix form col width) (pp-call-smart form col width))

                    ;; Non-block calls — width-aware formatting
                    (call? form)
                    (pp-call-smart form col width)

                    ;; Syntax-quote / unquote / unquote-splicing AST nodes
                    ;; `a + b would syntax-quote only a — the flat printer
                    ;; writes such an inner form as a call
                    (forms/syntax-quote? form)
                    (if (printer/infix-form? (:form form))
                      (flat form)
                      (str "`" (pp (:form form) (inc col) width)))

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
     (binding [printer/*width* width
               printer/*body-form-printer* (fn [f col] (pp f col width))]
       (pp form 0 width)))))

(defn- form-separator
  "Determine the separator between two consecutive forms based on :line metadata.
   If the next form starts on the line right after the previous form ends,
   use a single newline. Otherwise use a blank line (double newline)."
  [prev-form next-form]
  (let [prev-end (when (and (some? prev-form)
                            #?(:clj  (instance? clojure.lang.IMeta prev-form)
                               :cljs (satisfies? IMeta prev-form))
                            (meta prev-form))
                   (:end-line (meta prev-form)))
        next-start (elem-line next-form)]
    (if (and prev-end next-start (= next-start (inc prev-end)))
      "\n"
      "\n\n")))

(defn pprint-forms
  "Pretty-print a sequence of Clojure forms as sup text.
   Uses :line metadata to determine spacing between forms:
   consecutive forms get a single newline, others get a blank line.
   opts: {:width 80}"
  ([forms] (pprint-forms forms nil))
  ([forms opts]
   (let [trailing-ws (:trailing-ws (meta forms))
         trailing-comments (when trailing-ws
                             (extract-comments trailing-ws))
         rendered (printer/map-in-ns-context #(pprint-form % opts) forms)
         body (if (<= (count rendered) 1)
                (str/join rendered)
                (let [pairs (map vector forms (rest forms))
                      seps (mapv (fn [[a b]] (form-separator a b)) pairs)]
                  (apply str (first rendered)
                         (mapcat vector seps (rest rendered)))))]
     (if trailing-comments
       (str body "\n\n" (str/join "\n" trailing-comments))
       body))))
