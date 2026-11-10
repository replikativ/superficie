(ns superficie.comments
  "Comments as data, for both Clojure and superficie source.

   Reading: scan finds the ; comments of a source text, attach hangs them on
   the forms read from it. A form's comments are its :sup/comments metadata:
     {:before [c ...]   comment lines above the form
      :after  c         the comment at the end of the line where it ends
      :end    [c ...]}  comments after the last element of a collection,
                        before its closer (end, ), ])
   where each c is {:text \"; …\" :line n :blank-after? bool}. Comments after
   the last top-level form are the :end comments of the vector of forms.

   Printing: a printer wraps the text of each form that has comments in
   marker characters (wrap); place then removes the markers and writes each
   comment where its marker ended up: :before lines above the line where the
   form starts, :after at the end of the line where it ends, :end lines above
   the line holding the closer. This works however a form is laid out (on one
   line, broken, in a header, after a prefix), and both for superficie and
   Clojure output, since both write a comment as ; to the end of the line."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Forms and their comments
;; ---------------------------------------------------------------------------

(defn- meta-capable? [x]
  #?(:clj (instance? clojure.lang.IObj x) :cljs (satisfies? IWithMeta x)))

(defn comments-of
  "The :sup/comments of x, or nil."
  [x]
  (when (meta-capable? x) (:sup/comments (meta x))))

(defn- kids
  "The forms inside x a comment can attach to, in reading order, or nil
   when x is not a collection."
  [x]
  (cond
    #?@(:clj [(reader-conditional? x) [(:form x)]])
    (record? x) (when (contains? x :form) [(:form x)])
    (map? x) (mapcat identity x)
    (coll? x) (seq x)
    :else nil))

(defn- with-kids
  "x rebuilt from kids, as kids returned them."
  [x ks]
  (cond
    #?@(:clj [(reader-conditional? x) (reader-conditional (first ks) (:splicing? x))])
    (record? x) (assoc x :form (first ks))
    (seq? x) (with-meta (apply list ks) (meta x))
    (vector? x) (with-meta (vec ks) (meta x))
    (map? x) (with-meta (into (empty x) (map vec (partition 2 ks))) (meta x))
    (set? x) (with-meta (into (empty x) ks) (meta x))))

(defn has-comments?
  "Does x or a form inside it carry comments?"
  [x]
  (boolean (or (comments-of x) (some has-comments? (kids x)))))

(defn comment-groups
  "The :sup/comments maps of x and the forms inside it, in reading order."
  [x]
  (concat (when-let [c (comments-of x)] [c]) (mapcat comment-groups (kids x))))

(defn group-entries
  "All comment entries of a :sup/comments map, in source order."
  [{:keys [before after] closing :end}]
  (sort-by :line (concat before (when after [after]) closing)))

;; ---------------------------------------------------------------------------
;; Scanning
;; ---------------------------------------------------------------------------

(defn scan
  "The ; comments of Clojure or superficie source src, in order, as maps
   {:line :column :text :eol? bool :code s :blank-after? bool} (1-based, as
   the readers count); :eol? when code precedes the comment on its line,
   which is :code; :blank-after? when the next line is blank. Strings, regexes and
   character literals (\\; \\\") are skipped."
  [src]
  (let [n (count src)]
    (loop [i 0 line 1 col 1 code? false out []]
      (if (>= i n)
        out
        (let [c (nth src i)]
          (cond
            (= c \newline) (recur (inc i) (inc line) 1 false out)

            (= c \;)
            (let [stop (loop [j i] (if (or (>= j n) (= \newline (nth src j))) j (recur (inc j))))
                  text (str/trimr (subs src i stop))]
              (recur stop line (+ col (- stop i)) code?
                     (conj out (cond-> {:line line :column col :text text :eol? code?}
                                 code? (assoc :code (subs src (- i (dec col)) i))
                                 ;; a blank line follows in the source
                                 (let [nxt (inc stop)]
                                   (and (< nxt n)
                                        (loop [j nxt]
                                          (cond (or (>= j n) (= \newline (nth src j))) true
                                                (str/blank? (str (nth src j))) (recur (inc j))
                                                :else false))))
                                 (assoc :blank-after? true)))))

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
              (recur j line col true out))

            ;; character literal: \; and \" are not a comment or a string
            (= c \\) (recur (min n (+ i 2)) line (+ col 2) true out)

            (or (= c \space) (= c \tab) (= c \,) (= c \return)) (recur (inc i) line (inc col) code? out)

            :else (recur (inc i) line (inc col) true out)))))))

;; ---------------------------------------------------------------------------
;; Attaching
;; ---------------------------------------------------------------------------

(defn- own-pos
  [form key-l key-c]
  (when (and (meta-capable? form) (not (record? form)))
    (let [m (meta form)
          l (get m key-l)
          c (get m key-c)]
      (when (and l c) [l c]))))

(defn- form-start
  "[line column] where form starts: its own position, else (for a form the
   reader built, such as an infix call or a reader-macro wrapper) where its
   first positioned part starts."
  [form]
  (or (own-pos form :line :column)
      (some form-start (kids form))))

(defn- form-end
  "[line column] just past the end of form, or nil."
  [form]
  (or (own-pos form :end-line :end-column)
      (some form-end (reverse (kids form)))))

(defn start-line
  "The source line where form starts, or nil."
  [form]
  (first (form-start form)))

(defn end-line
  "The source line where form ends, or nil."
  [form]
  (first (form-end form)))

(defn- pos< [[l1 c1] [l2 c2]]
  (or (< l1 l2) (and (= l1 l2) (< c1 c2))))

(defn- add
  "Add comment entries cs to form's comments under k (:before :after :end)."
  [form k cs]
  (if (and (seq cs) (meta-capable? form))
    (vary-meta form update :sup/comments
               (fn [m]
                 (case k
                   :after (assoc m :after (first cs))
                   (update m k (fnil into []) cs))))
    form))

(defn- identity-map [] #?(:clj (java.util.IdentityHashMap.) :cljs (js/Map.)))
(defn- id-get [m k] #?(:clj (.get ^java.util.IdentityHashMap m k) :cljs (.get m k)))
(defn- id-put! [m k v] #?(:clj (.put ^java.util.IdentityHashMap m k v) :cljs (.set m k v)))

(defn- eol-targets
  "For each comment at the end of a line (:eol?), the form it is about: of
   the forms ending on that line before it, after which no other form starts
   and no collection opens before the comment, the one ending first (the
   innermost on a tie). Elements without a position may follow it: in
   `(if (pos? x) :yes :no) ; c` the comment is about x. Returns
   [targets on-path assigned]: an identity map from form to its comment, an
   identity map holding the forms on the way to a target, and the set of
   comments assigned."
  [forms eol-comments]
  (let [by-line (into {} (map (juxt :line identity)) eol-comments)
        cands (volatile! {})                    ; line -> [[end-col depth form path] ...]
        starts (volatile! {})                   ; line -> [col ...]
        visit (fn visit [x depth path]
                (when (meta-capable? x)
                  (let [path' (conj path x)]
                    (when-let [[l c] (own-pos x :line :column)]
                      (when (by-line l) (vswap! starts update l (fnil conj []) c)))
                    (when-let [[el ec] (form-end x)]
                      (when-let [cm (by-line el)]
                        (when (<= ec (:column cm))
                          (vswap! cands update el (fnil conj []) [ec depth x path']))))
                    (doseq [k (kids x)] (visit k (inc depth) path')))))
        _ (doseq [f forms] (visit f 0 []))
        best (into {}
                   (keep (fn [[line cs]]
                           (let [cm (by-line line)
                                 ok? (fn [[ec]]
                                       (and (not-any? #(and (>= % ec) (< % (:column cm))) (get @starts line))
                                            ;; not about x when a collection opens after it
                                            (not (re-find #"[(\[{]" (subs (:code cm) (min (dec ec) (count (:code cm))))))))]
                             (when-let [ok (seq (filter ok? cs))]
                               [line (first (sort-by (fn [[ec depth]] [ec (- depth)]) ok))]))))
                   @cands)
        targets (identity-map)
        on-path (identity-map)]
    (doseq [[line [_ _ form path]] best]
      (id-put! targets form (by-line line))
      (doseq [p path] (id-put! on-path p true)))
    [targets on-path (set (map by-line (keys best)))]))

(defn attach
  "Attach comments (from scan) to forms, read with :line :column :end-line
   :end-column positions:
     - a comment at the end of a line is the :after comment of the form
       ending last on that line before it (the innermost on a tie);
     - any other comment is a :before comment of the next element of its
       collection, or, when none follows, an :end comment of the collection.
   Elements without a position (numbers, keywords, strings) take none.
   Returns the forms, with the comments after the last one as the :end
   comments of the returned vector."
  [forms comments]
  (if (empty? comments)
    forms
    (let [[targets on-path assigned] (eol-targets forms (filter :eol? comments))
          pending (volatile! (seq (remove assigned comments)))
          take-before (fn [pos]
                        (loop [acc []]
                          (let [c (first @pending)]
                            (if (and c pos (or (= ::all pos) (pos< [(:line c) (:column c)] pos)))
                              (do (vswap! pending next) (recur (conj acc c)))
                              acc))))]
      (letfn [;; Distribute the comments before `stop` (::all: all of them;
              ;; nil: none after the last element) among ks; return
              ;; [ks' comments-after-the-last-element].
              (walk-kids [ks stop]
                (loop [ks (seq ks) acc []]
                  (if ks
                    (let [k (first ks)]
                      ;; only a form with a position of its own: one the
                      ;; reader built (an infix call, a block clause) passes
                      ;; them on to its first part, where they print
                      (if-let [start (own-pos k :line :column)]
                        (let [cs (take-before start)]
                          ;; (walk looks forms up by identity: before add copies k)
                          (recur (next ks) (conj acc (add (walk k) :before cs))))
                        (recur (next ks) (conj acc (walk k)))))
                    [acc (take-before stop)])))
              ;; x with the comments inside it attached
              (walk [x0]
                (let [first-pending (first @pending)
                      x-end (form-end x0)
                      ;; (forms are looked up by identity: x0, not the copy)
                      x (if-let [c (and (meta-capable? x0) (id-get targets x0))] (add x0 :after [c]) x0)]
                  (if (and (or (nil? first-pending)
                               ;; nothing pending before x ends
                               (and x-end (not (pos< [(:line first-pending) (:column first-pending)] x-end))))
                           (not (and (meta-capable? x0) (id-get on-path x0))))
                    x
                    (if-let [ks (kids x)]
                      ;; (a JVM reader conditional holds forms but no comments)
                      (let [[ks' left] (walk-kids ks (when (and (meta-capable? x)
                                                                (own-pos x :end-line :end-column))
                                                       x-end))]
                        (add (with-kids x ks') :end left))
                      x))))]
        (let [[out left] (walk-kids forms ::all)]
          (cond-> out
            (seq left) (vary-meta assoc :sup/comments {:end left})))))))

;; ---------------------------------------------------------------------------
;; Markers
;;
;; While a top-level form is printed (with-comments), each comment group gets
;; an id and three marker characters from the Unicode private use area:
;; before, end and after. Markers take no part in reading — place removes
;; every one — but a printer that inspects text it produced (does it end in
;; a word? start with `(`?) looks at (visible s).
;; ---------------------------------------------------------------------------

(def ^:private marker-base 0xE000)
(def ^:private marker-count 2133)          ; ids whose 3 markers fit below U+F8FF

(def ^:dynamic ^:private *ids*
  "While printing with comments: atom {:ids {group id} :groups [group ...]}."
  nil)

(defn- marker [id kind]
  (let [code (+ marker-base (* 3 id) (case kind :before 0 :end 1 :after 2))]
    #?(:clj (str (char code)) :cljs (.fromCharCode js/String code))))

(def ^:private marker-re #"[-]")

(defn visible
  "s without comment markers."
  [s]
  (if (re-find marker-re s) (str/replace s marker-re "") s))

(defn width
  "The length of s as it will be written: without comment markers."
  [s]
  (count (visible s)))

(defn- group-id [group]
  (let [state *ids*]
    (or (get-in @state [:ids group])
        (let [id (count (:groups @state))]
          (when (< id marker-count)
            (swap! state #(-> % (assoc-in [:ids group] id) (update :groups conj group)))
            id)))))

(defn- wrap-with
  [form s f]
  (if-let [group (and *ids* (comments-of form))]
    (if-let [id (group-id group)] (f id) s)
    s))

(defn wrap
  "s, the printed text of form, with form's comment markers around it (when
   printing with-comments and form has comments)."
  [form s]
  (wrap-with form s #(str (marker % :before) s (marker % :end) (marker % :after))))

(defn wrap-start
  "s with form's before marker: form's text starts here (a printer that writes
   a form as several pieces, such as a let as `x := v` lines)."
  [form s]
  (wrap-with form s #(str (marker % :before) s)))

(defn wrap-end
  "s with form's end and after markers: form's text ends here."
  [form s]
  (wrap-with form s #(str s (marker % :end) (marker % :after))))

;; ---------------------------------------------------------------------------
;; Placing
;; ---------------------------------------------------------------------------

(defn- marker-info [ch]
  (let [code #?(:clj (int ch) :cljs (.charCodeAt ch 0))
        k (- code marker-base)]
    (when (and (>= k 0) (< k (* 3 marker-count)))
      [(quot k 3) ([:before :end :after] (rem k 3))])))

(defn- scan-lines
  "Split s into lines, removing markers. Returns a vector of
   {:text :start-in-string? :end-in-string? :markers [[id kind col] ...]}.
   Strings, regexes and character literals are followed so that a line
   break inside a string is known."
  [s]
  (let [n (count s)]
    (loop [i 0 in-str? false start-in? false sb [] marks [] lines []]
      (let [finish (fn [] {:text (apply str sb) :start-in-string? start-in?
                           :end-in-string? in-str? :markers marks})]
        (if (>= i n)
          (conj lines (finish))
          (let [c (nth s i)]
            (cond
              (marker-info c) (recur (inc i) in-str? start-in? sb (conj marks (conj (marker-info c) (count sb))) lines)
              (= c \newline) (recur (inc i) in-str? in-str? [] [] (conj lines (finish)))
              in-str? (cond
                        (and (= c \\) (< (inc i) n)) (recur (+ i 2) true start-in? (conj sb c (nth s (inc i))) marks lines)
                        (= c \") (recur (inc i) false start-in? (conj sb c) marks lines)
                        :else (recur (inc i) true start-in? (conj sb c) marks lines))
              (and (= c \\) (< (inc i) n)) (recur (+ i 2) false start-in? (conj sb c (nth s (inc i))) marks lines)
              (= c \") (recur (inc i) true start-in? (conj sb c) marks lines)
              :else (recur (inc i) false start-in? (conj sb c) marks lines))))))))

(defn- indent-of [text]
  (count (re-find #"^ *" text)))

(defn- closer-line?
  "Does the line hold only closers (`end`, `)`…)? Then comments above it
   belong to the body it closes."
  [text]
  (boolean (re-matches #"\s*(end|\)|\]|\})([\s)\]}]|end)*:?" text)))

(defn- comment-lines
  "Lines for comment entries at indent; a blank line after an entry that had one."
  [entries indent]
  (let [pad (apply str (repeat indent " "))]
    (mapcat (fn [c] (cond-> [(str pad (:text c))] (:blank-after? c) (conj "")))
            entries)))

(defn place
  "Remove the markers from s and write the comments of groups (indexed by
   id) where their markers are. Returns [text placed-ids]."
  [s groups]
  (let [lines (scan-lines s)
        ;; first occurrence of a before marker, last of end and after
        occ (reduce (fn [m [li [id kind col]]]
                      (if (and (= kind :before) (contains? m [id kind]))
                        m
                        (assoc m [id kind] [li col])))
                    {}
                    (for [[li l] (map-indexed vector lines) mk (:markers l)] [li mk]))
        ;; a line inside a string cannot take comments above it
        target (fn [li] (loop [li li]
                          (if (and (pos? li) (:start-in-string? (nth lines li))) (recur (dec li)) li)))
        placements
        (reduce (fn [acc [[id kind] [li col]]]
                  (let [{:keys [before after] closing :end} (nth groups id)
                        line (nth lines li)]
                    (case kind
                      :before (update-in acc [(target li) :above] into
                                         (map #(vector % 0) before))
                      :end (update-in acc [(target li) :above] into
                                      (map #(vector % (if (closer-line? (:text line)) 2 0)) closing))
                      ;; at the end of the line, unless the line is taken, ends in
                      ;; a string, or opens a collection after the form (the
                      ;; comment would then be about what follows: see attach)
                      :after (if (or (nil? after) (contains? (get acc li) :eol) (:end-in-string? line)
                                     (re-find #"[(\[{]" (subs (:text line) col)))
                               (update-in acc [(target li) :above] into (when after [[(dissoc after :blank-after?) 0]]))
                               (assoc-in acc [li :eol] after)))))
                {}
                (sort-by (fn [[[_ kind] [li col]]] [li (case kind :before 0 :end 1 :after 2) col]) occ))
        out (mapcat (fn [li {:keys [text]}]
                      (let [{:keys [above eol]} (get placements li)
                            indent (indent-of text)
                            above (sort-by (comp :line first) above)]
                        (concat
                         (mapcat (fn [[c extra]] (comment-lines [c] (+ indent extra))) above)
                         [(if eol (str text " " (:text eol)) text)])))
                    (range) lines)]
    [(str/join "\n" out) (set (map (comp first key) occ))]))

(def ^:dynamic *on-missing*
  "Called with the comment entries a printer did not write (which then go
   above the form), for tests that check this never happens."
  nil)

(defn with-comments
  "Print form with (print-fn form), writing the comments of form and of the
   forms inside it. A comment whose form the printer did not write goes
   above the text."
  [form print-fn]
  (if-not (has-comments? form)
    (print-fn form)
    (binding [*ids* (atom {:ids {} :groups []})]
      (let [s (print-fn form)
            groups (:groups @*ids*)
            [text placed] (place s groups)
            missing (->> (comment-groups form)
                         (remove #(contains? placed (get-in @*ids* [:ids %])))
                         (mapcat group-entries)
                         (sort-by :line))]
        (when (and (seq missing) *on-missing*) (*on-missing* missing))
        (if (seq missing)
          (str (str/join "\n" (comment-lines missing 0)) "\n" text)
          text)))))

(defn end-lines
  "The lines of :end comments of the vector of top-level forms."
  [forms]
  (when-let [cs (:end (comments-of forms))]
    (comment-lines cs 0)))
