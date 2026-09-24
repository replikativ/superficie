(ns superficie.shapes
  "Shape descriptors: declarative block rendering for library macros.

   A shape tells superficie how a macro's arguments split into a block
   header and a block body:

     (deftm norm [x :- Double] :- Double (sqrt x))
     ;; shape [:name :doc? :params [:kw? :-] :body]
     deftm norm [x :- Double] :- Double:
       sqrt(x)
     end

   The reader does not need the shape to parse a block: the header is every
   form between the head and ':', the body is every form up to 'end', and the
   result is (head header... body...). Shapes only decide how the printer
   splits arguments. The printer verifies that `join` inverts `split` before
   it uses block syntax, so a shape can never produce output that reads back
   differently — at worst it falls back to call syntax.

   The one exception is `:wrap?`, which lifts a nested form such as raster's
   (deftm f (All [T] [x :- T] body)) into the header:

     deftm f All [T] [x :- T]:
       body
     end

   Reading that back needs the shape, so `join` re-nests the header.

   Slot vocabulary (all slots are decidable from the forms alone):
     :name        one required form
     :form        one required form
     :params      one required vector
     :doc?        an optional string
     :attr?       an optional map
     :form?       an optional form, taken only when a body form remains
     [:kw? K]     an optional pair `K form`, e.g. [:kw? :-] for `:- Ret`
     [:wrap? S n] if the next form is (S a1..an rest...) and is the last
                  argument, put S a1..an in the header and continue matching
                  the remaining slots against `rest`
     :body        the remaining forms (must be the last slot)

   Shapes are keyed by fully-qualified symbol. Sources, in precedence order:
     1. register-shape!
     2. :superficie/shape metadata on the var (eval mode, JVM), with
        :superficie/shape-options for options
     3. superficie/shapes.edn resources on the classpath (JVM): a map from
        symbol to a shape vector, or to {:shape [...] :options {...}}
     4. builtin-shapes below
   A shape's options (see builtin-options) come from the same source as it."
  #?(:clj (:require [clojure.edn :as edn])))

;; ---------------------------------------------------------------------------
;; Builtin shapes for the replikativ numerical and verification libraries
;; ---------------------------------------------------------------------------

(def builtin-shapes
  "Shapes shipped with superficie. Libraries can take these over by shipping
   their own superficie/shapes.edn resource."
  '{raster.core/deftm     [:name :doc? [:wrap? All 1] :params [:kw? :-] :body]
    raster.core/ftm       [[:wrap? All 1] :params [:kw? :-] :body]
    raster.par/map-void!  [:form :form :body]
    ansatz.core/defn      [:name :params :form? [:kw? :termination-by] :body]
    ansatz.core/theorem   [:name :params :form :body]
    ansatz.core/deftheorem [:name :params :form :body]
    ansatz.core/inductive [:name :params [:kw? :in] [:kw? :indices] :body]
    ;; spindel: reactive/probabilistic program blocks
    org.replikativ.spindel.spin.cps/spin [:body]
    org.replikativ.spindel.core/spin     [:body]
    org.replikativ.spindel.core/batch    [:body]
    org.replikativ.spindel.core/gen-aseq [:body]
    org.replikativ.spindel.core/with-context [:form :body]
    org.replikativ.spindel.core/for      [:params :body]})

;; Options that change how a shaped block's contents are read and printed.
;;   :dotted-calls  inside the block, `A.b(x)` is the plain call (A.b x) rather
;;                  than the Java method call (.b A x). For languages embedded
;;                  in Clojure data, like ansatz's Lean-style names
;;                  (RBTree.node, Nat.succ), whose terms never contain interop.
;;   :match-arms    inside the block, a match with [pattern body] clauses (ansatz's
;;                  pattern form) prints as `| pattern => body` arms.
;;   :index         in the block body, indexing with the given functions:
;;                  {:get f} prints (f x i j) as x[i, j], and an adjacent `x[i]`
;;                  reads as (f x i); {:set g} also prints (g x i v) as
;;                  x[i] <- v. A bare symbol f is {:get f}. raster uses its
;;                  dispatching aget and aset, so indexing is as polymorphic
;;                  as they are.
(def builtin-options
  '{raster.core/deftm      {:index {:get aget :set aset}}
    raster.core/ftm        {:index {:get aget :set aset}}
    raster.par/map-void!   {:index {:get aget :set aset}}
    ansatz.core/defn       {:dotted-calls true :match-arms true}
    ansatz.core/theorem    {:dotted-calls true :match-arms true}
    ansatz.core/deftheorem {:dotted-calls true :match-arms true}
    ansatz.core/inductive  {:dotted-calls true :match-arms true}})

;; ---------------------------------------------------------------------------
;; Descriptor validation
;; ---------------------------------------------------------------------------

(defn- slot-kind [slot]
  (if (vector? slot) (first slot) slot))

(def ^:private simple-slots #{:name :form :params :doc? :attr? :form? :body})

(defn valid-shape?
  "True when shape is a well-formed descriptor. `:body` must come last, and
   no `:form?` may precede a `:wrap?` (the wrap point would be ambiguous)."
  [shape]
  (and (vector? shape)
       (seq shape)
       (= :body (peek shape))
       (not-any? #{:body} (pop shape))
       (every? (fn [slot]
                 (or (contains? simple-slots slot)
                     (and (vector? slot) (= :kw? (first slot)) (= 2 (count slot))
                          (keyword? (second slot)))
                     (and (vector? slot) (= :wrap? (first slot)) (= 3 (count slot))
                          (symbol? (second slot)) (nat-int? (nth slot 2)))))
               shape)
       (let [kinds (map slot-kind shape)
             wrap-idx (first (keep-indexed (fn [i k] (when (= :wrap? k) i)) kinds))]
         (or (nil? wrap-idx)
             (not-any? #{:form?} (take wrap-idx kinds))))))

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(defn valid-options?
  "True when opts is nil or a map of known options (see builtin-options)."
  [opts]
  (or (nil? opts)
      (and (map? opts)
           (every? (fn [[k v]]
                     (case k
                       (:dotted-calls :match-arms) (boolean? v)
                       :index (or (symbol? v)
                                  (and (map? v) (symbol? (:get v))
                                       (every? #{:get :set} (keys v))
                                       (every? symbol? (vals v))))
                       false))
                   opts))))

(defonce ^:private registered (atom {}))

(defn register-shape!
  "Register a shape descriptor (and optional options, see builtin-options)
   for a fully-qualified macro symbol."
  ([qsym shape] (register-shape! qsym shape nil))
  ([qsym shape opts]
   (when-not (qualified-symbol? qsym)
     (throw (ex-info "Shapes are keyed by fully-qualified symbols" {:symbol qsym})))
   (when-not (valid-shape? shape)
     (throw (ex-info "Invalid shape descriptor" {:symbol qsym :shape shape})))
   (when-not (valid-options? opts)
     (throw (ex-info "Invalid shape options" {:symbol qsym :options opts})))
   (swap! registered assoc qsym {:shape shape :options opts})
   qsym))

(defn unregister-shape! [qsym]
  (swap! registered dissoc qsym)
  nil)

#?(:clj
   (defn- classpath-entry
     "A shapes.edn value — a shape vector, or {:shape [...] :options {...}} —
      as {:shape :options}, or nil when invalid."
     [v]
     (let [entry (if (map? v) (select-keys v [:shape :options]) {:shape v})]
       (when (and (valid-shape? (:shape entry)) (valid-options? (:options entry)))
         entry))))

#?(:clj
   (defn- load-classpath-shapes
     "Merge every superficie/shapes.edn resource on the classpath.
      Invalid entries are skipped with a warning rather than failing rendering."
     []
     (let [urls (try (enumeration-seq
                      (.getResources (.getContextClassLoader (Thread/currentThread))
                                     "superficie/shapes.edn"))
                     (catch Exception _ nil))]
       (reduce (fn [acc url]
                 (let [m (try (edn/read-string (slurp url))
                              (catch Exception e
                                (binding [*out* *err*]
                                  (println "superficie: cannot read" (str url) "-" (ex-message e)))
                                nil))]
                   (reduce-kv (fn [acc k v]
                                (if-let [entry (and (qualified-symbol? k) (classpath-entry v))]
                                  (assoc acc k entry)
                                  (do (binding [*out* *err*]
                                        (println "superficie: ignoring invalid shape for" k "in" (str url)))
                                      acc)))
                              acc
                              (if (map? m) m {}))))
               {}
               urls))))

(def ^:private classpath-shapes
  #?(:clj (delay (load-classpath-shapes))
     :cljs (delay {})))

(defn shaped-name?
  "True when any known shape is registered under a symbol with this name."
  [s]
  (boolean (some #(= s (name %))
                 (concat (keys @registered) (keys @classpath-shapes) (keys builtin-shapes)))))

#?(:clj
   (defn- var-entry
     "{:shape :options} from :superficie/shape and :superficie/shape-options
      metadata of an already-loaded var. Never loads code: an unloaded
      namespace simply has no var shape."
     [qsym]
     (when-let [ns (find-ns (symbol (namespace qsym)))]
       (when-let [v (.findInternedVar ^clojure.lang.Namespace ns (symbol (name qsym)))]
         (let [{shape :superficie/shape opts :superficie/shape-options} (meta v)]
           (when (and (valid-shape? shape) (valid-options? opts))
             {:shape shape :options opts}))))))

(defn- shape-entry
  "{:shape :options} for a fully-qualified symbol from the first source that
   has a shape: register-shape!, var metadata, shapes.edn, the builtins. The
   options come with the shape, so a library that ships its own shape also
   decides its options."
  [qsym]
  (when (qualified-symbol? qsym)
    (or (get @registered qsym)
        #?(:clj (var-entry qsym))
        (get @classpath-shapes qsym)
        (when-let [shape (get builtin-shapes qsym)]
          {:shape shape :options (get builtin-options qsym)}))))

(defn index-fns
  "The :index option as {:get f :set g-or-nil}, or nil."
  [opts]
  (let [i (:index opts)]
    (cond (symbol? i) {:get i}
          (map? i) i)))

(defn shape-for
  "The shape descriptor for a fully-qualified symbol, or nil."
  [qsym]
  (:shape (shape-entry qsym)))

(defn shape-options
  "Options for a shaped macro, or nil."
  [qsym]
  (:options (shape-entry qsym)))

;; ---------------------------------------------------------------------------
;; Static namespace context: resolve written heads through the file's ns form
;; ---------------------------------------------------------------------------

(def ^:dynamic *ns-context*
  "{:aliases {alias-sym ns-sym} :refers {sym qualified-sym}} from the current
   file's ns form. Lets static rendering (no loaded code) resolve `a/defn` and
   referred `deftm` the same way in the reader and the printer."
  nil)

(defn- libspec-entries
  "Normalize a :require libspec into [ns-sym opts-map] pairs, expanding
   prefix lists like (raster [core :as c] [par :as p])."
  [spec]
  (cond
    (symbol? spec) [[spec {}]]
    (and (vector? spec) (symbol? (first spec)))
    [[(first spec) (let [opts (rest spec)]
                     (if (even? (count opts)) (apply hash-map opts) {}))]]
    (and (seq? spec) (symbol? (first spec)))
    (let [prefix (first spec)]
      (mapcat (fn [sub]
                (for [[n opts] (libspec-entries sub)]
                  [(symbol (str prefix "." n)) opts]))
              (rest spec)))
    :else []))

(defn- add-libspecs [ctx specs]
  (reduce (fn [ctx [ns-sym opts]]
            (let [ctx (if-let [a (:as opts)]
                        (assoc-in ctx [:aliases a] ns-sym)
                        ctx)
                  refer (:refer opts)]
              (if (sequential? refer)
                (reduce #(assoc-in %1 [:refers %2] (symbol (str ns-sym) (str %2)))
                        ctx refer)
                ctx)))
          ctx
          (mapcat libspec-entries specs)))

(defn ns-context
  "The namespace context established by a top-level form, given the context
   before it: an (ns ...) form starts a fresh one, a top-level
   (require '[lib :as alias] ...) — common in scripts and examples — extends
   it. Returns nil for any other form."
  ([form] (ns-context form nil))
  ([form ctx]
   (cond
     (and (seq? form) (= 'ns (first form)))
     (reduce (fn [ctx clause]
               (if (and (seq? clause) (= :require (first clause)))
                 (add-libspecs ctx (rest clause))
                 ctx))
             {:aliases {} :refers {}}
             (drop 2 form))

     (and (seq? form) (= 'require (first form)))
     (add-libspecs (or ctx {:aliases {} :refers {}})
                   (keep (fn [x] (when (and (seq? x) (= 'quote (first x))) (second x)))
                         (rest form))))))

(defn resolve-static
  "Resolve a written head symbol to a fully-qualified symbol through ctx
   (defaults to *ns-context*). Returns nil when it cannot be resolved."
  ([sym] (resolve-static sym *ns-context*))
  ([sym ctx]
   (when (symbol? sym)
     (if-let [n (namespace sym)]
       (let [full (get (:aliases ctx) (symbol n))]
         (symbol (str (or full n)) (name sym)))
       (get (:refers ctx) sym)))))

;; ---------------------------------------------------------------------------
;; split / join
;; ---------------------------------------------------------------------------

(defn split
  "Split macro arguments into [header body] according to shape.
   Returns nil when the arguments do not fit the shape."
  [shape args]
  (loop [slots (seq shape) args (seq args) header []]
    (if-not slots
      (when-not args [header []])
      (let [slot (first slots)]
        (case (slot-kind slot)
          :body [header (vec args)]
          (:name :form) (when args (recur (next slots) (next args) (conj header (first args))))
          :params (when (vector? (first args))
                    (recur (next slots) (next args) (conj header (first args))))
          :doc? (if (string? (first args))
                  (recur (next slots) (next args) (conj header (first args)))
                  (recur (next slots) args header))
          :attr? (if (map? (first args))
                   (recur (next slots) (next args) (conj header (first args)))
                   (recur (next slots) args header))
          :form? (if (next args)
                   (recur (next slots) (next args) (conj header (first args)))
                   (recur (next slots) args header))
          :kw? (if (and (= (second slot) (first args)) (next args))
                 (recur (next slots) (nnext args) (conj header (first args) (second args)))
                 (recur (next slots) args header))
          :wrap? (let [[_ s n] slot
                       x (first args)]
                   (if (and (seq? x) (= s (first x)) (nil? (next args))
                            (> (count x) n))
                     (let [inner (rest x)]
                       (recur (next slots)
                              (seq (drop n inner))
                              (into (conj header s) (take n inner))))
                     (recur (next slots) args header))))))))

(defn join
  "Rebuild macro arguments from a parsed header and body. Inverse of split
   for every shape: only a matched :wrap? re-nests, everything else concatenates."
  [shape header body]
  (let [wrap-idx (first (keep-indexed (fn [i s] (when (= :wrap? (slot-kind s)) i)) shape))]
    (if-not wrap-idx
      (into (vec header) body)
      ;; Walk the slots before the wrap point over the header to find its position.
      (let [pos (loop [slots (take wrap-idx shape) i 0]
                  (if-not (seq slots)
                    i
                    (let [slot (first slots)
                          x (nth header i nil)]
                      (case (slot-kind slot)
                        (:name :form :params) (recur (rest slots) (inc i))
                        :doc? (recur (rest slots) (if (string? x) (inc i) i))
                        :attr? (recur (rest slots) (if (map? x) (inc i) i))
                        :kw? (recur (rest slots) (if (= (second slot) x) (+ i 2) i))
                        i))))
            [_ s] (nth shape wrap-idx)]
        (if (and (< pos (count header)) (= s (nth header pos)))
          (let [before (subvec (vec header) 0 pos)
                inner (subvec (vec header) (inc pos))]
            (conj before (apply list s (concat inner body))))
          (into (vec header) body))))))

(defn split-verified
  "split, but only when join inverts it exactly. Returns [header body] or nil."
  [shape args]
  (when-let [[header body :as r] (split shape args)]
    (when (= (seq args) (seq (join shape header body)))
      r)))

;; ---------------------------------------------------------------------------
;; Header recognition helpers shared by reader and printer
;; ---------------------------------------------------------------------------

(defn first-slot
  "The first slot kind of a shape, looking through a leading :wrap?."
  [shape]
  (let [k (slot-kind (first shape))]
    (if (= :wrap? k)
      (slot-kind (second shape))
      k)))

(defn requires-header?
  "False only for shapes whose first slot is :body."
  [shape]
  (not= :body (slot-kind (first shape))))

(defn head-str
  "How a head symbol is written back: exactly as it appeared in the source."
  [head]
  (str head))

(comment
  (split '[:name :doc? [:wrap? All 1] :params [:kw? :-] :body]
         '(f (All [T] [x :- T] :- T (inc x))))
  (join '[:name :doc? [:wrap? All 1] :params [:kw? :-] :body]
        '[f All [T] [x :- T] :- T] '[(inc x)]))
