(ns superficie.runtime
  "Runtime namespace resolution. When target project namespaces are loaded
   on the classpath, this module can introspect var metadata to classify
   forms into rendering categories automatically.

   Layer 0: Compiler specials (hardcoded, no vars exist)
   Layer 1: Metadata heuristics (automatic from arglists, :macro, :inline)
   Layer 2: :superficie/role metadata on vars (opt-in by library authors)
   Layer 3: Project config EDN registry
   Layer 4: render-as fallback (pass-through as sexp)"
  (:require [clojure.string :as str]))

;; === Layer 0: Compiler specials ===
;; These have NO vars in clojure.core — they are true compiler primitives.
;; Must stay hardcoded.

(def compiler-specials
  "True compiler special forms that have no var."
  '#{if def do let* loop* recur fn* try throw new set!
     quote var . & monitor-enter monitor-exit
     catch finally letfn* case* deftype* reify*
     import* clojure.core/import*})

;; === Layer 1: Metadata heuristic classification ===

(defn- arglist-has-name-first?
  "Does the arglist start with a 'name'-like parameter?"
  [arglist]
  (when (and (vector? arglist) (seq arglist))
    (let [first-arg (str (first arglist))]
      (or (= first-arg "name")
          (str/ends-with? first-arg "-name")
          (= first-arg "fn-name")
          (= first-arg "multifn")))))

(defn- arglist-has-body?
  "Does the arglist end with '& body' or similar?"
  [arglist]
  (when (and (vector? arglist) (>= (count arglist) 2))
    (let [last-two (take-last 2 arglist)]
      (and (= '& (first last-two))
           (let [rest-name (str (second last-two))]
             (or (str/includes? rest-name "body")
                 (str/includes? rest-name "forms")
                 (str/includes? rest-name "exprs")))))))

(defn- arglist-has-bindings?
  "Does the arglist contain a 'bindings' parameter?"
  [arglist]
  (some #(str/includes? (str %) "binding") arglist))

(defn classify-var
  "Classify a var into a rendering role based on its metadata.
   Returns one of:
     :infix-op     — binary operator, render as infix (a + b)
     :logical-op   — logical operator, render as infix (a and b)
     :threading     — threading macro, render as pipe (|> .>)
     :def-form     — definition form (def x = ...)
     :defn-form    — function definition (fn name(...): ... end)
     :fn-form      — anonymous fn-like (fn(...): ... end)
     :binding-form — binding block (let x = ...: ... end)
     :do-form      — body block (do: ... end)
     :block-form   — generic block with body
     nil           — no special rendering, use default call syntax"
  [v]
  (when (var? v)
    (let [m (meta v)
          sym-name (str (:name m))
          arglists (:arglists m)
          macro? (:macro m)
          inline? (boolean (:inline m))
          ;; Layer 2: explicit role override
          explicit-role (:superficie/role m)]
      (or
       ;; Layer 2: explicit metadata wins
       explicit-role

       ;; Inline binary operators: has :inline and [x y] arity
       (when (and inline?
                  (not macro?)
                  (some #(and (vector? %) (= 2 (count %))) arglists))
         :infix-op)

       ;; Macro classification by arglist patterns
       (when macro?
         (let [first-al (first arglists)]
           (cond
             ;; def-like: starts with 'name'
             (and (arglist-has-name-first? first-al)
                  (or (str/starts-with? sym-name "def")
                      (some #(arglist-has-body? %) arglists)))
             :defn-form

             ;; Simple def: (defonce name expr)
             (and (arglist-has-name-first? first-al)
                  (= 2 (count first-al)))
             :def-form

             ;; Binding form: has 'bindings' arg + body
             (and (arglist-has-bindings? first-al)
                  (arglist-has-body? first-al))
             :binding-form

             ;; Threading: [x & forms]
             (and (= 3 (count first-al))
                  (= '& (second first-al))
                  (let [rest-name (str (nth first-al 2))]
                    (str/includes? rest-name "form")))
             :threading

             ;; Body-only block: [& body] or [msg & body]
             (arglist-has-body? first-al)
             :block-form

             :else nil)))))))

;; === Runtime namespace loading ===

(defn require-ns
  "Attempt to require a namespace. Returns true on success."
  [ns-sym]
  (try
    (require ns-sym)
    true
    (catch Exception _ false)))

(defn classify-ns-vars
  "Classify all public vars in a namespace.
   Returns a map of {symbol -> role}."
  [ns-sym]
  (when-let [ns-obj (find-ns ns-sym)]
    (->> (ns-publics ns-obj)
         (keep (fn [[sym v]]
                 (when-let [role (classify-var v)]
                   [sym role])))
         (into {}))))

(defn classify-var-by-name
  "Resolve and classify a fully-qualified symbol."
  [qualified-sym]
  (when-let [v (resolve qualified-sym)]
    (classify-var v)))

;; === Integration: build runtime-enhanced context ===

(defn build-runtime-roles
  "Given a list of namespace symbols, require them and build a role map.
   Only includes vars with explicit :superficie/role metadata.
   Returns {qualified-symbol -> role}."
  [ns-syms]
  (->> ns-syms
       (filter require-ns)
       (mapcat (fn [ns-sym]
                 (when-let [ns-obj (find-ns ns-sym)]
                   (->> (ns-publics ns-obj)
                        (keep (fn [[sym v]]
                                (when-let [role (:superficie/role (meta v))]
                                  [(symbol (str ns-sym) (str sym)) role])))))))
       (into {})))

(defn build-heuristic-roles
  "Given a list of namespace symbols, require them and build a role map
   using metadata heuristics. Includes all classifiable vars.
   Use with caution — custom macros may not match standard rendering patterns.
   Returns {qualified-symbol -> role}."
  [ns-syms]
  (->> ns-syms
       (filter require-ns)
       (mapcat (fn [ns-sym]
                 (for [[sym role] (classify-ns-vars ns-sym)]
                   [(symbol (str ns-sym) (str sym)) role])))
       (into {})))

;; === Default core classifications ===
;; These override heuristics for well-known clojure.core forms
;; that don't perfectly match the heuristic patterns.

(def core-role-overrides
  "Explicit role assignments for clojure.core forms where
   heuristic classification is insufficient or wrong."
  {;; Logical ops (macro, not :inline)
   'clojure.core/and :logical-op
   'clojure.core/or  :logical-op

   ;; Comparison ops without :inline
   'clojure.core/not= :infix-op
   'clojure.core/mod  :infix-op

   ;; Threading macros
   'clojure.core/->     :threading
   'clojure.core/->>    :threading
   'clojure.core/some-> :threading
   'clojure.core/some->> :threading

   ;; Block forms that need specific rendering
   'clojure.core/if       :if-form
   'clojure.core/if-let   :if-bind-form
   'clojure.core/if-some  :if-bind-form
   'clojure.core/if-not   :if-form
   'clojure.core/when     :when-form
   'clojure.core/when-not :when-form
   'clojure.core/when-let  :when-bind-form
   'clojure.core/when-some :when-bind-form
   'clojure.core/when-first :when-bind-form
   'clojure.core/let       :let-form
   'clojure.core/binding   :let-form
   'clojure.core/with-open  :let-form
   'clojure.core/with-redefs :let-form
   'clojure.core/do        :do-form
   'clojure.core/loop      :loop-form
   'clojure.core/cond      :cond-form
   'clojure.core/condp     :condp-form
   'clojure.core/case      :case-form
   'clojure.core/try       :try-form
   'clojure.core/fn        :fn-form
   'clojure.core/for       :for-form
   'clojure.core/doseq     :for-form
   'clojure.core/dotimes   :for-form

   ;; Def forms
   'clojure.core/def      :def-form
   'clojure.core/defonce  :def-form
   'clojure.core/defmulti :def-form
   'clojure.core/defn     :defn-form
   'clojure.core/defn-    :defn-form
   'clojure.core/defmacro :defn-form

   ;; Protocol/record/type/reify/proxy forms
   'clojure.core/defmethod   :defmethod-form
   'clojure.core/defprotocol :defprotocol-form
   'clojure.core/defrecord   :defrecord-form
   'clojure.core/deftype     :defrecord-form
   'clojure.core/reify       :reify-form
   'clojure.core/proxy       :proxy-form

   ;; Unary / special
   'clojure.core/not    :not-form
   'clojure.core/deref  :deref-form
   'clojure.core/ns     :ns-form

   ;; Known third-party macros with defined block roles
   'clojure.core.match/match :match-form})

(defn resolve-role-for-var
  "Resolve the rendering role for a var object directly.
   Checks :superficie/role metadata first, then core-role-overrides by
   fully-qualified symbol. Returns a role keyword or nil."
  [v]
  (when (var? v)
    (let [m    (meta v)
          qsym (symbol (str (ns-name (:ns m))) (str (:name m)))]
      (or (:superficie/role m)
          (get core-role-overrides qsym)))))

(defn resolve-role
  "Resolve the rendering role for a qualified symbol.
   Checks: core overrides -> Layer 2 (var :superficie/role metadata) -> fallback.
   Returns a role keyword or nil."
  [qualified-sym]
  (or (get core-role-overrides qualified-sym)
      (when-let [v (try (resolve qualified-sym) (catch Exception _ nil))]
        (:superficie/role (meta v)))))
