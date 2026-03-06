(ns superficie.resolve
  "Static namespace resolver. Parses `ns` forms to determine which symbols
   resolve to which fully-qualified vars, so we can decide which forms
   get surface syntax treatment.

   Supports runtime-enhanced resolution: when target namespaces are loaded,
   var metadata is used to classify forms into rendering categories."
  (:require [rewrite-clj.zip :as z]))

;; The set of clojure.core vars that get special surface syntax treatment.
;; Organized by category for clarity.

(def arithmetic-ops
  "Operators rendered as infix."
  #{'+ '- '* '/ '< '> '<= '>= '= 'not= 'mod 'rem})

(def logical-ops
  "Logical operators rendered as infix."
  #{'and 'or})

(def threading-ops
  "Threading macros rendered as pipe operators."
  #{'-> '->> 'some-> 'some->>})

(def block-forms
  "Forms rendered with block syntax (if, when, let, etc.)."
  #{'if 'if-let 'if-some 'if-not
    'when 'when-let 'when-some 'when-not 'when-first
    'let 'binding 'with-open 'with-redefs
    'do 'loop
    'cond 'condp 'case
    'try 'fn 'fn*
    'for 'doseq 'dotimes 'while})

(def def-forms
  "Definition forms with dedicated rendering."
  #{'def 'defn 'defn- 'defmacro 'defonce})

(def special-forms
  "All forms that receive surface syntax treatment."
  (into #{} cat [arithmetic-ops logical-ops threading-ops
                 block-forms def-forms
                 #{'ns 'require 'use 'import 'refer
                   'not 'recur 'throw 'new 'set! 'quote
                   'var 'deref}]))

(defn parse-ns-form
  "Parse an ns form (as a seq) and return a resolution map.
   Returns {:refers {symbol -> qualified-symbol}
            :aliases {alias -> namespace}
            :excludes #{symbol}}"
  [ns-form]
  (let [result (atom {:refers {}
                      :aliases {}
                      :excludes #{}})]
    (doseq [clause (rest (rest ns-form))] ;; skip 'ns and the ns-name
      (when (sequential? clause)
        (let [[directive & args] clause]
          (case directive
            :require
            (doseq [spec args]
              (cond
                (vector? spec)
                (let [ns-sym (first spec)
                      opts (apply hash-map (rest spec))]
                  (when-let [alias (:as opts)]
                    (swap! result assoc-in [:aliases alias] ns-sym))
                  (when-let [refers (:refer opts)]
                    (when (sequential? refers)
                      (doseq [r refers]
                        (swap! result assoc-in [:refers r]
                               (symbol (str ns-sym) (str r)))))))

                (symbol? spec)
                nil)) ;; bare namespace require, no alias/refer

            :refer-clojure
            (let [opts (apply hash-map args)]
              (when-let [excludes (:exclude opts)]
                (swap! result update :excludes into excludes)))

            nil))))
    @result))

(defn resolve-symbol
  "Given a resolution context and a symbol, return the fully-qualified
   var it refers to, or nil if unknown."
  [{:keys [refers aliases excludes]} sym]
  (cond
    ;; Qualified symbol: foo/bar
    (namespace sym)
    (let [ns-alias (symbol (namespace sym))
          local-name (name sym)]
      (if-let [full-ns (get aliases ns-alias)]
        (symbol (str full-ns) local-name)
        ;; Already fully qualified or unknown alias
        sym))

    ;; Explicitly referred from another namespace
    (contains? refers sym)
    (get refers sym)

    ;; Excluded from clojure.core
    (contains? excludes sym)
    nil

    ;; Check if it's a clojure.core special form
    (contains? special-forms sym)
    (symbol "clojure.core" (str sym))

    :else nil))

(defn extract-ns-form
  "Extract the ns form from a rewrite-clj zipper at the first top-level form."
  [zloc]
  (loop [loc zloc]
    (when loc
      (if (= :uneval (z/tag loc))
        (recur (z/right loc))
        (let [node (z/sexpr loc)]
          (if (and (sequential? node) (= 'ns (first node)))
            node
            (recur (z/right loc))))))))

(defn build-context
  "Build a resolution context from source code (as a string).
   Optionally pass :runtime-roles from superficie.runtime for enhanced classification."
  ([source]
   (build-context source nil))
  ([source {:keys [runtime-roles]}]
   (let [zloc (z/of-string source {:track-position? true})
         ns-form (extract-ns-form zloc)
         ctx (if ns-form
               (parse-ns-form ns-form)
               {:refers {} :aliases {} :excludes #{}})]
     (if runtime-roles
       (assoc ctx :runtime-roles runtime-roles)
       ctx))))

;; === Role-based resolution ===
;; The renderer dispatches on roles instead of checking hardcoded symbol sets.

(defn resolve-role
  "Resolve the rendering role for a symbol in context.
   Returns a role keyword or nil. Uses runtime-roles from ctx when available,
   falls back to static classification."
  [ctx sym]
  (when (symbol? sym)
    (let [qualified (resolve-symbol ctx sym)]
      (when qualified
        ;; Check runtime roles first (includes core overrides + heuristic)
        (or (get (:runtime-roles ctx) qualified)
            ;; Static fallback for core forms when no runtime is available
            (when (= "clojure.core" (namespace qualified))
              (let [core-name (symbol (name qualified))]
                (cond
                  (contains? arithmetic-ops core-name)  :infix-op
                  (contains? logical-ops core-name)     :logical-op
                  (contains? threading-ops core-name)   :threading
                  (contains? #{'if 'if-not} core-name)  :if-form
                  (contains? #{'if-let 'if-some} core-name) :if-bind-form
                  (contains? #{'when 'when-not} core-name)  :when-form
                  (contains? #{'when-let 'when-some 'when-first} core-name) :when-bind-form
                  (contains? #{'let 'binding 'with-open 'with-redefs} core-name) :let-form
                  (contains? #{'cond} core-name) :cond-form
                  (contains? #{'condp} core-name) :condp-form
                  (contains? #{'case} core-name) :case-form
                  (contains? #{'do} core-name) :do-form
                  (contains? #{'loop} core-name) :loop-form
                  (contains? #{'try} core-name) :try-form
                  (contains? #{'fn 'fn*} core-name) :fn-form
                  (contains? #{'for 'doseq 'dotimes} core-name) :for-form
                  (contains? #{'def 'defonce} core-name) :def-form
                  (contains? #{'defn 'defn- 'defmacro} core-name) :defn-form
                  (contains? #{'not} core-name) :not-form
                  (contains? #{'deref} core-name) :deref-form
                  (= 'ns core-name) :ns-form
                  :else nil)))
            ;; Name-based fallback: if the unqualified name matches a known form,
            ;; treat it accordingly regardless of namespace
            (let [unqual (symbol (name qualified))]
              (cond
                (contains? arithmetic-ops unqual) :infix-op
                (contains? logical-ops unqual)    :logical-op
                (contains? #{'defn 'defn- 'defmacro} unqual) :defn-form
                :else nil)))))))

