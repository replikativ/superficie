(ns superficie.sci-repl
  "SCI-based REPL for superficie in the browser.
   Exposes globalThis.superficieRepl with:
     .evalSup(src) — parse + eval superficie source, returns {result, output, error}
     .reset()      — clear all defs and restart the default context

   Forkable sessions are available through .createForkable():
     .evalSup(worldId, src) — evaluate Superficie in one world
     .evalClj(worldId, src) — evaluate Clojure in one world
     .fork(worldId, opts)   — fork a world and return its description
     .worlds()              — list the session's worlds
     .reset()               — replace the session with a fresh root world"
  (:require [superficie.core :as core]
            [superficie.operators :as ops]
            [superficie.parse.expander :as expander]
            [sci.core :as sci]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; SCI-native match macro
;;
;; core.match cannot be loaded in the browser (its macro expander is JVM code).
;; This simplified version covers the common cases:
;;   Literals   — (= x pat)
;;   Wildcard   — _ or :else → unconditional catch-all
;;   Vectors    — positional match with optional symbol bindings
;;   Maps       — structural match with optional symbol bindings
;;
;; Usage (via superficie surface syntax):
;;   match x :
;;     1       => :one
;;     [a b]   => str(a b)
;;     {:k v}  => v
;;     _       => :other
;;   end
;;   ;; produces (match x 1 :one [a b] (str a b) {:k v} v :else :other)
;; ---------------------------------------------------------------------------

(defn- match-clause
  "Compile one [pattern result] pair into [cond-test cond-result].
   xsym is the gensym holding the matched expression's value."
  [xsym [pat result]]
  (cond
    ;; Wildcard / :else — unconditional (cond catch-all)
    (or (= '_ pat) (= :else pat))
    [:else result]

    ;; Vector structural pattern: [p1 p2 ...]
    ;; Symbols bind to the value at that position; _ ignores.
    (vector? pat)
    (let [n     (count pat)
          tests (keep-indexed (fn [i pi]
                                (when-not (or (symbol? pi) (= '_ pi))
                                  (list '= (list 'nth xsym i) pi)))
                              pat)
          binds (mapcat (fn [i pi]
                          (when (and (symbol? pi) (not= '_ pi))
                            [pi (list 'nth xsym i)]))
                        (range) pat)
          guard (list* 'and
                       (list 'sequential? xsym)
                       (list '= (list 'count xsym) n)
                       tests)]
      [guard (if (seq binds) (list 'let (vec binds) result) result)])

    ;; Map structural pattern: {:k p ...}
    ;; Symbols bind to the value at that key; _ ignores.
    (map? pat)
    (let [tests (keep (fn [[k v]]
                        (when-not (or (symbol? v) (= '_ v))
                          (list '= (list 'get xsym k) v)))
                      pat)
          binds (mapcat (fn [[k v]]
                          (when (and (symbol? v) (not= '_ v))
                            [v (list 'get xsym k)]))
                        pat)
          guard (list* 'and (list 'map? xsym) tests)]
      [guard (if (seq binds) (list 'let (vec binds) result) result)])

    ;; Literal — equality check
    :else
    [(list '= xsym pat) result]))

(def ^:private sci-match-macro
  "SCI-native implementation of clojure.core.match/match.
   Registered in the SCI context so that surface syntax match blocks evaluate."
  (with-meta
    (fn sci-match [_form _env & args]
      (let [[expr & clauses] args
            pairs  (partition 2 clauses)
            xsym   (gensym "x__")]
        (list 'let [xsym expr]
              (list* 'cond (mapcat #(match-clause xsym %) pairs)))))
    {:sci/macro true}))

;; ---------------------------------------------------------------------------
;; SCI contexts — definitions persist within a world and diverge across forks
;; ---------------------------------------------------------------------------

(def ^:private root-world-id "root")
(def ^:private default-eval-budget-ms 3000)

(defn- make-ctx [out-buf deadline runtime-mode]
  (sci/init
   {:runtime-mode runtime-mode
    :print-fn   (fn [s] (swap! out-buf conj s))
    :print-err-fn (fn [s] (swap! out-buf conj s))
    :interrupt-fn
    (fn []
      (when-let [limit @deadline]
        (when (> (.now js/Date) limit)
          (throw (js/Error. "Evaluation exceeded its time budget")))))
    :classes    {'Math js/Math}
    :namespaces
    {;; Register match in clojure.core.match for qualified require
     'clojure.core.match {'match  sci-match-macro
                          'match* sci-match-macro}
      ;; superficie.operators: Haskell-inspired stdlib ops
     'superficie.operators {'<$> ops/<$>
                            '<*> ops/<*>
                            '>>=  ops/>>=
                            '**   ops/**
                            '>>   ops/>>
                            '<<   ops/<<}
      ;; Also register bare names in user namespace for unqualified surface syntax
     'user               {'match  sci-match-macro
                          '<$>   ops/<$>
                          '<*>   ops/<*>
                          '>>=   ops/>>=
                          '**    ops/**
                          '>>    ops/>>
                          '<<    ops/<<}}}))

;; ---------------------------------------------------------------------------
;; Evaluation
;; ---------------------------------------------------------------------------

(defn- source-views [language src]
  (case language
    :superficie
    [src (try (core/sup->clj src) (catch :default _ nil))]

    :clojure
    [(try (core/clj->sup src) (catch :default _ nil)) src]))

(defn- eval-result
  "Evaluate source in ctx as either :superficie or :clojure.
   Returns a JS object with:
     .result  — pr-str of the last form's value (string)
     .output  — captured stdout (print/println calls)
     .error   — error message string, or null
     .sourceSup / .sourceClj — equivalent prompt views where conversion succeeds"
  [ctx out-buf deadline budget-ms language src]
  (reset! out-buf [])
  (reset! deadline (+ (.now js/Date) budget-ms))
  (let [[source-sup source-clj] (source-views language src)]
    (try
      (let [result (case language
                     :superficie
                     (let [forms (expander/expand-forms (core/sup->forms src))]
                       (reduce (fn [_ form] (sci/eval-form ctx form)) nil forms))

                     :clojure
                     (sci/eval-string* ctx src))
            output (str/join @out-buf)]
        #js {:result (pr-str result)
             :output output
             :error nil
             :sourceSup source-sup
             :sourceClj source-clj})
      (catch :default e
        #js {:result nil
             :output (str/join @out-buf)
             :error  (or (.-message e) (str e))
             :sourceSup source-sup
             :sourceClj source-clj})
      (finally
        (reset! deadline nil)))))

(defn- world-description [{:keys [id parent-id label]}]
  #js {:id id :parentId parent-id :label label})

(defn- session-worlds [{:keys [world-order worlds]}]
  (to-array (map #(world-description (get worlds %)) world-order)))

(defn- find-world [session-state world-id]
  (or (get-in @session-state [:worlds world-id])
      (throw (js/Error. (str "Unknown SCI world: " world-id)))))

(defn- option-value [opts property fallback]
  (let [value (when opts (aget opts property))]
    (if (nil? value) fallback value)))

(defn create-forkable-session
  "Create an isolated tree of forkable SCI REPL worlds.

   The returned JavaScript object keeps SCI contexts opaque and addresses them
   by stable string IDs. opts may provide maxRuntimeMs for the cooperative SCI
   interrupt budget."
  ([] (create-forkable-session nil))
  ([opts]
   (let [out-buf (atom [])
         deadline (atom nil)
         requested-budget (option-value opts "maxRuntimeMs" default-eval-budget-ms)
         budget-ms (if (and (number? requested-budget) (pos? requested-budget))
                     requested-budget
                     default-eval-budget-ms)
         make-root #(hash-map
                     :next-id 1
                     :world-order [root-world-id]
                     :worlds
                     {root-world-id
                      {:id root-world-id
                       :parent-id nil
                       :label "Original"
                       :ctx (make-ctx out-buf deadline :forkable)}})
         session-state (atom (make-root))
         evaluate (fn [language world-id src]
                    (let [{:keys [ctx]} (find-world session-state world-id)]
                      (eval-result ctx out-buf deadline budget-ms language src)))
         fork-world (fn [world-id fork-opts]
                      (let [{:keys [ctx]} (find-world session-state world-id)
                            next-id (:next-id @session-state)
                            child-id (str "world-" next-id)
                            child {:id child-id
                                   :parent-id world-id
                                   :label (option-value fork-opts "label"
                                                        (str "Fork " next-id))
                                   :ctx (sci/fork ctx)}]
                        (swap! session-state
                               (fn [state]
                                 (-> state
                                     (assoc :next-id (inc next-id))
                                     (update :world-order conj child-id)
                                     (assoc-in [:worlds child-id] child))))
                        (world-description child)))
         reset-session (fn []
                         (reset! session-state (make-root))
                         (world-description
                          (get-in @session-state [:worlds root-world-id])))]
     #js {:rootId root-world-id
          :evalSup (fn [world-id src]
                     (evaluate :superficie world-id src))
          :evalClj (fn [world-id src]
                     (evaluate :clojure world-id src))
          :fork fork-world
          :worlds (fn [] (session-worlds @session-state))
          :reset reset-session})))

(defn- make-default-repl []
  (let [out-buf (atom [])
        deadline (atom nil)]
    {:out-buf out-buf
     :deadline deadline
     :ctx (make-ctx out-buf deadline :standard)}))

(defonce ^:private default-repl (atom (make-default-repl)))

(defn eval-sup
  "Evaluate Superficie source in the backwards-compatible default REPL."
  [src]
  (let [{:keys [ctx out-buf deadline]} @default-repl]
    (eval-result ctx out-buf deadline default-eval-budget-ms :superficie src)))

(defn reset-ctx!
  "Clear all definitions and restart the backwards-compatible default REPL."
  []
  (reset! default-repl (make-default-repl))
  nil)

;; ---------------------------------------------------------------------------
;; Browser export
;; ---------------------------------------------------------------------------

(set! (.-superficieRepl js/globalThis)
      #js {:evalSup eval-sup
           :reset reset-ctx!
           :createForkable create-forkable-session})
