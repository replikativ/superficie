(ns superficie.sci-repl
  "SCI-based REPL for superficie in the browser.
   Exposes globalThis.superficieRepl with:
     .evalSup(src) — parse + eval superficie source, returns {result, output, error}
     .reset()      — clear all defs and restart the context"
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
;; SCI context — persists definitions across evaluations
;; ---------------------------------------------------------------------------

(defonce ^:private out-buf (atom []))

(defn- make-ctx []
  (sci/init
   {:print-fn   (fn [s] (swap! out-buf conj s))
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

(defonce ctx (atom (make-ctx)))

;; ---------------------------------------------------------------------------
;; Evaluation
;; ---------------------------------------------------------------------------

(defn eval-sup
  "Parse and evaluate a superficie source string in the SCI context.
   Returns a JS object with:
     .result  — pr-str of the last form's value (string)
     .output  — captured stdout (print/println calls)
     .error   — error message string, or null"
  [src]
  (reset! out-buf [])
  (try
    (let [forms  (expander/expand-forms (core/sup->forms src))
          result (reduce (fn [_ form] (sci/eval-form @ctx form)) nil forms)
          output (str/join @out-buf)]
      #js {:result (pr-str result) :output output :error nil})
    (catch :default e
      #js {:result nil
           :output (str/join @out-buf)
           :error  (or (.-message e) (str e))})))

(defn reset-ctx!
  "Clear all definitions and restart the SCI context."
  []
  (reset! ctx (make-ctx))
  nil)

;; ---------------------------------------------------------------------------
;; Browser export
;; ---------------------------------------------------------------------------

(set! (.-superficieRepl js/globalThis)
      #js {:evalSup eval-sup
           :reset   reset-ctx!})
