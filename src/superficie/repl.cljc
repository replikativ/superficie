(ns superficie.repl
  "Interactive superficie REPL.
   Reads superficie syntax, evaluates as Clojure, prints results.
   Follows the Clojure incremental read-eval-print model: each form is
   evaluated before the next is parsed, so macros and namespace aliases
   defined earlier in the session are live for all subsequent forms."
  (:refer-clojure :exclude [eval])
  (:require [superficie.pipeline :as pipeline]
            [superficie.parse.expander :as expander]
            [superficie.eval :as sup-eval]
            [superficie.errors :as errors]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Input completeness detection
;; ---------------------------------------------------------------------------

(defn- eof-group-error?
  "True if any grouper error was triggered by EOF (unclosed bracket)."
  [ctx]
  (some #(nil? (:actual-close %)) (:group-errors ctx)))

(defn- incomplete?
  "Returns true when the input string needs more lines before it can be parsed.
   Checks two levels:
     1. Bracket level — resilient grouper reports an unclosed bracket at EOF.
     2. Semantic level — reader throws with {:incomplete true} (e.g. missing 'end')."
  [s opts]
  (let [ctx (-> {:source s :opts opts} pipeline/scan pipeline/group)]
    (or (eof-group-error? ctx)
        (try
          (pipeline/parse ctx)
          false
          (catch #?(:clj Exception :cljs :default) e
            (boolean (:incomplete (ex-data e))))))))

;; ---------------------------------------------------------------------------
;; Line reader
;; ---------------------------------------------------------------------------

(defn- read-input
  "Prompt and read until the input is syntactically complete.
   Returns the accumulated string, or nil on EOF."
  [primary-prompt continuation-prompt read-line-fn reader-opts]
  (loop [lines []]
    (print (if (empty? lines) primary-prompt continuation-prompt))
    (flush)
    (let [line (read-line-fn)]
      (cond
        ;; EOF
        (nil? line) nil
        ;; Blank first line — re-prompt, don't accumulate
        (and (empty? lines) (str/blank? line)) (recur lines)
        :else
        (let [input (str/join "\n" (conj lines line))]
          (if (incomplete? input reader-opts)
            (recur (conj lines line))
            input))))))

;; ---------------------------------------------------------------------------
;; REPL core
;; ---------------------------------------------------------------------------

(defn start
  "Start the superficie REPL. Reads superficie syntax one expression at a time,
   evals as Clojure, and prints results. Macros and namespace aliases defined
   in the session take effect immediately for subsequent inputs.

   opts:
     :eval      — eval fn (default: clojure.core/eval; required on CLJS)
     :read-line — line-reader fn (default: clojure.core/read-line; required on CLJS)
     :banner    — string to print on startup (false to suppress)"
  ([] (start {}))
  ([opts]
   (let [eval-fn      (or (:eval opts)
                          #?(:clj  clojure.core/eval
                             :cljs (throw (ex-info "REPL requires :eval in ClojureScript" {}))))
         read-line-fn (or (:read-line opts)
                          #?(:clj  read-line
                             :cljs (throw (ex-info "REPL requires :read-line in ClojureScript" {}))))
         reader-opts  (sup-eval/eval-reader-opts opts)]
     (when (not (false? (:banner opts)))
       (println "superficie REPL — Ctrl-D to exit"))
     (loop []
       (let [ns-str       #?(:clj (str (ns-name *ns*)) :cljs "sup")
             primary      (str ns-str "=> ")
             continuation "   .. "
             input        (read-input primary continuation read-line-fn reader-opts)]
         (when input
           (try
             (let [ctx   (pipeline/run input reader-opts)
                   forms (expander/expand-forms (:forms ctx) reader-opts)]
               (doseq [form forms]
                 (let [result (eval-fn form)]
                   (sup-eval/register-block! form)
                   (prn result))))
             (catch #?(:clj Throwable :cljs :default) e
               (println (errors/format-error e input))))
           (recur)))))))

(defn -main [& _]
  (start))
