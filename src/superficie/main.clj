(ns superficie.main
  "CLI entry point for superficie."
  (:require [superficie.core :as core]
            [superficie.eval :as sup-eval]
            [clojure.tools.cli :as cli])
  (:gen-class))

(def cli-options
  [["-h" "--help"        "Show help"]
   ["-o" "--output FILE" "Output file (default: stdout)"]
   [nil  "--eval"        "Eval forms while converting (enables macro-aware block detection)"]])

(defn- read-stdin []
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (.read System/in)]
        (if (neg? ch)
          (str sb)
          (do (.append sb (char ch))
              (recur)))))))

(defn -main [& args]
  (let [{:keys [options arguments summary errors]} (cli/parse-opts args cli-options)]
    (cond
      errors
      (do (doseq [e errors] (binding [*out* *err*] (println e)))
          (System/exit 1))

      (:help options)
      (do (println "superficie - surface syntax for Clojure")
          (println)
          (println "Usage: superficie [options] <command> [file]")
          (println)
          (println "Commands:")
          (println "  to-sup [file.clj]   Convert Clojure to superficie syntax")
          (println "  to-clj [file.sup]   Convert superficie syntax to Clojure")
          (println)
          (println "If no file is given, reads from stdin.")
          (println)
          (println "Options:")
          (println summary)
          (println)
          (println "Examples:")
          (println "  superficie to-sup src/foo.clj")
          (println "  superficie to-sup src/foo.clj -o foo.sup")
          (println "  echo '(defn f [x] (+ x 1))' | superficie to-sup")
          (println "  superficie to-clj foo.sup")
          (println "  superficie to-clj foo.sup --eval   # macro-aware"))

      (empty? arguments)
      (do (binding [*out* *err*] (println "superficie: no command given. Try --help"))
          (System/exit 1))

      :else
      (let [[command file & _] arguments
            source    (if file (slurp file) (read-stdin))
            output-fn (fn [s]
                        (if-let [out (:output options)]
                          (spit out s)
                          (print s)))]
        (case command
          "to-sup"
          (if (:eval options)
            (let [sw (java.io.StringWriter.)]
              (sup-eval/eval-clj-string source {:sup-output sw})
              (output-fn (str sw)))
            (output-fn (core/clj->sup source)))

          "to-clj"
          (if (:eval options)
            (let [sw (java.io.StringWriter.)]
              (sup-eval/eval-string source {:clj-output sw})
              (output-fn (str sw)))
            (output-fn (core/sup->clj source)))

          ;; Legacy aliases
          "render" (output-fn (core/clj->sup source))
          "parse"  (output-fn (core/sup->clj source))

          (do (binding [*out* *err*]
                (println (str "superficie: unknown command '" command "'. Try --help")))
              (System/exit 1)))))))
