(ns superficie.main
  "CLI entry point for superficie."
  (:require [superficie.render :as render]
            [superficie.parse :as parse]
            [superficie.runtime :as rt]
            [clojure.tools.cli :as cli]
            [clojure.string :as str])
  (:gen-class))

(def cli-options
  [["-h" "--help" "Show help"]
   ["-o" "--output FILE" "Output file (default: stdout)"]
   [nil  "--runtime" "Use runtime classpath for symbol resolution"]])

(defn- read-stdin []
  (let [sb (StringBuilder.)]
    (loop []
      (let [ch (.read System/in)]
        (if (neg? ch)
          (str sb)
          (do (.append sb (char ch))
              (recur)))))))

(defn- render-source
  "Render Clojure source to superficie syntax."
  [source opts]
  (let [render-opts (when (:runtime opts)
                      {:runtime-roles rt/core-role-overrides})]
    (render/render-string source render-opts)))

(defn- parse-source
  "Parse superficie source to Clojure S-expressions."
  [source]
  (let [result (parse/parse-string source)]
    (if (vector? result)
      ;; Multiple top-level forms
      (str/join "\n\n" (map pr-str result))
      ;; Single form
      (pr-str result))))

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
          (println "  render [file.clj]   Convert Clojure to superficie syntax")
          (println "  parse  [file.sup]   Convert superficie syntax to Clojure")
          (println)
          (println "If no file is given, reads from stdin.")
          (println)
          (println "Options:")
          (println summary)
          (println)
          (println "Examples:")
          (println "  superficie render src/foo.clj")
          (println "  superficie render src/foo.clj -o foo.sup")
          (println "  echo '(defn f [x] (+ x 1))' | superficie render")
          (println "  superficie parse foo.sup")
          (println)
          (println "Classpath:")
          (println "  Without --runtime, renders using clojure.core roles only.")
          (println "  With --runtime, uses the current classpath for symbol resolution.")
          (println "  To render with project deps: clj -Sdeps '{:deps {io.github.user/superficie {:git/tag ...}}}' -M -m superficie.main --runtime render file.clj"))

      (empty? arguments)
      (do (binding [*out* *err*] (println "superficie: no command given. Try --help"))
          (System/exit 1))

      :else
      (let [[command file & _] arguments
            source (if file (slurp file) (read-stdin))
            output-fn (fn [s]
                        (if-let [out (:output options)]
                          (spit out s)
                          (print s)))]
        (case command
          "render"
          (output-fn (render-source source options))

          "parse"
          (output-fn (parse-source source))

          (do (binding [*out* *err*] (println (str "superficie: unknown command '" command "'")))
              (System/exit 1)))))))
