(ns superficie.roundtrip-check
  "Standalone roundtrip checker: runs clj->forms → forms->sup → sup->forms
   on real Clojure project files and reports pass/fail.

   Usage:
     clj -M -m superficie.roundtrip-check
     clj -M -m superficie.roundtrip-check /path/to/project1 /path/to/project2
     clj -M -m superficie.roundtrip-check --src-only /path/to/project

   Options:
     --src-only   Only check src/ subdirectory (default: all .clj/.cljc files)
     --verbose    Print failure details (first form diff per file)
     --summary    Print only the summary table (suppress per-file output)"
  (:require [superficie.core :as core]
            [superficie.forms :as forms]
            [superficie.operators]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:gen-class))

;; ---------------------------------------------------------------------------
;; Default project list
;; ---------------------------------------------------------------------------

(def default-projects
  "Projects to check when no paths are given on the command line.
   Each entry is {:name \"label\" :path \"/abs/path\" :src-only? bool}."
  (let [dev (str (System/getProperty "user.home") "/Development")]
    [{:name "proximum"   :path (str dev "/proximum")   :src-only? true}
     {:name "datahike"   :path (str dev "/datahike")    :src-only? true}
     {:name "stratum"    :path (str dev "/stratum")     :src-only? true}
     {:name "core.async" :path (str dev "/core.async")  :src-only? true}
     {:name "malli"      :path (str dev "/malli")       :src-only? true}
     {:name "datascript" :path (str dev "/datascript")  :src-only? true}
     {:name "clara-rules":path (str dev "/clara-rules") :src-only? true}
     {:name "sci"        :path (str dev "/sci")         :src-only? true}
     {:name "konserve"   :path (str dev "/konserve")    :src-only? true}
     {:name "rewrite-clj":path (str dev "/rewrite-clj") :src-only? true}
     {:name "babashka"   :path (str dev "/babashka")    :src-only? true}
     {:name "electric"   :path (str dev "/electric")    :src-only? true}
     {:name "onyx"       :path (str dev "/onyx")        :src-only? true}
     {:name "datalevin"  :path (str dev "/datalevin")   :src-only? true}]))

;; ---------------------------------------------------------------------------
;; File discovery
;; ---------------------------------------------------------------------------

(defn- clj-file? [^java.io.File f]
  (let [n (.getName f)]
    (or (str/ends-with? n ".clj")
        (str/ends-with? n ".cljc"))))

(defn- skip-dir? [^java.io.File d]
  (let [n (.getName d)]
    (contains? #{"target" ".cpcache" "node_modules" ".shadow-cljs" "classes"} n)))

(defn find-clj-files
  "Return all .clj/.cljc files under root, skipping build output dirs."
  [^java.io.File root]
  (->> (file-seq root)
       (remove (fn [^java.io.File f]
                 ;; skip if any ancestor is a blocked dir
                 (loop [p (.getParentFile f)]
                   (when p
                     (if (skip-dir? p) true (recur (.getParentFile p)))))))
       (filter #(and (.isFile %) (clj-file? %)))))

(defn find-files-in-project
  [{:keys [path src-only?]}]
  (let [root (io/file path)]
    (when (.isDirectory root)
      (if src-only?
        ;; Only look in src/ (and resources/ for data readers etc.)
        (mapcat #(find-clj-files (io/file root %))
                ["src" "src/main" "src/cljc" "src/clj"])
        (find-clj-files root)))))

;; ---------------------------------------------------------------------------
;; Roundtrip check
;; ---------------------------------------------------------------------------

(defn- normalize-for-eq
  "Recursively normalize forms for equality comparison.
   Converts java.util.regex.Pattern to its pattern string so two patterns
   with the same source compare equal (Pattern.equals uses identity).
   Unwraps SupRaw to its value so scientific-notation doubles (1.0E-4)
   compare equal whether wrapped or not.
   Normalizes fully-qualified operator symbols (clojure.core/+, etc.) back to
   unqualified — the printer emits qualified names in value positions to avoid
   infix ambiguity; normalize for comparison."
  [x]
  (cond
    (forms/raw? x)  (normalize-for-eq (forms/raw-value x))
    (instance? java.util.regex.Pattern x) (.pattern ^java.util.regex.Pattern x)
    ;; Qualified operator symbol emitted by printer in value positions → unqualified
    ;; Also handles operators with :str override (e.g. clojure.core/-> whose surface string is ".>")
    (and (symbol? x)
         (namespace x)
         (or (= x (get @superficie.operators/*surface-index* (name x)))
             (contains? @superficie.operators/*op-registry* x)))
    (symbol (name x))
    ;; Unqualified surface-string operator (e.g. ".>" for ->, "|>" for ->>) emitted by
    ;; printer inside syntax-quotes → resolve via surface-index and take the canonical name.
    (and (symbol? x)
         (nil? (namespace x))
         (contains? @superficie.operators/*surface-index* (name x)))
    (let [qsym (get @superficie.operators/*surface-index* (name x))]
      (symbol (name qsym)))
    ;; (new ClassName args*) ↔ (ClassName. args*) — both are Java constructor calls
    (and (seq? x) (= 'new (first x)) (symbol? (second x)) (seq (rest x)))
    (let [[_ class & args] x]
      (apply list (normalize-for-eq (symbol (str class "."))) (map normalize-for-eq args)))
    (seq? x)    (apply list (map normalize-for-eq x))
    (vector? x) (mapv normalize-for-eq x)
    (map? x)    (into {} (map (fn [[k v]] [(normalize-for-eq k) (normalize-for-eq v)]) x))
    (set? x)    (into #{} (map normalize-for-eq x))
    :else x))

(defn- forms-equal?
  "Compare two form sequences for equality, treating SupSyntaxQuote etc.
   as regular records (which implement .equals correctly).
   java.util.regex.Pattern is compared by source string (not identity)."
  [f1 f2]
  (= (normalize-for-eq (vec f1)) (normalize-for-eq (vec f2))))

(defn check-file
  "Run clj→sup→clj roundtrip on a single file.
   Returns {:path path :status :pass/:fail/:error :error msg :forms-in N}."
  [^java.io.File f]
  (let [path (.getPath f)]
    (try
      (let [src    (slurp f)
            forms1 (core/clj->forms src)]
        (if (empty? forms1)
          {:path path :status :pass :forms-in 0}
          (let [sup-str (core/forms->sup forms1)
                forms2  (core/sup->forms sup-str {:read-cond :preserve})]
            (if (forms-equal? forms1 forms2)
              {:path path :status :pass :forms-in (count forms1)}
              {:path path :status :fail
               :forms-in (count forms1)
               :forms-out (count forms2)
               :first-diff (let [idx (->> (map vector forms1 forms2)
                                          (keep-indexed (fn [i [a b]] (when (not= a b) i)))
                                          first)]
                             (when idx
                               {:index idx
                                :in  (nth forms1 idx)
                                :out (nth forms2 idx)}))}))))
      (catch Exception e
        {:path path :status :error :error (.getMessage e)}))))

;; ---------------------------------------------------------------------------
;; Reporting
;; ---------------------------------------------------------------------------

(defn- relative-path [base path]
  (let [base-str (str base)
        path-str (str path)]
    (if (str/starts-with? path-str base-str)
      (subs path-str (inc (count base-str)))
      path-str)))

(defn print-project-results
  [{:keys [name path]} results verbose?]
  (let [pass  (count (filter #(= :pass (:status %)) results))
        fail  (count (filter #(= :fail (:status %)) results))
        err   (count (filter #(= :error (:status %)) results))
        total (count results)]
    (println (format "%-16s  %3d / %3d  (%s)"
                     name pass total
                     (if (= pass total) "OK" (str fail " fail, " err " error"))))
    (when (and verbose? (> (+ fail err) 0))
      (doseq [r results
              :when (not= :pass (:status r))]
        (println (str "  " (:status r) ": " (relative-path path (:path r))))
        (when (:error r)
          (println (str "    " (:error r))))
        (when-let [d (:first-diff r)]
          (println (str "    form[" (:index d) "] differs:"))
          (println (str "      in:  " (pr-str (:in d))))
          (println (str "      out: " (pr-str (:out d)))))))
    {:name name :pass pass :fail fail :error err :total total}))

;; ---------------------------------------------------------------------------
;; Entry point
;; ---------------------------------------------------------------------------

(defn run-checks
  "Run roundtrip checks. Returns exit code (0 = all pass)."
  [{:keys [projects verbose? summary-only?]}]
  (let [rows (atom [])
        t0   (System/currentTimeMillis)]
    (doseq [proj projects]
      (let [files (find-files-in-project proj)]
        (if (empty? files)
          (when-not summary-only?
            (println (format "%-16s  (not found / no files)" (:name proj))))
          (let [results (mapv check-file files)
                row     (print-project-results proj results (and verbose? (not summary-only?)))]
            (swap! rows conj row)))))
    (let [all-rows   @rows
          total-pass (reduce + 0 (map :pass all-rows))
          total-files(reduce + 0 (map :total all-rows))
          elapsed    (/ (- (System/currentTimeMillis) t0) 1000.0)]
      (println)
      (println (format "Total: %d / %d files pass  (%.1fs)" total-pass total-files elapsed))
      (if (= total-pass total-files) 0 1))))

(defn -main [& args]
  (let [[flags paths] (reduce (fn [[flags paths] arg]
                                (if (str/starts-with? arg "--")
                                  [(conj flags arg) paths]
                                  [flags (conj paths arg)]))
                              [[] []] args)
        verbose?      (contains? (set flags) "--verbose")
        summary-only? (contains? (set flags) "--summary")
        src-only?     (not (contains? (set flags) "--all"))
        projects      (if (seq paths)
                        (mapv (fn [p] {:name (.getName (io/file p))
                                       :path p
                                       :src-only? src-only?})
                              paths)
                        default-projects)]
    (System/exit (run-checks {:projects      projects
                               :verbose?      verbose?
                               :summary-only? summary-only?}))))
