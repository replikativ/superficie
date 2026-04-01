(ns superficie.eval
  "Incremental read-eval-print for superficie source.
   Each form is evaluated before the next is parsed, so macros and
   namespace aliases defined in the file are visible to subsequent forms —
   the same model Clojure uses for loading source files."
  (:refer-clojure :exclude [eval])
  (:require [superficie.pipeline :as pipeline]
            [superficie.parse.reader :as reader]
            [superficie.parse.expander :as expander]
            [superficie.emit.printer :as printer]
            [clojure.string :as str]
            #?(:clj [superficie.runtime :as runtime])))

;; ---------------------------------------------------------------------------
;; Role → block-kind mapping
;; ---------------------------------------------------------------------------

#?(:clj
(def ^:private role->block-kind
  "Maps superficie rendering roles to reader block-dispatch kinds."
  {:defn-form      :defn-block
   :fn-form        :fn-block
   :if-form        :if-block
   :if-bind-form   :if-block
   :when-form      :when-block
   :when-bind-form :when-block
   :let-form       :let-block
   :binding-form   :let-block
   :do-form        :body-block
   :block-form     :body-block
   :for-form       :for-block
   :case-form      :case-block
   :match-form     :match-block
   :ns-form           :ns-block
   :def-form          :def-block
   :try-form          :try-block
   :cond-form         :cond-block
   :condp-form        :cond-block
   :loop-form         :let-block
   :defmethod-form    :defmethod-block
   :defprotocol-form  :defprotocol-block
   :defrecord-form    :defrecord-block
   :reify-form        :reify-block
   :proxy-form        :proxy-block}))


;; ---------------------------------------------------------------------------
;; Block dispatch update after eval
;; ---------------------------------------------------------------------------

#?(:clj
(defn- register-block-from-form!
  "After evaling a defmacro form, check its role via :superficie/role metadata
   and register it in block-dispatch so subsequent forms in the same file
   parse it correctly as a block."
  [form]
  (when (and (seq? form)
             (symbol? (first form))
             (= 'defmacro (first form)))
    (let [name-sym  (second form)
          explicit  (get (meta name-sym) :superficie/role)
          var-role  (try
                      (when-let [v (resolve name-sym)]
                        (runtime/resolve-role-for-var v))
                      (catch Exception _ nil))
          role      (or explicit var-role)
          kind      (get role->block-kind role)]
      (when kind
        (let [qsym (or (try (when-let [v (resolve name-sym)]
                              (symbol (str (ns-name (:ns (meta v))))
                                      (str (:name (meta v)))))
                            (catch Exception _ nil))
                       (symbol (str (ns-name *ns*)) (name name-sym)))]
          (swap! reader/block-dispatch assoc qsym kind)))))))

#?(:clj
(defn- make-resolve-sym-hook
  "Build a :resolve-sym hook for the reader.
   Called at parse time with a bare symbol string.
   Returns the fully-qualified symbol, or nil if not resolvable.

   Also auto-registers in block-dispatch when the var has :superficie/role
   metadata (e.g. third-party macros from already-loaded namespaces)."
  []
  (fn [sym-str]
    (let [sym (symbol sym-str)]
      (when-let [v (try (resolve sym) (catch Exception _ nil))]
        (let [qsym (symbol (str (ns-name (:ns (meta v))))
                           (str (:name (meta v))))]
          ;; Auto-register block kind for vars with :superficie/role metadata
          (when-not (get @reader/block-dispatch qsym)
            (when-let [role (runtime/resolve-role-for-var v)]
              (when-let [kind (get role->block-kind role)]
                (swap! reader/block-dispatch assoc qsym kind))))
          qsym)))))

;; ---------------------------------------------------------------------------
;; Public eval-context helpers (used by superficie.repl and other consumers)
;; ---------------------------------------------------------------------------

(defn eval-reader-opts
  "Build reader opts enriched for an eval context.
   Adds a :resolve-sym hook (JVM only) that resolves symbols to their
   fully-qualified form via the current *ns*, enabling namespace-aware
   block and operator dispatch.
   Strips internal-only keys (:eval :clj-output :sup-output)."
  ([] (eval-reader-opts {}))
  ([opts]
   (cond-> (dissoc opts :eval :clj-output :sup-output)
     #?(:clj  (not (:resolve-sym opts))
        :cljs false)
     (assoc :resolve-sym #?(:clj  (make-resolve-sym-hook)
                            :cljs nil)))))

(defn register-block!
  "Update reader block-dispatch after evaluating a form.
   Should be called after each eval so macros defined in the form are
   recognised as blocks when parsing subsequent forms."
  [form]
  #?(:clj  (register-block-from-form! form)
     :cljs nil))

;; ---------------------------------------------------------------------------
;; superficie source evaluator
;; ---------------------------------------------------------------------------

(defn eval-string
  "Read and eval a superficie source string, one form at a time.
   Each form is evaluated before the next is parsed so macros and
   namespace aliases defined in the file take effect immediately.

   opts:
     :eval       — eval fn (default: clojure.core/eval; required on CLJS)
     :clj-output — java.io.Writer to emit Clojure source alongside eval
     :sup-output — java.io.Writer to emit superficie source alongside eval"
  ([s] (eval-string s {}))
  ([s eval-fn-or-opts]
   (let [opts        (if (map? eval-fn-or-opts) eval-fn-or-opts {:eval eval-fn-or-opts})
         eval-fn     (or (:eval opts)
                         #?(:clj  clojure.core/eval
                            :cljs (throw (ex-info "eval-string requires :eval option in ClojureScript" {}))))
         clj-writer  (:clj-output opts)
         sup-writer  (:sup-output opts)
         reader-opts (cond-> (dissoc opts :eval :clj-output :sup-output)
                       #?(:clj  (not (:resolve-sym opts))
                          :cljs false)
                       (assoc :resolve-sym #?(:clj  (make-resolve-sym-hook)
                                              :cljs nil)))
         src         #?(:clj  (let [s' (str/trim-newline s)]
                                 ;; Strip leading #! shebang (Unix script convention)
                                 (if (str/starts-with? s' "#!")
                                   (let [nl (str/index-of s' "\n")]
                                     (if nl (subs s' (inc nl)) ""))
                                   s'))
                        :cljs s)
         ctx         (-> {:source src :opts reader-opts} pipeline/scan pipeline/group)
         p           (reader/streaming-parser (:tokens ctx) reader-opts src)]
     (loop [result nil]
       (if (reader/parser-done? p)
         result
         (let [raw (reader/parse-next! p)]
           (if (reader/discarded? raw)
             (recur result)
             (let [form   (first (expander/expand-forms [raw] reader-opts))
                   evaled (eval-fn form)]
               #?(:clj (register-block-from-form! form))
               #?(:clj (when clj-writer
                         (.write ^java.io.Writer clj-writer (pr-str form))
                         (.write ^java.io.Writer clj-writer "\n\n")))
               #?(:clj (when sup-writer
                         (.write ^java.io.Writer sup-writer (printer/print-form form))
                         (.write ^java.io.Writer sup-writer "\n\n")))
               (recur evaled)))))))))

(defn eval-file
  "Read and eval a .sup file. Returns the last result."
  ([path]
   (eval-string #?(:clj  (slurp path)
                   :cljs (throw (ex-info "eval-file not available in ClojureScript" {})))))
  ([path opts]
   (eval-string #?(:clj  (slurp path)
                   :cljs (throw (ex-info "eval-file not available in ClojureScript" {}))) opts)))

;; ---------------------------------------------------------------------------
;; Clojure source evaluator (clj → sup direction)
;; ---------------------------------------------------------------------------

#?(:clj
(defn eval-clj-string
  "Read and eval a Clojure source string, one form at a time.
   Writes superficie output to :sup-output writer.
   Used to convert .clj files to .sup with full macro awareness.

   opts:
     :eval       — eval fn (default: clojure.core/eval)
     :sup-output — java.io.Writer to emit superficie syntax alongside eval"
  ([s] (eval-clj-string s {}))
  ([s opts]
   (let [eval-fn    (or (:eval opts) clojure.core/eval)
         sup-writer (:sup-output opts)
         eof        (Object.)
         rdr        (java.io.PushbackReader. (java.io.StringReader. s))]
     (loop [result nil]
       (let [form (try
                    (read {:read-cond :preserve :eof eof} rdr)
                    (catch Exception e
                      (throw (ex-info (str "Clojure read error: " (ex-message e)) {} e))))]
         (if (identical? form eof)
           result
           (let [evaled (eval-fn form)]
             (register-block-from-form! form)
             (when sup-writer
               (.write ^java.io.Writer sup-writer (printer/print-form form))
               (.write ^java.io.Writer sup-writer "\n\n"))
             (recur evaled)))))))))
