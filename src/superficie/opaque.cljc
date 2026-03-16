(ns superficie.opaque
  "Utilities for exact raw Clojure islands that Superficie should preserve
   without interpreting or normalizing them."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]))

(defrecord OpaqueForm [raw kind])

(def ^:private placeholder-prefix "__superficie_opaque_")

(defn opaque-form?
  [value]
  (instance? OpaqueForm value))

(defn opaque-raw
  [value]
  (when (opaque-form? value)
    (:raw value)))

(defn- placeholder-symbol
  [index]
  (symbol (str placeholder-prefix index "__")))

(defn- whitespace-char?
  [ch]
  (contains? #{\space \tab \newline \return \formfeed \,} ch))

(defn- macro-terminating-char?
  [ch]
  (or (whitespace-char? ch)
      (contains? #{\( \) \[ \] \{ \} \" \; \^ \@ \~ \'} ch)))

(defn- boundary-char?
  [ch]
  (or (nil? ch)
      (whitespace-char? ch)
      (contains? #{\( \) \[ \] \{ \}} ch)))

(defn- char-at
  [source index]
  (when (< index (count source))
    (.charAt source index)))

(defn- comment-form-start?
  [source index]
  (let [fragment (subs source index)
        next-index (+ index (count "(comment"))]
    (and (str/starts-with? fragment "(comment")
         (boundary-char? (char-at source next-index)))))

(defn- escaped-symbol-raw
  [source index]
  (re-find #"^`[a-zA-Z_.*!?\-][a-zA-Z0-9_.*+!\-?/$#><=%&']*`"
           (subs source index)))

(defn- tagged-literal-start?
  [source index]
  (let [fragment (subs source index)
        next-char (char-at source (inc index))]
    (and (= \# (char-at source index))
         next-char
         (not (str/starts-with? fragment "#?"))
         (not (str/starts-with? fragment "#'"))
         (not (str/starts-with? fragment "#{"))
         (not (str/starts-with? fragment "#("))
         (not (str/starts-with? fragment "#\""))
         (not (str/starts-with? fragment "#_"))
         (re-matches #"[A-Za-z_*+!?.-]" (str next-char)))))

(declare consume-form-end)

(defn- skip-line-comment-end
  [source index]
  (loop [position index]
    (let [ch (char-at source position)]
      (if (or (nil? ch) (= ch \newline))
        position
        (recur (inc position))))))

(defn- skip-ws-and-comments
  [source index]
  (loop [position index]
    (let [ch (char-at source position)]
      (cond
        (nil? ch) position
        (whitespace-char? ch) (recur (inc position))
        (= ch \;) (recur (skip-line-comment-end source (inc position)))
        :else position))))

(defn- consume-string-end
  [source index]
  (loop [position (inc index)
         escaped? false]
    (let [ch (char-at source position)]
      (cond
        (nil? ch) position
        (and (not escaped?) (= ch \")) (inc position)
        (and (not escaped?) (= ch \\)) (recur (inc position) true)
        :else (recur (inc position) false)))))

(defn- consume-balanced-end
  [source index]
  (let [open-ch (char-at source index)
        close-ch ({\( \) \[ \] \{ \}} open-ch)]
    (loop [position (inc index)
           depth 1]
      (let [ch (char-at source position)]
        (cond
          (nil? ch) position
          (= ch \;) (recur (skip-line-comment-end source (inc position)) depth)
          (= ch \") (recur (consume-string-end source position) depth)
          (= ch open-ch) (recur (inc position) (inc depth))
          (= ch close-ch) (if (= depth 1)
                            (inc position)
                            (recur (inc position) (dec depth)))
          :else (recur (inc position) depth))))))

(defn- consume-atom-end
  [source index]
  (loop [position index]
    (let [ch (char-at source position)]
      (if (or (nil? ch) (macro-terminating-char? ch))
        position
        (recur (inc position))))))

(defn- consume-prefixed-form-end
  [source index prefix-length]
  (consume-form-end source (skip-ws-and-comments source (+ index prefix-length))))

(defn- consume-tagged-literal-end
  [source index]
  (let [tag-end (consume-atom-end source (inc index))
        value-start (skip-ws-and-comments source tag-end)]
    (consume-form-end source value-start)))

(defn- consume-metadata-end
  [source index]
  (let [meta-end (consume-form-end source (inc index))
        target-start (skip-ws-and-comments source meta-end)]
    (consume-form-end source target-start)))

(defn- consume-form-end
  [source index]
  (let [start (skip-ws-and-comments source index)
        fragment (subs source start)
        ch (char-at source start)]
    (cond
      (nil? ch) start
      (str/starts-with? fragment "#?@") (consume-prefixed-form-end source start 3)
      (str/starts-with? fragment "#?") (consume-prefixed-form-end source start 2)
      (str/starts-with? fragment "#_") (consume-prefixed-form-end source start 2)
      (str/starts-with? fragment "#'") (consume-prefixed-form-end source start 2)
      (str/starts-with? fragment "#{") (consume-balanced-end source (inc start))
      (str/starts-with? fragment "#(") (consume-balanced-end source (inc start))
      (str/starts-with? fragment "#\"") (consume-string-end source (inc start))
      (tagged-literal-start? source start) (consume-tagged-literal-end source start)
      (= ch \^) (consume-metadata-end source start)
      (= ch \') (consume-prefixed-form-end source start 1)
      (= ch \`) (consume-prefixed-form-end source start 1)
      (str/starts-with? fragment "~@") (consume-prefixed-form-end source start 2)
      (= ch \~) (consume-prefixed-form-end source start 1)
      (= ch \@) (consume-prefixed-form-end source start 1)
      (= ch \") (consume-string-end source start)
      (contains? #{\( \[ \{} ch) (consume-balanced-end source start)
      :else (consume-atom-end source start))))

(defn- detect-opaque-kind
  [source index {:keys [include-comment? include-uneval? include-quote-collection?]
                 :or {include-comment? false
                      include-uneval? false
                      include-quote-collection? false}}]
  (let [fragment (subs source index)
        ch (char-at source index)]
    (cond
      (str/starts-with? fragment "#?@") :reader-conditional-splicing
      (str/starts-with? fragment "#?") :reader-conditional
      (and include-uneval? (str/starts-with? fragment "#_")) :uneval
      (and include-comment? (comment-form-start? source index)) :comment
      (tagged-literal-start? source index) :tagged-literal
      ;; Quoted collections: preserve original formatting of data literals
      (and include-quote-collection?
           (= ch \')
           (let [next-ch (char-at source (inc index))]
             (contains? #{\( \[ \{} next-ch))) :quote-collection
      (= ch \`) :syntax-quote
      (str/starts-with? fragment "~@") :unquote-splicing
      (= ch \~) :unquote
      :else nil)))

(defn- extract-opaque-raw
  [source index kind]
  (let [end-index (case kind
                    :reader-conditional (consume-prefixed-form-end source index 2)
                    :reader-conditional-splicing (consume-prefixed-form-end source index 3)
                    :tagged-literal (consume-tagged-literal-end source index)
                    :quote-collection (consume-prefixed-form-end source index 1)
                    :syntax-quote (consume-prefixed-form-end source index 1)
                    :unquote (consume-prefixed-form-end source index 1)
                    :unquote-splicing (consume-prefixed-form-end source index 2)
                    :uneval (consume-prefixed-form-end source index 2)
                    :comment (consume-form-end source index)
                    nil)]
    (when end-index
      (subs source index end-index))))

(defn preprocess-source
  "Replace opaque Clojure islands with placeholder symbols so another parser
   can safely process the surrounding structure.

   Returns {:source <rewritten-source>
            :mapping {placeholder-symbol -> OpaqueForm}}."
  ([source]
   (preprocess-source source nil))
  ([source opts]
   (let [length (count source)]
     (loop [index 0
            counter 0
            in-string? false
            escaped? false
            in-comment? false
            parts []
            mapping {}]
       (if (>= index length)
         {:source (apply str parts)
          :mapping mapping}
         (let [ch (char-at source index)]
           (cond
             in-comment?
             (recur (inc index)
                    counter
                    false
                    false
                    (not= ch \newline)
                    (conj parts (str ch))
                    mapping)

             in-string?
             (let [next-escaped? (and (not escaped?) (= ch \\))
                   closing-quote? (and (not escaped?) (= ch \"))]
               (recur (inc index)
                      counter
                      (not closing-quote?)
                      next-escaped?
                      false
                      (conj parts (str ch))
                      mapping))

             (= ch \;)
             (recur (inc index)
                    counter
                    false
                    false
                    true
                    (conj parts (str ch))
                    mapping)

             (= ch \")
             (recur (inc index)
                    counter
                    true
                    false
                    false
                    (conj parts (str ch))
                    mapping)

             :else
             (if-let [escaped-raw (escaped-symbol-raw source index)]
               (recur (+ index (count escaped-raw))
                      counter
                      false
                      false
                      false
                      (conj parts escaped-raw)
                      mapping)
               (let [kind (detect-opaque-kind source index opts)
                     raw (when kind (extract-opaque-raw source index kind))]
                 (if (and kind raw)
                   (let [placeholder (placeholder-symbol counter)]
                     (recur (+ index (count raw))
                            (inc counter)
                            false
                            false
                            false
                            (conj parts (name placeholder))
                            (assoc mapping placeholder (->OpaqueForm raw kind))))
                   (recur (inc index)
                          counter
                          false
                          false
                          false
                          (conj parts (str ch))
                          mapping)))))))))))

(defn restore-opaque-forms
  "Replace placeholder symbols in a parsed form tree with OpaqueForm values."
  [form mapping]
  (walk/postwalk (fn [value]
                   (if (and (symbol? value) (contains? mapping value))
                     (get mapping value)
                     value))
                 form))