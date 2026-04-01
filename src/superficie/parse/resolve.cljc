(ns superficie.parse.resolve
  "Token value resolution: converts raw token text to Clojure values.
   All resolution is native — no delegation to read-string."
  (:require [clojure.string :as str]
            [superficie.errors :as errors]
            [superficie.forms :as forms]))

;; ---------------------------------------------------------------------------
;; String
;; ---------------------------------------------------------------------------

(def ^:private esc-chars
  {\n \newline \t \tab \r \return \\ \\ \" \" \b \backspace \f \formfeed})

(defn- parse-unicode [raw idx loc]
  "Parse \\uNNNN starting at idx (pointing at 'u'). Returns [char new-idx]."
  (let [end (min (+ idx 5) (count raw))
        n   (- end idx 1)]
    (when (< n 4)
      (errors/reader-error
        (str "Invalid unicode escape — expected 4 hex digits after \\u, got " n) loc))
    (let [hex  (subs raw (inc idx) end)
          code (try #?(:clj  (Integer/parseInt hex 16)
                       :cljs (let [v (js/parseInt hex 16)]
                               (when (js/isNaN v) (throw (ex-info "nan" {})))
                               v))
                    (catch #?(:clj Exception :cljs :default) _
                      (errors/reader-error (str "Invalid unicode escape \\u" hex) loc)))]
      [#?(:clj (char code) :cljs (.fromCharCode js/String code)) end])))

(defn- append! [b x]
  #?(:clj (.append ^StringBuilder b x) :cljs (.push b x))
  b)

(defn resolve-string
  "Resolve a quoted string token to a Clojure string value.
   Strings containing \\uNNNN escapes are wrapped in SupRaw to preserve
   the original notation through the printer."
  [raw loc]
  (let [inner    (subs raw 1 (dec (count raw)))
        n        (count inner)
        has-esc? (volatile! false)
        b        #?(:clj (StringBuilder.) :cljs #js [])
        result
        (loop [i 0]
          (if (>= i n)
            #?(:clj (.toString b) :cljs (.join b ""))
            (let [c (.charAt inner i)]
              (if (not= c \\)
                (do (append! b c) (recur (inc i)))
                (do
                  (when (>= (inc i) n)
                    (errors/reader-error "Unterminated escape sequence in string" loc))
                  (let [e (.charAt inner (inc i))]
                    (cond
                      (get esc-chars e)
                      (do (append! b (get esc-chars e)) (recur (+ i 2)))

                      (= e \u)
                      (let [[ch ni] (parse-unicode inner (inc i) loc)]
                        (vreset! has-esc? true)
                        (append! b ch)
                        (recur ni))

                      :else
                      (errors/reader-error
                        (str "Unsupported escape sequence \\" e " in string")
                        loc))))))))]
    (if @has-esc?
      (forms/->SupRaw result raw)
      result)))

;; ---------------------------------------------------------------------------
;; Character
;; ---------------------------------------------------------------------------

(def ^:private named-chars
  {"newline"  \newline "space"     \space "tab"  \tab
   "backspace" \backspace "formfeed" \formfeed "return" \return})

(defn resolve-char
  "Resolve a character literal token (e.g. \\a, \\newline, \\u0041) to a char."
  [raw loc]
  (let [body (subs raw 1)]    ; strip leading backslash
    (cond
      (= 1 (count body))
      (.charAt body 0)

      (contains? named-chars body)
      (get named-chars body)

      (and (str/starts-with? body "u") (= 5 (count body)))
      (let [hex  (subs body 1)
            code (try #?(:clj  (Integer/parseInt hex 16)
                         :cljs (let [v (js/parseInt hex 16)]
                                 (when (js/isNaN v) (throw (ex-info "nan" {})))
                                 v))
                      (catch #?(:clj Exception :cljs :default) _
                        (errors/reader-error (str "Invalid unicode character \\u" hex) loc)))]
        (forms/->SupRaw #?(:clj (char code) :cljs (.fromCharCode js/String code)) raw))

      #?@(:clj
          [(and (str/starts-with? body "o") (<= 2 (count body) 4))
           (let [oct  (subs body 1)
                 code (try (Integer/parseInt oct 8)
                           (catch Exception _
                             (errors/reader-error
                               (str "Invalid octal character \\" body) loc)))]
             (when (> code 0377)
               (errors/reader-error (str "Octal character out of range: \\" body) loc))
             (forms/->SupRaw (char code) raw))])

      :else
      (errors/reader-error (str "Invalid character literal: " raw) loc))))

;; ---------------------------------------------------------------------------
;; Number
;; ---------------------------------------------------------------------------

(defn resolve-number
  "Resolve a number token to a Clojure numeric value.
   Handles integers, floats, hex (0x), octal (0), radix (NNr),
   ratios, BigInt (N), BigDecimal (M), and ##Inf / ##-Inf / ##NaN."
  [raw loc]
  (try
    (cond
      (= raw "##Inf")  #?(:clj Double/POSITIVE_INFINITY :cljs  js/Infinity)
      (= raw "##-Inf") #?(:clj Double/NEGATIVE_INFINITY :cljs (- js/Infinity))
      (= raw "##NaN")  #?(:clj Double/NaN               :cljs  js/NaN)

      #?@(:clj
          [;; BigDecimal M suffix
           (str/ends-with? raw "M")
           (BigDecimal. (subs raw 0 (dec (count raw))))

           ;; BigInt N suffix
           (str/ends-with? raw "N")
           (clojure.lang.BigInt/fromBigInteger
             (java.math.BigInteger. (subs raw 0 (dec (count raw)))))

           ;; Ratio  3/4
           (str/includes? raw "/")
           (let [slash (str/index-of raw "/")]
             (/ (Long/parseLong (subs raw 0 slash))
                (Long/parseLong (subs raw (inc slash)))))

           ;; Hex  0xFF  +0xff  -0xFF
           (re-find #"^[+-]?0[xX]" raw)
           (let [neg? (str/starts-with? raw "-")
                 hex  (subs raw (if (or (str/starts-with? raw "+") neg?) 3 2))
                 v    (Long/parseLong hex 16)]
             (forms/->SupRaw (if neg? (- v) v) raw))

           ;; Radix  2r1010  16rFF
           (re-matches #"[+-]?\d{1,2}r[0-9a-zA-Z]+" raw)
           (let [neg? (str/starts-with? raw "-")
                 s    (cond-> raw (or neg? (str/starts-with? raw "+")) (subs 1))
                 sep  (str/index-of s "r")
                 radix (Integer/parseInt (subs s 0 sep))
                 v     (Long/parseLong (subs s (inc sep)) radix)]
             (forms/->SupRaw (if neg? (- v) v) raw))

           ;; Octal  010  -077
           (re-matches #"[+-]?0[0-7]+" raw)
           (let [neg? (str/starts-with? raw "-")
                 oct  (subs raw (if (or neg? (str/starts-with? raw "+")) 2 1))
                 v    (Long/parseLong oct 8)]
             (forms/->SupRaw (if neg? (- v) v) raw))]

          :cljs
          [(str/ends-with? raw "N")
           (errors/reader-error "BigInt literals (N suffix) are not supported in ClojureScript" loc)
           (str/ends-with? raw "M")
           (errors/reader-error "BigDecimal literals (M suffix) are not supported in ClojureScript" loc)
           (str/includes? raw "/")
           (errors/reader-error "Ratio literals are not supported in ClojureScript" loc)
           (re-find #"^[+-]?0[xX]" raw)
           (errors/reader-error "Hex literals are not supported in ClojureScript" loc)
           (re-matches #"[+-]?\d{1,2}r[0-9a-zA-Z]+" raw)
           (errors/reader-error "Radix literals are not supported in ClojureScript" loc)
           (re-matches #"[+-]?0[0-7]+" raw)
           (errors/reader-error "Octal literals are not supported in ClojureScript" loc)])

      ;; Float  1.5  1e-3  — wrap scientific notation in SupRaw to preserve it
      (or (str/includes? raw ".") (str/includes? raw "e") (str/includes? raw "E"))
      (let [v #?(:clj (Double/parseDouble raw) :cljs (js/parseFloat raw))]
        (if (or (str/includes? raw "e") (str/includes? raw "E"))
          (forms/->SupRaw v raw)
          v))

      ;; Plain integer
      :else
      #?(:clj (Long/parseLong raw) :cljs (js/parseInt raw 10)))

    (catch #?(:clj Exception :cljs :default) ex
      (if (instance? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) ex)
        (throw ex)
        (errors/reader-error
          (str "Invalid number " raw " — " (#?(:clj ex-message :cljs .-message) ex))
          (assoc loc :cause ex))))))

;; ---------------------------------------------------------------------------
;; Regex
;; ---------------------------------------------------------------------------

(defn resolve-regex
  "Resolve a regex literal token to a compiled Pattern."
  [raw loc]
  (let [pattern (subs raw 2 (dec (count raw)))]   ; strip #" and trailing "
    (try #?(:clj  (java.util.regex.Pattern/compile pattern)
            :cljs (js/RegExp. pattern))
         (catch #?(:clj Exception :cljs :default) ex
           (errors/reader-error
             (str "Invalid regex " raw " — " (#?(:clj ex-message :cljs .-message) ex))
             (assoc loc :cause ex))))))

;; ---------------------------------------------------------------------------
;; Auto-resolve keywords  ::foo
;; ---------------------------------------------------------------------------

(defn resolve-auto-keyword
  "Resolve ::foo. Calls resolve-fn when provided; otherwise defers to eval
   time via forms/deferred-auto-keyword (JVM) or errors on CLJS."
  [raw loc resolve-fn]
  (if resolve-fn
    (try (resolve-fn raw)
         (catch #?(:clj Exception :cljs :default) ex
           (errors/reader-error
             (str "Failed to resolve keyword " raw " — "
                  (#?(:clj ex-message :cljs .-message) ex))
             (assoc loc :cause ex))))
    #?(:clj  (forms/deferred-auto-keyword raw)
       :cljs (errors/reader-error
               (str "Auto-resolve keywords (" raw ") require :resolve-keyword in opts")
               (assoc loc :hint "Pass :resolve-keyword (fn [kw] ...) in the opts map")))))

;; ---------------------------------------------------------------------------
;; Tagged literals
;; ---------------------------------------------------------------------------

(defn resolve-tagged-literal
  "Resolve a tagged literal. JVM: produces TaggedLiteral. CLJS: not supported."
  [tag data loc]
  #?(:clj  (tagged-literal tag data)
     :cljs (errors/reader-error
             (str "Tagged literals (#" tag ") are not supported in the ClojureScript reader")
             loc)))
