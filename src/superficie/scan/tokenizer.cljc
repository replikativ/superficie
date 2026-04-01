(ns superficie.scan.tokenizer
  "Character-level scanner and tokenizer.
   Produces a flat vector of typed tokens from a superficie source string.
   Each token is a map: {:type kw :value str :line int :col int :offset int}
   with optional :end-line :end-col :end-offset for multi-character tokens."
  (:require [superficie.errors :as errors]))

;; ---------------------------------------------------------------------------
;; Character classification
;; ---------------------------------------------------------------------------

(defn- ch-code [c] #?(:clj (int c) :cljs (.charCodeAt c 0)))

(let [c0 (ch-code \0) c9 (ch-code \9)
      ca (ch-code \a) cz (ch-code \z)
      cA (ch-code \A) cZ (ch-code \Z)
      c7 (ch-code \7) cf (ch-code \f) cF (ch-code \F)]
  (defn- digit?   [c] (when c (let [n (ch-code c)] (and (>= n c0) (<= n c9)))))
  (defn- hex?     [c] (when c (let [n (ch-code c)]
                                (or (and (>= n c0) (<= n c9))
                                    (and (>= n ca) (<= n cf))
                                    (and (>= n cA) (<= n cF))))))
  (defn- octal?   [c] (when c (let [n (ch-code c)] (and (>= n c0) (<= n c7)))))
  (defn- letter?  [c] (when c (let [n (ch-code c)]
                                (or (and (>= n ca) (<= n cz))
                                    (and (>= n cA) (<= n cZ)))))))

(defn- ws?    [c] (and c (or (= c \space) (= c \tab) (= c \newline) (= c \return) (= c \,))))
(defn- delim? [c] (and c (#{\( \) \[ \] \{ \} \" \; \@ \^ \` \~ \\} c)))

(defn- sym-start? [c] (and c (not (ws? c)) (not (digit? c)) (not (delim? c))))
(defn- sym-char?  [c] (and c (not (ws? c)) (not (delim? c))))

;; ---------------------------------------------------------------------------
;; Scanner state — mutable position / line / col tracking
;; ---------------------------------------------------------------------------

(defn- scanner [s]
  {:src s :len (count s)
   :pos  (volatile! 0)
   :line (volatile! 1)
   :col  (volatile! 1)})

(defn- at-end? [{:keys [pos len]}]  (>= @pos len))
(defn- peek1   [{:keys [src pos len]}] (when (< @pos len) (nth src @pos)))
(defn- peek2   [{:keys [src pos len]}]
  (let [i (inc @pos)] (when (< i len) (nth src i))))

(defn- advance!
  "Consume and return the current character. Advances position and tracks line/col."
  [{:keys [src pos line col]}]
  (let [c (nth src @pos)]
    (if (= c \newline)
      (do (vswap! line inc) (vreset! col 1))
      (vswap! col inc))
    (vswap! pos inc)
    c))

(defn- loc [{:keys [line col pos]}]
  {:line @line :col @col :offset @pos})

;; ---------------------------------------------------------------------------
;; String builder (portable)
;; ---------------------------------------------------------------------------

(defn- sb   [] #?(:clj (StringBuilder.)    :cljs #js []))
(defn- sb+  [b c] #?(:clj (.append ^StringBuilder b c) :cljs (.push b (str c))) b)
(defn- sb++ [b s] #?(:clj (.append ^StringBuilder b ^String s) :cljs (.push b s)) b)
(defn- ->s  [b]  #?(:clj (.toString ^StringBuilder b)  :cljs (.join b "")))

(defn- scan-while [sc pred]
  (let [b (sb)]
    (loop []
      (when (and (not (at-end? sc)) (pred (peek1 sc)))
        (sb+ b (advance! sc))
        (recur)))
    (->s b)))

;; ---------------------------------------------------------------------------
;; Token construction
;; ---------------------------------------------------------------------------

(defn- token
  ([type value start]
   {:type type :value value
    :line (:line start) :col (:col start) :offset (:offset start)})
  ([type value start end]
   {:type type :value value
    :line (:line start) :col (:col start) :offset (:offset start)
    :end-line (:line end) :end-col (:col end) :end-offset (:offset end)}))

(defn- tok [sc type value start]
  (token type value start (loc sc)))

;; ---------------------------------------------------------------------------
;; Lexical forms
;; ---------------------------------------------------------------------------

(defn- scan-string! [sc start]
  "Scan a double-quoted string (opening \" already peeked, not consumed).
   Returns the raw string including surrounding quotes."
  (advance! sc) ; consume opening "
  (let [b (sb+ (sb) \")]
    (loop []
      (when (at-end? sc)
        (errors/reader-error "Unterminated string — missing closing \""
                             (assoc start :incomplete true)))
      (let [c (advance! sc)]
        (sb+ b c)
        (cond
          (= c \") nil
          (= c \\) (do (when-not (at-end? sc) (sb+ b (advance! sc))) (recur))
          :else    (recur))))
    (->s b)))

(defn- scan-symbol [sc]
  "Scan a symbol allowing one namespace-qualifier slash.
   Special case: clojure.core// is a qualified symbol where the name is /
   (the division operator). A second / is accepted only when it is not
   followed by another sym-char — this prevents misreading 'clojure.core///foo'."
  (let [b (sb)]
    (loop [slashed false]
      (let [c (peek1 sc)]
        (cond
          (and (= c \/) (not slashed)) (do (sb+ b (advance! sc)) (recur true))
          ;; Second / after namespace: allowed only as a bare name (not followed by sym-char)
          (and (= c \/) slashed (not (sym-char? (peek2 sc)))) (do (sb+ b (advance! sc)) nil)
          (= c \/)  nil ; second slash followed by sym-char — terminate
          (sym-char? c) (do (sb+ b (advance! sc)) (recur slashed))
          :else         nil)))
    (->s b)))

(defn- scan-number [sc]
  "Scan a number token (sign already consumed into prefix if present)."
  (scan-while sc #(or (digit? %) (letter? %) (#{\. \/ \+ \-} %))))

(defn- scan-char! [sc start]
  "Scan a character literal (backslash already peeked, not consumed)."
  (advance! sc) ; consume backslash
  (when (at-end? sc)
    (errors/reader-error "Unterminated character literal — expected a character after \\"
                         (assoc start :incomplete true)))
  (let [b (sb+ (sb) \\)
        c (advance! sc)]
    (sb+ b c)
    (cond
      (= c \u)   ; \uXXXX unicode
      (let [n (loop [i 0]
                (if (and (< i 4) (not (at-end? sc)) (hex? (peek1 sc)))
                  (do (sb+ b (advance! sc)) (recur (inc i)))
                  i))]
        (when (< n 4)
          (errors/reader-error
           (str "Incomplete unicode escape: expected 4 hex digits after \\u, got " n)
           (cond-> start (at-end? sc) (assoc :incomplete true)))))

      (= c \o)   ; \oXXX octal
      (loop [i 0]
        (when (and (< i 3) (not (at-end? sc)) (octal? (peek1 sc)))
          (sb+ b (advance! sc))
          (recur (inc i))))

      (letter? c) ; named chars: \newline, \space etc.
      (loop []
        (when (and (not (at-end? sc)) (letter? (peek1 sc)))
          (sb+ b (advance! sc))
          (recur))))
    (->s b)))

;; ---------------------------------------------------------------------------
;; Main tokenizer
;; ---------------------------------------------------------------------------

(defn tokenize
  "Tokenize a superficie source string into a vector of tokens."
  [src]
  (let [sc  (scanner src)
        out (transient [])]
    (loop []
      (if (at-end? sc)
        (persistent! out)
        (let [start (loc sc)
              c     (peek1 sc)]
          (cond
            ;; Whitespace and commas (commas are whitespace in Clojure)
            (ws? c)
            (do (advance! sc) (recur))

            ;; Line comment
            (= c \;)
            (do (scan-while sc #(not= % \newline)) (recur))

            ;; String
            (= c \")
            (do (conj! out (tok sc :string (scan-string! sc start) start)) (recur))

            ;; Character literal
            (= c \\)
            (do (conj! out (tok sc :char (scan-char! sc start) start)) (recur))

            ;; # dispatch
            (= c \#)
            (let [c2 (peek2 sc)]
              (cond
                (= c2 \{)  (do (advance! sc) (advance! sc)
                               (conj! out (tok sc :open-set "#{" start)) (recur))
                (= c2 \()  (do (advance! sc) (advance! sc)
                               (conj! out (tok sc :open-anon-fn "#(" start)) (recur))
                (= c2 \")  (do (advance! sc)
                               (conj! out (tok sc :regex
                                               (str "#" (scan-string! sc (loc sc)))
                                               start))
                               (recur))
                (= c2 \')  (do (advance! sc) (advance! sc)
                               (conj! out (tok sc :var-quote "#'" start)) (recur))
                (= c2 \_)  (do (advance! sc) (advance! sc)
                               (conj! out (tok sc :discard "#_" start)) (recur))
                (= c2 \?)  (do (advance! sc) (advance! sc)
                               (let [splice? (= (peek1 sc) \@)
                                     _       (when splice? (advance! sc))
                                     prefix  (if splice? "#?@" "#?")]
                                 (conj! out (tok sc :reader-cond-start prefix start)))
                               (recur))
                (= c2 \:)  (do (advance! sc) (advance! sc)
                               (let [ns-part (scan-symbol sc)]
                                 (conj! out (tok sc :namespaced-map-start
                                                 (str "#:" ns-part) start)))
                               (recur))
                (= c2 \#)  (do (advance! sc) (advance! sc)
                               (conj! out (tok sc :number
                                               (str "##" (scan-symbol sc)) start))
                               (recur))
                (nil? c2)  (do (advance! sc)
                               (errors/reader-error
                                "Unexpected # at end of input — expected #{}, #\"\", #', #_, #?, or a tagged literal"
                                start))
                (sym-start? c2)
                (do (advance! sc)
                    (conj! out (tok sc :tagged-literal (str "#" (scan-symbol sc)) start))
                    (recur))
                :else
                (do (advance! sc)
                    (errors/reader-error
                     (str "Invalid dispatch: #" c2
                          " — # must be followed by {, \", ', _, ?, :, #, or a tag name")
                     start))))

            (= c \@)  (do (advance! sc) (conj! out (tok sc :deref "@" start)) (recur))
            (= c \^)  (do (advance! sc) (conj! out (tok sc :meta "^" start)) (recur))
            (= c \')  (do (advance! sc) (conj! out (tok sc :quote "'" start)) (recur))
            (= c \`)  (do (advance! sc)
                          (when (at-end? sc)
                            (errors/reader-error
                             "Unexpected end of input after ` — expected a form to syntax-quote"
                             (assoc start :incomplete true)))
                          (conj! out (tok sc :syntax-quote "`" start))
                          (recur))
            (= c \~)  (do (advance! sc)
                          (if (= (peek1 sc) \@)
                            (do (advance! sc)
                                (conj! out (tok sc :unquote-splicing "~@" start)))
                            (conj! out (tok sc :unquote "~" start)))
                          (recur))

            (= c \()  (do (advance! sc) (conj! out (tok sc :open-paren   "(" start)) (recur))
            (= c \))  (do (advance! sc) (conj! out (tok sc :close-paren  ")" start)) (recur))
            (= c \[)  (do (advance! sc) (conj! out (tok sc :open-bracket "[" start)) (recur))
            (= c \])  (do (advance! sc) (conj! out (tok sc :close-bracket "]" start)) (recur))
            (= c \{)  (do (advance! sc) (conj! out (tok sc :open-brace   "{" start)) (recur))
            (= c \})  (do (advance! sc) (conj! out (tok sc :close-brace  "}" start)) (recur))

            ;; Keyword
            (= c \:)
            (do (advance! sc)
                (let [auto?  (= (peek1 sc) \:)
                      _      (when auto? (advance! sc))
                      kw-str (scan-symbol sc)
                      value  (str (if auto? "::" ":") kw-str)]
                  (conj! out (tok sc :keyword value start)))
                (recur))

            ;; Unsigned number
            (digit? c)
            (do (conj! out (tok sc :number (scan-number sc) start)) (recur))

            ;; Signed number: - or + immediately followed by a digit
            (and (or (= c \-) (= c \+)) (digit? (peek2 sc)))
            (let [sign (advance! sc)]
              (conj! out (tok sc :number (str sign (scan-number sc)) start))
              (recur))

            ;; Symbol or operator
            (sym-start? c)
            (do (conj! out (tok sc :symbol (scan-symbol sc) start)) (recur))

            :else
            (do (advance! sc)
                (errors/reader-error (str "Unexpected character: " c) start))))))))

;; ---------------------------------------------------------------------------
;; Whitespace attachment
;;
;; After tokenizing, walk the token vector and attach the source text between
;; consecutive tokens as :ws on the following token.  Used by the pretty-printer
;; to reconstruct original comments and blank lines.
;; ---------------------------------------------------------------------------

(defn- line-col->offset
  "Convert 1-indexed line/col to a 0-indexed character offset.
   Only \\n advances the line counter (matches the scanner's line model)."
  [src line col]
  (let [n (count src)]
    (loop [i 0 cur-line 1 cur-col 1]
      (cond
        (and (= cur-line line) (= cur-col col)) i
        (>= i n) i
        (= (nth src i) \newline) (recur (inc i) (inc cur-line) 1)
        :else (recur (inc i) cur-line (inc cur-col))))))

(defn attach-whitespace
  "Attach the leading whitespace/comments before each token as :ws.
   The whitespace after the last token is stored as :trailing-ws metadata
   on the returned vector."
  [tokens src]
  (if (empty? tokens)
    (if (seq src) (with-meta [] {:trailing-ws src}) [])
    (let [n       (count tokens)
          src-len (count src)]
      (loop [i 0 prev-end 0 out (transient [])]
        (if (>= i n)
          (let [result   (persistent! out)
                trailing (when (< prev-end src-len) (subs src prev-end))]
            (cond-> result trailing (with-meta {:trailing-ws trailing})))
          (let [t       (nth tokens i)
                t-start (or (:offset t)
                            (line-col->offset src (:line t) (:col t)))
                ws      (when (< prev-end t-start) (subs src prev-end t-start))
                t'      (cond-> t ws (assoc :ws ws))
                t-end   (or (:end-offset t)
                            (if (and (:end-line t) (:end-col t))
                              (line-col->offset src (:end-line t) (:end-col t))
                              (+ t-start (count (:value t)))))]
            (recur (inc i) t-end (conj! out t'))))))))
