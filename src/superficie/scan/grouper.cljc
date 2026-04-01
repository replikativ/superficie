(ns superficie.scan.grouper
  "Resilient LL grouper: flat token stream → shrubbery tree.
   Resolves bracket structure only — operators and block keywords remain as
   ShrubToken leaves for the enforest pass to handle.

   Contract: group-tokens NEVER throws. Missing or mismatched delimiters produce
   ShrubError nodes embedded in the tree; the surrounding structure is valid."
  (:require [superficie.forms :as f]))

;; ---------------------------------------------------------------------------
;; Grouper state
;; ---------------------------------------------------------------------------

(defn- make-grouper [tokens]
  {:tokens tokens :pos (volatile! 0)})

(defn- geof? [{:keys [tokens pos]}]
  (>= @pos (count tokens)))

(defn- gpeek
  ([g] (gpeek g 0))
  ([{:keys [tokens pos]} offset]
   (let [i (+ @pos offset)]
     (when (< i (count tokens)) (nth tokens i)))))

(defn- gadvance! [{:keys [pos]}]
  (vswap! pos inc))

(defn- gloc [tok]
  ;; Include :ws so the enforest can detect adjacent-paren calls (f(x) vs f (x))
  (cond-> (select-keys tok [:line :col :offset])
    (:ws tok) (assoc :ws (:ws tok))))

;; ---------------------------------------------------------------------------
;; Token classification
;; ---------------------------------------------------------------------------

;; Tokens that open a nested container
(def ^:private open-types
  #{:open-paren :open-bracket :open-brace :open-set :open-anon-fn})

;; Tokens that close any container (stop the current group-seq)
(def ^:private all-close-types
  #{:close-paren :close-bracket :close-brace})

;; For each opening token type: expected closing token type
(def ^:private expected-close-for
  {:open-paren   :close-paren
   :open-bracket :close-bracket
   :open-brace   :close-brace
   :open-set     :close-brace
   :open-anon-fn :close-paren})

;; Constructor for each opening token type
(def ^:private ctor-for
  {:open-paren   f/->ShrubParens
   :open-bracket f/->ShrubBrackets
   :open-brace   f/->ShrubBraces
   :open-set     f/->ShrubSet
   :open-anon-fn f/->ShrubAnonFn})

;; Human-readable name for error messages
(def ^:private close-name
  {:close-paren   ")"
   :close-bracket "]"
   :close-brace   "}"})

(def ^:private open-name
  {:open-paren   "("
   :open-bracket "["
   :open-brace   "{"
   :open-set     "#{"
   :open-anon-fn "#("})

;; ---------------------------------------------------------------------------
;; Core algorithm
;; ---------------------------------------------------------------------------

(declare group-item)

(defn- group-seq
  "Parse a sequence of items, stopping when any closing delimiter or EOF is seen.
   Does NOT consume the stopping token — callers inspect it to decide success/error.
   Returns [items stop-tok-or-nil]."
  [g]
  (loop [items (transient [])]
    (if (geof? g)
      [(persistent! items) nil]
      (let [tok  (gpeek g)
            type (:type tok)]
        (if (contains? all-close-types type)
          [(persistent! items) tok]   ; stop, don't consume
          (recur (conj! items (group-item g))))))))

(defn- group-container
  "Parse a container that starts with open-tok (already consumed by caller).
   Reads children via group-seq, then expects the matching close.
   Returns the appropriate Shrub node — either a well-formed container or a ShrubError."
  [g open-tok]
  (let [open-type   (:type open-tok)
        exp-close   (expected-close-for open-type)
        ctor        (ctor-for open-type)
        loc         (gloc open-tok)
        [items stop-tok] (group-seq g)]
    (cond
      ;; EOF — unclosed container
      (nil? stop-tok)
      (f/->ShrubError
        (str "Unclosed " (open-name open-type) " — expected " (close-name exp-close))
        items loc exp-close nil open-type)

      ;; Correct matching close
      (= (:type stop-tok) exp-close)
      (do (gadvance! g)   ; consume the matching close
          (ctor items loc))

      ;; Wrong close delimiter — do NOT consume it; let the parent handle it
      :else
      (f/->ShrubError
        (str "Expected " (close-name exp-close)
             " to close " (open-name open-type)
             " but found " (or (close-name (:type stop-tok)) (:value stop-tok)))
        items loc exp-close stop-tok open-type))))

(defn- group-item
  "Parse one shrubbery item, consuming the relevant token(s).
   - Opening delimiter: recurse into group-container
   - Unexpected closing delimiter: emit ShrubError and consume it
   - Any other token: wrap in ShrubToken"
  [g]
  (let [tok  (gpeek g)
        type (:type tok)]
    (cond
      ;; Opening delimiter — consume it, then group the container
      (contains? open-types type)
      (do (gadvance! g)
          (group-container g tok))

      ;; Unexpected closing delimiter — consume it and emit an error leaf
      ;; (a well-formed group-seq never reaches here for expected closers)
      (contains? all-close-types type)
      (do (gadvance! g)
          (f/->ShrubError
            (str "Unexpected " (close-name type)
                 " — no matching open delimiter")
            [] (gloc tok) nil tok nil))

      ;; Everything else: leaf token
      :else
      (do (gadvance! g)
          (f/->ShrubToken tok)))))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn group-tokens
  "Group a flat token vector into a shrubbery tree.
   Resolves bracket nesting only; never throws.
   Returns a ShrubGroup whose :items are the top-level shrubbery nodes."
  [tokens]
  (if (empty? tokens)
    (f/->ShrubGroup [] nil)
    (let [g             (make-grouper tokens)
          [items stop]  (group-seq g)]
      ;; At the top level, any stop-tok is an unexpected closer
      (let [items' (if stop
                     (conj items (f/->ShrubError
                                   (str "Unexpected " (close-name (:type stop))
                                        " at top level")
                                   [] (gloc stop) nil stop nil))
                     items)]
        ;; After consuming the stop-tok (if any), continue draining remaining tokens
        (if stop
          (do (gadvance! g)
              (let [[rest-items _] (group-seq g)]
                (f/->ShrubGroup (into items' rest-items) nil)))
          (f/->ShrubGroup items' nil))))))

(defn grouper-errors
  "Collect all ShrubError nodes from a shrubbery tree in document order."
  [shrub]
  (f/collect-shrub-errors shrub))
