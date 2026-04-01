(ns superficie.parse.enforest
  "Enforest pass: shrubbery tree → Clojure forms.

   The enforest delegates semantic analysis (Pratt climbing, block dispatch,
   macro sugar) to the existing reader.  Its job is to:
     1. Flatten the shrubbery tree back to a well-formed token stream, healing
        ShrubError nodes by re-wrapping partial children with the expected
        synthetic delimiters.
     2. Feed that stream to reader/parse-tokens.
     3. Return the forms together with any structural errors found in the tree.

   The heal strategy (option D): a ShrubError with open-type :open-bracket that
   expected :close-bracket gets emitted as [ ...items... ] — the partial children
   are re-bracketed so the reader sees structurally valid input.  Stray closers
   (open-type nil) are dropped silently; the error is still reported."
  (:require [superficie.forms :as f]
            [superficie.parse.reader :as reader]))

;; ---------------------------------------------------------------------------
;; Token synthesis helpers
;; ---------------------------------------------------------------------------

(def ^:private synthetic-open
  "Synthetic opening token for each open-type keyword."
  {:open-paren    {:type :open-paren    :value "("  :synthetic true}
   :open-bracket  {:type :open-bracket  :value "["  :synthetic true}
   :open-brace    {:type :open-brace    :value "{"  :synthetic true}
   :open-set      {:type :open-set      :value "#{" :synthetic true}
   :open-anon-fn  {:type :open-anon-fn  :value "#(" :synthetic true}})

(def ^:private synthetic-close
  "Synthetic closing token for each expected-close-type keyword."
  {:close-paren   {:type :close-paren   :value ")" :synthetic true}
   :close-bracket {:type :close-bracket :value "]" :synthetic true}
   :close-brace   {:type :close-brace   :value "}" :synthetic true}})

;; ---------------------------------------------------------------------------
;; shrub->tokens: flatten shrubbery tree to a well-formed token vector
;; ---------------------------------------------------------------------------

(defn shrub->tokens
  "Recursively flatten a shrubbery node to a vector of tokens.

   Well-formed nodes become their constituent tokens unchanged.
   ShrubError nodes are healed:
     - If open-type is known, re-wrap partial children with synthetic
       open + expected-close delimiters (the reader sees valid syntax).
     - Stray closers (open-type nil) are dropped; the error is reported
       separately via grouper-errors."
  [node]
  (cond
    (f/shrub-token? node)
    [(:tok node)]

    (f/shrub-error? node)
    (let [{:keys [items open-type expected-close]} node]
      (if open-type
        ;; Heal: re-wrap the partial children with synthetic delimiters
        (let [opener (get synthetic-open open-type)
              closer (get synthetic-close expected-close)
              loc    (:loc node)
              opener (cond-> opener
                       loc (merge (select-keys loc [:line :col :offset])))]
          (into (if opener [opener] [])
                (concat (mapcat shrub->tokens items)
                        (if closer [closer] []))))
        ;; Stray closer — drop it (error already collected by grouper-errors)
        (mapcat shrub->tokens items)))

    (f/shrub-group? node)
    (vec (mapcat shrub->tokens (:items node)))

    (f/shrub-parens? node)
    (let [loc (:loc node)]
      (into [{:type :open-paren :value "("
              :line (:line loc) :col (:col loc) :offset (:offset loc)
              :ws (:ws loc)}]
            (concat (mapcat shrub->tokens (:items node))
                    [{:type :close-paren :value ")"}])))

    (f/shrub-brackets? node)
    (let [loc (:loc node)]
      (into [{:type :open-bracket :value "["
              :line (:line loc) :col (:col loc) :offset (:offset loc)
              :ws (:ws loc)}]
            (concat (mapcat shrub->tokens (:items node))
                    [{:type :close-bracket :value "]"}])))

    (f/shrub-braces? node)
    (let [loc (:loc node)]
      (into [{:type :open-brace :value "{"
              :line (:line loc) :col (:col loc) :offset (:offset loc)
              :ws (:ws loc)}]
            (concat (mapcat shrub->tokens (:items node))
                    [{:type :close-brace :value "}"}])))

    (f/shrub-set? node)
    (let [loc (:loc node)]
      (into [{:type :open-set :value "#{"
              :line (:line loc) :col (:col loc) :offset (:offset loc)
              :ws (:ws loc)}]
            (concat (mapcat shrub->tokens (:items node))
                    [{:type :close-brace :value "}"}])))

    (f/shrub-anon-fn? node)
    (let [loc (:loc node)]
      (into [{:type :open-anon-fn :value "#("
              :line (:line loc) :col (:col loc) :offset (:offset loc)
              :ws (:ws loc)}]
            (concat (mapcat shrub->tokens (:items node))
                    [{:type :close-paren :value ")"}])))

    :else
    []))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn enforest-forms
  "Enforest a ShrubGroup (from scan/grouper) into a vector of Clojure forms.
   Heals structural errors and delegates to reader/parse-tokens."
  ([shrub]         (enforest-forms shrub nil nil))
  ([shrub opts]    (enforest-forms shrub opts nil))
  ([shrub opts source]
   (let [tokens (shrub->tokens shrub)]
     (reader/parse-tokens (vec tokens) opts source))))

(defn enforest-result
  "Like enforest-forms but also returns structural errors from the shrubbery.
   Returns {:forms [...] :errors [...ShrubError...]}."
  ([shrub]         (enforest-result shrub nil nil))
  ([shrub opts]    (enforest-result shrub opts nil))
  ([shrub opts source]
   {:forms  (enforest-forms shrub opts source)
    :errors (vec (f/collect-shrub-errors shrub))}))
