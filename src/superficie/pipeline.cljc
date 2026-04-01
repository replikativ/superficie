(ns superficie.pipeline
  "Pipeline composition: source string → tokens → shrubbery → Clojure forms.

   Stages:
     scan   — tokenize + attach whitespace           :raw-tokens, :tokens
     group  — resilient LL bracket grouper           :shrubbery, :group-errors
     parse  — enforest: heal errors + reader pass    :forms

   run returns the full context map with all keys.
   run-resilient additionally returns :group-errors alongside :forms."
  (:require [superficie.scan.tokenizer  :as tokenizer]
            [superficie.scan.grouper    :as grouper]
            [superficie.parse.enforest  :as enforest]
            [superficie.parse.reader    :as reader]))

(defn scan
  "Tokenize source text. Attaches leading whitespace/comments as :ws on each token."
  [ctx]
  (let [src (:source ctx)]
    (when-not (string? src)
      (throw (ex-info (str "Pipeline :source must be a string, got "
                           (if (nil? src) "nil" (type src))) {})))
    (let [tokens (tokenizer/tokenize src)]
      (assoc ctx
             :raw-tokens tokens
             :tokens     (tokenizer/attach-whitespace tokens src)))))

(defn group
  "Group the flat token stream into a shrubbery tree.
   Never throws — structural errors become :group-errors entries."
  [ctx]
  (when-not (:tokens ctx)
    (throw (ex-info "Pipeline :tokens missing — run scan before group" {})))
  (let [shrub (grouper/group-tokens (:tokens ctx))]
    (assoc ctx
           :shrubbery   shrub
           :group-errors (vec (grouper/grouper-errors shrub)))))

(defn parse
  "Enforest the shrubbery into Clojure forms.
   Heals structural errors so the reader can process a well-formed token stream."
  [ctx]
  (when-not (:shrubbery ctx)
    (throw (ex-info "Pipeline :shrubbery missing — run group before parse" {})))
  (assoc ctx :forms
         (enforest/enforest-forms (:shrubbery ctx) (:opts ctx) (:source ctx))))

(defn run
  "Run the full pipeline: source → scan → group → parse → forms.
   Returns the context map with :source, :raw-tokens, :tokens,
   :shrubbery, :group-errors, and :forms."
  ([source]       (run source nil))
  ([source opts]  (-> {:source source :opts opts} scan group parse)))

(defn run-resilient
  "Like run but returns {:forms [...] :errors [...ShrubError...]} — errors are
   structural errors from the grouper, reported without throwing."
  ([source]       (run-resilient source nil))
  ([source opts]
   (let [ctx (run source opts)]
     {:forms       (:forms ctx)
      :errors      (:group-errors ctx)})))
