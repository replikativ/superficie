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
            [superficie.parse.reader    :as reader]
            [superficie.errors          :as errors]))

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

(defn- throw-group-error!
  "Report a structural (bracket) error at its own location. Unless the caller
   asked for resilient parsing, an unbalanced bracket is an error: healing it
   silently would change what the program means."
  [ctx {:keys [message loc actual-close open-type]}]
  (errors/reader-error
   message
   (cond-> {:line (:line loc) :col (:col loc) :source (:source ctx)}
     (nil? actual-close) (assoc :incomplete true)
     (and open-type (nil? actual-close))
     (assoc :hint "Add the missing closing delimiter, or remove the one opened here")
     (and open-type actual-close (:line actual-close))
     (assoc :secondary [{:line (:line actual-close) :col (:col actual-close)
                         :label "found this instead"}]))))

(defn parse
  "Enforest the shrubbery into Clojure forms.
   Structural errors from the grouper are thrown at their location. With
   {:resilient true} in the ctx they are healed instead, so the reader can
   still process a well-formed token stream (see run-resilient)."
  [ctx]
  (when-not (:shrubbery ctx)
    (throw (ex-info "Pipeline :shrubbery missing — run group before parse" {})))
  (when-let [ge (and (not (:resilient ctx)) (first (:group-errors ctx)))]
    (throw-group-error! ctx ge))
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
   (let [ctx (-> {:source source :opts opts :resilient true} scan group parse)]
     {:forms       (:forms ctx)
      :errors      (:group-errors ctx)})))
