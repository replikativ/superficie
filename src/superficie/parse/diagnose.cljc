(ns superficie.parse.diagnose
  "Indentation-based diagnosis for block errors.

   Superficie blocks are delimited by ':' and 'end', not by indentation, so
   the reader never consults indentation to parse. When a parse has already
   failed, though, indentation is the best evidence of intent: the printer and
   almost all hand-written code put each 'end' at the indentation of the line
   that opened its block. This namespace matches 'end's to block headers by
   indentation, the way Python would, and reports which block is probably
   missing its 'end' (or which 'end' has no block).

   Only tokens at bracket depth 0 take part; blocks inside brackets are skipped."
  (:require [clojure.string :as str]))

(def ^:private openers #{:open-paren :open-bracket :open-brace :open-set :open-anon-fn})
(def ^:private closers #{:close-paren :close-bracket :close-brace})
(def ^:private continuations #{"else" "else:" "catch" "catch:" "finally" "finally:"})

(defn- header-end-tok?
  "Does tok end a block header (':' or a word with a fused ':')?"
  [tok]
  (and (#{:keyword :symbol} (:type tok))
       (let [v (:value tok)]
         (or (= ":" v)
             (and (= :symbol (:type tok)) (str/ends-with? v ":") (not= ":" v))))))

(defn- lines
  "Group tokens with a :line into [{:line :depth0 :toks [[tok depth] ...]}],
   where depth is the bracket depth before the token."
  [tokens]
  (let [{:keys [acc]}
        (reduce (fn [{:keys [depth acc]} tok]
                  (let [d' (cond (openers (:type tok)) (inc depth)
                                 (closers (:type tok)) (max 0 (dec depth))
                                 :else depth)]
                    {:depth d'
                     :acc (if-let [line (:line tok)]
                            (let [last-line (peek acc)]
                              (if (and last-line (= line (:line last-line)))
                                (conj (pop acc) (update last-line :toks conj [tok depth]))
                                (conj acc {:line line :depth0 depth :toks [[tok depth]]})))
                            acc)}))
                {:depth 0 :acc []}
                tokens)]
    acc))

(defn diagnose
  "Match block headers to 'end' tokens by indentation.
   Returns {:unclosed [{:line :col}...] :stray [{:line :col}...]} in source
   order. :unclosed are header lines whose 'end' is probably missing; :stray
   are 'end' tokens that match no open header."
  [tokens]
  (loop [ls (lines tokens)
         stack []            ; open headers: {:line :col}
         stmt nil            ; current statement start (line at depth 0)
         unclosed []
         stray []]
    (if-let [{:keys [line depth0 toks]} (first ls)]
      (let [[first-tok first-depth] (first toks)
            stmt (if (zero? depth0)
                   {:line line :col (:col first-tok)}
                   stmt)
            [last-tok last-depth] (peek toks)]
        (cond
          ;; A closing 'end' that starts its line.
          (and (zero? first-depth) (= :symbol (:type first-tok)) (= "end" (:value first-tok)))
          (let [col (:col first-tok)
                idx (last (keep-indexed (fn [i o] (when (= col (:col o)) i)) stack))]
            (cond
              (empty? stack)
              (recur (rest ls) stack stmt unclosed (conj stray {:line line :col col}))
              idx ; headers above the one this 'end' lines up with are unclosed
              (recur (rest ls) (subvec stack 0 idx) stmt
                     (into unclosed (subvec stack (inc idx))) stray)
              :else ; lines up with nothing — assume it closes the innermost block
              (recur (rest ls) (pop stack) stmt unclosed stray)))

          (and (= :symbol (:type first-tok)) (continuations (:value first-tok)))
          (recur (rest ls) stack stmt unclosed stray)

          (and (zero? last-depth) (header-end-tok? last-tok) stmt)
          (recur (rest ls) (conj stack stmt) stmt unclosed stray)

          :else
          (recur (rest ls) stack stmt unclosed stray)))
      {:unclosed (vec (sort-by :line (into unclosed stack)))
       :stray stray})))

(defn end-hint
  "A hint map {:hint :secondary} explaining a block error from indentation,
   or nil when indentation shows nothing unusual."
  [tokens]
  (let [{:keys [unclosed stray]} (diagnose tokens)]
    (cond
      (seq unclosed)
      (let [{:keys [line col]} (peek unclosed)]
        {:hint (str "By indentation, the block opened at line " line
                    " has no 'end' — add 'end' at column " col " where the block finishes")
         :secondary [{:line line :col col :label "this block is not closed"}]})

      (seq stray)
      (let [{:keys [line col]} (first stray)]
        {:hint (str "The 'end' at line " line " lines up with no open block — remove it, "
                    "or check the ':' of the header it should close")
         :secondary [{:line line :col col :label "unmatched 'end'"}]}))))
