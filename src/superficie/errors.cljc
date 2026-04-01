(ns superficie.errors
  "Error infrastructure for the superficie reader and tokenizer.
   All parse errors go through `reader-error` for uniform location tracking
   and message formatting."
  (:require [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; Source-line extraction
;; ---------------------------------------------------------------------------

(defn source-context
  "Return the source line at 1-indexed line number, or nil if unavailable.
   Uses str/split-lines (splits on \\n and \\r\\n), which is the display model
   and may differ from the scanner's \\n-only model for CRLF sources."
  [source line]
  (when (and source line (not (str/blank? source)))
    (let [lines (str/split-lines source)
          i     (dec line)]
      (when (and (>= i 0) (< i (count lines)))
        (nth lines i)))))

;; ---------------------------------------------------------------------------
;; Error construction
;; ---------------------------------------------------------------------------

(defn reader-error
  "Throw a reader/tokenizer error with structured location data.
   data may contain :line, :col (1-indexed), :cause, :source, :hint,
   :secondary (vector of {:line :col :label} maps), :incomplete.
   :source-context is derived from :source + :line when both are present."
  ([msg] (reader-error msg {}))
  ([msg {:keys [line col cause source] :as data}]
   (let [loc-suffix (when (and line col) (str " (line " line ", col " col ")"))
         full-msg   (str msg loc-suffix)
         ex-data    (cond-> (dissoc data :cause :source)
                      (and source line)
                      (assoc :source-context (source-context source line)))]
     (throw (ex-info full-msg ex-data cause)))))

;; ---------------------------------------------------------------------------
;; Error formatting
;; ---------------------------------------------------------------------------

(defn- gutter
  "Right-aligned line-number gutter string of total width w."
  [n w]
  (let [s   (str n)
        pad (- w (count s))]
    (str (apply str (repeat pad " ")) s " | ")))

(defn- blank-gutter [w] (str (apply str (repeat w " ")) " | "))

(defn- underline
  "Underline from col to end-col (exclusive). Single char → ^, multi → ~~~."
  [col end-col]
  (let [start (max 1 (or col 1))
        end   (or end-col start)
        len   (max 1 (- end start))]
    (str (apply str (repeat (dec start) " "))
         (if (= 1 len) "^" (apply str (repeat len "~"))))))

(defn format-error
  "Format a parse exception as a human-readable string with source context,
   underline spans, secondary locations, and hints."
  ([e] (format-error e nil))
  ([e source]
   (let [msg  (ex-message e)
         data (when (instance? #?(:clj clojure.lang.ExceptionInfo :cljs ExceptionInfo) e)
                (ex-data e))
         line     (:line data)
         col      (:col data)
         end-col  (:end-col data)
         hint     (:hint data)
         secondary (:secondary data)
         ctx-line (or (when (and source line) (source-context source line))
                      (:source-context data))
         all-lines (cond-> [] line (conj line)
                           secondary (into (keep :line secondary)))
         w (if (seq all-lines) (count (str (apply max all-lines))) 1)
         out (volatile! [(str "Error: " msg)])]
     (when ctx-line
       (vswap! out conj (str "\n" (gutter line w) ctx-line))
       (when (and col (pos? col))
         (let [dlen  (count ctx-line)
               c     (min col (inc dlen))
               ec    (when end-col (min end-col (inc dlen)))]
           (vswap! out conj (str "\n" (blank-gutter w) (underline c ec))))))
     (doseq [{sl :line sc :col label :label} secondary]
       (when-let [ctx (and source sl (source-context source sl))]
         (vswap! out conj (str "\n" (gutter sl w) ctx))
         (when (and sc (pos? sc))
           (let [csc (min sc (inc (count ctx)))]
             (vswap! out conj (str "\n" (blank-gutter w)
                                   (apply str (repeat (dec csc) " ")) "^ " label))))))
     (when hint (vswap! out conj (str "\nHint: " hint)))
     (apply str @out))))
