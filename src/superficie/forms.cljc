(ns superficie.forms
  "Shared form-level predicates and constructors.
   Cross-stage contracts used by both the parser and printer."
  (:require [clojure.string :as str]
            #?(:cljs [cljs.tools.reader.impl.utils :refer [ReaderConditional]])))

;; ---------------------------------------------------------------------------
;; Deferred auto-resolve keywords
;;
;; When no :resolve-keyword fn is provided, ::foo keywords are emitted as
;; (clojure.core/read-string \"::foo\") — a form that resolves at eval time
;; in the user's namespace. The printer detects this encoding to round-trip
;; ::foo text correctly.
;; ---------------------------------------------------------------------------

(defn deferred-auto-keyword
  "Wrap a :: keyword string as a deferred eval form: (clojure.core/read-string \"::foo\")."
  [raw]
  (list 'clojure.core/read-string raw))

(defn deferred-auto-keyword?
  "True when form is a deferred ::foo produced by the reader."
  [form]
  (and (seq? form)
       (= 2 (count form))
       (= 'clojure.core/read-string (first form))
       (let [s (second form)]
         (and (string? s) (str/starts-with? s "::")))))

(defn deferred-auto-keyword-raw [form] (second form))

;; ---------------------------------------------------------------------------
;; Portable reader-conditional support
;;
;; JVM: use native clojure.lang.ReaderConditional via reader-conditional /
;;      reader-conditional?.  CLJS: polyfill with a defrecord.
;; Using the native JVM type means forms from sup->forms and clj->forms are
;; the same type — required for roundtrip equality tests.
;; ---------------------------------------------------------------------------

#?(:cljs (defrecord SupReaderConditional [form splicing]))

(defn make-reader-conditional
  "Portable reader-conditional constructor."
  [form splicing?]
  #?(:clj  (reader-conditional form splicing?)
     :cljs (->SupReaderConditional form splicing?)))

(defn sup-reader-conditional?
  "True when x is a reader conditional (portable across JVM and CLJS).
   Recognises both our SupReaderConditional and cljs.tools.reader's native type."
  [x]
  #?(:clj  (reader-conditional? x)
     :cljs (or (instance? SupReaderConditional x)
               (instance? ReaderConditional x))))

(defn rc-form
  "The form list inside a reader conditional."
  [rc]
  #?(:clj  (.-form ^clojure.lang.ReaderConditional rc)
     :cljs (:form rc)))

(defn rc-splicing?
  "True when the reader conditional is a splicing form (#?@)."
  [rc]
  #?(:clj  (.-splicing ^clojure.lang.ReaderConditional rc)
     :cljs (if (instance? SupReaderConditional rc)
             (:splicing rc)
             (:splicing? rc))))

;; ---------------------------------------------------------------------------
;; Syntax-quote / unquote / unquote-splicing AST nodes
;;
;; The parser preserves backtick/tilde as AST nodes rather than eagerly
;; expanding them. The printer reconstructs the original notation from these
;; nodes. Runtime paths expand them before eval.
;; ---------------------------------------------------------------------------

(defrecord SupSyntaxQuote      [form])
(defrecord SupUnquote          [form])
(defrecord SupUnquoteSplicing  [form])

(defn syntax-quote?      [x] (instance? SupSyntaxQuote x))
(defn unquote?           [x] (instance? SupUnquote x))
(defn unquote-splicing?  [x] (instance? SupUnquoteSplicing x))

;; ---------------------------------------------------------------------------
;; Raw value wrapper
;;
;; Numbers (0xFF, 1e2), characters (\u0041), and strings with unicode escapes
;; are primitive types that cannot carry metadata.  SupRaw pairs the resolved
;; value with the original source text so the printer can reproduce the exact
;; notation.  Runtime paths unwrap before eval.
;; ---------------------------------------------------------------------------

(defrecord SupRaw [value raw])

(defn raw?       [x] (instance? SupRaw x))
(defn raw-value  [x] (:value x))
(defn raw-text   [x] (:raw x))

;; ---------------------------------------------------------------------------
;; Internal metadata keys
;;
;; These keys are used by the pipeline for bookkeeping and are excluded when
;; checking for user-visible metadata (^:private, ^String, ^{:doc "..."}).
;; ---------------------------------------------------------------------------

(def internal-meta-keys
  "Pipeline-internal metadata keys — not emitted by the printer."
  #{:line :column :file :ws
    :sup/sugar :sup/order :sup/ns :sup/meta-chain})

(defn strip-internal-meta
  "Return m with all internal pipeline keys removed."
  [m]
  (apply dissoc m internal-meta-keys))

;; ---------------------------------------------------------------------------
;; % parameter utilities  (#() short fn)
;;
;; Both the reader (building the fn param vector) and printer (detecting #()
;; short-form) need to identify % parameter symbols.
;; ---------------------------------------------------------------------------

(defn percent-param-type
  "If sym is a % parameter symbol return its type: :bare, :rest, or integer N.
   Returns nil otherwise."
  [sym]
  (when (symbol? sym)
    (let [n (name sym)]
      (cond
        (= n "%")  :bare
        (= n "%&") :rest
        (and (str/starts-with? n "%")
             (> (count n) 1)
             (re-matches #"\d+" (subs n 1)))
        #?(:clj  (Long/parseLong (subs n 1))
           :cljs (js/parseInt (subs n 1) 10))
        :else nil))))

;; ---------------------------------------------------------------------------
;; Shrubbery tree node types
;;
;; The grouper (scan/grouper.cljc) produces a tree of these nodes from a flat
;; token stream.  The grouper resolves bracket structure only — operators and
;; block keywords remain as ShrubToken leaves for the enforest pass.
;;
;; The key invariant: the grouper NEVER throws.  Mismatched or missing
;; delimiters produce ShrubError nodes embedded in the tree; the rest of the
;; tree is structurally correct.
;; ---------------------------------------------------------------------------

(defrecord ShrubToken    [tok])
(defrecord ShrubGroup    [items loc])          ; top-level sequence
(defrecord ShrubParens   [items loc])          ; ( ... )
(defrecord ShrubBrackets [items loc])          ; [ ... ]
(defrecord ShrubBraces   [items loc])          ; { ... }
(defrecord ShrubSet      [items loc])          ; #{ ... }
(defrecord ShrubAnonFn   [items loc])          ; #( ... )
(defrecord ShrubError    [message items loc    ; error node; items = partial children
                          expected-close       ; token type kw that was expected (:close-paren etc.)
                          actual-close         ; token that terminated recovery, or nil for EOF
                          open-type])          ; opening token type (:open-paren etc.), nil for top-level stray closers

(defn shrub-token?    [x] (instance? ShrubToken x))
(defn shrub-group?    [x] (instance? ShrubGroup x))
(defn shrub-parens?   [x] (instance? ShrubParens x))
(defn shrub-brackets? [x] (instance? ShrubBrackets x))
(defn shrub-braces?   [x] (instance? ShrubBraces x))
(defn shrub-set?      [x] (instance? ShrubSet x))
(defn shrub-anon-fn?  [x] (instance? ShrubAnonFn x))
(defn shrub-error?    [x] (instance? ShrubError x))

(defn shrub-container?
  "True for any shrubbery node that holds child items."
  [x]
  (or (instance? ShrubGroup x)
      (instance? ShrubParens x)
      (instance? ShrubBrackets x)
      (instance? ShrubBraces x)
      (instance? ShrubSet x)
      (instance? ShrubAnonFn x)
      (instance? ShrubError x)))

(defn shrub-items
  "Return child items of a shrubbery container node, or nil for leaf tokens."
  [x]
  (when (shrub-container? x) (:items x)))

(defn shrub-loc
  "Return source location {:line :col :offset} of the node's opening position."
  [x]
  (:loc x))

(defn collect-shrub-errors
  "Walk a shrubbery tree depth-first and collect all ShrubError nodes."
  [node]
  (cond
    (shrub-error? node)
    (cons node (mapcat collect-shrub-errors (:items node)))
    (shrub-container? node)
    (mapcat collect-shrub-errors (:items node))
    :else nil))
