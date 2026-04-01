(ns superficie.operators
  "Extensible operator registry for the superficie surface syntax.
   All operators are keyed by fully-qualified symbols.
   A secondary surface-string index enables fast tokenizer lookups.

   Standard clojure.core operators are pre-registered.
   Haskell-inspired operators (<$>, <*>, >>=, **) are defined as real
   vars and auto-registered when this namespace loads.")

;; ---------------------------------------------------------------------------
;; Primary registry: qualified-sym → entry
;; entry: {:prec N :assoc :left|:right :kind :infix|:prefix|:postfix
;;          :variadic true|false   ;; flatten (+ a b) + c → (+ a b c)
;;          :comparison true|false ;; chain   a < b < c → (and (< a b) (< b c))
;;          :str "override"        ;; ONLY when (name sym) ≠ surface string (->> / ->)
;;          :expander (fn [l r])   ;; optional: transform at parse time}
;; ---------------------------------------------------------------------------

(def ^:dynamic *op-registry*
  "Global atom: qualified-sym → operator entry.
   Consulted by the Pratt climber and printer. Thread-safe via swap!."
  (atom
   {;; Arithmetic
    'clojure.core/+    {:prec 40 :assoc :left  :kind :infix :variadic true}
    'clojure.core/-    {:prec 40 :assoc :left  :kind :infix}
    'clojure.core/*    {:prec 50 :assoc :left  :kind :infix :variadic true}
    'clojure.core//    {:prec 50 :assoc :left  :kind :infix}
    'clojure.core/mod  {:prec 50 :assoc :left  :kind :infix}
    'clojure.core/rem  {:prec 50 :assoc :left  :kind :infix}
     ;; Logic
    'clojure.core/and  {:prec 20 :assoc :left  :kind :infix :variadic true}
    'clojure.core/or   {:prec 10 :assoc :left  :kind :infix :variadic true}
     ;; Comparison
    'clojure.core/=    {:prec 30 :assoc :left  :kind :infix :comparison true}
    'clojure.core/not= {:prec 30 :assoc :left  :kind :infix :comparison true}
    'clojure.core/<    {:prec 30 :assoc :left  :kind :infix :comparison true}
    'clojure.core/>    {:prec 30 :assoc :left  :kind :infix :comparison true}
    'clojure.core/<=   {:prec 30 :assoc :left  :kind :infix :comparison true}
    'clojure.core/>=   {:prec 30 :assoc :left  :kind :infix :comparison true}
     ;; Threading — :str because (name '->>)="->>" ≠ surface "|>"
    'clojure.core/->>  {:prec 5  :assoc :left  :kind :infix :variadic true :str "|>"}
    'clojure.core/->   {:prec 5  :assoc :left  :kind :infix :variadic true :str ".>"}}))

;; ---------------------------------------------------------------------------
;; Secondary index: surface-string → qualified-sym
;; Rebuilt whenever *op-registry* changes.
;; Used as fallback when no :resolve-sym context is available.
;; ---------------------------------------------------------------------------

(def ^:dynamic *surface-index*
  "Maps surface operator string → qualified symbol.
   Rebuilt on each register-op!. Used for fast infix-tok? checks."
  (atom {}))

(defn- rebuild-surface-index! []
  (reset! *surface-index*
          (reduce (fn [idx [qsym entry]]
                    (assoc idx (or (:str entry) (name qsym)) qsym))
                  {} @*op-registry*)))

;; Initialize on load
(rebuild-surface-index!)

;; ---------------------------------------------------------------------------
;; Registration API
;; ---------------------------------------------------------------------------

(defn register-op!
  "Register or update an operator in the global registry.
   qsym must be a fully-qualified symbol (e.g., 'my.ns/<$>).
   opts map: {:prec N :assoc :left/:right :kind :infix/:prefix/:postfix
              :variadic bool :comparison bool
              :str \"surface\" ; only when (name qsym) ≠ surface string
              :expander (fn [left right] form)}"
  [qsym opts]
  (swap! *op-registry* assoc qsym opts)
  (let [surf-str (or (:str opts) (name qsym))]
    (swap! *surface-index* assoc surf-str qsym)))

(defn surface-string
  "Return the surface operator string for a registered operator."
  [qsym]
  (let [e (get @*op-registry* qsym)]
    (when e (or (:str e) (name qsym)))))

;; ---------------------------------------------------------------------------
;; defsupoperator macro — auto-qualifies to the calling namespace
;; ---------------------------------------------------------------------------

(defmacro defsupoperator
  "Register a user-defined surface operator in the current namespace.

  Basic form:
    (defsupoperator <> :prec 35)

  With associativity:
    (defsupoperator ** :prec 55 :assoc :right)

  With a parse-time expander (receives left/right AST, returns Clojure form):
    (defsupoperator <+> :prec 40
      [left right] `(xor ~left ~right))

  The symbol is qualified to *ns* and registered in *op-registry*."
  [op & more]
  (let [op-str  (if (symbol? op) (name op) (str op))
        qsym    (symbol (str *ns*) op-str)
        [opts-kvs body-part] (split-with (complement vector?) more)
        opts-map (apply hash-map opts-kvs)
        expander (when (seq body-part)
                   (let [[params & body] body-part]
                     `(fn ~params ~@body)))]
    `(register-op! '~qsym
                   ~(cond-> (merge {:kind :infix :assoc :left} opts-map)
                      expander (assoc :expander expander)))))

;; ---------------------------------------------------------------------------
;; superficie.operators: Haskell-inspired stdlib operators
;;
;; These are real Clojure vars (with :superficie/op metadata) that are
;; also registered as infix operators. Requiring this namespace makes them
;; available both as functions and as surface syntax operators.
;; ---------------------------------------------------------------------------

(def ^{:superficie/op {:prec 45 :assoc :left :kind :infix}
       :doc "Functor map — (f <$> xs) = (map f xs). Like Haskell's (<$>)."}
  <$> map)

(def ^{:superficie/op {:prec 50 :assoc :left :kind :infix}
       :doc "Applicative apply — (fs <*> xs) = (mapcat #(map % xs) fs). Like Haskell's (<*>)."}
  <*> (fn [fs xs] (mapcat #(map % xs) fs)))

(def ^{:superficie/op {:prec 1 :assoc :right :kind :infix}
       :doc "Monadic bind — (xs >>= f) = (mapcat f xs) for sequences. Like Haskell's (>>=)."}
  >>= (fn [xs f] (mapcat f xs)))

(def ^{:superficie/op {:prec 55 :assoc :right :kind :infix}
       :doc "Exponentiation — (a ** b) = (Math/pow a b). Like ** in Python/Julia."}
  **  #?(:clj  (fn [base exp] (Math/pow (double base) (double exp)))
         :cljs (fn [base exp] (js/Math.pow base exp))))

(def ^{:superficie/op {:prec 60 :assoc :right :kind :infix}
       :doc "Left-to-right function composition — (f >> g) = (comp g f).
             (inc >> str) 3  →  \"4\""}
  >> (fn [f g] (comp g f)))

(def ^{:superficie/op {:prec 60 :assoc :right :kind :infix}
       :doc "Right-to-left function composition — (f << g) = (comp f g).
             Like Haskell's (.).  (str << inc) 3  →  \"4\""}
  << comp)

(def ^{:doc "Clause-separator for case/match/cond surface syntax blocks.
             Not a runtime function — throws if called at runtime."}
  =>
  (fn [& _]
    (throw (ex-info "=> is a case/match/cond syntax separator and cannot be called at runtime"
                    {}))))

;; Register all superficie.operators stdlib ops on load
(doseq [[sym v] {`<$>  {:prec 45 :assoc :left  :kind :infix}
                 `<*>  {:prec 50 :assoc :left  :kind :infix}
                 `>>=  {:prec 1  :assoc :right :kind :infix}
                 `**   {:prec 55 :assoc :right :kind :infix
                        :expander #?(:clj  (fn [l r] (list 'Math/pow l r))
                                     :cljs (fn [l r] (list 'js/Math.pow l r)))}
                 `>>   {:prec 60 :assoc :right :kind :infix
                        :expander (fn [f g] (list 'comp g f))}
                 `<<   {:prec 60 :assoc :right :kind :infix
                        :expander (fn [f g] (list 'comp f g))}}]
  (register-op! sym v))
