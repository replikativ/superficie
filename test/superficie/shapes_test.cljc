(ns superficie.shapes-test
  "Shape descriptors: block rendering for library macros (raster, ansatz)."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.core :as core]
            [superficie.shapes :as shapes]))

(def ^:private deftm-shape '[:name :doc? [:wrap? All 1] :params [:kw? :-] :body])

(def ^:private ns-header
  "(ns demo (:require [raster.core :refer [deftm ftm]] [raster.par :as par] [ansatz.core :as a]))\n")

(defn- render [clj-src]
  (core/clj->sup (str ns-header clj-src)))

(defn- roundtrips? [clj-src]
  (let [src (str ns-header clj-src)
        forms (core/clj->forms src)]
    (and (= forms (core/sup->forms (core/forms->sup forms)))
         (= forms (core/sup->forms (core/pprint-sup forms))))))

;; ---------------------------------------------------------------------------
;; split / join
;; ---------------------------------------------------------------------------

(deftest test-split-join-inverse
  (doseq [args '[(f [x :- Long] :- Long (inc x))
                 (f "doc" [x] x y)
                 (f (All [T] [x :- T] :- T (inc x)))
                 (f [x])]]
    (let [[header body :as r] (shapes/split deftm-shape args)]
      (is (some? r) (pr-str args))
      (is (= args (seq (shapes/join deftm-shape header body))) (pr-str args)))))

(deftest test-split-rejects-misfit
  (testing "a wrapped form that is not the last argument does not fit"
    (is (nil? (shapes/split-verified deftm-shape '(f (All [T] [x] x) extra)))))
  (testing "a missing parameter vector does not fit"
    (is (nil? (shapes/split-verified deftm-shape '(f x))))))

(deftest test-valid-shape
  (is (shapes/valid-shape? deftm-shape))
  (is (not (shapes/valid-shape? [:name :body :params])) "body must be last")
  (is (not (shapes/valid-shape? '[:form? [:wrap? All 1] :body])) "ambiguous wrap point"))

;; ---------------------------------------------------------------------------
;; ns context
;; ---------------------------------------------------------------------------

(deftest test-ns-context
  (let [ctx (shapes/ns-context '(ns x (:require [raster.core :refer [deftm]]
                                                [ansatz.core :as a]
                                                (raster [par :as p]))))]
    (is (= 'raster.core/deftm (shapes/resolve-static 'deftm ctx)))
    (is (= 'ansatz.core/defn (shapes/resolve-static 'a/defn ctx)))
    (is (= 'raster.par/map-void! (shapes/resolve-static 'p/map-void! ctx)))
    (is (nil? (shapes/resolve-static 'defn ctx)))))

;; ---------------------------------------------------------------------------
;; Rendering
;; ---------------------------------------------------------------------------

(deftest test-deftm-renders-as-block
  (let [out (render "(deftm norm [x :- Double y :- Double] :- Double (sqrt (+ (* x x) (* y y))))")]
    (is (str/includes? out "deftm norm [x :- Double y :- Double] :- Double:\n  sqrt(x * x + y * y)\nend"))))

(deftest test-deftm-all-wrapper
  (let [out (render "(deftm id (All [T] [x :- T] :- T x))")]
    (is (str/includes? out "deftm id All [T] [x :- T] :- T:\n  x\nend"))))

(deftest test-alias-is-kept
  (testing "a/defn stays a/defn — dropping the alias would change the meaning"
    (let [out (render "(a/defn double [n :- Nat] Nat (+ n n))")]
      (is (str/includes? out "a/defn double [n :- Nat] Nat:"))
      (is (not (re-find #"(?m)^defn " out))))))

(deftest test-unresolved-head-stays-a-call
  (testing "without the ns form, deftm is unknown and renders as a call"
    (is (= "deftm(f, [x], x)" (core/forms->sup ['(deftm f [x] x)])))))

(deftest test-quoted-shape-is-not-a-block
  (testing "the reader parses no blocks inside a quote, so the printer prints a call"
    (let [out (core/forms->sup ['(ns demo (:require [ansatz.core :as a]))
                                ''(a/theorem foo [x] P (simp))])]
      (is (str/ends-with? out "'a/theorem(foo, [x], P, simp())")))))

(deftest test-long-params-break-inside-brackets
  (let [out (render (str "(deftm f [aaaaaaaa :- (Array double) bbbbbbbb :- (Array double)"
                         " cccccccc :- (Array double) dddddddd :- (Array double)] :- Double 1.0)"))]
    (is (str/includes? out "deftm f [aaaaaaaa :- Array(double),\n         bbbbbbbb :- Array(double),"))))

(deftest test-shape-roundtrips
  (doseq [src ["(deftm clamp01 \"Clamp.\" [x :- Float] :- Float (if (< x 0.0) 0.0 x))"
               "(deftm sgd! (All [T] [p :- (Array T) lr :- T] :- (Array T) (dotimes [i (alength p)] (aset p i (* lr (aget p i)))) p))"
               "(deftm c! [U :- (Array float) n :- Long] :- Void (par/map-void! idx n (aset U idx (float 0.0))))"
               "(a/defn gd-step [x :- Real, eta :- Real] Real (sub Real x eta))"
               "(a/defn ^Nat double [^Nat n] (+ n n))"
               "(a/theorem add-zero [n :- Nat] (= Nat (+ n 0) n) (simp Nat.add_zero))"
               "(a/theorem no-tactics [n :- Nat] (= Nat n n))"
               "(def f (ftm [x :- Double] :- Double (* x x)))"
               "(defmacro mk [n] `(deftm ~n [x :- Long] :- Long x))"
               "(eval '(a/theorem t [x :- Nat] (= Nat x x) (rfl)))"]]
    (is (roundtrips? src) src)))

(deftest test-registered-shape
  (shapes/register-shape! 'my.lib/defwidget [:name :params :body])
  (try
    (let [src "(ns w (:require [my.lib :as ml]))\n(ml/defwidget button [label] (render label))"
          out (core/clj->sup src)]
      (is (str/includes? out "ml/defwidget button [label]:\n  render(label)\nend"))
      (is (= (core/clj->forms src) (core/sup->forms out))))
    (finally
      (shapes/unregister-shape! 'my.lib/defwidget))))

;; ---------------------------------------------------------------------------
;; Header colon
;; ---------------------------------------------------------------------------

(deftest test-colon-fuses-after-plain-symbol
  (is (= "when k <= steps:\n  f(k)\nend" (core/forms->sup ['(when (<= k steps) (f k))])))
  (testing "no fusing where the fused token would read differently"
    (is (= "if x = nil :\n  1\nend" (core/forms->sup ['(if (= x nil) 1)])))
    (is (str/starts-with? (core/forms->sup ['(if match [a] b)]) "if match :"))))
