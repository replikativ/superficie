(ns superficie.shorthands-test
  "Readable shorthands: #() literals, arrow lambdas in call arguments, `|`
   match arms in ansatz forms, indexing in raster kernels, and shape options."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.core :as core]
            [superficie.emit.printer :as printer]
            [superficie.shapes :as shapes]))

(defn- norm
  "forms with each #() the Clojure reader expanded (fn* [p1__N#] …) written as
   superficie reads it back, (fn [%1] …): equal up to the reader's gensyms."
  [x]
  (cond
    (and (seq? x) (printer/clj-anon-fn x)) (norm (printer/clj-anon-fn x))
    (seq? x) (apply list (map norm x))
    (vector? x) (mapv norm x)
    (map? x) (into {} (map (fn [[k v]] [(norm k) (norm v)])) x)
    :else x))

(defn- roundtrips? [forms]
  (and (= (norm forms) (norm (core/sup->forms (core/forms->sup forms))))
       (= (norm forms) (norm (core/sup->forms (core/pprint-sup forms))))))

(def ^:private kernel-ns
  '(ns k (:require [raster.core :refer [deftm]] [raster.par :as par] [ansatz.core :as a])))

(deftest test-anon-fn-literals
  (testing "#() from Clojure source prints as #(), not as fn* with gensyms"
    (is (= "map(#(inc(%)), xs)" (core/forms->sup '[(map #(inc %) xs)])))
    (is (= "map(#(%1 + %2), xs, ys)" (core/forms->sup '[(map #(+ %1 %2) xs ys)])))
    (is (= "map(#(apply(f, %, %&)), xs)" (core/forms->sup '[(map #(apply f % %&) xs)]))))
  (testing "and reads back to the same function"
    (is (roundtrips? '[(map #(inc %) xs)]))
    (is (roundtrips? '[(map #(f %2) xs ys)]))
    (is (roundtrips? '[(every? #(contains? b %) (keys a))]))
    (is (roundtrips? '[(defmacro m [x] `(map #(list ~x %) xs))])))
  (testing "a #() inside a block header's brackets"
    (is (roundtrips? '[(defn f [x] (let [a (map #(g %) x)] a))]))))

(deftest test-arrow-lambdas
  (testing "a one-line fn with plain parameters, as a call argument"
    (is (= "map(x -> x * x, xs)" (core/forms->sup '[(map (fn [x] (* x x)) xs)])))
    (is (= "reduce((acc, x) -> acc + x, 0, xs)"
           (core/forms->sup '[(reduce (fn [acc x] (+ acc x)) 0 xs)])))
    (is (roundtrips? '[(map (fn [x] (* x x)) xs) (reduce (fn [acc x] (+ acc x)) 0 xs)])))
  (testing "everywhere else the fn block stays"
    (is (= "def f: fn [x]: x end" (core/forms->sup '[(def f (fn [x] x))])))
    (is (= "{:f fn [x]: x end}" (core/forms->sup '[{:f (fn [x] x)}])))
    (is (str/includes? (core/forms->sup '[(or r (fn [a b] (g a b)))]) "fn [a b]:"))
    (is (str/includes? (core/forms->sup '[(map (fn [^long x] x) xs)]) "fn [^long x]:"))
    (is (str/includes? (core/forms->sup '[(map (fn [[a b]] a) xs)]) "fn [[a b]]:")))
  (testing "a multi-line body keeps the block"
    (let [forms '[(map (fn [x] (let [y (* x x)] (if (pos? y) y (- y)))) xs)]]
      (is (str/includes? (core/pprint-sup forms) "fn [x]:"))
      (is (roundtrips? forms))))
  (testing "-> needs spaces on both sides; otherwise it is threading or a symbol"
    (is (= '[(let [b (-> a (f))] b)] (core/sup->forms "let [b ->(a, f())]:\n  b\nend")))
    (is (= '[(f -> a)] (core/sup->forms "f(->, a)")))
    (is (roundtrips? '[[a -> b] {a -> b c} (f x -> a)]))))

(deftest test-match-arms-in-ansatz-forms
  (let [forms [kernel-ns
               '(a/defn len [xs :- (List Nat)] Nat
                  (match xs [nil 0] [(cons h t) (+ 1 (len t))]))]]
    (is (str/includes? (core/pprint-sup forms)
                       "  match xs:\n    | nil => 0\n    | cons(h, t) => 1 + len(t)\n  end"))
    (is (roundtrips? forms)))
  (testing "mixing arm styles is an error"
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (core/sup->forms "match x:\n  | 1 => a\n  2 => b\nend")))))

(deftest test-empty-defrecord
  (is (= "defrecord Empty [a b]: end" (core/forms->sup '[(defrecord Empty [a b])])))
  (is (roundtrips? '[(defrecord Empty [a b])])))

(deftest test-indexing-in-kernels
  (let [forms [kernel-ns
               '(deftm g [U :- (Array double) i :- Long] :- Double
                  (+ (aget U i) (aget (f U) i) (aget U i 2)))]]
    (is (str/includes? (core/pprint-sup forms) "  U[i] + f(U)[i] + U[i, 2]\n"))
    (is (roundtrips? forms)))
  (testing "outside a kernel, aget stays a call"
    (is (= "aget(U, i)" (core/forms->sup '[(aget U i)]))))
  (testing "in the header, name[x] is not indexing"
    (is (= [kernel-ns '(deftm foo [x :- Long] :- Long (aget x 0))]
           (core/sup->forms (str (core/forms->sup [kernel-ns])
                                 "\n\ndeftm foo[x :- Long] :- Long:\n  x[0]\nend"))))
    (is (roundtrips? [kernel-ns '(deftm foo [x :- Long] :- Long
                                   (par/map-void! i (aget x 0) (aset x i (aget x i))))]))))

(deftest test-index-stores
  (let [forms [kernel-ns
               '(deftm f [U :- (Array double) n :- Long] :- Void
                  (par/map-void! i n (aset U i (* 0.5 (aget U i))))
                  (aset U 0 1 (aget U 1)))]]
    (is (str/includes? (core/pprint-sup forms) "    U[i] <- 0.5 * U[i]\n"))
    (is (str/includes? (core/pprint-sup forms) "  U[0, 1] <- U[1]\n"))
    (is (roundtrips? forms)))
  (testing "a store in value position is parenthesized"
    (let [forms [kernel-ns '(deftm f [U :- Long] :- Long (+ 1 (aset U 0 1)) (let [x (aset U 0 1)] x))]]
      (is (str/includes? (core/pprint-sup forms) "1 + (U[0] <- 1)"))
      (is (str/includes? (core/pprint-sup forms) "let [x (U[0] <- 1)]:"))
      (is (roundtrips? forms))))
  (testing "a long value breaks after the arrow"
    (let [forms [kernel-ns '(deftm f [U :- Long] :- Long
                              (aset U i (* alpha (+ (aget U (- i 1)) (aget U (+ i 1))
                                                    (aget U (- i W)) (aget U (+ i W))))))]]
      (is (= (norm forms) (norm (core/sup->forms (core/pprint-sup forms {:width 40})))))))
  (testing "<- stores only into an index"
    (let [head (str (core/forms->sup [kernel-ns]) "\n\ndeftm f [U :- Long] :- Long:\n  ")]
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (core/sup->forms (str head "aget(U, 0) <- 1\nend"))))
      (is (thrown? #?(:clj Exception :cljs js/Error)
                   (core/sup->forms (str head "U[] \nend")))))))

(deftest test-let-statements
  (testing "x := v binds for the rest of the body, at any statement"
    (is (= '[(defn g [] (print 1) (let [x 1 y (+ x 1)] (f y) (let [z 2] (h z))))]
           (core/sup->forms "defn g []:\n  print(1)\n  x := 1\n  y := x + 1\n  f(y)\n  z := 2\n  h(z)\nend"))))
  (testing "elsewhere := is the keyword"
    (is (= '[[x := y] (f x := 1) {x :=} (quote [x := y])]
           (core/sup->forms "[x := y]\n\nf(x, :=, 1)\n\n{x :=}\n\n'[x := y]")))
    (is (roundtrips? '[[x := y] (f x := 1) {x :=} (quote [x := y]) (do x := y)])))
  (testing "at top level it is an error"
    (is (thrown? #?(:clj Exception :cljs js/Error) (core/sup->forms "x := 1")))))

(deftest test-shape-options
  (testing "options come with the shape from the same source"
    (is (= '{:index {:get aget :set aset}} (shapes/shape-options 'raster.core/deftm)))
    (shapes/register-shape! 'my.lib/kernel [:name :params :body] '{:index at})
    (try
      (let [forms '[(ns w (:require [my.lib :refer [kernel]]))
                    (kernel k [A i] (at A i))]]
        (is (str/includes? (core/pprint-sup forms) "kernel k [A i]:\n  A[i]\nend"))
        (is (= forms (core/sup->forms (core/pprint-sup forms)))))
      (finally (shapes/unregister-shape! 'my.lib/kernel))))
  (testing "invalid options are rejected"
    (is (shapes/valid-options? '{:index my.ns/at}))
    (is (not (shapes/valid-options? '{:index "at"})))
    (is (not (shapes/valid-options? '{:index {:set aset}})))
    (is (not (shapes/valid-options? '{:unknown true})))
    (is (thrown? #?(:clj Exception :cljs js/Error)
                 (shapes/register-shape! 'my.lib/bad [:body] '{:dotted-calls "yes"})))))
