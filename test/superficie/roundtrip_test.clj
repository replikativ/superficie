(ns superficie.roundtrip-test
  "Tests that Clojure -> .sup -> Clojure round-trips correctly."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.render :as render]
            [superficie.parse :as parse]
            [superficie.resolve :as resolve]))

(def ctx
  "Default resolution context (clojure.core fully available)."
  {:refers {} :aliases {} :excludes #{}})

(defn roundtrip
  "Render a Clojure form to .sup, then parse it back."
  [form]
  (let [rendered (render/render-form ctx form)]
    (parse/parse-string rendered)))

(deftest test-roundtrip-atoms
  (testing "numbers"
    (is (= 42 (roundtrip 42)))
    (is (= 3.14 (roundtrip 3.14))))
  (testing "strings"
    (is (= "hello" (roundtrip "hello"))))
  (testing "keywords"
    (is (= :foo (roundtrip :foo)))
    (is (= :foo/bar (roundtrip :foo/bar))))
  (testing "symbols"
    (is (= 'my-func (roundtrip 'my-func)))
    (is (= 'foo/bar (roundtrip 'foo/bar))))
  (testing "nil and booleans"
    (is (nil? (roundtrip nil)))
    (is (= true (roundtrip true)))
    (is (= false (roundtrip false)))))

(deftest test-roundtrip-collections
  (is (= [1 2 3] (roundtrip [1 2 3])))
  (is (= {:a 1} (roundtrip {:a 1})))
  (is (= #{1 2} (roundtrip #{1 2}))))

(deftest test-roundtrip-arithmetic
  (is (= '(+ 1 2) (roundtrip '(+ 1 2))))
  (is (= '(* a b) (roundtrip '(* a b))))
  (is (= '(> x 0) (roundtrip '(> x 0))))
  (is (= '(and a b) (roundtrip '(and a b))))
  (is (= '(or a b) (roundtrip '(or a b)))))

(deftest test-roundtrip-calls
  (is (= '(println "hello") (roundtrip '(println "hello"))))
  (is (= '(map inc [1 2 3]) (roundtrip '(map inc [1 2 3]))))
  (is (= '(f) (roundtrip '(f)))))

(deftest test-roundtrip-interop
  (is (= '(.toLowerCase s) (roundtrip '(.toLowerCase s))))
  (is (= '(.getBytes s "UTF-8") (roundtrip '(.getBytes s "UTF-8")))))

(deftest test-roundtrip-def
  (is (= '(def x 42) (roundtrip '(def x 42)))))

(deftest test-roundtrip-if
  (is (= '(if (> x 0) "positive" "negative")
         (roundtrip '(if (> x 0) "positive" "negative")))))

(deftest test-roundtrip-let
  (is (= '(let [x 1 y 2] (+ x y))
         (roundtrip '(let [x 1 y 2] (+ x y))))))

(deftest test-roundtrip-ns
  (testing "ns with require and import"
    (is (= '(ns myapp.core
              (:require [clojure.string :as str]
                        [clojure.set :refer [union intersection]])
              (:import [java.util Date]))
           (roundtrip '(ns myapp.core
                         (:require [clojure.string :as str]
                                   [clojure.set :refer [union intersection]])
                         (:import [java.util Date]))))))
  (testing "bare ns"
    (is (= '(ns foo.bar)
           (roundtrip '(ns foo.bar)))))
  (testing "ns with gen-class"
    (is (= '(ns foo.bar (:gen-class))
           (roundtrip '(ns foo.bar (:gen-class)))))))

(deftest test-roundtrip-unary
  (is (= '(not x) (roundtrip '(not x))))
  (is (= '(deref atom) (roundtrip '(deref atom)))))

(deftest test-roundtrip-quote-var
  (testing "quote"
    (is (= '(quote x) (roundtrip '(quote x))))
    (is (= '(quote [1 2 3]) (roundtrip '(quote [1 2 3])))))
  (testing "var"
    (is (= '(var my-var) (roundtrip '(var my-var))))))

(deftest test-roundtrip-varargs
  (is (= '(defn foo [a b & rest] (apply + a b rest))
         (roundtrip '(defn foo [a b & rest] (apply + a b rest))))))

(deftest test-roundtrip-for
  (is (= '(for [x (range 10) :when (even? x)] (* x x))
         (roundtrip '(for [x (range 10) :when (even? x)] (* x x)))))
  (is (= '(doseq [x [1 2 3]] (println x))
         (roundtrip '(doseq [x [1 2 3]] (println x))))))

(deftest test-roundtrip-case
  (is (= '(case x :a 1 :b 2 :default)
         (roundtrip '(case x :a 1 :b 2 :default))))
  (is (= '(case x :a 1 :b 2)
         (roundtrip '(case x :a 1 :b 2)))))

(deftest test-roundtrip-binding-forms
  (is (= '(when-let [x (get m :key)] (println x))
         (roundtrip '(when-let [x (get m :key)] (println x)))))
  (is (= '(if-let [x (find m :key)] x nil)
         (roundtrip '(if-let [x (find m :key)] x nil)))))

(deftest test-roundtrip-nested
  (is (= '(let [x 1] (if (> x 0) (+ x 1) (- x 1)))
         (roundtrip '(let [x 1] (if (> x 0) (+ x 1) (- x 1))))))
  (is (= '(do (println "a") (let [x 1] x))
         (roundtrip '(do (println "a") (let [x 1] x))))))

(deftest test-roundtrip-try
  (is (= '(try (/ 1 0) (catch ArithmeticException e (str e)) (finally (println "done")))
         (roundtrip '(try (/ 1 0) (catch ArithmeticException e (str e)) (finally (println "done")))))))

(deftest test-roundtrip-loop
  (is (= '(loop [i 0] (when (< i 10) (println i) (recur (+ i 1))))
         (roundtrip '(loop [i 0] (when (< i 10) (println i) (recur (+ i 1))))))))

(deftest test-roundtrip-threading
  (is (= '(-> x (assoc :a 1) (update :b inc))
         (roundtrip '(-> x (assoc :a 1) (update :b inc))))))

(deftest test-roundtrip-defmacro
  (is (= '(defmacro my-when [test & body] (list 'if test (cons 'do body)))
         (roundtrip '(defmacro my-when [test & body] (list 'if test (cons 'do body)))))))

(deftest test-roundtrip-condp
  (is (= '(condp = x 1 :one 2 :two :other)
         (roundtrip '(condp = x 1 :one 2 :two :other))))
  (is (= '(condp = x 1 :one 2 :two)
         (roundtrip '(condp = x 1 :one 2 :two)))))

(deftest test-roundtrip-multi-arity
  (is (= '(defn greet ([] (greet "World")) ([name] (str "Hello, " name)))
         (roundtrip '(defn greet ([] (greet "World")) ([name] (str "Hello, " name)))))))

(deftest test-roundtrip-for-let
  (is (= '(for [x (range 10) :let [y (* x x)] :when (even? y)] y)
         (roundtrip '(for [x (range 10) :let [y (* x x)] :when (even? y)] y)))))

(deftest test-roundtrip-precedence
  (testing "lower-precedence sub-expressions preserved"
    (is (= '(* (+ a b) c) (roundtrip '(* (+ a b) c))))
    (is (= '(* a (+ b c)) (roundtrip '(* a (+ b c)))))
    (is (= '(* (+ a b) (- c d)) (roundtrip '(* (+ a b) (- c d))))))
  (testing "higher-precedence needs no parens and still round-trips"
    (is (= '(+ (* a b) c) (roundtrip '(+ (* a b) c))))
    (is (= '(+ a (* b c)) (roundtrip '(+ a (* b c))))))
  (testing "logical with comparison"
    (is (= '(and (> a 0) (> b 0)) (roundtrip '(and (> a 0) (> b 0)))))
    (is (= '(and (or a b) c) (roundtrip '(and (or a b) c))))))

(deftest test-roundtrip-core-async-symbols
  (testing "core.async operator symbols as values"
    (is (= '<! (roundtrip '<!)))
    (is (= '>! (roundtrip '>!)))
    (is (= '<!! (roundtrip '<!!)))
    (is (= '>!! (roundtrip '>!!))))
  (testing "core.async operator symbols in calls"
    (is (= '(<! ch) (roundtrip '(<! ch))))
    (is (= '(>! ch val) (roundtrip '(>! ch val))))
    (is (= '(<!! ch) (roundtrip '(<!! ch))))
    (is (= '(>!! ch val) (roundtrip '(>!! ch val))))))

(deftest test-roundtrip-when-while
  (testing "when block"
    (is (= '(when (> x 0) (println x))
           (roundtrip '(when (> x 0) (println x))))))
  (testing "when with multiple body forms"
    (is (= '(when true (println "a") (println "b"))
           (roundtrip '(when true (println "a") (println "b"))))))
  (testing "when-let"
    (is (= '(when-let [x (get m :key)] (println x))
           (roundtrip '(when-let [x (get m :key)] (println x))))))
  (testing "while"
    (is (= '(while (pos? (deref counter)) (swap! counter dec))
           (roundtrip '(while (pos? (deref counter)) (swap! counter dec)))))))

(deftest test-roundtrip-interop-extended
  (testing "new constructor roundtrips to canonical form"
    (is (= '(HashMap. 16)
           (roundtrip '(new HashMap 16)))))
  (testing "field access"
    (is (= '(.-field obj)
           (roundtrip '(.-field obj)))))
  (testing "static method"
    (is (= '(Integer/parseInt "42")
           (roundtrip '(Integer/parseInt "42"))))))

(deftest test-roundtrip-cond-threading
  (testing "cond-> threading"
    (is (= '(cond-> m true (assoc :a 1) false (assoc :b 2))
           (roundtrip '(cond-> m true (assoc :a 1) false (assoc :b 2))))))
  (testing "some-> renders as -> and roundtrips"
    (is (= '(-> x (str/trim) (str/lower-case))
           (roundtrip '(some-> x str/trim str/lower-case)))))
  (testing "some->> renders as ->> and roundtrips"
    (is (= '(->> x (filter odd?) (map inc))
           (roundtrip '(some->> x (filter odd?) (map inc)))))))

(deftest test-roundtrip-destructuring
  (testing "map destructuring in let"
    (is (= '(let [{:keys [a b]} m] (+ a b))
           (roundtrip '(let [{:keys [a b]} m] (+ a b))))))
  (testing "vector destructuring in let"
    (is (= '(let [[x y] pair] (+ x y))
           (roundtrip '(let [[x y] pair] (+ x y))))))
  (testing "destructuring in defn"
    (is (= '(defn f [{:keys [x y]}] (+ x y))
           (roundtrip '(defn f [{:keys [x y]}] (+ x y)))))))
