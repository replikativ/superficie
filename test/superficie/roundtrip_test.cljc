(ns superficie.roundtrip-test
  "Tests that Clojure → .sup → Clojure round-trips correctly via the new pipeline."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.core :as core]))

(defn- roundtrip
  "Convert a Clojure form to sup then back to a form."
  [form]
  (first (core/sup->forms (core/forms->sup [form]))))

#?(:clj
   (defn- clj-roundtrip
     "Convert a Clojure source string through sup and back."
     [clj-src]
     (core/sup->clj (core/clj->sup clj-src))))

;; ---------------------------------------------------------------------------
;; Atoms
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-atoms
  (is (= 42       (roundtrip 42)))
  (is (= 3.14     (roundtrip 3.14)))
  (is (= "hello"  (roundtrip "hello")))
  (is (= :foo     (roundtrip :foo)))
  (is (= :foo/bar (roundtrip :foo/bar)))
  (is (= 'my-func (roundtrip 'my-func)))
  (is (= 'foo/bar (roundtrip 'foo/bar)))
  (is (nil?       (roundtrip nil)))
  (is (= true     (roundtrip true)))
  (is (= false    (roundtrip false))))

;; ---------------------------------------------------------------------------
;; Collections
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-collections
  (is (= [1 2 3]    (roundtrip [1 2 3])))
  (is (= {:a 1}     (roundtrip {:a 1})))
  (is (= #{1 2}     (roundtrip #{1 2}))))

;; ---------------------------------------------------------------------------
;; Operators
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-arithmetic
  (is (= '(+ 1 2)   (roundtrip '(+ 1 2))))
  (is (= '(* a b)   (roundtrip '(* a b))))
  (is (= '(> x 0)   (roundtrip '(> x 0))))
  (is (= '(and a b) (roundtrip '(and a b))))
  (is (= '(or a b)  (roundtrip '(or a b)))))

(deftest test-roundtrip-precedence
  (is (= '(* (+ a b) c)    (roundtrip '(* (+ a b) c))))
  (is (= '(+ (* a b) c)    (roundtrip '(+ (* a b) c))))
  (is (= '(and (> a 0) (> b 0)) (roundtrip '(and (> a 0) (> b 0))))))

;; ---------------------------------------------------------------------------
;; Calls
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-calls
  (is (= '(println "hello")   (roundtrip '(println "hello"))))
  (is (= '(map inc [1 2 3])   (roundtrip '(map inc [1 2 3]))))
  (is (= '(f)                  (roundtrip '(f)))))

;; ---------------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-def
  (is (= '(def x 42) (roundtrip '(def x 42)))))

(deftest test-roundtrip-defn
  (is (= '(defn greet [name] (println name))
         (roundtrip '(defn greet [name] (println name)))))
  (is (= '(defn add [a b] (+ a b))
         (roundtrip '(defn add [a b] (+ a b)))))
  (testing "varargs"
    (is (= '(defn foo [a b & rest] (apply + a b rest))
           (roundtrip '(defn foo [a b & rest] (apply + a b rest))))))
  (testing "multi-arity"
    (is (= '(defn greet ([] (greet "World")) ([name] (str "Hello, " name)))
           (roundtrip '(defn greet ([] (greet "World")) ([name] (str "Hello, " name))))))))

(deftest test-roundtrip-defmacro
  (is (= '(defmacro my-when [test & body] (list 'if test (cons 'do body)))
         (roundtrip '(defmacro my-when [test & body] (list 'if test (cons 'do body)))))))

(deftest test-roundtrip-if
  (is (= '(if (> x 0) "positive" "negative")
         (roundtrip '(if (> x 0) "positive" "negative"))))
  (is (= '(if (> x 0) "positive")
         (roundtrip '(if (> x 0) "positive")))))

(deftest test-roundtrip-let
  (is (= '(let [x 1 y 2] (+ x y))
         (roundtrip '(let [x 1 y 2] (+ x y))))))

(deftest test-roundtrip-when
  (is (= '(when (> x 0) (println x))
         (roundtrip '(when (> x 0) (println x)))))
  (is (= '(when true (println "a") (println "b"))
         (roundtrip '(when true (println "a") (println "b"))))))

(deftest test-roundtrip-cond
  (is (= '(cond (< x 0) :neg (= x 0) :zero :else :pos)
         (roundtrip '(cond (< x 0) :neg (= x 0) :zero :else :pos)))))

(deftest test-roundtrip-case
  (is (= '(case x 1 "one" 2 "two" "other")
         (roundtrip '(case x 1 "one" 2 "two" "other")))))

(deftest test-roundtrip-for
  (is (= '(for [x xs y ys] [x y])
         (roundtrip '(for [x xs y ys] [x y]))))
  (is (= '(for [x xs :when (> x 0)] x)
         (roundtrip '(for [x xs :when (> x 0)] x)))))

(deftest test-roundtrip-try
  (is (= '(try (/ 1 0) (catch ArithmeticException e (str e)) (finally (println "done")))
         (roundtrip '(try (/ 1 0) (catch ArithmeticException e (str e)) (finally (println "done")))))))

(deftest test-roundtrip-loop
  (is (= '(loop [i 0] (when (< i 10) (recur (+ i 1))))
         (roundtrip '(loop [i 0] (when (< i 10) (recur (+ i 1))))))))

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-threading
  (is (= '(->> data (filter even?) (map inc))
         (roundtrip '(->> data (filter even?) (map inc)))))
  (is (= '(-> x (assoc :a 1) (update :b inc))
         (roundtrip '(-> x (assoc :a 1) (update :b inc))))))

;; ---------------------------------------------------------------------------
;; Interop
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-interop
  (is (= '(.toLowerCase s)        (roundtrip '(.toLowerCase s))))
  (is (= '(.getBytes s "UTF-8")   (roundtrip '(.getBytes s "UTF-8"))))
  (is (= '(.-field obj)           (roundtrip '(.-field obj))))
  (is (= '(Integer/parseInt "42") (roundtrip '(Integer/parseInt "42")))))

(deftest test-roundtrip-constructor
  ;; new Foo() → Foo.() → roundtrips back to (Foo.)
  (is (= '(HashMap. 16) (roundtrip '(new HashMap 16)))))

;; ---------------------------------------------------------------------------
;; Destructuring
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-destructuring
  (is (= '(let [{:keys [a b]} m] (+ a b))
         (roundtrip '(let [{:keys [a b]} m] (+ a b)))))
  (is (= '(let [[x y] pair] (+ x y))
         (roundtrip '(let [[x y] pair] (+ x y)))))
  (is (= '(defn f [{:keys [x y]}] (+ x y))
         (roundtrip '(defn f [{:keys [x y]}] (+ x y))))))

;; ---------------------------------------------------------------------------
;; Nested
;; ---------------------------------------------------------------------------

(deftest test-roundtrip-nested
  (is (= '(let [x 1] (if (> x 0) (+ x 1) (- x 1)))
         (roundtrip '(let [x 1] (if (> x 0) (+ x 1) (- x 1))))))
  (is (= '(do (println "a") (let [x 1] x))
         (roundtrip '(do (println "a") (let [x 1] x))))))

;; ---------------------------------------------------------------------------
;; clj-roundtrip (JVM only)
;; ---------------------------------------------------------------------------

#?(:clj
   (deftest test-clj-roundtrip-simple
     (is (= "(+ 1 2)" (clj-roundtrip "(+ 1 2)")))
     (is (= "(defn f [x] (+ x 1))"
            (clj-roundtrip "(defn f [x] (+ x 1))")))
     (is (= "(def x 42)" (clj-roundtrip "(def x 42)")))))

#?(:clj
   (deftest test-clj-roundtrip-reader-conditional
     (let [src "#?(:clj 1 :cljs 2)"
           result (clj-roundtrip src)]
       (is (re-find #"#\?" result)))))

;; ---------------------------------------------------------------------------
;; sup->clj / clj->sup text-to-text
;; ---------------------------------------------------------------------------

(deftest test-sup->clj
  (is (= "(+ 1 2)"
         (core/sup->clj "1 + 2")))
  (is (= "(defn f [x] (+ x 1))"
         (core/sup->clj "defn f [x]:\n  x + 1\nend"))))

#?(:clj
   (deftest test-clj->sup
     (is (= "1 + 2"
            (core/clj->sup "(+ 1 2)")))
     (is (= "defn f [x]:\n  x + 1\nend"
            (core/clj->sup "(defn f [x] (+ x 1))")))))
