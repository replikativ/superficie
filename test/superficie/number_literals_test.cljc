(ns superficie.number-literals-test
  "Number literals keep their notation, which matters in JS: there 4.0 is the
   number 4, so a renderer printing values would show integer arithmetic."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.core :as core]
            [superficie.forms :as forms]))

(deftest test-floats-print-as-written
  (is (= "f(0.0, 4.0, 1.5, -0.0, 100.0)" (core/clj->sup "(f 0.0 4.0 1.5 -0.0 100.0)")))
  (is (= "4.0 * u - 0.5" (core/clj->sup "(- (* 4.0 u) 0.5)"))))

(deftest test-reader-conditionals-in-clojure-source
  (is (= "#?(:clj f(4.0) :cljs g(1))" (core/clj->sup "#?(:clj (f 4.0) :cljs (g 1))")))
  (is (= "defn h []:\n  #?@(:clj [1 2] :cljs [3])\nend"
         (core/clj->sup "(defn h [] #?@(:clj [1 2] :cljs [3]))"))))

#?(:cljs
   (deftest test-js-number-notation
     (testing "Clojure source: literals JS cannot print back keep their text"
       (is (= "f(1e3, 2.50, 0xFF, 1/2, 2N, 1.5M, 2r101, 017, 12345678901234567890)"
              (core/clj->sup "(f 1e3 2.50 0xFF 1/2 2N 1.5M 2r101 017 12345678901234567890)")))
       (is (= "defmacro m [x]:\n  `*(~x, 4.0)\nend" (core/clj->sup "(defmacro m [x] `(* ~x 4.0))")))
       (testing "and strings, comments, characters and keywords are untouched"
         (is (= "f(\"4.0\", \\a, :1.5, a1.0)" (core/clj->sup "(f \"4.0\" \\a :1.5 a1.0) ; 4.0")))))
     (testing "superficie source: hex, ratio, N and M read as their JS value"
       (let [[[_ & args]] (core/sup->forms "f(4.0, 0xFF, 1/2, 2N, 1.5M, 1.5)")]
         (is (= [4 255 0.5 2 1.5] (map #(if (forms/raw? %) (forms/raw-value %) %) (butlast args))))
         (is (= "f(4.0, 0xFF, 1/2, 2N, 1.5M, 1.5)"
                (core/forms->sup (core/sup->forms "f(4.0, 0xFF, 1/2, 2N, 1.5M, 1.5)"))))))))
