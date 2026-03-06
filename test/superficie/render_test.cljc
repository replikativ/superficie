(ns superficie.render-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.render :as render]
            [superficie.resolve :as resolve]))

(deftest test-atoms
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "42" (render/render-form ctx 42)))
    (is (= "\"hello\"" (render/render-form ctx "hello")))
    (is (= ":foo" (render/render-form ctx :foo)))
    (is (= "nil" (render/render-form ctx nil)))
    (is (= "true" (render/render-form ctx true)))
    (is (= "my-sym" (render/render-form ctx 'my-sym)))))

(deftest test-collections
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "[1, 2, 3]" (render/render-form ctx [1 2 3])))
    (is (= "{:a 1, :b 2}" (render/render-form ctx {:a 1 :b 2})))
    (is (= "#{}" (render/render-form ctx #{})))))

(deftest test-infix
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "1 + 2" (render/render-form ctx '(+ 1 2))))
    (is (= "1 + 2 + 3" (render/render-form ctx '(+ 1 2 3))))
    (is (= "a * b" (render/render-form ctx '(* a b))))
    (is (= "x > 0" (render/render-form ctx '(> x 0))))
    (is (= "-x" (render/render-form ctx '(- x))))))

(deftest test-function-call
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "println(\"hello\", \"world\")"
           (render/render-form ctx '(println "hello" "world"))))
    (is (= "map(inc, [1, 2, 3])"
           (render/render-form ctx '(map inc [1 2 3]))))))

(deftest test-defn
  (is (= "defn greet(name):\n  println(name)\nend"
         (render/render-form
          {:refers {} :aliases {} :excludes #{}}
          '(defn greet [name] (println name))))))

(deftest test-if
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "if x > 0:\n  \"positive\"\nelse:\n  \"negative\"\nend"
           (render/render-form ctx '(if (> x 0) "positive" "negative"))))))

(deftest test-let
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "let x := 1, y := 2:\n  x + y\nend"
           (render/render-form ctx '(let [x 1 y 2] (+ x y)))))))

(deftest test-threading
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "data\n  |> filter(even?)\n  |> map(inc)"
           (render/render-form ctx '(->> data (filter even?) (map inc)))))))

(deftest test-interop
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (is (= "s.toLowerCase()"
           (render/render-form ctx '(.toLowerCase s))))
    (is (= "Math/pow(2, 10)"
           (render/render-form ctx '(Math/pow 2 10))))))

(deftest test-ns-resolution
  (testing "excluded core forms are not rewritten"
    (let [ctx {:refers {} :aliases {} :excludes #{'+ '-}}]
      ;; + is excluded from core, so render as regular call
      (is (= "+(1, 2)" (render/render-form ctx '(+ 1 2)))))))

(deftest test-precedence-parens
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (testing "lower-precedence sub-expr gets parens"
      (is (= "(a + b) * c" (render/render-form ctx '(* (+ a b) c))))
      (is (= "a * (b + c)" (render/render-form ctx '(* a (+ b c)))))
      (is (= "(a + b) * (c - d)" (render/render-form ctx '(* (+ a b) (- c d))))))
    (testing "same or higher precedence — no parens needed"
      (is (= "a * b + c" (render/render-form ctx '(+ (* a b) c))))
      (is (= "a + b * c" (render/render-form ctx '(+ a (* b c))))))
    (testing "comparison with arithmetic"
      (is (= "a + b > c" (render/render-form ctx '(> (+ a b) c))))
      (is (= "a > b + c" (render/render-form ctx '(> a (+ b c))))))
    (testing "logical with comparison"
      (is (= "a > 0 and b > 0" (render/render-form ctx '(and (> a 0) (> b 0)))))
      (is (= "(a or b) and c" (render/render-form ctx '(and (or a b) c)))))
    (testing "not with lower-precedence arg wraps in parens"
      (is (= "not (a or b)" (render/render-form ctx '(not (or a b))))))
    (testing "deeply nested"
      (is (= "(a + b) * (c + d)" (render/render-form ctx '(* (+ a b) (+ c d))))))))

(deftest test-full-render
  (let [source "(ns myapp.core
  (:require [clojure.string :as str]))

(defn greet [name]
  (str \"Hello, \" name))

(defn process [data]
  (->> data
       (filter even?)
       (map inc)
       (reduce +)))"]
    (is (string? (render/render-string source)))))

(deftest test-discard-forms
  (testing "#_ top-level forms are skipped"
    (is (not (str/includes? (render/render-string "#_ (def unused 0)\n(def x 42)")
                            "unused")))
    (is (str/includes? (render/render-string "#_ (def unused 0)\n(def x 42)")
                       "def x := 42"))
    (is (not (str/includes? (render/render-string "(def x 1)\n\n#_(def skip 99)\n\n(def y 2)")
                            "skip"))))
  (testing "#_ only form produces empty output"
    (is (= "" (render/render-string "#_(foo bar)")))))

(deftest test-render-string-multiple-forms
  (testing "multiple top-level forms"
    (let [result (render/render-string "(def a 1)\n\n(def b 2)\n\n(defn f [x] x)")]
      (is (str/includes? result "def a := 1"))
      (is (str/includes? result "def b := 2"))
      (is (str/includes? result "defn f(x):")))))

(deftest test-reader-conditionals
  (testing "top-level #? preserved verbatim"
    (is (= "#?(:clj (def x 1) :cljs (def x 2))"
           (render/render-string "#?(:clj (def x 1) :cljs (def x 2))"))))
  (testing "form containing #? falls back to sexp passthrough"
    (let [result (render/render-string "(defn foo [s]\n  #?(:clj (Integer/parseInt s)\n     :cljs (js/parseInt s 10)))")]
      (is (str/includes? result "#?(:clj"))
      (is (str/includes? result ":cljs"))
      (is (not (str/includes? result "read-string")))))
  (testing "pure forms alongside #? still render as superficie"
    (let [result (render/render-string "(defn pure [x] (inc x))\n\n(defn mixed [s]\n  #?(:clj (.toUpperCase s)\n     :cljs (.toUpperCase s)))")]
      (is (str/includes? result "defn pure(x):"))
      (is (str/includes? result "#?(:clj")))))

;; let-flattening: tail lets rendered as statements, not blocks
(deftest test-let-flattening
  (let [ctx {:refers {} :aliases {} :excludes #{}}]
    (testing "tail let in defn is flattened"
      (let [result (render/render-form ctx '(defn foo [x] (let [y 1] (+ x y))))]
        (is (str/includes? result "let y := 1"))
        (is (not (str/includes? result "end\nend")))
        (is (not (str/includes? result "let y := 1:")))))
    (testing "nested tail lets are merged"
      (let [result (render/render-form ctx '(defn foo [x] (let [a 1] (let [b 2] (+ a b)))))]
        (is (str/includes? result "let a := 1"))
        (is (str/includes? result "let b := 2"))
        (is (not (str/includes? result "let a := 1:")))))
    (testing "non-tail let keeps block syntax"
      (let [result (render/render-form ctx '(defn foo [x] (let [y 1] (use y)) (other)))]
        (is (str/includes? result "let y := 1:"))
        (is (str/includes? result "end\n  other()"))))
    (testing "let in if-else branches is flattened"
      (let [result (render/render-form ctx '(if true (let [x 1] x) (let [y 2] y)))]
        (is (str/includes? result "let x := 1"))
        (is (not (str/includes? result "let x := 1:")))
        (is (str/includes? result "let y := 2"))
        (is (not (str/includes? result "let y := 2:")))))
    (testing "binding/with-open are not flattened"
      (let [result (render/render-form ctx '(defn foo [] (binding [*x* 1] (bar))))]
        (is (str/includes? result "binding *x* := 1:"))
        (is (str/includes? result "end\nend"))))))
