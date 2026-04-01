(ns superficie.render-test
  "Tests for superficie.core/forms->sup and superficie.emit.printer/print-form."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.core :as core]
            [superficie.emit.printer :as p]))

;; ---------------------------------------------------------------------------
;; Atoms
;; ---------------------------------------------------------------------------

(deftest test-atoms
  (is (= "42"        (p/print-form 42)))
  (is (= "3.14"      (p/print-form 3.14)))
  (is (= "\"hello\"" (p/print-form "hello")))
  (is (= ":foo"      (p/print-form :foo)))
  (is (= ":foo/bar"  (p/print-form :foo/bar)))
  (is (= "nil"       (p/print-form nil)))
  (is (= "true"      (p/print-form true)))
  (is (= "false"     (p/print-form false)))
  (is (= "my-sym"    (p/print-form 'my-sym))))

;; ---------------------------------------------------------------------------
;; Collections
;; ---------------------------------------------------------------------------

(deftest test-collections
  (is (= "[1 2 3]"           (p/print-form [1 2 3])))
  (is (= "{:a 1}"            (p/print-form {:a 1})))
  (is (str/starts-with?      (p/print-form #{1 2 3}) "#{"))
  (is (= "[]"                (p/print-form [])))
  (is (= "{}"                (p/print-form {}))))

;; ---------------------------------------------------------------------------
;; Infix operators
;; ---------------------------------------------------------------------------

(deftest test-infix
  (is (= "1 + 2"       (p/print-form '(+ 1 2))))
  (is (= "1 + 2 + 3"   (p/print-form '(+ 1 2 3))))
  (is (= "a * b"       (p/print-form '(* a b))))
  (is (= "x > 0"       (p/print-form '(> x 0))))
  (is (= "a and b"     (p/print-form '(and a b))))
  (is (= "a or b"      (p/print-form '(or a b)))))

;; ---------------------------------------------------------------------------
;; Calls
;; ---------------------------------------------------------------------------

(deftest test-function-call
  (is (= "println(\"hello\" \"world\")" (p/print-form '(println "hello" "world"))))
  (is (= "map(inc [1 2 3])"             (p/print-form '(map inc [1 2 3]))))
  (is (= "f()"                          (p/print-form '(f)))))

;; ---------------------------------------------------------------------------
;; Blocks
;; ---------------------------------------------------------------------------

(deftest test-def
  (is (= "def x: 42"    (p/print-form '(def x 42))))
  (is (= "defonce y: 0" (p/print-form '(defonce y 0))))
  (is (= "defmulti area: :shape" (p/print-form '(defmulti area :shape))))
  (testing "defmulti complex form falls back to call syntax"
    (is (= "defmulti(foo :dispatch :default :other)"
           (p/print-form '(defmulti foo :dispatch :default :other))))))

(deftest test-defmethod
  (is (= "defmethod area :circle [shape]:\n  Math/PI\nend"
         (p/print-form '(defmethod area :circle [shape] Math/PI)))))

(deftest test-defprotocol
  (is (= "defprotocol Drawable:\n  draw [this]\n  area [this]\nend"
         (p/print-form '(defprotocol Drawable (draw [this]) (area [this])))))
  (testing "with docstring"
    (is (= "defprotocol Shape \"A shape\":\n  area [this]\nend"
           (p/print-form '(defprotocol Shape "A shape" (area [this]))))))
  (testing "multi-arity method signature"
    (is (= "defprotocol Resizable:\n  resize [this factor] [this w h]\nend"
           (p/print-form '(defprotocol Resizable (resize [this factor] [this w h])))))))

(deftest test-defrecord
  (is (= "defrecord Point [x y]:\n  Drawable\n  draw [this]:\n    \"pt\"\n  end\n  area [this]:\n    0.0\n  end\nend"
         (p/print-form '(defrecord Point [x y] Drawable (draw [this] "pt") (area [this] 0.0))))))

(deftest test-reify
  (is (= "reify:\n  Runnable\n  run [this]:\n    println(\"hi\")\n  end\nend"
         (p/print-form '(reify Runnable (run [this] (println "hi")))))))

(deftest test-proxy
  (is (= "proxy [java.io.InputStream] []:\n  read []:\n    -1\n  end\nend"
         (p/print-form '(proxy [java.io.InputStream] [] (read [] -1))))))

(deftest test-defn
  (is (= "defn greet [name]:\n  str(\"Hello\" name)\nend"
         (p/print-form '(defn greet [name] (str "Hello" name)))))
  (testing "with docstring"
    (is (= "defn greet \"Greets a person\" [name]:\n  println(name)\nend"
           (p/print-form '(defn greet "Greets a person" [name] (println name)))))))

(deftest test-fn
  (is (= "fn [x]:\n  x + 1\nend"
         (p/print-form '(fn [x] (+ x 1))))))

(deftest test-if
  (is (= "if x > 0 :\n  \"pos\"\nelse:\n  \"neg\"\nend"
         (p/print-form '(if (> x 0) "pos" "neg"))))
  (testing "without else"
    (is (= "if x > 0 :\n  \"pos\"\nend"
           (p/print-form '(if (> x 0) "pos"))))))

(deftest test-let
  (is (= "let [x 1]:\n  x + 2\nend"
         (p/print-form '(let [x 1] (+ x 2))))))

(deftest test-when
  (is (= "when x > 0 :\n  println(\"pos\")\nend"
         (p/print-form '(when (> x 0) (println "pos"))))))

(deftest test-case
  (is (= "case x :\n  1 => \"one\"\n  2 => \"two\"\n  => \"default\"\nend"
         (p/print-form '(case x 1 "one" 2 "two" "default")))))

(deftest test-try
  (is (= "try:\n  risky()\ncatch [Exception e]:\n  handle(e)\nend"
         (p/print-form '(try (risky) (catch Exception e (handle e)))))))

;; ---------------------------------------------------------------------------
;; Threading
;; ---------------------------------------------------------------------------

(deftest test-threading
  (is (= "accounts |> filter(:active) |> map(:balance) |> reduce(+)"
         (p/print-form '(->> accounts (filter :active) (map :balance) (reduce +)))))
  (is (= "account .> update(:balance *(1.05))"
         (p/print-form '(-> account (update :balance (* 1.05)))))))

;; ---------------------------------------------------------------------------
;; Interop
;; ---------------------------------------------------------------------------

(deftest test-interop
  (is (= "\"hello\".toUpperCase()"     (p/print-form '(.toUpperCase "hello"))))
  (is (= "\"hello\".replace(\"l\" \"r\")" (p/print-form '(.replace "hello" "l" "r"))))
  (is (= "point.-x"                    (p/print-form '(.-x point))))
  (is (= "Math/abs(-1)"               (p/print-form '(Math/abs -1))))
  (is (= "new java.util.Date()"       (p/print-form '(java.util.Date.))))
  (is (= "new StringBuilder(\"init\")" (p/print-form '(StringBuilder. "init")))))

;; ---------------------------------------------------------------------------
;; Reader macros (with sugar metadata)
;; ---------------------------------------------------------------------------

(deftest test-reader-macros
  (testing "deref with sugar"
    (is (= "@state" (p/print-form (with-meta '(clojure.core/deref state) {:sup/sugar true})))))
  (testing "quote with sugar"
    (is (= "'foo"   (p/print-form (with-meta '(quote foo) {:sup/sugar true})))))
  (testing "var with sugar"
    (is (= "#'foo"  (p/print-form (with-meta '(var foo) {:sup/sugar true}))))))

;; ---------------------------------------------------------------------------
;; Metadata
;; ---------------------------------------------------------------------------

(deftest test-metadata
  (is (= "^:private x"    (p/print-form (with-meta 'x {:private true}))))
  (is (= "^String x"      (p/print-form (with-meta 'x {:tag 'String}))))
  (is (= "^:dynamic *x*"  (p/print-form (with-meta '*x* {:dynamic true})))))

;; ---------------------------------------------------------------------------
;; clj->sup and forms->sup via core API
;; ---------------------------------------------------------------------------

(deftest test-clj->sup
  (is (= "defn f [x]:\n  x + 1\nend"
         (core/clj->sup "(defn f [x] (+ x 1))")))
  (testing "multiple forms"
    (let [result (core/clj->sup "(def a 1)\n(def b 2)")]
      (is (str/includes? result "def a: 1"))
      (is (str/includes? result "def b: 2")))))

(deftest test-forms->sup
  (is (= "def x: 42\n\nprintln(x)"
         (core/forms->sup ['(def x 42) '(println x)]))))

;; ---------------------------------------------------------------------------
;; Reader conditionals
;; ---------------------------------------------------------------------------

#?(:clj
(deftest test-reader-conditional-in-output
  (let [result (core/clj->sup "#?(:clj (def x 1) :cljs (def x 2))")]
    (is (re-find #"#\?" result)))))
