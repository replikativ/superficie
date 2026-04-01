(ns superficie.parse-test
  "Tests for superficie.core/sup->forms — parsing .sup syntax to Clojure forms."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.core :as core]))

(defn- parse1
  "Parse a single-form sup string, return the form."
  [s]
  (first (core/sup->forms s)))

;; ---------------------------------------------------------------------------
;; Atoms
;; ---------------------------------------------------------------------------

(deftest test-atoms
  (testing "numbers"
    (is (= 42    (parse1 "42")))
    (is (= 3.14  (parse1 "3.14")))
    (is (= -1    (parse1 "-1"))))
  (testing "strings"
    (is (= "hello" (parse1 "\"hello\""))))
  (testing "keywords"
    (is (= :foo     (parse1 ":foo")))
    (is (= :foo/bar (parse1 ":foo/bar"))))
  (testing "symbols"
    (is (= 'my-func (parse1 "my-func")))
    (is (= 'even?   (parse1 "even?")))
    (is (= 'foo/bar (parse1 "foo/bar")))
    (is (= '+       (parse1 "+"))))
  (testing "nil and booleans"
    (is (nil?      (parse1 "nil")))
    (is (= true    (parse1 "true")))
    (is (= false   (parse1 "false")))))

;; ---------------------------------------------------------------------------
;; Collections
;; ---------------------------------------------------------------------------

(deftest test-collections
  (is (= [1 2 3]        (parse1 "[1 2 3]")))
  (is (= [1 2 3]        (parse1 "[1, 2, 3]")))   ; commas as whitespace
  (is (= {:a 1 :b 2}   (parse1 "{:a 1 :b 2}")))
  (is (= #{1 2 3}       (parse1 "#{1 2 3}")))
  (is (= []             (parse1 "[]")))
  (is (= {}             (parse1 "{}"))))

;; ---------------------------------------------------------------------------
;; Infix operators
;; ---------------------------------------------------------------------------

(deftest test-infix-arithmetic
  (testing "basic"
    (is (= '(+ 1 2)   (parse1 "1 + 2")))
    (is (= '(- a b)   (parse1 "a - b")))
    (is (= '(* x y)   (parse1 "x * y"))))
  (testing "precedence: * tighter than +"
    (is (= '(+ (* a b) c) (parse1 "a * b + c")))
    (is (= '(+ a (* b c)) (parse1 "a + b * c"))))
  (testing "comparison"
    (is (= '(> x 0)   (parse1 "x > 0")))
    (is (= '(<= a b)  (parse1 "a <= b"))))
  (testing "logical"
    (is (= '(and a b) (parse1 "a and b")))
    (is (= '(or a b)  (parse1 "a or b")))))

;; ---------------------------------------------------------------------------
;; Calls
;; ---------------------------------------------------------------------------

(deftest test-function-call
  (is (= '(println "hello") (parse1 "println(\"hello\")")))
  (is (= '(map inc [1 2 3]) (parse1 "map(inc [1 2 3])")))
  (is (= '(map inc [1 2 3]) (parse1 "map(inc, [1, 2, 3])"))) ; commas ok
  (is (= '(f)               (parse1 "f()"))))

;; ---------------------------------------------------------------------------
;; Interop
;; ---------------------------------------------------------------------------

(deftest test-interop
  (testing "method call postfix"
    (is (= '(.toLowerCase s)          (parse1 "s.toLowerCase()")))
    (is (= '(.getBytes s "UTF-8")     (parse1 "s.getBytes(\"UTF-8\")")))
    (is (= '(.toUpperCase (.trim s))  (parse1 "s.trim().toUpperCase()"))))
  (testing "field access"
    (is (= '(.-x point) (parse1 "point.-x"))))
  (testing "constructor"
    (is (= '(HashMap.)         (parse1 "new HashMap()")))
    (is (= '(StringBuilder. "x") (parse1 "new StringBuilder(\"x\")"))))
  (testing "static"
    (is (= '(Math/abs -1)  (parse1 "Math/abs(-1)")))
    (is (= 'Math/PI        (parse1 "Math/PI")))))

;; ---------------------------------------------------------------------------
;; Block forms
;; ---------------------------------------------------------------------------

(deftest test-def
  (is (= '(def x 42)          (parse1 "def x: 42")))
  (is (= '(def max-retries 3) (parse1 "def max-retries: 3")))
  ;; call syntax still works as a fallback
  (is (= '(def x 42)          (parse1 "def(x 42)"))))

(deftest test-defn
  (is (= '(defn greet [name] (println name))
         (parse1 "defn greet [name]:\n  println(name)\nend")))
  (is (= '(defn add [a b] (+ a b))
         (parse1 "defn add [a b]:\n  a + b\nend"))))

(deftest test-defn-docstring
  (is (= '(defn greet "Greets someone" [name] (println name))
         (parse1 "defn greet \"Greets someone\" [name]:\n  println(name)\nend"))))

(deftest test-if
  (is (= '(if (> x 0) "positive" "negative")
         (parse1 "if x > 0 :\n  \"positive\"\nelse:\n  \"negative\"\nend")))
  (testing "if without else"
    (is (= '(if (> x 0) "positive")
           (parse1 "if x > 0 :\n  \"positive\"\nend")))))

(deftest test-let
  (is (= '(let [x 1 y 2] (+ x y))
         (parse1 "let [x 1 y 2]:\n  x + y\nend"))))

(deftest test-when
  (is (= '(when (> x 0) (println "pos"))
         (parse1 "when x > 0 :\n  println(\"pos\")\nend"))))

(deftest test-fn
  (is (= '(fn [x] (+ x 1))
         (parse1 "fn [x]:\n  x + 1\nend"))))

(deftest test-cond
  (is (= '(cond (< x 0) :negative (= x 0) :zero :else :positive)
         (parse1 "cond:\n  x < 0 => :negative\n  x = 0 => :zero\n  :else => :positive\nend"))))

(deftest test-case
  (is (= '(case x 1 "one" 2 "two" "default")
         (parse1 "case x :\n  1 => \"one\"\n  2 => \"two\"\n  => \"default\"\nend"))))

(deftest test-for
  (is (= '(for [x xs y ys] [x y])
         (parse1 "for [x xs y ys]:\n  [x y]\nend")))
  (is (= '(for [x xs :when (> x 0)] x)
         (parse1 "for [x xs :when x > 0]:\n  x\nend"))))

(deftest test-try
  (is (= '(try (println "hi") (catch Exception e (println e)))
         (parse1 "try:\n  println(\"hi\")\ncatch [Exception e]:\n  println(e)\nend"))))

(deftest test-threading
  (is (= '(->> data (filter even?) (map inc))
         (parse1 "data |> filter(even?) |> map(inc)")))
  (is (= '(-> account (update :balance (* 1.05)))
         (parse1 "account .> update(:balance *(1.05))"))))

;; ---------------------------------------------------------------------------
;; Metadata and reader macros
;; ---------------------------------------------------------------------------

(deftest test-metadata
  (testing "keyword metadata"
    (let [r (parse1 "^:private x")]
      (is (= 'x r))
      (is (= true (:private (meta r))))))
  (testing "type hint"
    (let [r (parse1 "^String x")]
      (is (= 'x r))
      (is (= 'String (:tag (meta r))))))
  (testing "map metadata"
    (let [r (parse1 "^{:doc \"hello\"} x")]
      (is (= "hello" (:doc (meta r)))))))

(deftest test-quote-deref-var
  (is (= '(quote foo)          (parse1 "'foo")))
  (is (= '(clojure.core/deref state) (parse1 "@state")))
  (is (= '(var foo)            (parse1 "#'foo"))))

;; ---------------------------------------------------------------------------
;; Reader conditionals and tagged literals
;; ---------------------------------------------------------------------------

#?(:clj
(deftest test-reader-conditional
  (testing "reader conditional preserved with :read-cond :preserve"
    (let [forms (core/sup->forms "#?(:clj 1 :cljs 2)" {:read-cond :preserve})]
      (is (= 1 (count forms)))
      (is (reader-conditional? (first forms)))))
  (testing "reader conditional evaluated for current platform"
    (let [forms (core/sup->forms "#?(:clj :jvm :cljs :js)")]
      (is (= 1 (count forms)))
      (is (= :jvm (first forms)))))))

#?(:clj
(deftest test-tagged-literal
  (let [forms (core/sup->forms "#inst \"2024-01-01\"")]
    (is (= 1 (count forms))))))

;; ---------------------------------------------------------------------------
;; Symbols
;; ---------------------------------------------------------------------------

(deftest test-symbols
  (testing "dotted Java class names"
    (is (= 'java.util.HashMap (parse1 "java.util.HashMap"))))
  (testing "namespace-qualified"
    (is (= 'java.util.UUID/nameUUIDFromBytes
           (parse1 "java.util.UUID/nameUUIDFromBytes"))))
  (testing "special chars"
    (is (= 'int96->epoch-millis (parse1 "int96->epoch-millis")))
    (is (= 'some->>             (parse1 "some->>")))
    (is (= '<!!                 (parse1 "<!!"))))
  (testing "operator symbols as values"
    (is (= '-> (parse1 "->")))
    (is (= '>= (parse1 ">=")))))

;; ---------------------------------------------------------------------------
;; defmulti / defmethod
;; ---------------------------------------------------------------------------

(deftest test-defmulti
  (is (= '(defmulti area :shape)
         (parse1 "defmulti area: :shape")))
  (is (= '(defmulti process-event :type)
         (parse1 "defmulti process-event: :type"))))

(deftest test-defmethod
  (is (= '(defmethod area :circle [shape] Math/PI)
         (parse1 "defmethod area :circle [shape]:\n  Math/PI\nend")))
  (is (= '(defmethod area :default [shape] 0)
         (parse1 "defmethod area :default [shape]:\n  0\nend"))))

;; ---------------------------------------------------------------------------
;; defprotocol
;; ---------------------------------------------------------------------------

(deftest test-defprotocol
  (is (= '(defprotocol Drawable (draw [this]) (area [this]))
         (parse1 "defprotocol Drawable:\n  draw [this]\n  area [this]\nend")))
  (testing "with docstring on protocol"
    (is (= '(defprotocol Shape "A geometric shape" (area [this]))
           (parse1 "defprotocol Shape \"A geometric shape\":\n  area [this]\nend"))))
  (testing "multi-arity method signature"
    (is (= '(defprotocol Resizable (resize [this factor] [this w h]))
           (parse1 "defprotocol Resizable:\n  resize [this factor] [this w h]\nend"))))
  (testing "method with docstring"
    (is (= '(defprotocol P (f [this] "doc"))
           (parse1 "defprotocol P:\n  f [this] \"doc\"\nend")))))

;; ---------------------------------------------------------------------------
;; defrecord / deftype
;; ---------------------------------------------------------------------------

(deftest test-defrecord
  (is (= '(defrecord Point [x y] Drawable (draw [this] "pt") (area [this] 0.0))
         (parse1 "defrecord Point [x y]:\n  Drawable\n  draw [this]:\n    \"pt\"\n  end\n  area [this]:\n    0.0\n  end\nend")))
  (testing "no interfaces"
    (is (= '(defrecord Empty [])
           (parse1 "defrecord Empty []:\nend")))))

(deftest test-deftype
  (is (= '(deftype MyType [x] MyProto (doIt [this] x))
         (parse1 "deftype MyType [x]:\n  MyProto\n  doIt [this]:\n    x\n  end\nend"))))

;; ---------------------------------------------------------------------------
;; reify / proxy
;; ---------------------------------------------------------------------------

(deftest test-reify
  (is (= '(reify Runnable (run [this] (println "hi")))
         (parse1 "reify:\n  Runnable\n  run [this]:\n    println(\"hi\")\n  end\nend"))))

(deftest test-proxy
  (is (= '(proxy [java.io.InputStream] [] (read [] -1))
         (parse1 "proxy [java.io.InputStream] []:\n  read []:\n    -1\n  end\nend"))))

;; ---------------------------------------------------------------------------
;; Multiple forms
;; ---------------------------------------------------------------------------

(deftest test-multiple-forms
  (is (= ['(def x 42) '(println x)]
         (core/sup->forms "def x: 42\nprintln(x)"))))
