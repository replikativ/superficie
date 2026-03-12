(ns superficie.parse-test
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.parse :as parse]
            [superficie.opaque :as opaque]))

(deftest test-atoms
  (testing "numbers"
    (is (= 42 (parse/parse-string "42")))
    (is (= 3.14 (parse/parse-string "3.14")))
    (is (= -1 (parse/parse-string "-1")))
    (is (= 0xFF (parse/parse-string "0xFF"))))
  (testing "strings"
    (is (= "hello" (parse/parse-string "\"hello\""))))
  (testing "keywords"
    (is (= :foo (parse/parse-string ":foo")))
    (is (= :foo/bar (parse/parse-string ":foo/bar"))))
  (testing "symbols"
    (is (= 'my-func (parse/parse-string "my-func")))
    (is (= 'even? (parse/parse-string "even?")))
    (is (= 'foo/bar (parse/parse-string "foo/bar")))
    (is (= '+ (parse/parse-string "+")))
    (is (= 'str (parse/parse-string "str"))))
  (testing "nil and booleans"
    (is (nil? (parse/parse-string "nil")))
    (is (= true (parse/parse-string "true")))
    (is (= false (parse/parse-string "false")))))

(deftest test-collections
  (is (= [1 2 3] (parse/parse-string "[1, 2, 3]")))
  (is (= {:a 1 :b 2} (parse/parse-string "{:a 1, :b 2}")))
  (is (= #{1 2 3} (parse/parse-string "#{1, 2, 3}")))
  (is (= [] (parse/parse-string "[]")))
  (is (= {} (parse/parse-string "{}"))))

(deftest test-infix-arithmetic
  (testing "basic"
    (is (= '(+ 1 2) (parse/parse-string "1 + 2")))
    (is (= '(- a b) (parse/parse-string "a - b")))
    (is (= '(* x y) (parse/parse-string "x * y"))))
  (testing "precedence: * binds tighter than +"
    (is (= '(+ (* a b) c) (parse/parse-string "a * b + c")))
    (is (= '(+ a (* b c)) (parse/parse-string "a + b * c"))))
  (testing "comparison"
    (is (= '(> x 0) (parse/parse-string "x > 0")))
    (is (= '(<= a b) (parse/parse-string "a <= b"))))
  (testing "logical"
    (is (= '(and a b) (parse/parse-string "a and b")))
    (is (= '(or a b) (parse/parse-string "a or b")))))

(deftest test-unary
  (is (= '(not x) (parse/parse-string "not x")))
  (is (= '(- x) (parse/parse-string "-x")))
  (is (= '(deref atom) (parse/parse-string "@atom")))
  (is (= '(throw ex) (parse/parse-string "throw ex"))))

(deftest test-function-call
  (is (= '(println "hello") (parse/parse-string "println(\"hello\")")))
  (is (= '(map inc [1 2 3]) (parse/parse-string "map(inc, [1, 2, 3])")))
  (is (= '(f) (parse/parse-string "f()"))))

(deftest test-method-call
  (is (= '(.toLowerCase s) (parse/parse-string "s.toLowerCase()")))
  (is (= '(.getBytes s "UTF-8") (parse/parse-string "s.getBytes(\"UTF-8\")"))))

(deftest test-def
  (is (= '(def x 42) (parse/parse-string "def x := 42")))
  (is (= '(def max-retries 3) (parse/parse-string "def max-retries := 3"))))

(deftest test-defn
  (is (= '(defn greet [name] (println name))
         (parse/parse-string
          "defn greet(name):
             println(name)
           end")))
  (is (= '(defn add [a b] (+ a b))
         (parse/parse-string
          "defn add(a, b):
             a + b
           end"))))

(deftest test-if
  (is (= '(if (> x 0) "positive" "negative")
         (parse/parse-string
          "if x > 0:
             \"positive\"
           else:
             \"negative\"
           end")))
  (testing "if without else"
    (is (= '(if (> x 0) "positive")
           (parse/parse-string
            "if x > 0:
               \"positive\"
             end")))))

(deftest test-let
  (is (= '(let [x 1 y 2] (+ x y))
         (parse/parse-string
          "let x := 1, y := 2:
             x + y
           end"))))

(deftest test-cond
  (is (= '(cond (< x 0) :negative (= x 0) :zero :else :positive)
         (parse/parse-string
          "cond:
             x < 0 => :negative
             x = 0 => :zero
             :else => :positive
           end"))))

(deftest test-threading
  (is (= '(->> data (filter even?) (map inc))
         (parse/parse-string
          "data
             |> filter(even?)
             |> map(inc)"))))

(deftest test-try
  (is (= '(try (println "hi") (catch Exception e (println e)))
         (parse/parse-string
          "try:
             println(\"hi\")
           catch Exception e:
             println(e)
           end"))))

(deftest test-anon-fn
  (is (= '(fn [x] (+ x 1))
         (parse/parse-string
          "fn(x):
             x + 1
           end"))))

(deftest test-do
  (is (= '(do (println "a") (println "b"))
         (parse/parse-string
          "do:
             println(\"a\")
             println(\"b\")
           end"))))

(deftest test-metadata
  (testing "keyword metadata on symbol"
    (let [r (parse/parse-string "^:private x")]
      (is (= 'x r))
      (is (= true (:private (meta r))))))
  (testing "type hint metadata"
    (let [r (parse/parse-string "^String x")]
      (is (= 'x r))
      (is (= 'String (:tag (meta r))))))
  (testing "map metadata"
    (let [r (parse/parse-string "^{:doc \"hello\"} x")]
      (is (= 'x r))
      (is (= "hello" (:doc (meta r))))))
  (testing "metadata in def"
    (let [r (parse/parse-string "def ^:private x := 42")]
      (is (= 'def (first r)))
      (is (= true (:private (meta (second r)))))))
  (testing "metadata in defn"
    (let [r (parse/parse-string "defn ^:private foo(x):\n  x\nend")]
      (is (= 'defn (first r)))
      (is (= true (:private (meta (second r))))))))

(deftest test-defmacro
  (is (= '(defmacro unless [pred & body] pred)
         (parse/parse-string
          "defmacro unless(pred, & body):
             pred
           end"))))

(deftest test-condp
  (is (= '(condp = x 1 :one 2 :two :other)
         (parse/parse-string
          "condp = x:
             1 => :one
             2 => :two
             else => :other
           end"))))

(deftest test-multi-arity-defn
  (is (= '(defn greet ([] (greet "World")) ([name] (str "Hello, " name)))
         (parse/parse-string
          "defn greet
             ():
               greet(\"World\")
             (name):
               str(\"Hello, \", name)
           end"))))

(deftest test-broad-symbols
  (testing "namespace-qualified symbols"
    (is (= 'foo/bar (parse/parse-string "foo/bar")))
    (is (= 'str/blank? (parse/parse-string "str/blank?"))))
  (testing "dotted Java class names"
    (is (= 'java.util.HashMap (parse/parse-string "java.util.HashMap"))))
  (testing "dotted namespace-qualified symbols"
    (is (= 'java.util.UUID/nameUUIDFromBytes
           (parse/parse-string "java.util.UUID/nameUUIDFromBytes")))
    (is (= 'clojure.core.async/<!!
           (parse/parse-string "clojure.core.async/<!!"))))
  (testing "symbols with special chars"
    (is (= 'int96->epoch-millis (parse/parse-string "int96->epoch-millis")))
    (is (= 'some->> (parse/parse-string "some->>")))
    (is (= 'cond-> (parse/parse-string "cond->")))
    (is (= 'as-> (parse/parse-string "as->")))
    (is (= 'a/<! (parse/parse-string "a/<!"))))
  (testing "operator symbols as values"
    (is (= '+ (parse/parse-string "+")))
    (is (= '-> (parse/parse-string "->")))
    (is (= '->> (parse/parse-string "->>")))
    (is (= '>= (parse/parse-string ">="))))
  (testing "core.async operator symbols"
    (is (= '<! (parse/parse-string "<!")))
    (is (= '>! (parse/parse-string ">!")))
    (is (= '<!! (parse/parse-string "<!!")))
    (is (= '>!! (parse/parse-string ">!!")))))

(deftest test-opaque-forms
  (testing "top-level opaque reader forms parse as preserved islands"
    (let [conditional (parse/parse-string "#?(:clj 1 :cljs 2)")
          tagged (parse/parse-string "#inst \"2020-01-01\"")
          syntax-quoted (parse/parse-string "`(foo ~bar ~@baz)")]
      (is (opaque/opaque-form? conditional))
      (is (= "#?(:clj 1 :cljs 2)" (parse/emit-form conditional)))
      (is (opaque/opaque-form? tagged))
      (is (= "#inst \"2020-01-01\"" (parse/emit-form tagged)))
      (is (opaque/opaque-form? syntax-quoted))
      (is (= "`(foo ~bar ~@baz)" (parse/emit-form syntax-quoted)))))
  (testing "opaque forms can live inside ordinary surface blocks"
    (is (= "(defn f [x] #?(:clj x :cljs x))"
           (parse/emit-source
            (parse/parse-string
             "defn f(x):\n  #?(:clj x :cljs x)\nend"))))
    (is (= "(def x #inst \"2020-01-01\")"
           (parse/emit-source
            (parse/parse-string
             "def x := #inst \"2020-01-01\"")))))
  (testing "comment forms can be preserved as raw islands in bodies"
    (is (= "(defn f [x] (comment x) x)"
           (parse/emit-source
            (parse/parse-string
             "defn f(x):\n  (comment x)\n  x\nend"))))))
