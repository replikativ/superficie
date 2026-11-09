(ns superficie.comments-test
  "Tests for keeping the ; comments of Clojure source in clj->sup."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.core :as core]))

(defn- keeps-forms?
  "The rendering with comments reads back to the forms of the source."
  [clj-src]
  (= (pr-str (core/clj->forms clj-src))
     (pr-str (core/sup->forms (core/clj->sup clj-src)))))

(deftest test-end-of-line-comment
  (testing "a comment at the end of a line goes above the code it ended"
    (let [src "(defn power [n x]                          ; the meaning\n  (if (= n 0) 1 (* x (power (dec n) x))))"]
      (is (= "; the meaning\ndefn power [n x]:\n  if n = 0:\n    1\n  else:\n    x * power(dec(n), x)\n  end\nend"
             (core/clj->sup src)))
      (is (keeps-forms? src))))
  (testing "after a body statement"
    (is (= "defn f [x]:\n  ; say hi\n  println(\"hi\")\n  g(x)\nend"
           (core/clj->sup "(defn f [x]\n  (println \"hi\") ; say hi\n  (g x))"))))
  (testing "after a top-level form"
    (is (= "; one\ndef x: 1\ndef y: 2" (core/clj->sup "(def x 1) ; one\n(def y 2)")))))

(deftest test-comment-lines
  (testing "a comment on its own line goes above the next form"
    (is (= ";; header\ndefn f [x]:\n  ;; lead\n  g(x)\nend\n\n; tail"
           (core/clj->sup ";; header\n(defn f [x]\n  ;; lead\n  (g x))\n\n; tail"))))
  (testing "a comment with nothing after it in its form goes above that form"
    (is (= ";; TODO\ndefn g []:\n  foo()\nend" (core/clj->sup "(defn g []\n  (foo)\n  ;; TODO\n  )")))))

(deftest test-comments-inside-flat-parts
  (testing "comments in parts written on one line go above the form, in order"
    (let [src "(if ; test\n  (pos? x) ; then\n  :yes ; else\n  :no)"]
      (is (= "; test\n; then\n; else\nif pos?(x):\n  :yes\nelse:\n  :no\nend" (core/clj->sup src)))
      (is (keeps-forms? src))))
  (testing "let bindings, infix operands, map entries"
    (let [src "(defn f [x]\n  (let [a 1 ; a is one\n        b @x] ; deref\n    (+ a ; left\n       b)))"]
      (is (= "defn f [x]:\n  ; a is one\n  a := 1\n  ; deref\n  b := @x\n  ; left\n  a + b\nend"
             (core/clj->sup src)))
      (is (keeps-forms? src)))
    (is (= "defn f [x]:\n  ; c\n  a := 1\n  g(a)\nend"
           (core/clj->sup "(defn f [x]\n  (let [a 1] ; c\n    (g a)))")))
    (is (= "; one\n{:a 1, :b 2}" (core/clj->sup "{:a 1 ; one\n :b 2}")))))

(deftest test-comments-and-prefixes
  (testing "a comment never sits between a prefix and its form"
    (is (= "defmacro m [x]:\n  ; guard\n  `when ~x :\n     foo()\n   end\nend"
           (core/clj->sup "(defmacro m [x]\n  `(when ~x ; guard\n     (foo)))")))
    (is (= "; f\n@future(1)" (core/clj->sup "@(future ; f\n   1)")))))

(deftest test-not-comments
  (testing "; inside strings, regexes and character literals is not a comment"
    (let [src "(str \"a;b\" \\; #\"c;d\" x) ; real"]
      (is (= "; real\nstr(\"a;b\", \\;, #\"c;d\", x)" (core/clj->sup src))))))

(deftest test-comments-off
  (is (= "def x: 1\ndef y: 2" (core/clj->sup "(def x 1) ; one\n(def y 2)" {:comments false})))
  (testing "clj->forms keeps no comments unless asked"
    (is (nil? (:ws (meta (first (core/clj->forms "; c\n(f x)"))))))
    (is (= "; c" (:ws (meta (first (core/clj->forms "; c\n(f x)" {:comments true}))))))))

(deftest test-nil-and-false-elements
  (testing "nil and false elements are kept around comments"
    (is (keeps-forms? "(defn f\n  ([e] (f e nil false))\n  ([e s] ; c\n   (g e s nil)))"))
    (is (keeps-forms? "[nil ; c\n false 1]"))))

(deftest test-fn-literal
  (testing "a #() before a comment does not take the comments after it"
    (is (= "defn f [xs]:\n  filter(#(% > 0), xs)\nend\n\n; about g\ndef g: 1"
           (core/clj->sup "(defn f [xs] (filter #(> % 0) xs))\n\n; about g\n(def g 1)")))))

