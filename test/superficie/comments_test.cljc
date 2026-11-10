(ns superficie.comments-test
  "Comments through every path: Clojure → superficie, superficie →
   superficie, superficie → Clojure (see superficie.comments)."
  (:require [clojure.test :refer [deftest is testing]]
            [superficie.core :as core]))

(defn- keeps-forms?
  "The rendering with comments reads back to the forms of the source."
  [clj-src]
  (= (pr-str (core/clj->forms clj-src))
     (pr-str (core/sup->forms (core/clj->sup clj-src)))))

(defn- fixed-point?
  "Superficie text that reads and prints back as itself, and survives a trip
   through Clojure."
  [sup]
  (and (= sup (core/pprint-sup (core/sup->forms sup)))
       (= sup (core/clj->sup (core/sup->clj sup)))))

;; ---------------------------------------------------------------------------
;; Clojure → superficie
;; ---------------------------------------------------------------------------

(deftest test-end-of-line-comments
  (testing "a comment at the end of a line stays at the end of that line"
    (let [src "(defn power [n x]                          ; the meaning\n  (if (= n 0) 1 (* x (power (dec n) x))))"
          sup "defn power [n x]: ; the meaning\n  if n = 0:\n    1\n  else:\n    x * power(dec(n), x)\n  end\nend"]
      (is (= sup (core/clj->sup src)))
      (is (keeps-forms? src))
      (is (fixed-point? sup))))
  (testing "after a body statement, a binding, an operand"
    (is (= "defn f [x]:\n  println(\"hi\") ; say hi\n  g(x)\nend"
           (core/clj->sup "(defn f [x]\n  (println \"hi\") ; say hi\n  (g x))")))
    (is (= "defn f [x]:\n  a := 1 ; a is one\n  b := @x ; deref\n  a + b ; left\nend"
           (core/clj->sup "(defn f [x]\n  (let [a 1 ; a is one\n        b @x] ; deref\n    (+ a ; left\n       b)))"))))
  (testing "after a top-level form"
    (is (= "def x: 1 ; one\ndef y: 2" (core/clj->sup "(def x 1) ; one\n(def y 2)"))))
  (testing "about a form that elements without a position follow"
    (is (= "if pos?(x): ; then\n  :yes\nelse:\n  :no\nend"
           (core/clj->sup "(if (pos? x) ; then\n  :yes\n  :no)")))))

(deftest test-comment-lines
  (testing "a comment on its own line goes above the next form, blank lines kept"
    (is (= ";; header\n\ndefn f [x]:\n  ;; lead\n  g(x)\nend\n\n; tail"
           (core/clj->sup ";; header\n\n(defn f [x]\n  ;; lead\n  (g x))\n\n; tail"))))
  (testing "a comment after the last element goes above the closer"
    (is (= "defn g []:\n  foo()\n  ;; TODO\nend" (core/clj->sup "(defn g []\n  (foo)\n  ;; TODO\n  )")))
    (is (= "try:\n  f()\nfinally:\n  ;; always\n  g()\nend"
           (core/clj->sup "(try\n  (f)\n  (finally\n    ;; always\n    (g)))"))))
  (testing "inside an ns clause and before a reader conditional"
    (is (= "ns a.b:\n  require:\n    [x.y :as y]\n    ;; only on the JVM\n    #?(:clj [x.z :as z])\n  end\nend"
           (core/clj->sup "(ns a.b\n  (:require [x.y :as y]\n            ;; only on the JVM\n            #?(:clj [x.z :as z])))")))))

(deftest test-comments-in-one-line-parts
  (testing "a comment whose form is written on one line goes to that line"
    (is (= "; one\n{:a 1, :b 2}" (core/clj->sup "{:a 1 ; one\n :b 2}")))
    (is (= "f(a, b) ; after a" (core/pprint-sup (core/sup->forms "f(a, ; after a\n  b)"))))))

(deftest test-comments-and-prefixes
  (testing "a comment never sits between a prefix and its form"
    (is (= "defmacro m [x]:\n  `when ~x : ; guard\n     foo()\n   end\nend"
           (core/clj->sup "(defmacro m [x]\n  `(when ~x ; guard\n     (foo)))")))
    (is (= "@future(1) ; f" (core/clj->sup "@(future ; f\n   1)")))))

(deftest test-not-comments
  (testing "; inside strings, regexes and character literals is not a comment"
    (is (= "str(\"a;b\", \\;, #\"c;d\", x) ; real"
           (core/clj->sup "(str \"a;b\" \\; #\"c;d\" x) ; real")))))

(deftest test-nil-and-false-elements
  (testing "nil and false elements are kept around comments"
    (is (keeps-forms? "(defn f\n  ([e] (f e nil false))\n  ([e s] ; c\n   (g e s nil)))"))
    (is (keeps-forms? "[nil ; c\n false 1]"))))

(deftest test-fn-literal
  (testing "a #() before a comment does not take the comments after it"
    (is (= "defn f [xs]:\n  filter(#(% > 0), xs)\nend\n\n; about g\ndef g: 1"
           (core/clj->sup "(defn f [xs] (filter #(> % 0) xs))\n\n; about g\n(def g 1)")))))

(deftest test-comments-off
  (is (= "def x: 1\ndef y: 2" (core/clj->sup "(def x 1) ; one\n(def y 2)" {:comments false})))
  (is (= "f(a, b)" (core/pprint-sup (core/sup->forms "f(a, ; c\n  b)" {:comments false}))))
  (testing "clj->forms keeps no comments unless asked"
    (is (nil? (:sup/comments (meta (first (core/clj->forms "; c\n(f x)"))))))
    (is (= [";c"] (map :text (:before (:sup/comments (meta (first (core/clj->forms ";c\n(f x)" {:comments true}))))))))))

;; ---------------------------------------------------------------------------
;; superficie → superficie and → Clojure
;; ---------------------------------------------------------------------------

(def ^:private sup-src
  "defn f [x]: ; header\n  a := 1 ; one\n  ; before b\n  b := 2\n  if a < b: ; test\n    a\n  else:\n    ; in else\n    b\n    ; end of else\n  end\n  ; end of f\nend\n; tail")

(deftest test-superficie-comments
  (testing "every comment of superficie source is kept where it is"
    (is (= sup-src (core/pprint-sup (core/sup->forms sup-src)))))
  (testing "Clojure output writes them at the same places"
    (is (= "(defn f [x] ; header\n  (let [a 1 ; one\n        ; before b\n        b 2]\n    (if (< a b) ; test\n      a\n      ; in else\n      b\n      ; end of else\n    ))\n  ; end of f\n)\n; tail"
           (core/sup->clj sup-src))))
  (testing "and the round trip through Clojure comes back"
    (is (fixed-point? sup-src))))

(deftest test-clojure-round-trip
  (testing "Clojure → superficie → Clojure keeps comments in place"
    (let [clj "(defn f [x]\n  ;; lead\n  (let [a 1 ; a is one\n        b @x] ; deref\n    (+ a b))) ; left"]
      (is (= clj (core/sup->clj (core/clj->sup clj))))
      (is (fixed-point? (core/clj->sup clj))))))
