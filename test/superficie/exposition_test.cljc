(ns superficie.exposition-test
  "Rendering raster and ansatz code for reading: layout, docstrings, quoted
   terms, and forms that used to read back differently."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.core :as core]))

(defn- rt-flat [forms] (core/sup->forms (core/forms->sup forms)))
(defn- rt-pp
  ([forms] (core/sup->forms (core/pprint-sup forms)))
  ([forms width] (core/sup->forms (core/pprint-sup forms {:width width}))))

(defn- roundtrips? [forms & [width]]
  (and (= forms (rt-flat forms))
       (= forms (if width (rt-pp forms width) (rt-pp forms)))))

(deftest test-require-form-sets-aliases
  (testing "scripts use (require '[ansatz.core :as a]) instead of an ns form"
    (let [forms '[(require (quote [ansatz.core :as a]))
                  (a/theorem t [n :- Nat] (= Nat n n) (rfl))]]
      (is (str/includes? (core/pprint-sup forms) "a/theorem t [n :- Nat] =(Nat, n, n):\n  rfl()\nend"))
      (is (roundtrips? forms)))))

(deftest test-operators-in-ns-clauses
  (testing "operator symbols in :refer / :exclude lists are data, not infix"
    (is (roundtrips? '[(ns demo
                         (:refer-clojure :exclude [+ - * / < > <= >=])
                         (:require [raster.numeric :refer [+ - * / < >]]))]))))

(deftest test-symbol-named-new
  (testing "a symbol `new` followed by a call is not constructor syntax"
    (is (roundtrips? '[(match new (List Nat) x)]))
    (is (roundtrips? '[[new (List Nat)]]))
    (is (roundtrips? '[{new (f x)}])))
  (testing "constructor syntax still works"
    (is (= '[(Foo. 1)] (core/sup->forms "new Foo(1)")))))

(deftest test-call-heads-under-pprint
  (testing "Lean-style dotted heads and var heads keep their call structure"
    (is (roundtrips? '[(RBTree.node Nat (RBColor.black) (RBTree.leaf Nat) 1 (RBTree.leaf Nat))] 30))
    (is (roundtrips? '[((var f) aaaaaaaa bbbbbbbb cccccccc)] 20)))
  (testing "operator symbols as arguments are not read as infix"
    (is (roundtrips? '[(swap! slot + (if (contains? #{:long :double} (:stack-type fi)) 2 1))] 30))))

(deftest test-syntax-quote-of-infix-in-body
  (is (roundtrips? '[(defn f [a b] (when a `(= ~a ~b)))])))

(deftest test-quoted-terms
  (testing "a quote binds one form, so non-atomic targets print as S-expressions"
    (is (= "'(/ x 2)" (core/forms->sup ['(quote (/ x 2))])))
    (is (= "'(Value.vnil)" (core/forms->sup ['(quote (Value.vnil))])))
    (is (= "'(1 2 3)" (core/forms->sup ['(quote (1 2 3))])))
    (is (= "'f(g(x))" (core/forms->sup ['(quote (f (g x)))]))))
  (is (roundtrips? '[(quote (/ x 2)) (quote (Value.vnil x)) (quote (1 2 3))])))

(deftest test-multiline-docstrings
  (let [forms '[(ns demo (:require [raster.core :refer [deftm]]))
                (defn f "First line.\n  Second line." [x] x)
                (deftm g "Doc.\n  More." [x :- Long] :- Long x)]
        out (core/pprint-sup forms)]
    (is (str/includes? out "defn f \"First line.\n  Second line.\" [x]:"))
    (is (str/includes? out "deftm g \"Doc.\n  More.\" [x :- Long] :- Long:"))
    (is (roundtrips? forms))))

(deftest test-long-let-one-binding-per-line
  (let [forms '[(defn f [x]
                  (let [aaaaaaaaaa (compute-something x)
                        bbbbbbbbbb (compute-something-else aaaaaaaaaa)
                        cccccccccc (combine aaaaaaaaaa bbbbbbbbbb)]
                    cccccccccc))]
        out (core/pprint-sup forms)]
    (is (str/includes? out "  let [aaaaaaaaaa compute-something(x),\n       bbbbbbbbbb "))
    (is (roundtrips? forms))))

(deftest test-dotted-calls-inside-ansatz-forms
  (let [forms '[(require (quote [ansatz.core :as a]))
                (a/theorem succ-add [n :- Nat] (= Nat (Nat.succ (Nat.add n 0)) (Nat.succ n)) (simp Nat.add_zero))
                (a/defn up [s :- String] String (.toUpperCase s))
                (defn host [s] (.toUpperCase s))
                (def t (RBTree.node Nat))]
        out (core/pprint-sup forms)]
    (testing "Lean-style names read as plain calls inside ansatz blocks"
      (is (str/includes? out "=(Nat, Nat.succ(Nat.add(n, 0)), Nat.succ(n)):")))
    (testing "a Java call inside an ansatz block uses the explicit form"
      (is (str/includes? out "  .toUpperCase(s)")))
    (testing "outside ansatz blocks, A.b(x) is still a method call"
      (is (str/includes? out "  s.toUpperCase()"))
      (is (str/includes? out "def t: (RBTree.node)(Nat)")))
    (is (roundtrips? forms))))
