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
    (is (str/includes? out "defn f\n    \"First line.\n  Second line.\"\n    [x]:"))
    (is (str/includes? out "deftm g\n    \"Doc.\n  More.\"\n    [x :- Long] :- Long:"))
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

(deftest test-nested-comparisons-and-grouping
  (testing "comparisons chain, so a comparison operand of a comparison is grouped"
    (is (= "(a > b) not= (c > d)" (core/forms->sup ['(not= (> a b) (> c d))])))
    (is (roundtrips? '[(and (not= (> yi y) (> yj y)) (< x (+ xi 1)))])))
  (testing "a parenthesized group is one operand: no chaining or flattening across it"
    (is (= '[(= (> a b) x)] (core/sup->forms "(a > b) = x")))
    (is (= '[(and (< a b) (< b c))] (core/sup->forms "a < b < c")))
    (is (= '[(* (* a b) c)] (core/sup->forms "(a * b) * c")))
    (is (= '[(* a b c)] (core/sup->forms "a * b * c"))))
  (testing "nested same-op arithmetic keeps its shape"
    (is (= "(a * b) * c" (core/forms->sup ['(* (* a b) c)])))
    (is (roundtrips? '[(* (* a b) c) (and (and a b) c) (+ a b c)]))))

(deftest test-builtin-tagged-literals
  (is (uuid? (first (core/sup->forms "#uuid \"7c1a2e3e-9b1c-4d6a-8f0e-c17900000001\""))))
  (is (inst? (first (core/sup->forms "#inst \"2026-09-25T00:00:00Z\"")))))

(deftest test-block-word-as-a-name
  (testing "a function may be called match"
    (is (roundtrips? '[(defn match "Doc." [x] (inc x))]))))

(deftest test-docstring-header-layout
  (let [forms '[(ns demo (:require [raster.core :refer [deftm]]))
                (deftm weight "The power kernel.\n  A second line." [att :- Double d :- Double] :- Double (* att d))
                (defn f "First.\n  Second." [x] x)]
        out (core/pprint-sup forms)]
    (is (str/includes? out "deftm weight\n    \"The power kernel.\n  A second line.\"\n    [att :- Double d :- Double] :- Double:"))
    (is (str/includes? out "defn f\n    \"First.\n  Second.\"\n    [x]:"))
    (is (roundtrips? forms))))

(deftest test-spindel-blocks
  (let [forms '[(ns demo (:require [org.replikativ.spindel.spin.cps :refer [spin]]))
                (defn model [a0 a1]
                  (spin (let [alpha (sample (uniform a0 a1) :id :alpha)] alpha)))]
        out (core/pprint-sup forms)]
    (is (str/includes? out "  spin:\n    let [alpha sample(uniform(a0, a1), :id, :alpha)]:"))
    (is (roundtrips? forms))))

(deftest test-block-binding-value-layout
  (let [forms '[(defn f [y]
                  (let [a 1
                        yhat (if (contains? y :turnover) (:turnover y) y)]
                    yhat))]
        out (core/pprint-sup forms)]
    (is (str/includes? out "       yhat if contains?(y, :turnover):\n              :turnover(y)\n            else:\n              y\n            end]:"))
    (is (roundtrips? forms))))

(deftest test-numeric-equality-infix
  (is (= "a == b" (core/forms->sup ['(== a b)])))
  (is (roundtrips? '[(== a 0.0) (== a b c) (f ==) (== (== a b) true)])))

(deftest test-long-infix-breaks-before-operators
  (let [form '(- (+ (aget U (+ (* ym W) x)) (aget U (+ (* yp W) x))
                    (aget U (+ (* y W) xm)) (aget U (+ (* y W) xp)))
                 (* c u))
        out (core/pprint-sup [form] {:width 50})]
    (testing "a same-precedence chain breaks before its operators, aligned"
      (is (str/includes? out "\n+ aget(U, y * W + xm)"))
      (is (str/includes? out "\n- c * u")))
    (is (= [form] (core/sup->forms out)))))
