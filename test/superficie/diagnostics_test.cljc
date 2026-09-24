(ns superficie.diagnostics-test
  "Error messages for code typed by people: missing ':' or 'end', stray
   terminators, unbalanced brackets, and habits from other languages."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [superficie.core :as core]
            [superficie.errors :as errors]))

(defn- read-error
  "Read s and return {:msg :data} of the thrown error, or nil when it parses."
  [s]
  (try (core/sup->forms s) nil
       (catch #?(:clj Exception :cljs :default) e
         {:msg (ex-message e) :data (ex-data e)})))

(deftest test-missing-colon-is-an-error
  (testing "a header without ':' used to parse silently into loose symbols"
    (let [{:keys [msg data]} (read-error "defn f [x]\n  x + 1\nend")]
      (is (str/starts-with? msg "Unexpected 'end'"))
      (is (str/includes? (:hint data) "`defn` at line 1"))
      (is (= [1 1] ((juxt :line :col) (first (:secondary data))))))))

(deftest test-missing-colon-inside-body
  (let [{:keys [data]} (read-error "defn f [x]:\n  when x > 0\n    g(x)\n  end\nend")]
    (is (str/includes? (:hint data) "`when` at line 2"))))

(deftest test-missing-end-by-indentation
  (let [{:keys [msg data]} (read-error "defn f [x]:\n  if x > 0:\n    1\n  else:\n    2\nend")]
    (is (str/includes? msg "Expected 'end'"))
    (is (str/includes? (:hint data) "block opened at line 2"))))

(deftest test-stray-end
  (let [{:keys [msg data]} (read-error "defn f [x]:\n  x\nend\nend")]
    (is (str/starts-with? msg "Unexpected 'end'"))
    (is (= 4 (:line data)))))

(deftest test-stray-else
  (is (str/starts-with? (:msg (read-error "else:\n  1\nend")) "Unexpected 'else'")))

(deftest test-unbalanced-brackets-are-errors
  (testing "unclosed and extra delimiters are reported, not silently healed"
    (let [{:keys [msg data]} (read-error "defn f [x]:\n  g(x, h(x)\nend")]
      (is (str/starts-with? msg "Unclosed ("))
      (is (= [2 4] ((juxt :line :col) data))))
    (is (some? (read-error "f(x")))
    (is (some? (read-error "f(x))")))))

(deftest test-def-with-call-syntax
  (let [{:keys [data]} (read-error "def f(x):\n  x + 1\nend")]
    (is (str/includes? (:hint data) "defn f [params]:"))))

(deftest test-format-error-shows-hint
  (let [src "defn f [x]\n  x\nend"
        e (try (core/sup->forms src) nil (catch #?(:clj Exception :cljs :default) e e))
        out (errors/format-error e src)]
    (is (str/includes? out "header without ':'"))
    (is (str/includes? out "Hint:"))))

(deftest test-unresolved-symbol-hints
  (testing "operators typed without spaces read as one name"
    (is (= "`W-1` is read as one name — operators need spaces: `W - 1`"
           (errors/unresolved-symbol-hint "W-1" '(defn f [W] W-1) (constantly false))))
    (is (str/includes? (errors/unresolved-symbol-hint "lap-u" '(fn [lap u] lap-u) (constantly false))
                       "`lap - u`")))
  (testing "a genuinely unknown name gets no hint"
    (is (nil? (errors/unresolved-symbol-hint "my-fn" '(my-fn x) (constantly false)))))
  (testing "habits from other languages"
    (is (= "write `true`" (errors/unresolved-symbol-hint "True" nil (constantly false))))
    (is (str/includes? (errors/unresolved-symbol-hint "elif" nil (constantly false)) "cond:")))
  (testing "both JVM and SCI messages are recognized"
    (is (= "W-1" (errors/unresolved-symbol "Unable to resolve symbol: W-1 in this context")))
    (is (= "W-1" (errors/unresolved-symbol "Could not resolve symbol: W-1")))))
