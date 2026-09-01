(ns superficie.sci-repl-test
  (:require [cljs.test :refer [deftest is testing]]
            [superficie.sci-repl :as repl]))

(def seed
  "def label: :root
def counter: atom(0)
def alias: counter

defn state []:
  {:label label :counter deref(counter) :alias deref(alias)}
end

state()")

(deftest forkable-session-test
  (let [session (repl/create-forkable-session)
        root-id (.-rootId session)]
    (is (= "root" root-id))
    (is (= "{:label :root, :counter 0, :alias 0}"
           (.-result (.evalSup session root-id seed))))
    (is (= ":root"
           (.-result
            (.evalClj session root-id
                      "(def ^:dynamic *scope* :root) (defn scope [] *scope*) (scope)"))))
    (let [child (.fork session root-id #js {:label "Left"})
          child-id (.-id child)]
      (testing "captured functions, Vars, atoms, and aliases use the selected world"
        (is (= "{:label :left, :counter 1, :alias 1}"
               (.-result
                (.evalSup session child-id
                          "def label: :left\nswap!(counter, inc)\nstate()"))))
        (is (= "{:label :root, :counter 0, :alias 0}"
               (.-result (.evalSup session root-id "state()")))))
      (testing "Clojure and Superficie evaluate in the same child"
        (is (= "2" (.-result (.evalClj session child-id "(swap! counter inc)"))))
        (is (= "2" (.-result (.evalSup session child-id "deref(counter)")))))
      (testing "dynamic bindings resolve and unwind inside the selected world"
        (is (= ":inner"
               (.-result
                (.evalClj session child-id
                          "(binding [*scope* :child] (set! *scope* :inner) (scope))"))))
        (is (= ":root" (.-result (.evalClj session child-id "(scope)"))))
        (is (= ":root" (.-result (.evalClj session root-id "(scope)")))))
      (testing "the session exposes only serializable world descriptions"
        (let [worlds (.worlds session)]
          (is (= 2 (alength worlds)))
          (is (= "root" (.-parentId (aget worlds 1)))))))))

(deftest backwards-compatible-default-repl-test
  (repl/reset-ctx!)
  (is (= "42" (.-result (repl/eval-sup "def answer: 42\nanswer"))))
  (repl/reset-ctx!)
  (is (some? (.-error (repl/eval-sup "answer")))))
