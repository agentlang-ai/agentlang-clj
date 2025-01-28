(ns agentlang.test.raw
  (:require [clojure.test :refer :all]
            [agentlang.lang.raw :refer :all]))

(def test-component 'test-comp)

(defn reset-state []
  (raw-store-reset!)
  (intern-component test-component ['(component test-comp)]))

(deftest create-function-test
  (testing "Basic function creation without docstring"
    (reset-state)
    (create-function test-component 'add '[x y] '(+ x y))
    (is (= '[x y] (get-function-params test-component 'add)))
    (is (= '(+ x y) (get-function-body test-component 'add))))

  (testing "Function creation with docstring"
    (reset-state)
    (create-function test-component 'greet "Greets a user" '[name] '(str "Hello " name))
    (let [fn-def (find-defn test-component 'greet)]
      (is (= "Greets a user" (nth fn-def 2)))
      (is (= '[name] (nth fn-def 3))))))


(deftest update-function-test
  (testing "Update existing function parameters"
    (reset-state)
    (create-function test-component 'add '[a b] '(+ a b))
    (update-function test-component 'add '[x y z] '(+ x y z))
    (is (= '[x y z] (get-function-params test-component 'add))))

  (testing "Add docstring via update"
    (reset-state)
    (create-function test-component 'multiply '[a b] '(* a b))
    (update-function test-component 'multiply "Multiplies values" '[a b] '(* a b))
    (is (= "Multiplies values" (nth (find-defn test-component 'multiply) 2)))))

(deftest error-handling-test
  (testing "Invalid function name type"
    (reset-state)
    (is (thrown? Exception (create-function test-component :not-a-symbol [] nil))))

  (testing "Non-vector parameter list"
    (reset-state)
    (is (thrown? Exception (create-function test-component 'bad-fn 'not-a-vector nil)))))


(deftest component-integration-test
  (testing "Multiple functions in component"
    (reset-state)
    (create-function test-component 'func1 '[a] 'a)
    (create-function test-component 'func2 '[b] 'b)
    (is (= ['func1 'func2] (get-function-names test-component)))

    (testing "Delete and verify removal"
      (delete-function test-component 'func1)
      (is (= ['func2] (get-function-names test-component))))))
