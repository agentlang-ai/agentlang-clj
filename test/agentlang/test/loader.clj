(ns agentlang.test.loader
  "Loader specific tests. Only work for Clojure."
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util.runtime :as ur]
            [agentlang.component :as cn]
            [agentlang.lang :refer [entity dataflow]]
            [agentlang.lang.raw :as raw]
            [agentlang.lang.tools.loader :as loader]
            [agentlang.evaluator :as e]
            [agentlang.test.util :as tu]))

(deftest test-load-script
  (is :Sample.Simple (loader/load-script nil "test/sample/simple.al"))
  (is (nil? (loader/load-script "test/sample/dependencies" "model_1/model.al"))))

(deftest test-read-expressions
  (is :Sample.Simple/LoggingPolicy (first (loader/read-expressions "test/sample/simple.al")))
  (is (some #{:Sample.Simple/E2} (loader/read-expressions "test/sample/simple.al")))
  (let [exp (first (loader/read-expressions "test/sample/dependencies/model_1/model.al"))]
    (is [:Model1.C1] (:components exp))
    (is [:Model2] (:dependencies exp))))

(deftest test-load-dependencies
  (let [[model model-root] (loader/read-model "test/sample/dependencies/model_1/model.al")]
    (is (= [:Model1.C1] (ur/load-model model model-root [] nil)))
    (is (cn/component-exists? :Model1.C1))
    (is (cn/component-exists? :Model2.C1))))

(deftest issue-321
  (is :Sample.Test (loader/load-script nil "test/sample/test.al"))
  (let [evt (cn/make-instance {:Sample.Test/SayHello {:Name "Clojure"}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :Sample.Test/HelloWorld r))
    (is (= (:Message r) "Hello World from Clojure"))
    (is (= (:Name r) "Clojure"))))

(deftest test-load-script-with-raw
  (is :Sample.Tiny (loader/load-script nil "test/sample/tiny.al"))
  (tu/finalize-component :Sample.Tiny)
  (let [expected-spec '(do (component :Sample.Tiny)
                           (entity :Sample.Tiny/A {:Id :Identity, :X :Int})
                           (defn tiny-f [x] (* 100 x))
                           (entity :Sample.Tiny/B {:Id :Identity, :X :Int, :Y '(sample.tiny/tiny-f :X)})
                           (dataflow :Agentlang.Kernel.Identity/PostSignUp #:Sample.Tiny{:A {:X 200}}))
        b-update-exp '(entity :Sample.Tiny/B {:Id :Identity :K :Int :Y '(sample.tiny/tiny-f :K)})
        new-df '(dataflow :Sample.Tiny/Evt
                          [:eval '(* :Sample.Tiny/Evt.X 2) :as :P]
                          {:Sample.Tiny/A {:X '(sample.tiny/tiny-f :P)}})
        updated-spec-01 `(do ~@(concat (assoc (vec (rest expected-spec)) 3 b-update-exp)
                                       [new-df]))]
    (is (= expected-spec (raw/as-edn :Sample.Tiny)))
    (eval b-update-exp)
    (eval new-df)
    (is (= updated-spec-01 (raw/as-edn :Sample.Tiny)))
    (let [r (tu/first-result {:Sample.Tiny/Evt {:X 10}})]
      (is (cn/instance-of? :Sample.Tiny/A r))
      (is (= (* 10 2 100) (:X r))))))
