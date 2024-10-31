(ns agentlang.test.fixes04
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.lang
             :refer [component event entity relationship dataflow record]]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-1494-throws-bug
  (defcomponent :I494
    (defn testerr [] 100)
    (dataflow
     :I494/Test1
     [:eval '(agentlang.test.fixes04/testerr)
      :throws
      [:error {:Agentlang.Kernel.Lang/Response {:HTTP {:status 422 :body :Error}}}]]))
  (is (= 100 (tu/result {:I494/Test1 {}}))))

(deftest issue-1447-list-of-bug
  (defcomponent :I1447
    (entity
     :I1447/Simple
     {:Id {:type :Int :guid true}
      :IntArr {:listof :Int :optional true}
      :FloatArr {:listof :Float :optional true}
      :BooleanArr {:listof :Boolean :optional true}
      :StrArr {:listof :String :optional true}}))
  (let [r (first
           (tu/result
            {:I1447/Create_Simple
             {:Instance
              {:I1447/Simple
               {:Id 1
                :IntArr [1 2 3 4]
                :BooleanArr [true false true]
                :StrArr ["a" "b" "c"]
                :FloatArr [1.2 3.4]}}}}))]
    (is (cn/instance-of? :I1447/Simple r))
    (is (= [true false true] (:BooleanArr r)))
    (is (= [1.2 3.4] (:FloatArr r)))
    (is (= [1 2 3 4] (:IntArr r)))
    (is (= ["a" "b" "c"] (:StrArr r)))))

(deftest issue-1490-destruct-nested
  (defcomponent :I1490
    (defn nested-result []
      {:Name "David"})
    (dataflow
     :I1490/Test1
     [:match true
      true [[:eval (quote (agentlang.test.fixes04/nested-result))
             :as :R]
            :R.Name]
      :as :K]))
  (is (= "David" (tu/result {:I1490/Test1 {}}))))
