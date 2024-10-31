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
