(ns agentlang.test.timer
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.evaluator :as e]
            [agentlang.lang.datetime :as dt]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest basic-timer
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :BasicTimer
     (entity
      :BasicTimer/E
      {:X {:type :Int
           :indexed true}})
     (event
      :BasicTimer/OnTimer
      {:X :Int})
     (dataflow
      :BasicTimer/StartTimer
      {:Agentlang.Kernel.Lang/Timer
       {:Name "BasicTimer/Timer01"
        :Expiry 1
        :ExpiryEvent
        [:q# {:BasicTimer/OnTimer
              {:X [:uq# :BasicTimer/StartTimer.X]}}]}})
     (dataflow
      :BasicTimer/OnTimer
      {:BasicTimer/E
       {:X :BasicTimer/OnTimer.X}})
     (dataflow
      :BasicTimer/LookupEByX
      {:BasicTimer/E {:X? :BasicTimer/LookupEByX.X}}))
   (defn query-e [x]
     (let [r (tu/first-result
              {:BasicTimer/LookupEByX
               {:X x}})]
       (is (cn/instance-of? :BasicTimer/E r))
       (is (= (:X r) x))))
   (let [r (tu/first-result
            {:BasicTimer/StartTimer
             {:X 100}})]
     (is (cn/instance-of? :Agentlang.Kernel.Lang/Timer r))
     (tu/sleep 3000 #(query-e 100)))))
