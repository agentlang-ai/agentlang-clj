(ns agentlang.test.timer
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow schedule]]
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
     (let [r (first
              (tu/invoke
               {:BasicTimer/LookupEByX
                {:X x}}))]
       (is (cn/instance-of? :BasicTimer/E r))
       (is (= (:X r) x))))
   (let [r (tu/invoke
            {:BasicTimer/StartTimer
             {:X 100}})]
     (is (cn/instance-of? :Agentlang.Kernel.Lang/Timer r))
     (tu/sleep 3000 #(query-e 100)))))

(deftest repeat-timer
  (let [a1 (atom 0)
        a2 (atom 0)]
    (defn set-a1 [] (reset! a1 (+ @a1 100)))
    (defn set-a2 [] (reset! a2 (+ @a2 100)))
    (defcomponent :RepeatTimer
      (dataflow
       :RepeatTimer/StartNormalTimer
       {:Agentlang.Kernel.Lang/Timer
        {:Name "RepeatTimer/Timer01"
         :Expiry 1
         :ExpiryEvent
         [:q# {:RepeatTimer/OnTimer01 {}}]}})
      (dataflow :RepeatTimer/OnTimer01 [:call '(agentlang.test.timer/set-a1)])
      (dataflow
       :RepeatTimer/StartRepeatTimer
       {:Agentlang.Kernel.Lang/Timer
        {:Name "RepeatTimer/Timer02"
         :Restart true
         :Expiry 1
         :ExpiryEvent
         [:q# {:RepeatTimer/OnTimer02 {}}]}})
      (dataflow :RepeatTimer/OnTimer02 [:call '(agentlang.test.timer/set-a2)]))
    (let [timer? (partial cn/instance-of? :Agentlang.Kernel.Lang/Timer)]
      (is (timer? (tu/invoke {:RepeatTimer/StartNormalTimer {}})))
      (is (timer? (tu/invoke {:RepeatTimer/StartRepeatTimer {}})))
      (Thread/sleep 3000)
      (is (= @a1 100))
      (is (> @a2 100))
      (is (timer? (first (tu/invoke {:Agentlang.Kernel.Lang/CancelTimer {:TimerName "RepeatTimer/Timer02"}}))))
      (Thread/sleep 3000)
      (let [c @a2]
        (Thread/sleep 3000)
        (is (= c @a2))))))

(deftest retry-timer
  (let [a (atom 0)]
    (defn check-a1 []
      (let [v @a]
        (reset! a (+ v 1))
        (when (zero? v)
          (u/throw-ex "invalid a1"))
        v))
    (defcomponent :RetryTimer
      (dataflow
       :RetryTimer/StartTimer
       {:Agentlang.Kernel.Lang/Timer
        {:Name "RetryTimer/Timer01"
         :Retries 2
         :Expiry 1
         :ExpiryEvent
         [:q# {:RetryTimer/OnTimer01 {}}]}})
      (dataflow :RetryTimer/OnTimer01 [:call '(agentlang.test.timer/check-a1)]))
    (let [timer? (partial cn/instance-of? :Agentlang.Kernel.Lang/Timer)]
      (is (timer? (tu/invoke {:RetryTimer/StartTimer {}})))
      (Thread/sleep 3000)
      (is (= @a 2))
      (Thread/sleep 3000)
      (is (= @a 2)))))

(deftest schedule-tests
  #?(:clj
     (let [x (atom nil)
           y (atom 0)]
       (defn set-st-x! [e] (reset! x e))
       (defn inc-y [] (swap! y inc) @y)
       (defcomponent :ST
         (schedule
          :ST/Timer01
          {:after [2 :seconds]
           :event {:ST/Evt1 {}}})
         (schedule
          :ST/Timer02
          {:every [1 :seconds]
           :event {:ST/Evt2 {}}})
         (entity :ST/E {:Id {:type :Int :id true} :X :Int})
         (dataflow
          :ST/Evt1
          {:ST/E {:Id 1 :X 100} :as :E}
          [:call '(agentlang.test.timer/set-st-x! :E)]
          :E)
         (dataflow
          :ST/Evt2
          [:call '(agentlang.test.timer/inc-y)]))
       (u/run-init-fns)
       (Thread/sleep 3000)
       (is (pos? @y))
       (let [e @x]
         (is (cn/instance-of? :ST/E e))
         (is (= 100 (:X e)))))))
