(ns agentlang.test.features01
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            #?(:clj  [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest match-attribute-spec
  (defcomponent :Mas
    (entity
     :Mas/A
     {:Id {:type :Int :id true} :X :Int})
    (entity
     :Mas/B
     {:Id {:type :Int :id true}
      :A {:type :Path :to :Mas/A}
      :Y :Int
      :Z [:match
          [:= :A.X 1] 100
          [:< :A.X 100] [:* :Y :A.X]
          1000]})
    (dataflow
     :Mas/FindB
     {:Mas/B {:Id? :Mas/FindB.Id}}))
  (is (= #{:Mas/A} (cn/entities-reached-via-path-attributes :Mas/B)))
  (is (= #{:Z} (set (cn/match-attributes :Mas/B))))
  (is (= #{:A} (set (cn/path-attributes :Mas/B))))
  (let [cra (fn [id x]
              (tu/invoke
               {:Mas/Create_A
                {:Instance {:Mas/A {:Id id :X x}}}}))
        a? (partial cn/instance-of? :Mas/A)
        crb (fn [id a y]
              (tu/invoke
               {:Mas/Create_B
                {:Instance {:Mas/B {:Id id :A a :Y y}}}}))
        b? (partial cn/instance-of? :Mas/B)
        [a1 a2] (mapv cra [1 2] [10 20])
        _ (is (every? a? [a1 a2]))
        b (crb 90 (li/path-attr a1) 2)]
    (is (b? b))
    (let [rs (tu/invoke {:Mas/FindB {:Id 90}})
          _ (is (= 1 (count rs)))
          r (first rs)]
      (is (b? r))
      (is (= 20 (:Z r))))))
