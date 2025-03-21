(ns agentlang.test.features01
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.suspension :as susp]
            [agentlang.exec-graph :as exg]
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
  (is (= #{:Mas/A} (set (keys (cn/entities-reached-via-path-attributes :Mas/B)))))
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

(deftest exec-graph-01
  (defcomponent :Exg01
    (entity
     :Exg01/A
     {:Id {:type :Int :id true}
      :X :Int})
    (entity
     :Exg01/B
     {:Id {:type :Int :id true}
      :Y :Int})
    (event :Exg01/E {:Y :Int})
    (dataflow
     :Exg01/Create
     {:Exg01/A {:Id :Exg01/Create.Id :X :Exg01/Create.X}}
     {:Exg01/E {:Y :Exg01/Create.Y}})
    (dataflow
     :Exg01/E
     {:Exg01/B {:Id '(* :Exg01/E.Y 10) :Y :Exg01/E.Y}}))
  (exg/call-with-exec-graph
   (fn []
     (let [b (tu/invoke {:Exg01/Create {:Id 1 :X 10 :Y 100}})]
       (is (cn/instance-of? :Exg01/B b))
       (let [g (exg/load-graph :Exg01/Create)]
         (is (and (exg/graph? g) (exg/event-graph? g)))
         (is (= (exg/graph-name g) :Exg01/Create))
         (let [[n1 n2] (exg/graph-nodes g)]
           (is (exg/pattern? n1))
           (is (cn/instance-of? :Exg01/A (exg/pattern-result n1)))
           (is (and (exg/graph? n2) (exg/event-graph? n2)))
           (is (cn/instance-of? :Exg01/B (:result n2)))))))))

(require '[agentlang.inference.service.model])
(require '[agentlang.inference.service.channel.core :as cc])
(require '[agentlang.inference.service.channel.ttl])

(def ch {:channel-type :ttl :name :test01})
(.start (Thread. #(cc/channel-start ch)))
(Thread/sleep 60000)
(cc/channel-shutdown ch)
