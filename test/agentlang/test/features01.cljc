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
       (let [g (exg/load-graph)]
         (is (and (exg/graph? g) (exg/event-graph? g)))
         (is (= (exg/graph-name g) :Exg01/Create))
         (let [[n1 n2] (exg/graph-nodes g)]
           (is (exg/pattern? n1))
           (is (cn/instance-of? :Exg01/A (cn/make-instance (exg/pattern-result n1))))
           (is (exg/event-graph? (first (exg/pattern-sub-graphs n2))))
           (is (cn/instance-of? :Exg01/B (cn/make-instance (:result n2))))))))))

(deftest exec-graph-02
  (defcomponent :Exg02
    (entity
     :Exg02/A
     {:Id {:type :Int :id true}
      :X :Int})
    (dataflow
     :Exg02/Evt01
     {:Exg02/A {:Id? :Exg02/Evt01.A} :as [:A]}
     {:Exg02/Evt02 {} :as :As}
     [:for-each :As
      {:Exg02/Evt03 {:Id :%.X :X :A.X}}])
    (dataflow
     :Exg02/Evt02
     {:Exg02/A? {}})
    (dataflow
     :Exg02/Evt03
     {:Exg02/A {:Id :Exg02/Evt03.Id :X :Exg02/Evt03.X}}))
  (let [[cra a?] (tu/make-create :Exg02/A)
        as (mapv cra [{:Id 1 :X 10} {:Id 2 :X 20}])]
    (is (= 2 (count as)))
    (is (every? a? as))
    (exg/call-with-exec-graph
     (fn []
       (let [as (tu/invoke {:Exg02/Evt01 {:A 1}})]
         (is (pos? (count as)))
         (is (every? a? as))
         (let [g (exg/load-graph)]
           (is (exg/event-graph? g))
           (let [nodes (exg/graph-nodes g)
                 xs (exg/pattern-sub-graphs (second nodes))
                 ys (exg/pattern-sub-graphs (last nodes))]
             (is (= 1 (count xs)))
             (is (exg/event-graph? (first xs)))
             (is (> (count ys) 1))
             (is (every? exg/event-graph? ys)))))))))

(deftest issue-1726
  (defcomponent :UqIn
    (entity
     :UqIn/E
     {:Id {:type :Int :id true}
      :Start :String
      :End :String
      :meta {:unique-in
             [:and [:<= :Start] [:>= :End]]}})
    (entity
     :UqIn/F
     {:Id {:type :Int :id true}
      :X :Int
      :meta {:unique-in [:> :X]}}))
  (let [cre (fn [id s e]
             (tu/invoke
              {:UqIn/Create_E
               {:Instance
                {:UqIn/E
                 {:Id id
                  :Start s
                  :End e}}}}))
        e? (partial cn/instance-of? :UqIn/E)]
    (is (e? (cre 1 "2024-12-01" "2025-01-02")))
    (tu/is-error #(cre 2 "2024-12-01" "2025-01-01"))
    (is (e? (cre 2 "2024-01-03" "2025-01-20")))
    (is (e? (cre 3 "2024-12-02" "2025-01-22")))
    (tu/is-error #(cre 4 "2024-12-02" "2024-12-05")))
  (let [crf (fn [id x]
              (tu/invoke
               {:UqIn/Create_F
                {:Instance
                 {:UqIn/F
                  {:Id id :X x}}}}))
        f? (partial cn/instance-of? :UqIn/F)]
    (is (f? (crf 1 10)))
    (tu/is-error #(crf 2 9))
    (is (f? (crf 2 11)))))
