#_(do (ns agentlang.test.features01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.lang.syntax :as ls]
            [agentlang.evaluator :as e]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest eval-block
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :EvBlock
     (record
      :EvBlock/R
      {:A :Int
       :B :Int})
     (entity
      :EvBlock/E
      {:X :Int
       :Y {:eval
           {:patterns
            [{:EvBlock/R
              {:A '(+ :EvBlock/E.X 10)
               :B 100}}]}}}))
   (let [e (tu/first-result
            {:EvBlock/Create_E
             {:Instance
              {:EvBlock/E
               {:X 5}}}})
         y ((:Y e))]
     (is (cn/instance-of? :EvBlock/E e))
     (is (= 15 (:A y)))
     (is (= 100 (:B y))))))

(deftest custom-identity ; Issue #610
  (defcomponent :CustId
    (entity
     :CustId/E
     {:SeqNo {:type :Int
              :unique true
              tu/guid true}
      :X :String}))
  (let [scm (cn/entity-schema :CustId/E)
        id-scm (cn/find-attribute-schema (:SeqNo scm))]
    (is (and (:unique id-scm) (:indexed id-scm)
             (tu/guid id-scm)))
    (let [e1 (tu/first-result
              {:CustId/Create_E
               {:Instance
                {:CustId/E
                 {:SeqNo 1
                  :X "abc"}}}})
          e2 (tu/first-result
              {:CustId/Create_E
               {:Instance
                {:CustId/E
                 {:SeqNo 2
                  :X "xyz"}}}})
          r1 (tu/first-result
              {:CustId/Lookup_E
               {:SeqNo 1}})
          r2 (tu/first-result
              {:CustId/Lookup_E
               {:SeqNo 2}})]
      (is (cn/same-instance? e1 r1))
      (is (cn/same-instance? e2 r2))
      (let [d1 (tu/first-result
                {:CustId/Delete_E
                 {:SeqNo 1}})
            r3 (tu/eval-all-dataflows
                {:CustId/Lookup_E
                 {:SeqNo 1}})
            r4 (tu/first-result
                {:CustId/Lookup_E
                 {:SeqNo 2}})]
        (is (= 1 (:SeqNo d1)))
        (is (= :not-found (:status (first r3))))
        (is (= 2 (:SeqNo r4)))))))

(deftest issue-624-instances-from-map
  (defcomponent :I624
    (entity
     :I624/E
     {:X :Int
      :Y :Int
      :Z {:type :Int :default 24}})
    (record
     :I624/R
     {:A :Int
      :B :I624/E})
    (dataflow
     :I624/MakeE
     {:I624/E {}
      :from :I624/MakeE.Data
      :as :E}
     {:I624/R {:A '(+ :E.X :E.Y)
               :B :E}})
    (dataflow
     :I624/MakeE2
     {:I624/E {:Z 200 :X 5}
      :from :I624/MakeE2.Data
      :as :E}
     {:I624/R {:A '(+ :E.X :E.Y)
               :B :E}}))
  (let [r (tu/first-result
           {:I624/MakeE {:Data {:X 10 :Y 4}}})]
    (is (cn/instance-of? :I624/R r))
    (is (cn/instance-of? :I624/E (:B r)))
    (is (= 24 (get-in r [:B :Z])))
    (is (= (:A r) 14 (+ (get-in r [:B :X]) (get-in r [:B :Y]))))
    (let [e (tu/first-result
             {:I624/Lookup_E {cn/id-attr (get-in r [:B cn/id-attr])}})]
      (is (and (= (:X e) 10) (= (:Y e) 4) (= (:Z e) 24)))))
  (let [r (tu/first-result
           {:I624/MakeE2 {:Data {:Y 4}}})]
    (is (cn/instance-of? :I624/R r))
    (is (cn/instance-of? :I624/E (:B r)))
    (is (= 200 (get-in r [:B :Z])))
    (is (= 5 (get-in r [:B :X])))
    (is (= (:A r) 9 (+ (get-in r [:B :X]) (get-in r [:B :Y]))))
    (let [e (tu/first-result
             {:I624/Lookup_E {cn/id-attr (get-in r [:B cn/id-attr])}})]
      (is (and (= (:X e) 5) (= (:Y e) 4) (= (:Z e) 200))))))

(deftest issue-625-upsert-subtype
  (defcomponent :I625Ups
    (entity
     :I625Ups/P
     {:X {:type :Int
          :indexed true}})
    (event
     :I625Ups/MakeP
     {:Data :Map})

    (dataflow
     :I625Ups/MakeP
     {:I625Ups/P {} :from :I625Ups/MakeP.Data})

    (entity
     :I625Ups/C
     {:meta {:inherits :I625Ups/P}
      :Y :Int})

    (dataflow
     :I625Ups/MakeC
     {:I625Ups/MakeP
      {:Data [:q# {:X 10 :Y 20}]}
      :with-types {:I625Ups/P :I625Ups/C}}))

  (let [c (tu/first-result
           {:I625Ups/MakeC {}})
        p (tu/first-result
           {:I625Ups/MakeP
            {:Data {:X 100}}})]
    (is (cn/instance-of? :I625Ups/C c))
    (is (and (= 10 (:X c)) (= 20 (:Y c))))
    (is (cn/instance-of? :I625Ups/P p))
    (is (= 100 (:X p)))
    (let [id (cn/id-attr c)
          r (tu/first-result
             {:I625Ups/Lookup_C
              {cn/id-attr id}})]
      (is (cn/same-instance? c r)))))

(deftest issue-625-subtype-ref
  (defcomponent :I625Sr
    (entity
     :I625Sr/P
     {:X {:type :Int
          :indexed true}
      :Y :Int})
    (entity
     :I625Sr/C
     {:meta {:inherits :I625Sr/P}
      :Z :Int})
    (event
     :I625Sr/Evt
     {:P :I625Sr/P})
    (dataflow
     :I625Sr/Evt
     {:I625Sr/P {:X? :I625Sr/Evt.P.X}}))
  (let [p (tu/first-result
           {:I625Sr/Create_P
            {:Instance
             {:I625Sr/P
              {:X 1 :Y 100}}}})
        c (tu/first-result
           {:I625Sr/Create_C
            {:Instance
             {:I625Sr/C
              {:X 1 :Y 2 :Z 3}}}})
        p2 (tu/first-result
            {:I625Sr/Evt
             {:P c}})]
    (is (cn/instance-of? :I625Sr/P p))
    (is (and (= (:X p) 1) (= (:Y p) 100)))
    (is (cn/instance-of? :I625Sr/C c))
    (is (and (= (:X c) 1) (= (:Y c) 2) (= (:Z c) 3)))
    (is (cn/same-instance? p p2))))

(deftest issue-625-with-event-type
  (defcomponent :I625Wevt
    (record
     :I625Wevt/R1
     {})
    (event
     :I625Wevt/P
     {})
    (dataflow
     :I625Wevt/P
     {:I625Wevt/R1 {}})
    (record
     :I625Wevt/R2
     {})
    (event
     :I625Wevt/C
     {:meta {:inherits :I625Wevt/P}})
    (dataflow
     :I625Wevt/C
     {:I625Wevt/R2 {}})
    (dataflow
     :I625Wevt/Evt1
     {:I625Wevt/P {}})
    (dataflow
     :I625Wevt/Evt2
     {:I625Wevt/Evt1 {}
      :with-types {:I625Wevt/P :I625Wevt/C}}))
  (let [r1 (tu/first-result
            {:I625Wevt/Evt1 {}})
        r2 (tu/first-result
            {:I625Wevt/Evt2 {}})]
    (is (cn/instance-of? :I625Wevt/R1 r1))
    (is (cn/instance-of? :I625Wevt/R2 r2))))

(deftest issue-625-with-types-bubble-up
  (defcomponent :I625Wtb
    (record
     :I625Wtb/P
     {})
    (record
     :I625Wtb/C
     {:meta {:inherits :I625Wtb/P}})
    (dataflow
     :I625Wtb/Evt1
     {:I625Wtb/P {}})
    (dataflow
     :I625Wtb/Evt2
     {:I625Wtb/Evt1 {}})
    (dataflow
     :I625Wtb/Evt3
     {:I625Wtb/Evt2 {}
      :with-types {:I625Wtb/P :I625Wtb/C}}))
  (let [p1 (tu/first-result
            {:I625Wtb/Evt2 {}})
        p2 (tu/first-result
            {:I625Wtb/Evt3 {}})]
    (is (cn/instance-of? :I625Wtb/P p1))
    (is (cn/instance-of? :I625Wtb/C p2))))

(deftest issue-630-upsert-with-value-pattern
  (defcomponent :I630
    (entity
     :I630/E
     {:id {:type :Int
           tu/guid true}
      :X :Int})
    (dataflow
     :I630/FindE
     {:I630/E {:id? :I630/FindE.E}})
    (dataflow
     :I630/Evt1
     {:I630/E
      {:id? :I630/Evt1.E}
      :as [:R]}
     {:R {:X 200}})
    (dataflow
     :I630/Evt2
     {:I630/Evt2.E {:X 300}}))
  (let [e (tu/first-result
           {:I630/Create_E
            {:Instance
             {:I630/E
              {:id 1 :X 10}}}})
        r0 (tu/first-result
            {:I630/FindE {:E 1}})
        r1 (tu/first-result
            {:I630/Evt1
             {:E 1}})
        r2 (tu/first-result
            {:I630/FindE {:E 1}})
        r3 (tu/first-result
            {:I630/Evt2
             {:E e}})
        r4 (tu/first-result
            {:I630/FindE {:E 1}})]
    (is (cn/same-instance? e r0))
    (is (= 200 (:X r1)))
    (is (cn/same-instance? r1 r2))
    (is (= 300 (:X r3)))
    (is (cn/same-instance? r4 r3))))

(deftest issue-630-upsert-multiple
  (defcomponent :I630M
    (entity
     :I630M/E
     {:X {:type :Int
          :indexed true}
      :Y {:type :Int
          :expr '(* :X 2)}})
    (dataflow
     :I630M/FindE
     :I630M/E?)
    (dataflow
     :I630M/Evt1
     {:I630M/E? {} :as :R}
     {:R {:X '(* :Y 10)}}))
  (let [sum #(apply + (mapv %1 %2))
        sum-xs (partial sum :X)
        sum-ys (partial sum :Y)
        xs [1 2 3 4 5]
        ys (mapv (partial * 2) xs)
        ys10 (mapv (partial * 10) ys)
        es (mapv
            #(tu/first-result
              {:I630M/Create_E
               {:Instance
                {:I630M/E
                 {:X %}}}})
            xs)
        r0 (tu/result
            {:I630M/FindE {}})
        r1 (tu/result
            {:I630M/Evt1 {}})
        r2 (tu/result
            {:I630M/FindE {}})]
    (is (= (sum-xs r0) (sum-xs es) (apply + xs)))
    (is (= (sum-ys r0) (sum-ys es) (apply + ys)))
    (let [tos r1]
      (is (= (sum-xs tos) (sum-xs tos) (apply + ys10)))
      (is (= (sum-ys tos) (* 2 (sum-xs tos)))))
    (is (= (sum-xs r2) (apply + ys10)))
    (is (= (sum-ys r2) (* 2 (sum-xs r2))))))

(deftest delete-by-multiple-attributes
  (defcomponent :DelMulti
    (entity
     :DelMulti/E
     {:X {:type :Int
          :indexed true}
      :Y {:type :Int
          :indexed true}})
    (dataflow
     :DelMulti/Del
     [:delete :DelMulti/E {:X :DelMulti/Del.X
                           :Y :DelMulti/Del.Y}])
    (dataflow
     :DelMulti/FindAll
     :DelMulti/E?)
    (dataflow
     :DelMulti/Find
     {:DelMulti/E {:X? :DelMulti/Find.X
                   :Y? :DelMulti/Find.Y}}))
  (let [_ (mapv #(tu/first-result
                  {:DelMulti/Create_E
                   {:Instance
                    {:DelMulti/E
                     {:X %1 :Y %2}}}})
                [1 1 1 2] [10 20 10 20])
        find {:DelMulti/Find {:X 1 :Y 10}}
        r0 (tu/result find)
        r1 (tu/result
            {:DelMulti/FindAll {}})
        r2 (tu/result
            {:DelMulti/Del
             {:X 1 :Y 10}})
        r3 (tu/eval-all-dataflows find)]
    (is (= 2 (count r0)))
    (is (= 4 (count r1)))
    (is (= 2 (count r2)))
    (let [ids0 (mapv cn/id-attr r0)
          ids2 (mapv cn/id-attr r2)]
      (= (sort ids0) (sort ids2)))
    (= :not-found (:status (first r3))))))
