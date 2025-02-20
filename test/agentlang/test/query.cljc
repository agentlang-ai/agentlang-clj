#_(do (ns agentlang.test.query
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.evaluator :as e]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.store.util :as stu]
            #?(:clj  [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest q01
  (defcomponent :Q01
    (entity {:Q01/E {:X :Int}}))
  (let [e (cn/make-instance :Q01/E {:X 10})
        e1 (first (tu/fresult (e/eval-all-dataflows {:Q01/Create_E {:Instance e}})))
        id (cn/id-attr e1)
        e2 (first (tu/fresult (e/eval-all-dataflows {:Q01/Lookup_E {cn/id-attr id}})))]
    (is (cn/instance-of? :Q01/E e2))
    (is (cn/same-instance? e1 e2))))

(deftest q02
  (defcomponent :Q02
    (entity {:Q02/E {:X {:type :Int
                         :indexed true}
                     :Y {:type :Int
                         :indexed true}}})
    (event {:Q02/QE01 {:Y :Int}})
    (dataflow :Q02/QE01
              {:Q02/E {:X? [:>= 10]
                       :Y :Q02/QE01.Y}})
    (event {:Q02/QE02 {:X :Int
                       :Y :Int}})
    (dataflow :Q02/QE02
              {:Q02/E {:X? [:>= :Q02/QE02.X]
                       :Y? :Q02/QE02.Y}}))
  (let [es [(cn/make-instance :Q02/E {:X 10 :Y 4})
            (cn/make-instance :Q02/E {:X 12 :Y 6})
            (cn/make-instance :Q02/E {:X 9 :Y 3})]
        evts (map #(cn/make-instance :Q02/Create_E {:Instance %}) es)
        f (comp first #(:result (first (e/eval-all-dataflows %))))
        insts (mapv f evts)
        ids (mapv cn/id-attr insts)]
    (is (every? true? (map #(cn/instance-of? :Q02/E %) insts)))
    (let [r01 (tu/fresult (e/eval-all-dataflows {:Q02/QE01 {:Y 100}}))
          r (e/eval-all-dataflows {:Q02/QE02 {:X 5 :Y 100}})
          r02 (tu/fresult r)
          ts01 r01]
      (is (= 2 (count r01)))
      (is (every? #(and (some #{(:X %)} [10 12]) (= 100 (:Y %))) ts01))
      (is (= 2 (count r02)))
      (is (every? #(and (some #{(:X %)} [10 12]) (= 100 (:Y %))) r02)))))

(deftest query-all
  (defcomponent :QueryAll
    (entity {:QueryAll/E {:X :Int :N :String}})
    (event {:QueryAll/AllE {}})
    (dataflow :QueryAll/AllE
              :QueryAll/E?))
  (let [es [(cn/make-instance :QueryAll/E {:X 1 :N "e01"})
            (cn/make-instance :QueryAll/E {:X 2 :N "e02"})]
        evts (mapv #(cn/make-instance :QueryAll/Create_E {:Instance %}) es)
        _ (mapv tu/fresult (mapv #(e/eval-all-dataflows %) evts))
        result (tu/fresult (e/eval-all-dataflows {:QueryAll/AllE {}}))]
    (doseq [r result]
      (is (cn/instance-of? :QueryAll/E r))
      (is (= (if (= 1 (:X r)) "e01" "e02") (:N r))))))

(deftest alias-on-query-result
  (defcomponent :QueryAlias
    (entity {:QueryAlias/E {:X {:type :Int
                                :indexed true}
                            :N :String}})
    (event {:QueryAlias/Evt {:X :Int}})
    (dataflow :QueryAlias/Evt
              {:QueryAlias/E {:X? :QueryAlias/Evt.X} :as :R}
              :R))
  (let [es [(cn/make-instance :QueryAlias/E {:X 1 :N "e01"})
            (cn/make-instance :QueryAlias/E {:X 2 :N "e02"})
            (cn/make-instance :QueryAlias/E {:X 1 :N "e03"})]
        evts (map #(cn/make-instance :QueryAlias/Create_E {:Instance %}) es)
        es_result (doall (map (comp (comp first tu/fresult)
                                    #(e/eval-all-dataflows %))
                              evts))
        result (tu/fresult (e/eval-all-dataflows {:QueryAlias/Evt {:X 1}}))]
    (doseq [r result]
      (is (cn/instance-of? :QueryAlias/E r))
      (is (= 1 (:X r)))
      (is (let [n (:N r)]
            (some #{n} #{"e01" "e03"}))))))

(deftest query-alias-in-expr
  (defcomponent :QueryAliasInExpr
    (entity {:QueryAliasInExpr/OrderLine
             {:Title :String
              :Qty :Int}})
    (entity {:QueryAliasInExpr/ProductBatch
             {:Title :String
              :AvailableQty {:type :Int :check pos?}}})
    (dataflow :QueryAliasInExpr/AllocateOrderLine
              {:QueryAliasInExpr/OrderLine
               {tu/q-id-attr :QueryAliasInExpr/AllocateOrderLine.LineId}
               :as :OL}
              {:QueryAliasInExpr/ProductBatch
               {tu/q-id-attr :QueryAliasInExpr/AllocateOrderLine.BatchId
                :AvailableQty '(- :AvailableQty :OL.Qty)}}))
  (let [batch (cn/make-instance
               {:QueryAliasInExpr/ProductBatch
                {:Title "Table"
                 :AvailableQty 20}})
        evt (cn/make-instance
             {:QueryAliasInExpr/Create_ProductBatch
              {:Instance batch}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        batch-id (cn/id-attr r)
        order-line (cn/make-instance
                    {:QueryAliasInExpr/OrderLine
                     {:Title "Table"
                      :Qty 21}})
           evt (cn/make-instance
                {:QueryAliasInExpr/Create_OrderLine
                 {:Instance order-line}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        line-id (cn/id-attr r)
        evt (cn/make-instance
             {:QueryAliasInExpr/AllocateOrderLine
              {:BatchId batch-id :LineId line-id}})
        _ (tu/is-error #(doall (e/eval-all-dataflows evt)))
        order-line (cn/make-instance
                    {:QueryAliasInExpr/OrderLine
                     {:Title "Table"
                      :Qty 2}})
        evt (cn/make-instance
             {:QueryAliasInExpr/Create_OrderLine
              {:Instance order-line}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))
        line-id (cn/id-attr r)
        evt (cn/make-instance
             {:QueryAliasInExpr/AllocateOrderLine
              {:BatchId batch-id :LineId line-id}})
        r (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (= (:AvailableQty r) 18))))

(deftest query-by-id-and-delete
  (defcomponent :QIdDel
    (entity {:QIdDel/E {:X {:type :Int
                            :indexed true}}})
    (event {:QIdDel/FindByIdAndDel
            {:EId :UUID}})
    (dataflow :QIdDel/FindByIdAndDel
              [:delete :QIdDel/E {cn/id-attr :QIdDel/FindByIdAndDel.EId}]))
  (let [e (cn/make-instance :QIdDel/E {:X 100})
        e01 (first (tu/fresult (e/eval-all-dataflows {:QIdDel/Create_E {:Instance e}})))
        id (cn/id-attr e01)
        devt (cn/make-instance :QIdDel/FindByIdAndDel {:EId (cn/id-attr e01)})
        _ (doall (e/eval-all-dataflows devt))
        levt (cn/make-instance :QIdDel/Lookup_E {cn/id-attr id})
        lookup-result (doall (e/eval-all-dataflows levt))]
    (is (= :not-found (:status (first lookup-result))))))

(deftest query-and-delete
  (defcomponent :QDel
    (entity {:QDel/E {:X {:type :Int
                          :indexed true}}})
    (event {:QDel/FindAndDel
            {:X :Int}})
    (dataflow :QDel/FindAndDel
              {:QDel/E {:X? :QDel/FindAndDel.X}}
              [:delete :QDel/E {cn/id-attr (tu/append-id :QDel/E)}])
    (dataflow :QDel/DeleteById
              [:delete :QDel/E {cn/id-attr :QDel/DeleteById.EId}]))
  (let [e (cn/make-instance :QDel/E {:X 100})
        e01 (first (tu/fresult (e/eval-all-dataflows {:QDel/Create_E {:Instance e}})))
        id (cn/id-attr e01)
        devt (cn/make-instance :QDel/FindAndDel {:X 100})
        _ (doall (e/eval-all-dataflows devt))
        levt (cn/make-instance :QDel/Lookup_E {cn/id-attr id})
        lookup-result (doall (e/eval-all-dataflows levt))
        devt (cn/make-instance :QDel/FindAndDel {:X 100})
        del-result1 (doall (e/eval-all-dataflows devt))
        devt (cn/make-instance :QDel/DeleteById {:EId id})
        del-result2 (doall (e/eval-all-dataflows devt))]
    (is (= :not-found (:status (first del-result1))))
    (is (= :ok (:status (first del-result2))))
    (is (= :not-found (:status (first lookup-result))))))

(deftest delete-by-attribute
  (defcomponent :QDel
    (entity {:QDel/E {:X {:type :Int
                          :indexed true}}})
    (dataflow :QDel/DeleteByAttr
      [:delete :QDel/E {:X 50}])
    (dataflow :QDel/DeleteWithAlias
      [:delete :QDel/E {:X 100} :as [:DELRES]]
      :DELRES)
    (dataflow :QDel/find1
      {:QDel/E {:X? 100}})
    (dataflow :QDel/find2
      {:QDel/E {:X? 50}}))
  (let [e (cn/make-instance :QDel/E {:X 100})
        e1 (cn/make-instance :QDel/E {:X 50})
        _ (first (tu/fresult (e/eval-all-dataflows {:QDel/Create_E {:Instance e}})))
        _ (first (tu/fresult (e/eval-all-dataflows {:QDel/Create_E {:Instance e1}})))
        devt (cn/make-instance :QDel/DeleteByAttr {})
        delete-result (doall (e/eval-all-dataflows devt))
        devt (cn/make-instance :QDel/DeleteWithAlias {})
        delete-with-alias-result (doall (e/eval-all-dataflows devt))
        devt (cn/make-instance :QDel/find1 {})
        find-result-1 (doall (e/eval-all-dataflows devt))
        devt (cn/make-instance :QDel/find2 {})
        find-result-2 (doall (e/eval-all-dataflows devt))]
    (is (= :ok (:status (first delete-result))))
    (is (= 50 (get-in (first delete-result) [:result 0 :X])))
    (is (= :ok (:status (first delete-with-alias-result))))
    (is (= 100 (get-in (first delete-with-alias-result) [:result :X])))
    (is (= :not-found (:status (first find-result-1))))
    (is (= :not-found (:status (first find-result-2))))))

(deftest issue-255-query-non-indexed
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I255
     (entity {:I255/E {:X {:type :Int
                           :indexed true}
                       :Y :Int}})
     (dataflow :I255/Q1
               {:I255/E {:X? :I255/Q1.X
                         :Y? [:< 5]}})
     (dataflow :I255/Q2
               {:I255/E {:X? :I255/Q2.X
                         :Y? [:or [:> :X] [:= :I255/Q2.Y]]}}))
   (let [es [(cn/make-instance :I255/E {:X 10 :Y 4})
             (cn/make-instance :I255/E {:X 10 :Y 6})
             (cn/make-instance :I255/E {:X 10 :Y 3})
             (cn/make-instance :I255/E {:X 9 :Y 2})
             (cn/make-instance :I255/E {:X 10 :Y 20})]
         evts (map #(cn/make-instance :I255/Create_E {:Instance %}) es)
         f (comp first #(:result (first (e/eval-all-dataflows %))))
         insts (map f evts)]
     (is (every? true? (map #(cn/instance-of? :I255/E %) insts)))
     (let [r (tu/fresult (e/eval-all-dataflows {:I255/Q1 {:X 10}}))]
       (is (= (count r) 2))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (< (:Y e) 5)))))
     (let [r (tu/fresult (e/eval-all-dataflows {:I255/Q2 {:X 10 :Y 3}}))]
       (is (= (count r) 1))
       (doseq [e r]
         (is (and (= 10 (:X e))
                  (or (> (:Y e) 10)
                      (= (:Y e) 3)))))))))

(deftest test-unique-date-time
  (defcomponent :Dt01
    (entity {:Dt01/E {:Name :String
                      :LastAccountAccess
                      {:type :DateTime
                       ;; Disable this for postgres
                                        ;:unique true
                       }}}))
  (let [e (cn/make-instance :Dt01/E {:Name "Birkhe" :LastAccountAccess "2018-07-28T12:15:30"})
        e1 (first (tu/fresult (e/eval-all-dataflows {:Dt01/Create_E {:Instance e}})))
        id (cn/id-attr e1)
        e2 (first (tu/fresult (e/eval-all-dataflows {:Dt01/Lookup_E {cn/id-attr id}})))
        laa (:LastAccountAccess e2)]
    (is (cn/instance-of? :Dt01/E e2))
    (is (cn/same-instance? e1 e2))
    (is (= "2018-07-28T12:15:30" laa))))

(deftest query-in-for-each
  (defcomponent :Qfe
    (entity {:Qfe/E {:X {:type :Int
                         :indexed true}}})
    (record {:Qfe/R {:Y :Int}})
    (dataflow
     :Qfe/Evt1
     [:for-each :Qfe/E? {:Qfe/R {:Y '(+ 10 :Qfe/E.X)}}])
    (dataflow
     :Qfe/Evt2
     [:for-each {:Qfe/E {:X? 20}}
      {:Qfe/R {:Y '(+ 10 :Qfe/E.X)}}])
    (dataflow
     :Qfe/Evt3
     {:Qfe/E {:X :Qfe/Evt3.I} :as :E1}
     {:Qfe/E {:X '(+ :E1.X 1)} :as :E2}
     [:for-each {:Qfe/E {:X? 10}}
      {:Qfe/R {:Y :Qfe/E.X}}])
    (dataflow
     :Qfe/Evt4
     {:Qfe/E {:X :Qfe/Evt4.I} :as :E1}
     {:Qfe/E {:X '(+ :E1.X 1)} :as :E2}
     [:for-each :Qfe/E?
      {:Qfe/R {:Y :Qfe/E.X}}]))
  (defn make-e [x]
    (let [evt (cn/make-instance
               {:Qfe/Create_E
                {:Instance
                 (cn/make-instance {:Qfe/E {:X x}})}})
          e (first (tu/fresult (e/eval-all-dataflows evt)))]
      (is (cn/instance-of? :Qfe/E e))
      (is (= x (:X e)))
      e))
  (let [e1 (make-e 10)
        e2 (make-e 20)
        evt1 (cn/make-instance {:Qfe/Evt1 {}})
        rs1 (tu/fresult (e/eval-all-dataflows evt1))
        evt2 (cn/make-instance {:Qfe/Evt2 {:X 20}})
        rs2 (tu/fresult (e/eval-all-dataflows evt2))
        evt3 (cn/make-instance {:Qfe/Evt3 {:I 10}})
        rs3 (tu/fresult (e/eval-all-dataflows evt3))
        evt4 (cn/make-instance {:Qfe/Evt4 {:I 100}})
        rs4 (tu/fresult (e/eval-all-dataflows evt4))]
    (doseq [r rs1]
      (is (cn/instance-of? :Qfe/R r))
      (let [y (:Y r)]
        (is (or (= y 20) (= y 30)))))
    (is (= 1 (count rs2)))
    (is (= 30 (:Y (first rs2))))
    (is (= 2 (count rs3)))
    (doseq [r rs3]
      (is (= 10 (:Y r))))
    (is (= 6 (count rs4)))
    (doseq [r rs4]
      (let [y (:Y r)]
        (some #{y} #{10 11 20 100 101})))))

(deftest like-operator
  (defcomponent :LikeOpr
    (entity
     {:LikeOpr/E
      {:X {:type :String
           :indexed true}}})
    (dataflow
     :LikeOpr/Q
     {:LikeOpr/E
      {:X? [:like :LikeOpr/Q.S]}}))
  (let [e1 (cn/make-instance :LikeOpr/E {:X "hi"})
        e2 (cn/make-instance :LikeOpr/E {:X "bye"})
        e3 (cn/make-instance :LikeOpr/E {:X "hello"})
        [r1 r2 r3] (mapv #(tu/first-result {:LikeOpr/Create_E {:Instance %}}) [e1 e2 e3])
        qrs1 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "h%"}}))
        qrs2 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "b%"}}))
        qrs3 (tu/fresult (e/eval-all-dataflows {:LikeOpr/Q {:S "%ell%"}}))]
    (doseq [r qrs1]
      (is (or (cn/same-instance? r1 r) (cn/same-instance? r3 r))))
    (is (cn/same-instance? (first qrs2) r2))
    (is (cn/same-instance? (first qrs3) r3))))

(deftest query-command
  (defcomponent :QueryCommand
    (entity
     :QueryCommand/E
     {:X {:type :Int
          :indexed true}
      :Y :Int})
    (record
     :QueryCommand/F
     {:A :Int
      :B :Int})
    (dataflow
     :QueryCommand/FindE
     [:query :QueryCommand/FindE.Q])
    (dataflow
     :QueryCommand/EtoF
     [:query :QueryCommand/EtoF.Q :as :R]
     [:for-each
      :R
      {:QueryCommand/F
       {:A :%.X
        :B :%.Y}}]))
  (let [es (mapv
            #(tu/first-result
              (cn/make-instance
               {:QueryCommand/Create_E
                {:Instance
                 (cn/make-instance
                  {:QueryCommand/E
                   {:X %1 :Y %2}})}}))
            [10 20 30 40]
            [9 7 12 1])
        rs01 (:result
              (first
               (e/eval-all-dataflows
                (cn/make-instance
                 {:QueryCommand/FindE
                  {:Q {:QueryCommand/E?
                       {:where [:>= :X 20]
                        :order-by [:Y]}}}}))))
        rs02 (:result
              (first
               (e/eval-all-dataflows
                (cn/make-instance
                 {:QueryCommand/EtoF
                  {:Q {:QueryCommand/E?
                       {:where [:>= :X 20]
                        :order-by [:Y]}}}}))))]
    (is (every? #(>= (:X %) 20) rs01))
    (is (apply < (mapv :Y rs01)))
    (is (every? (partial cn/instance-of? :QueryCommand/F) rs02))
    (is (every? #(>= (:A %) 20) rs02))
    (is (apply < (mapv :B rs02)))))

(deftest ref-first-result
  (defcomponent :Rfr
    (record {:Rfr/R {:A :Int :B :Int}})
    (entity {:Rfr/E {:N {:type :String :indexed true} :X :Int}})
    (dataflow
     :Rfr/J
     {:Rfr/E {:N? :Rfr/J.N1} :as :E1}
     {:Rfr/E {:N? :Rfr/J.N2} :as :E2}
     {:Rfr/R {:A :E1.X :B :E2.X}}))
  (let [e1 (cn/make-instance :Rfr/E {:N "ABC" :X 10})
        e2 (cn/make-instance :Rfr/E {:N "EFG" :X 20})
        r1 (first (tu/fresult (e/eval-all-dataflows {:Rfr/Create_E {:Instance e1}})))
        r2 (first (tu/fresult (e/eval-all-dataflows {:Rfr/Create_E {:Instance e2}})))
        k1 (first (tu/fresult (e/eval-all-dataflows {:Rfr/J {:N1 "EFG" :N2 "ABC"}})))]
    (is (cn/instance-of? :Rfr/R k1))
    (is (= 20 (:A k1)))
    (is (= 10 (:B k1)))))

(deftest select-all
  (defn- make-query [env]
    (let [xs (env :SelAll/FindE.Xs)]
      (str "SELECT * FROM " (stu/entity-table-name :SelAll/E)
           " WHERE (" (stu/attribute-column-name :X) " in ("
           (s/join "," (map str xs)) "))")))
  (defcomponent :SelAll
    (entity
     {:SelAll/E
      {:X {:type :Int
           :indexed true}}})
    (dataflow
     :SelAll/FindE
     [:query {:SelAll/E? make-query}]))
  (let [es (mapv #(cn/make-instance
                   {:SelAll/E
                    {:X %}})
                 (range 5))
        _ (mapv #(tu/first-result
                  (cn/make-instance
                   {:SelAll/Create_E
                    {:Instance %}}))
                es)
        rs (tu/fresult
            (e/eval-all-dataflows
             (cn/make-instance
              {:SelAll/FindE
               {:Xs [2 1 4]}})))]
    (is (= 3 (count rs)))
    (is (every? (fn [r] (some #{(:X r)} [2 1 4])) rs))))

(deftest aggregates
  (defcomponent :Agrgts
    (entity :Agrgts/E {:X :Int})
    (record :Agrgts/Result {:R :Int})
    (dataflow
     :Agrgts/Evt1
     {:Agrgts/E?
      {:where [:> :X 3]
       :count :X
       :sum :X
       :avg :X
       :max :X
       :min :X}
      :as [:R]}
     :R))
  (let [es (mapv #(tu/first-result
                   {:Agrgts/Create_E
                    {:Instance
                     {:Agrgts/E {:X %}}}})
                 [1 2 3 4 5 6])
        r (tu/result {:Agrgts/Evt1 {}})]
    (is (= (count es) 6))
    (is (every? (partial cn/instance-of? :Agrgts/E) es))
    (is (= (assoc r :avg (int (:avg r))) {:count 3, :sum 15, :avg 5, :max 6, :min 4}))))

(deftest issue-766-fn-in-query
  (defcomponent :I766
    (entity
     :I766/E
     {:X {:type :Int :indexed true}
      :Y {:type :Int :indexed true}})
    (defn i766-f [y] (* y 10))
    (dataflow
     :I766/Q
     {:I766/E
      {:X? [:>= :I766/Q.X] :Y? '(agentlang.test.query/i766-f :I766/Q.Y)}}))
  (let [es (mapv #(tu/first-result
                   {:I766/Create_E
                    {:Instance
                     {:I766/E {:X %1 :Y %2}}}})
                 [1 2 3 4 5] [10 100 200 300 200])
        rs (tu/result
            {:I766/Q {:X 2 :Y 20}})]
    (is (= 2 (count rs)))
    (is (every?
         true?
         (mapv #(and (or (= 3 (:X %)) (= 5 (:X %)))
                     (= 200 (:Y %)))
               rs)))))

(deftest ai-test
  (defcomponent :Acme
    (entity :Acme/Sales {:Date :DateTime :Price :Decimal})
    (dataflow :Acme/SalesSummary {:Acme/Sales? {:where [:> :Date :Acme/SalesSummary.Date] :sum :Price :avg :Price}})
    (dataflow :Acme/SalesRange {:Acme/Sales? {:where [:> :Date :Acme/SalesRange.Date] :max :Price :min :Price}})
    (dataflow :Acme/SalesCount {:Acme/Sales? {:where [:> :Date :Acme/SalesCount.Date] :count :Price}})
    (dataflow :Acme/SalesBelowOrEqual1500 {:Acme/Sales? {:where [:<= :Price 1500] :order-by [:Date]}})
    (dataflow :Acme/SalesBelowOrEqual1500Desc {:Acme/Sales? {:where [:<= :Price 1500] :order-by [[:Date :desc]]}})
    (dataflow :Acme/SalesByPrice {:Acme/Sales? {:order-by [:Price]}}))
  (let [all-sales? (partial every? (partial cn/instance-of? :Acme/Sales))]
    (is all-sales?
        (mapv #(tu/first-result
                {:Acme/Create_Sales
                 {:Instance
                  {:Acme/Sales {:Date %1 :Price %2}}}})
              ["2023-07-04T10:56:46.41097409"
               "2023-05-04T10:56:46.41097409"
               "2023-07-03T10:56:46.41097409"
               "2023-06-04T10:56:46.41097409"
               "2023-04-04T10:56:46.41097409"]
              [1000.0 1500.0 2000.0 2200.0 800.0]))
    (let [rs (tu/first-result {:Acme/SalesSummary {:Date "2023-06-04T10:56:46.41097409"}})]
      (is (= (float (:sum rs)) 3000.0))
      (is (= (float (:avg rs)) 1500.0)))
    (let [rs (tu/first-result {:Acme/SalesRange {:Date "2023-06-04T10:56:46.41097409"}})]
      (is (= (float (:max rs)) 2000.0))
      (is (= (float (:min rs)) 1000.0)))
    (let [rs (tu/first-result {:Acme/SalesCount {:Date "2023-06-04T10:56:46.41097409"}})]
      (is (= (:count rs) 2)))
    (let [rs (tu/result {:Acme/SalesBelowOrEqual1500 {}})]
      (is (all-sales? rs))
      (is (= (count rs) 3))
      (let [rs2 (tu/result {:Acme/SalesBelowOrEqual1500Desc {}})]
        (is (all-sales? rs2))
        (is (= (count rs2) 3))
        (is (= (reverse rs) rs2))))
    (let [rs (tu/result {:Acme/SalesByPrice {}})]
      (is (all-sales? rs))
      (is (= (count rs) 5))
      (is (= (float (:Price (first rs))) 800.0))
      (is (= (float (:Price (last rs))) 2200.0)))))

(deftest issue-1123-query-deleted
  (defcomponent :I1123
    (entity
     :I1123/E
     {:X :Int
      :Id {:type :Int :guid true}})
    (dataflow
     :I1123/QueryAllDeleted
     {:I1123/E?
      {:deleted true}})
    (dataflow
     :I1123/QueryDeleted
     {:I1123/E?
      {:deleted true
       :where [:= :Id :I1123/QueryDeleted.Id]}}))
  (let [[e1 e2 e3 :as es] (mapv #(tu/first-result
                                  {:I1123/Create_E
                                   {:Instance
                                    {:I1123/E {:Id % :X (* 2 %)}}}})
                                [1 2 3])
        e? (partial cn/instance-of? :I1123/E)]
    (is (and (= 3 (count es)) (every? e? es)))
    (is (cn/same-instance? e1 (tu/first-result {:I1123/Lookup_E {:Id 1}})))
    (is (not (seq (tu/result {:I1123/QueryDeleted {:Id 1}}))))
    (is (cn/same-instance? e1 (tu/first-result {:I1123/Delete_E {:Id 1}})))
    (is (not (e? (tu/first-result {:I1123/Lookup_E {:Id 1}}))))
    (is (cn/same-instance? e1 (tu/first-result {:I1123/QueryDeleted {:Id 1}})))
    (is (cn/same-instance? e2 (tu/first-result {:I1123/Delete_E {:Id 2}})))
    (let [es (tu/result {:I1123/QueryAllDeleted {}})]
      (is (and (= 2 (count es)) (every? e? es)))))))
