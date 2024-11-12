(ns agentlang.test.features05
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.pprint :as pp]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.util :as u]
            [agentlang.inference :as i]
            [agentlang.lang
             :refer [component record event entity view
                     relationship dataflow rule
                     resolver inference]]
            [agentlang.lang.syntax :as ls]
            [agentlang.lang.raw :as lr]
            [agentlang.lang.internal :as li]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest rule-basic
  (defcomponent :Rule01
    (entity :Rule01/A {:Id :Identity :X :Int})
    (entity :Rule01/B {:Id :Identity :Y :Int})
    (rule
     :Rule01/R1
     {:Rule01/A {:X 100} :as :A}
     {:Rule01/B {:Y [:<= 200]} :as :B}
     :then
     {:Rule01/Event1 {:A :A.Id}}
     {:Rule01/Event2 {:B :B.Id}})
    (rule
     :Rule01/R2
     {:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}
     :then
     {:Rule01/Event3 {:A :A.Id}}
     {:meta {:priority 10 :passive true :category :Rule01.Abc}}))
  (is (= #{:R1 :R2} (set (keys (cn/fetch-rules :Rule01)))))
  (let [spec (cn/fetch-rule :Rule01/R1)]
    (is (= [{:Rule01/A {:X 100} :as :A}
            {:Rule01/B {:Y [:<= 200]} :as :B}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event1 {:A :A.Id}}
            {:Rule01/Event2 {:B :B.Id}}]
           (cn/rule-consequence spec)))
    (is (cn/rule-has-least-priority? spec))
    (is (not (cn/rule-is-passive? spec)))
    (is (not (cn/rule-category spec))))
  (let [spec (cn/fetch-rule :Rule01/R2)]
    (is (= [{:Rule01/A {:X [:or [:= 0] [:= 100]]} :as :A}]
           (cn/rule-condition spec)))
    (is (= [{:Rule01/Event3 {:A :A.Id}}]
           (cn/rule-consequence spec)))
    (is (= 10 (cn/rule-priority spec)))
    (is (cn/rule-is-passive? spec))
    (is (= :Rule01.Abc (cn/rule-category spec))))
  (is (cn/remove-rule :Rule01/R2))
  (is (not (cn/fetch-rule :Rule01/R2)))
  (is (cn/fetch-rule :Rule01/R1))
  (is (= #{:R1} (set (keys (cn/fetch-rules :Rule01))))))

(deftest rule-fire-01
  (defcomponent :Rf01
    (entity :Rf01/A {:Id :Identity :X :Int})
    (entity :Rf01/B {:Id :Identity :Y :Int :A :UUID})
    (dataflow :Rf01/BbyA {:Rf01/B {:A? :Rf01/BbyA.A}})
    (rule
     :Rf01/R1
     {:Rf01/A {:X 100} :as :InstA}
     :then
     {:Rf01/B {:Y 100 :A :InstA.Id}})
    (rule
     :Rf01/R2
     {:Rf01/A {:X [:>= 500]} :as :InstA}
     :then
     {:Rf01/B {:Y '(* 100 :InstA.X) :A :InstA.Id}})
    (rule
     :Rf01/R3
     [:delete {:Rf01/A {:X 100}}]
     :then
     [:delete :Rf01/B {:A :Rf01/A.Id}]))
  (let [make-a (fn [x]
                 (let [r (first
                          (tu/eval-all-dataflows
                           {:Rf01/Create_A
                            {:Instance
                             {:Rf01/A {:X x}}}}))]
                   [(:env r) (first (:result r))]))
        delete-a (fn [a]
                   (let [r (first
                            (tu/eval-all-dataflows
                             {:Rf01/Delete_A {:Id (:Id a)}}))]
                     [(:env r) (first (:result r))]))
        [[env1 a1] [env2 a2]] (mapv make-a [10 100])
        a? (partial cn/instance-of? :Rf01/A)
        b? (partial cn/instance-of? :Rf01/B)
        b-by-a (fn [a] (tu/eval-all-dataflows {:Rf01/BbyA {:A (:Id a)}}))
        is-no-b-by-a (fn [a] (= :not-found (:status (first (b-by-a a)))))
        is-b-by-a (fn [rname a]
                    (let [bs (:result (first (b-by-a a)))]
                      (is (every? b? bs))
                      (is (= 1 (count bs)))
                      (if (= rname :R2)
                        (is (= (* 100 (:X a)) (:Y (first bs))))
                        (is (= 100 (:Y (first bs)))))))
        is-b-in-env (fn [env]
                      (is (b? (first (:result (first (deref (first (env/rule-futures env)))))))))]
    (is (every? a? [a1 a2]))
    (is (nil? (seq (env/rule-futures env1))))
    (is-b-in-env env2)
    (is-no-b-by-a a1)
    (is-b-by-a :R1 a2)
    (let [[env3 a3] (make-a 500), [env4 a4] (make-a 501)]
      (is (every? a? [a3 a4]))
      (is-b-in-env env3)
      (is-b-in-env env4)
      (is-b-by-a :R2 a3)
      (is-b-by-a :R2 a4)
      (let [[env a] (delete-a a4)]
        (is (cn/same-instance? a a4))
        (is (nil? (seq (env/rule-futures env)))))
      (is-b-by-a :R2 a4)
      (is-b-by-a :R1 a2)
      (let [[env a] (delete-a a2)]
        (is (cn/same-instance? a a2))
        (let [bs (:result (first (deref (first (env/rule-futures env)))))]
          (is (= 1 (count bs)))
          (is (= 100 (:Y (first bs))))
          (is (= (:Id a2) (:A (first bs)))))
        (is-no-b-by-a a2)))))

(deftest issue-1252-rules-inference-raw-syntax
  (defcomponent :I1252R
    (entity :I1252R/A {:Id :Identity :X :Int})
    (entity :I1252R/B {:Id :Identity :Y :Int})
    (rule
     :I1252R/R1
     {:I1252R/A {:X 10}}
     :then
     {:I1252R/B {:Y 200}}
     {:meta {:priority 1}})
    (inference
     :I1252R/I1
     {:instructions "this is a test"}))
  (is (= (lr/as-edn :I1252R)
         '(do
            (component :I1252R)
            (entity :I1252R/A {:Id :Identity, :X :Int})
            (entity :I1252R/B {:Id :Identity, :Y :Int})
            (rule
             :I1252R/R1
             #:I1252R{:A {:X 10}}
             :then
             #:I1252R{:B {:Y 200}}
             {:meta {:priority 1}})
            (inference
             :I1252R/I1
             {:instructions "this is a test"})))))

(deftest issue-1300-joins
  (defcomponent :I1300J
    (entity
     :I1300J/Customer
     {:Id {:type :Int :guid true}
      :Name :String})
    (entity
     :I1300J/Order
     {:Id {:type :Int :guid true}
      :CustomerId :Int
      :Date :Now})
    (dataflow
     :I1300J/CustomerOrders
     {:I1300J/Order? {}
      :join [{:I1300J/Customer {:Id? :I1300J/Order.CustomerId}}]
      :with-attributes {:CustomerName :I1300J/Customer.Name
                        :CustomerId :I1300J/Customer.Id
                        :OrderId :I1300J/Order.Id}})
    (dataflow
     :I1300J/OrdersWithCustomers
     {:I1300J/Order? {}
      :left-join [{:I1300J/Customer {:Id? :I1300J/Order.CustomerId}}]}))
  (let [cust (fn [id name]
               (tu/first-result
                {:I1300J/Create_Customer
                 {:Instance
                  {:I1300J/Customer {:Id id :Name name}}}}))
        cust? (partial cn/instance-of? :I1300J/Customer)
        order (fn [id cust-id]
                (tu/first-result
                 {:I1300J/Create_Order
                  {:Instance
                   {:I1300J/Order {:Id id :CustomerId cust-id}}}}))
        order? (partial cn/instance-of? :I1300J/Order)
        cs (mapv cust [1001 1002 1003] ["jay" "mat" "joe"])
        _ (is (every? cust? cs))
        os (mapv order [1 2 3 4 5] [1001 1002 1001 1003 1003])
        _ (is (every? order? os))
        rs (tu/result {:I1300J/CustomerOrders {}})]
    (is (and (= 5 (count rs)) (is (every? map? rs))))
    (let [rs1 (filter #(= 1001 (:CustomerId %)) rs)
          p? (fn [ordid] (is (= 1 (count (filter #(= ordid (:OrderId %)) rs1)))))]
      (is (= 2 (count rs1)))
      (p? 1)
      (p? 3))
    (let [rs (tu/result {:I1300J/OrdersWithCustomers {}})]
      (is (= 5 (count rs)))
      (is (every? map? rs))
      (is (= (+ 1 2 3 4 5)
             (reduce + 0 (mapv :Id rs)))))))

(deftest query-with-attrs
  (defcomponent :Qwa
    (entity
     :Qwa/E
     {:Id {:type :Int :guid true}
      :X :Int})
    (dataflow
     :Qwa/Q
     [:query {:Qwa/E? {:where [:>= :X :Qwa/Q.X]
                       :with-attributes {:id :Qwa/E.Id :v :Qwa/E.X}}}]))
  (let [mke (fn [id x]
              (tu/first-result
               {:Qwa/Create_E
                {:Instance
                 {:Qwa/E {:Id id :X x}}}}))
        e? (partial cn/instance-of? :Qwa/E)
        es (mapv mke [1 2 3] [5 10 15])]
    (is (every? e? es))
    (is (= 25 (reduce + 0 (mapv :v (tu/result {:Qwa/Q {:X 10}})))))))

(deftest issue-1301-views
  (defcomponent :I1301
    (entity
     :I1301/Customer
     {:Id {:type :Int :guid true}
      :Name :String})
    (entity
     :I1301/Order
     {:Id {:type :Int :guid true}
      :CustomerId :Int
      :Date :Now})
    (view
     :I1301/CustomerOrder
     {:CustomerName :I1301/Customer.Name
      :CustomerId :I1301/Customer.Id
      :OrderId :I1301/Order.Id
      :query {:I1301/Order? {}
              :join [{:I1301/Customer {:Id? :I1301/Order.CustomerId}}]}})
    (view
     :I1301/CustomerName
     {:Name :I1301/Customer.Name
      :query {:I1301/Customer? {}}}))
  (let [cust (fn [id name]
               (tu/first-result
                {:I1301/Create_Customer
                 {:Instance
                  {:I1301/Customer {:Id id :Name name}}}}))
        cust? (partial cn/instance-of? :I1301/Customer)
        order (fn [id cust-id]
                (tu/first-result
                 {:I1301/Create_Order
                  {:Instance
                   {:I1301/Order {:Id id :CustomerId cust-id}}}}))
        order? (partial cn/instance-of? :I1301/Order)
        cs (mapv cust [1001 1002 1003] ["jay" "mat" "joe"])
        _ (is (every? cust? cs))
        os (mapv order [1 2 3 4 5] [1001 1002 1001 1003 1003])
        _ (is (every? order? os))
        rs (tu/result {:I1301/LookupAll_CustomerOrder {}})
        co? (partial cn/instance-of? :I1301/CustomerOrder)]
    (is (and (= 5 (count rs)) (is (every? co? rs))))
    (let [rs1 (filter #(= 1001 (:CustomerId %)) rs)
          p? (fn [ordid] (is (= 1 (count (filter #(= ordid (:OrderId %)) rs1)))))]
      (is (= 2 (count rs1)))
      (p? 1)
      (p? 3))
    (let [rs (tu/result {:I1301/LookupAll_CustomerName {}})
          cn? (partial cn/instance-of? :I1301/CustomerName)]
      (is (= 3 (count rs)))
      (is (every? cn? rs))
      (is (= (set ["jay" "mat" "joe"]) (set (mapv :Name rs)))))))

(deftest views-on-contains
  (defcomponent :Voc
    (entity
     :Voc/Family
     {:Id :Identity
      :FamilyName :String})
    (entity
     :Voc/FamilyMember
     {:Id :Identity
      :Name :String})
    (relationship
     :Voc/Members
     {:meta {:contains [:Voc/Family
                        :Voc/FamilyMember]
             :cascade-on-delete true}})
    (view
     :Voc/MembersView
     {:query {:Voc/FamilyMember? {}
              :join [{:Voc/Family {:Id? (li/make-ref :Voc/FamilyMember li/parent-attr)}}]}
      :FamilyName :Voc/Family.FamilyName
      :Name :Voc/FamilyMember.Name}))
  (let [mkfm #(tu/first-result
               {:Voc/Create_Family
                {:Instance
                 {:Voc/Family
                  {:FamilyName %}}}})
        f? (partial cn/instance-of? :Voc/Family)
        [f1 f2] (mapv mkfm ["one" "two"])
        mkfmm (fn [fid n]
                (tu/first-result
                 {:Voc/Create_FamilyMember
                  {:Instance {:Voc/FamilyMember {:Name n}}
                   li/path-attr (str "/Family/" fid "/Members/")}}))
        fm? (partial cn/instance-of? :Voc/FamilyMember)
        f1id (:Id f1)
        f2id (:Id f2)
        fm1 (mkfmm f1id "a")
        fm2 (mkfmm f2id "b")
        fm3 (mkfmm f1id "c")]
    (is (every? f? [f1 f2]))
    (is (every? fm? [fm1 fm2 fm3]))
    (let [rs (tu/result {:Voc/LookupAll_MembersView {}})]
      (is (every? (partial cn/instance-of? :Voc/MembersView) rs))
      (is (= 3 (count rs)))
      (let [r (filter #(= "two" (:FamilyName %)) rs)]
        (is (= 1 (count r)))
        (is (= "b" (:Name (first r)))))
      (let [r (filter #(= "one" (:FamilyName %)) rs)]
        (is (= 2 (count r)))
        (is (every? #(let [n (:Name %)]
                       (or (= n "a") (= n "c")))
                    r))))))

(deftest nil-parent-id
  (defcomponent :Npid
    (entity :Npid/A {:Id {:type :Int :guid true}})
    (entity :Npid/B {:Id {:type :Int :guid true}
                     :No {:type :Int :id true}})
    (relationship :Npid/AB {:meta {:contains [:Npid/A :Npid/B]}}))
  (let [a1 (tu/first-result
            {:Npid/Create_A
             {:Instance
              {:Npid/A {:Id 1}}}})
        b1 (tu/first-result
            {:Npid/Create_B
             {:Instance
              {:Npid/B {:Id 101 :No 23}}}})
        a? (partial cn/instance-of? :Npid/A)
        b? (partial cn/instance-of? :Npid/B)]
    (is (a? a1))
    (is (b? b1))
    (is (nil? (li/parent-attr b1)))
    (let [b2 (tu/first-result
              {:Npid/Create_B
               {:Instance
                {:Npid/B {:Id 102 :No 24}}
                li/path-attr "/A/1/AB/"}})]
      (is (b? b2))
      (is (= "1" (li/parent-attr b2))))))

(deftest issue-1377-pattern-doc
  (defcomponent :I1377
    (entity :I1377/E {:X :Int})
    (dataflow
     :I1377/MakeE
     {:I1377/E {:X :I1377/MakeE.X}
      :meta {:doc "Create a new instance of E"}})
    (dataflow
     :I1377/FindE
     {:I1377/E {:X? :I1377/FindE.X}
      :meta {:doc "Find instances of E by X"}}))
  (let [e1 (tu/first-result {:I1377/MakeE {:X 100}})
        e2 (tu/first-result {:I1377/MakeE {:X 200}})]
    (is (every? (partial cn/instance-of? :I1377/E) [e1 e2]))
    (is (cn/same-instance? e1 (tu/first-result {:I1377/FindE {:X 100}})))))

(deftest issue-1377-syntax
  (let [e1 (ls/upsert {ls/record-tag :Acme/Error
                       ls/attrs-tag {:Message "Pattern error"}})
        e2 (ls/upsert {ls/record-tag :Acme/NoData
                       ls/attrs-tag {:Message "No data found"}})
        p1 (ls/upsert {ls/record-tag :Acme/Person
                       ls/meta-tag {:doc "Create a new Person"}
                       ls/attrs-tag {:Name "Joe"}
                       ls/alias-tag :P
                       ls/throws-tag {ls/error-tag e1 ls/not-found-tag e2}})
        p2 (ls/query-upsert {ls/record-tag :Acme/Person
                             ls/meta-tag {:doc "Fetch Person by name"}
                             ls/attrs-tag {:Name? "Joe"}
                             ls/alias-tag :P})
        p3 (ls/query-object {ls/record-tag :Acme/Person?
                             ls/meta-tag {:doc "Query Person by name"}
                             ls/query-tag {:where [:= :Name "Joe"]}
                             ls/alias-tag [:P]})
        has-doc? #(string? (:doc (ls/meta-tag %)))]
    (is (every? has-doc? [p1 p2 p3]))
    (is (= (ls/raw p1)
           {:Acme/Person {:Name "Joe"},
            :meta {:doc "Create a new Person"},
            :as :P,
            :throws
            {:error #:Acme{:Error {:Message "Pattern error"}},
             :not-found #:Acme{:NoData {:Message "No data found"}}}}))
    (is (= (ls/raw p2) {:Acme/Person {:Name? "Joe"}, :meta {:doc "Fetch Person by name"}, :as :P}))
    (is (= (ls/raw p3) {:Acme/Person? {:where [:= :Name "Joe"]}, :meta {:doc "Query Person by name"}, :as [:P]}))
    (is (every? has-doc? (mapv #(ls/introspect (ls/raw %)) [p1 p2 p3])))))

(deftest raw-resolver
  (defcomponent :RR
    (entity :RR/E {:X :Int})
    (resolver
     :RR/R1
     {:type :remote :path [:RR/E]})
    (event :RR/Evt {:Y :Int})
    (dataflow
     :RR/Evt
     {:RR/E {:X :RR/Evt.Y}}))
  (is (= (lr/as-edn :RR)
         '(do
            (component :RR)
            (entity :RR/E {:X :Int})
            (resolver :RR/R1 {:type :remote, :path [:RR/E]})
            (event :RR/Evt {:Y :Int})
            (dataflow :RR/Evt #:RR{:E {:X :RR/Evt.Y}}))))
  (cn/remove-resolver :RR/R1)
  (is (= (lr/as-edn :RR)
         '(do
            (component :RR)
            (entity :RR/E {:X :Int})
            (event :RR/Evt {:Y :Int})
            (dataflow :RR/Evt #:RR{:E {:X :RR/Evt.Y}})))))

(deftest resolver-component-api
  (let [db (atom nil)]
    (defcomponent :Rc1
      (entity :Rc1/E1 {:X :Int})
      (entity :Rc1/E2 {:Y :Int})
      (resolver
       :Rc1/R1
       {:paths [:Rc1/E1]
        :with-methods {:create #(swap! db conj %)
                       :query (fn [_] @db)}})
      (require '[agentlang.resolver.remote])
      (resolver
       :Rc1/R2
       {:paths [:Rc2/E2]
        :type :remote})))
  (let [db (atom nil)]
    (defcomponent :Rc2
      (entity :Rc2/E1 {:X :Int})
      (resolver
       :Rc2/R1
       {:require {:precond #(reset! db [])}
        :paths [:Rc2/E1]
        :with-methods {:create #(reset! db %)}})))
  (u/run-init-fns)
  (let [cr-e1 #(tu/first-result {:Rc1/Create_E1
                                 {:Instance
                                  {:Rc1/E1 {:X %}}}})
        e1? (partial cn/instance-of? :Rc1/E1)
        e1s (mapv cr-e1 [1 2 3])]
    (is (every? e1? e1s))
    (let [e1s (tu/result {:Rc1/LookupAll_E1 {}})]
      (is (and (= 3 (count e1s)) (every? e1? e1s)))))
  (let [res? (fn [m k] (map? (k m)))
        rs (cn/find-resolvers :Rc1)]
    (is (= 2 (count (keys rs))))
    (is (every? (partial res? rs) [:R1 :R2]))
    (is (cn/remove-resolver :Rc1/R1))
    (let [rs (cn/find-resolvers :Rc1)]
      (is (= 1 (count (keys rs))))
      (is (every? (partial res? rs) [:R2])))
    (let [rs (cn/find-resolvers :Rc2)]
      (is (= 1 (count (keys rs))))
      (is (every? (partial res? rs) [:R1])))))

(deftest component-definition
  (component
   :Cd01
   {:clj-import '[(:require [agentlang.lang.datetime :as dt]
                          [clojure.java.io :as io])
                  (:use [agentlang.util])]
    :refer [:Acme.Core]})
  (entity :Cd01/R {:X :Int})
  (let [[_ cdef] (cn/component-definition :Cd01)]
    (is (= '[(:require [agentlang.lang.datetime :as dt]
                       [clojure.java.io :as io])
             (:use [agentlang.util])]
           (:clj-import cdef)))
    (is (= [:Acme.Core] (cn/component-references :Cd01)))
    (is (= {:require '[[agentlang.lang.datetime :as dt] [clojure.java.io :as io]],
            :use '[[agentlang.util]]}
           (cn/component-clj-imports :Cd01)))
    (cn/set-component-clj-imports!
     :Cd01
     {:require '[[java.io :as io] [abc.kk :as kk]]
      :use '[agentlang.util]})
    (is (= {:require '[[java.io :as io] [abc.kk :as kk]]
            :use '[agentlang.util]}
           (cn/component-clj-imports :Cd01)))
    (cn/set-component-references! :Cd01 [:Acme.Core :Accounts.Core])
    (is (= [:Acme.Core :Accounts.Core] (cn/component-references :Cd01)))))

(deftest fns-in-raw
  (component :Fir)
  (entity :Fir/E {:X :Int})
  (event :Fir/Evt {:X :Int})
  (is (= '(do
            (component :Fir)
            (entity :Fir/E {:X :Int})
            (event :Fir/Evt {:X :Int}))
         (lr/as-edn :Fir)))
  (lr/create-function :Fir 'compute-x '[event-x] '(* event-x 10))
  (is (= '(do
            (component :Fir)
            (entity :Fir/E {:X :Int})
            (event :Fir/Evt {:X :Int})
            (defn compute-x [event-x] (* event-x 10)))
         (lr/as-edn :Fir)))
  (dataflow :Fir/Evt {:Fir/E {:X '(compute-x :Fir/Evt.X)}})
  (is (= '(do
            (component :Fir)
            (entity :Fir/E {:X :Int})
            (event :Fir/Evt {:X :Int})
            (defn compute-x [event-x] (* event-x 10))
            (dataflow :Fir/Evt #:Fir{:E {:X '(compute-x :Fir/Evt.X)}}))
         (lr/as-edn :Fir)))
  (is (= '[event-x] (lr/get-function-params :Fir 'compute-x)))
  (is (= '(* event-x 10) (lr/get-function-body :Fir 'compute-x)))
  (lr/update-function :Fir 'compute-x '[x] '(- x 100))
  (is (= '(do
            (component :Fir)
            (entity :Fir/E {:X :Int})
            (event :Fir/Evt {:X :Int})
            (defn compute-x [x] (- x 100))
            (dataflow :Fir/Evt #:Fir{:E {:X '(compute-x :Fir/Evt.X)}}))
         (lr/as-edn :Fir)))
  (is (= '[compute-x] (lr/get-function-names :Fir)))
  (lr/delete-function :Fir 'compute-x)
  (is (= '(do
            (component :Fir)
            (entity :Fir/E {:X :Int})
            (event :Fir/Evt {:X :Int})
            (dataflow :Fir/Evt #:Fir{:E {:X '(compute-x :Fir/Evt.X)}}))
         (lr/as-edn :Fir)))
  (is (nil? (seq (lr/get-function-names :Fir)))))

(deftest before-update-return-value
  (defcomponent :Burv
    (entity
     :Burv/E
     {:Id {:type :Int :guid true}
      :X :Int
      :Y {:type :Int :default 0}})
    (defn calculate-y [inst]
      (assoc inst :Y (* 10 (:X inst))))
    (dataflow
     [:before :update :Burv/E]
     [:eval '(agentlang.test.features05/calculate-y :Instance)]))
  (let [e1 (tu/first-result
            {:Burv/Create_E
             {:Instance
              {:Burv/E {:Id 1 :X 20}}}})]
    (is (cn/instance-of? :Burv/E e1))
    (is (zero? (:Y e1)))
    (let [e2 (tu/first-result
              {:Burv/Update_E
               {:Id 1 :Data {:X 30}}})]
      (is (cn/instance-eq? e1 e2))
      (is (= 30 (:X e2)))
      (is (= 300 (:Y e2)))
      (let [e3 (tu/first-result
                {:Burv/Lookup_E {:Id 1}})]
        (is (cn/same-instance? e2 e3))))))

(deftest rethrow
  (defcomponent :Rethrow
    (entity :Rethrow/E {:Id {:type :Int :guid true} :X :Int})
    (defn raise-an-error [] (u/throw-ex "ERROR!"))
    (dataflow
     :Rethrow/Error
     [:eval '(agentlang.test.features05/raise-an-error)
      :throws
      {:error {:Rethrow/E {:Id 1 :X 200}}}])
    (dataflow
     :Rethrow/FindE
     {:Rethrow/E {:X? :Rethrow/FindE.E}
      :throws
      {:not-found {:Rethrow/E {:Id :Rethrow/FindE.E :X 200}}}}))
  (let [r (first (tu/eval-all-dataflows {:Rethrow/Error {}}))]
    (is (= :error (:status r)))
    (is (cn/instance-of? :Rethrow/E (first (:result r))))
    (is (= "ERROR!" (:message r))))
  (let [r (first (tu/eval-all-dataflows {:Rethrow/FindE {:E 100}}))]
    (is (= :not-found (:status r)))
    (is (cn/instance-of? :Rethrow/E (first (:result r))))
    (is (= 100 (:Id (first (:result r)))))
    (is (nil? (:message r)))))

(deftest implicit-rethrow
  (let [db (atom [])
        assert-id! (fn [e]
                     (when (neg? (:Id e))
                       (u/throw-ex "Id cannot be a negative integer")))]
    (defcomponent :IRethrow
      (entity :IRethrow/E {:Id {:type :Int :guid true} :X :Int})
      (record :IRethrow/FailureInfo {:Reason :Any})
      (resolver
       :IRethrow/R
       {:paths [:IRethrow/E]
        :with-methods {:create #(do (assert-id! %) (swap! db conj %) %)
                       :query (fn [[_ {[_ _ id] :where}]]
                                (assert-id! {:Id id})
                                (when-let [e (first (filter #(= id (:Id %)) @db))]
                                  [e]))}})
      (dataflow
       :IRethrow/FindE
       {:IRethrow/E
        {:Id? :IRethrow/FindE.E}
        :as :E1
        :throws
        {:not-found {:IRethrow/FailureInfo {:Reason '(str "Data not found for Id " :IRethrow/FindE.E)}}
         :error {:IRethrow/FailureInfo {:Reason :Error}}}}
       :E1)
      (dataflow
       :IRethrow/CreateE
       {:IRethrow/E
        {:Id :IRethrow/CreateE.Id
         :X :IRethrow/CreateE.X}
        :throws
        {:error {:IRethrow/FailureInfo {:Reason :Error}}}}))
    (u/run-init-fns)
    (let [e? (partial cn/instance-of? :IRethrow/E)
          fi? (partial cn/instance-of? :IRethrow/FailureInfo)]
      (let [r (first (tu/eval-all-dataflows {:IRethrow/FindE {:E 100}}))
            obj (first (:result r))]
        (is (= :not-found (:status r)))
        (is (fi? obj))
        (is (= "Data not found for Id 100" (:Reason obj))))
      (let [e (tu/first-result
               {:IRethrow/Create_E
                {:Instance
                 {:IRethrow/E {:Id 100 :X 20}}}})]
        (is (e? e))
        (is (cn/same-instance? e (tu/first-result {:IRethrow/FindE {:E 100}}))))
      (let [r (first (tu/eval-all-dataflows {:IRethrow/FindE {:E -1}}))
            obj (first (:result r))]
        (is (= :error (:status r)))
        (is (fi? obj))
        (is (= :error (get-in obj [:Reason :status])))
        (is (= (get-in obj [:Reason :message]) (:message r))))
      (let [r (first (tu/eval-all-dataflows {:IRethrow/CreateE {:Id 2 :X 20}}))]
        (is (e? (first (:result r)))))
      (let [r (first (tu/eval-all-dataflows {:IRethrow/CreateE {:Id -2 :X 20}}))
            obj (first (:result r))]
        (is (= :error (:status r)))
        (is (fi? obj))
        (is (= :error (get-in obj [:Reason :status])))
        (is (= (get-in obj [:Reason :message]) (:message r)))))))

(deftest throws-syntax
  (let [p (fn [p1]
            (let [ir1  (ls/introspect p1)]
              (is (= p1 (ls/raw ir1)))))]
    (p {:Acme/Employee
        {:Email "joe@acme.com"
         :Id 101}
        :as :E
        :throws
        {:error {:Acme/FailedToCreateEmployee {}}}})
    (p {:Acme/Employee
        {:Email? "joe@acme.com"
         :Id 102}
        :as [:E]
        :throws
        {:error {:Acme/FailedToUpdateEmployee {:Email "joe@acme.com"}}
         :not-found {:Acme/EmployeeNotFound {:Email "joe@acme.com"}}}})
    (p {:Acme/Employee? {:where [:= :Email "joe@acme.com"]}
        :as [:E]
        :throws
        {:error {:Acme/FailedToLookupEmployee {:Email "joe@acme.com"}}
         :not-found {:Acme/EmployeeNotFound {:Email "joe@acme.com"}}}})
    (p [:delete :Acme/Employee {:Email "joe@acme.com"}
        :as :E
        :throws
        {:error {:Acme/FailedToUpdateEmployee {:Email "joe@acme.com"}}
         :not-found {:Acme/EmployeeNotFound {:Email "joe@acme.com"}}}])
    (p [:eval `(~'(symbol "quote") (~(symbol "some-fn") :A :B))
        :as :Result
        :throws
        {:error {:Acme/FailedToCallFn {:Name "some-fn"}}}])))

(deftest throws-raw
  (defcomponent :throwsRaw
    (entity
     :throwsRaw/Employee
     {:Email {:type :Email :guid true}
      :Name :String})
    (record :throwsRaw/Error {:Message :String})
    (record :throwsRaw/NotFound {:Message :String})
    (dataflow
     :throwsRaw/E
     {:throwsRaw/Employee
      {:Email :throwsRaw/E.Email
       :Name "Jojo"}
      :as [:E0]
      :throws
      {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}}}
     {:throwsRaw/Employee
      {:Email? :throwsRaw/E.Email
       :Name "Toto"}
      :as [:E1]
      :throws
      {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
       :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}}
     {:throwsRaw/Employee? {:where [:= :Email :throwsRaw/E.Email]}
      :as [:E2]
      :throws
      {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
       :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}}
     [:delete :throwsRaw/Employee {:Email :throwsRaw/E.Email}
      :as :E3
      :throws
      {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
       :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}]))
  (is (= (lr/as-edn :throwsRaw)
         '(do
            (component :throwsRaw)
            (entity
             :throwsRaw/Employee
             {:Email {:type :Email :guid true}
              :Name :String})
            (record :throwsRaw/Error {:Message :String})
            (record :throwsRaw/NotFound {:Message :String})
            (dataflow
             :throwsRaw/E
             {:throwsRaw/Employee
              {:Email :throwsRaw/E.Email
               :Name "Jojo"}
              :as [:E0]
              :throws
              {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}}}
             {:throwsRaw/Employee
              {:Email? :throwsRaw/E.Email
               :Name "Toto"}
              :as [:E1]
              :throws
              {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
               :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}}
             {:throwsRaw/Employee? {:where [:= :Email :throwsRaw/E.Email]}
              :as [:E2]
              :throws
              {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
               :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}}
             [:delete :throwsRaw/Employee {:Email :throwsRaw/E.Email}
              :as :E3
              :throws
              {:error {:throwsRaw/Error {:Message :throwsRaw/E.Email}}
               :not-found {:throwsRaw/NotFound {:Message :throwsRaw/E.Email}}}])))))
