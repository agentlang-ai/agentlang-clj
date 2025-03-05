(ns agentlang.test.resolver
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.lang
             :refer [component attribute event relationship
                     entity record dataflow resolver]]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry :as rg]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(defn- db-write [db inst]
  (swap! db conj inst)
  inst)

(defn- db-lookup [db params]
  (let [qattrs (r/query-attributes params)]
    (if (r/query-all? qattrs)
      @db
      (let [[opr k v] (first (vals qattrs))]
        (when (= opr :=)
          (vec (filter #(and (= (k %) v)) @db)))))))

(defn- db-delete [db [entity-name insts]]
  (reset! db (vec (filter #(not (some #{%} insts)) @db)))
  insts)

(defn- db-update [db inst]
  (reset! db (vec (mapv #(if (cn/instance-eq? inst %)
                           inst
                           %)
                        @db)))
  inst)

(defn- make-db-resolver [db n paths]
  (resolver
   n {:paths paths
      :with-methods
      {:create (partial db-write db)
       :query (partial db-lookup db)
       :delete (partial db-delete db)
       :update (partial db-update db)}}))

(deftest basic-resolver
  (let [db (atom [])]
    (defcomponent :Bres
      (entity :Bres/A {:Id {:type :Int :id true} :X :Int})
      (dataflow
       :Bres/FindA
       {:Bres/A {:Id? :Bres/FindA.Id} :as [:A]}
       :A))
    (make-db-resolver db :Bres/Resolver [:Bres/A])
    (u/run-init-fns)
    (let [cra (fn [id x]
                (tu/invoke
                 {:Bres/Create_A
                  {:Instance
                   {:Bres/A {:Id id :X x}}}}))
          a? (partial cn/instance-of? :Bres/A)
          is-a (fn [id a]
                 (is (a? a))
                 (is (= id (:Id a))))
          as (mapv cra [1 2 3] [10 20 30])
          is-find-a (fn [id]
                      (is-a id (tu/invoke {:Bres/FindA {:Id id}})))]
      (is (every? a? as))
      (is-find-a 1)
      (is-find-a 3)
      (is-a 3 (first (tu/invoke {:Bres/Delete_A {:path (li/path-attr (last as))}})))
      (is-find-a 1)
      (is-find-a 2)
      (is (nil? (tu/invoke {:Bres/FindA {:Id 3}}))))))

(deftest resolver-with-contains-01
  (let [db (atom [])]
    (defcomponent :ResC
      (entity :ResC/A {:Id {:type :Int :id true} :X :Int})
      (entity :ResC/B {:Id {:type :Int :id true} :Y :Int})
      (relationship :ResC/AB {:meta {:contains [:ResC/A :ResC/B]}})
      (dataflow
       :ResC/CreateB
       {:ResC/B {:Id :ResC/CreateB.Id :Y :ResC/CreateB.Y}
        :ResC/AB {:ResC/A {:Id? :ResC/CreateB.A}}})
      (dataflow
       :ResC/FindB
       {:ResC/B? {}
        :ResC/AB? {:ResC/A {:Id :ResC/FindB.A}}}))
    (make-db-resolver db :ResC/Resolver [:ResC/A])
    (u/run-init-fns)
    (let [cra (fn [id x]
                (tu/invoke
                 {:ResC/Create_A
                  {:Instance
                   {:ResC/A {:Id id :X x}}}}))
          a? (partial cn/instance-of? :ResC/A)
          as (mapv cra [1 2 3] [10 20 30])
          crb (fn [id y a]
                (tu/invoke {:ResC/CreateB {:Id id :Y y :A a}}))
          b? (partial cn/instance-of? :ResC/B)
          bs (mapv crb [100 200 300 100] [90 91 92 93] [1 2 1 3])
          is-bs (fn [ids bs]
                  (is (= (count ids) (count bs)))
                  (is (every? b? bs))
                  (is (= (set (mapv :Id bs)) (set ids))))]
      (is (every? a? as))
      (is (every? b? bs))
      (is-bs [100 300] (tu/invoke {:ResC/FindB {:A 1}}))
      (is-bs [200] (tu/invoke {:ResC/FindB {:A 2}})))))
