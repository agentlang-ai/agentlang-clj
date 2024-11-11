(ns agentlang.test.migrations
  "Migration specific tests."
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util.runtime :as ur]
            [agentlang.component :as cn]
            [agentlang.store :as s]
            [agentlang.lang.tools.loader :as loader]
            [agentlang.test.util :as tu]
            [agentlang.global-state :as gs]
            [agentlang.evaluator :as ev]))

(defn- reset-models-state []
  (gs/uninstall-standalone-patterns!)
  (let [components (cn/component-names)]
    (loop [c components]
      (when (seq c)
        (when-not
         (contains? (set (cn/internal-component-names)) (first c))
          (cn/remove-component (first c)))
        (recur (rest c))))))

(defn- load-model 
  ([model-name model-path reset-state]
   (when reset-state 
     (reset-models-state))
   (let [[model model-root] (loader/read-model model-path)]
     (loader/load-components-from-model model model-root)
     (ur/init-runtime model-name nil)))
  ([model-name model-path]
   (load-model model-name model-path false)))

(defn- clear-model-init [model-name]
  (s/remove-inited-component model-name)
  (cn/unregister-model model-name))

(deftest test-same-ent
  (let [model-name :Factory
        old-model "test/sample/migrations/1-same-ent/old/factory/model.al"
        new-model "test/sample/migrations/1-same-ent/model.al"] 
    
    (load-model model-name old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))

    (let [customers (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Customer {}})))]
      (is (= 5 (count customers))))

    (clear-model-init model-name)
    (load-model model-name new-model)
    (ev/eval-all-dataflows
     (cn/make-instance {:Agentlang.Kernel.Lang/Migrations {}}))

    (let [customers (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Customer {}})))
          fs (first customers)
          persons (tu/fresult (ev/eval-all-dataflows
                               (cn/make-instance {:Factory/LookupAll_Person {}})))
          fp (first persons)
          customers_select (tu/fresult (ev/eval-all-dataflows
                                        (cn/make-instance {:Factory/LookupAll_CustomerMale {}})))
          fcs (first customers_select)]
      (is (and (= 5 (count customers)) (and (:Name fs) (:Age fs) (:Gender fs))))
      (is (and (= 5 (count persons)) (and (:Name fp) (:Age fp) (:Gender fp))))
      (is (and (= 3 (count customers_select)) (and (:Name fcs) (:Age fcs) (:Gender fcs)))))
    (clear-model-init model-name)))

(deftest test-attr-change
  (let [model-name :Factory
        old-model "test/sample/migrations/2-attr-change/old/factory/model.al"
        new-model "test/sample/migrations/2-attr-change/model.al"]
    
    (load-model model-name old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))

    (let [shipments (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Shipment {}})))
          fs (first shipments)]
      (is (= 5 (count shipments)))
      (is (and (:Price fs) (:Quantity fs) (:Country fs)
               (:CustomerFirstName fs) (:CustomerLastName fs))))

    (clear-model-init model-name)
    (load-model model-name new-model)
    (ev/eval-all-dataflows
     (cn/make-instance {:Agentlang.Kernel.Lang/Migrations {}}))

    (let [shipments (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Shipment {}})))
          fs (first shipments)]
      (is (= 5 (count shipments)))
      (is (and (:MinPrice fs) (:MaxPrice fs) (:Amount fs)
               (:BuyerName fs) (:Address fs) (:Verified fs))))
    (clear-model-init model-name)))

(deftest test-rel-contains
  (let [model-name :Manager
        old-model "test/sample/migrations/4-rel-contains/old/manager/model.al"
        new-model "test/sample/migrations/4-rel-contains/model.al"]
    
    (load-model model-name old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Manager/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model model-name new-model)
    (ev/eval-all-dataflows
     (cn/make-instance {:Agentlang.Kernel.Lang/Migrations {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/LookupAll_Workspace {}})))
          ws1 (first (filter #(= (:WorkspaceName %) "WS1") ws))
          ws1-user (first (filter #(= (:__Id__ %) (:User ws1)) users))]
      (is (= 3 (count users)))
      (is (seq (:User (first ws))))
      (is (= (count ws) 3))
      (is (= "User1" (:Name ws1-user))))))
