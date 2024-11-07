(ns agentlang.test.migrations
  "Migration specific tests."
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util.runtime :as ur]
            [agentlang.component :as cn]
            [agentlang.store :as s]
            [agentlang.lang.tools.loader :as loader]
            [agentlang.evaluator :as e]
            [agentlang.test.util :as tu]
            [agentlang.evaluator :as ev]))

(defn- load-model [model-name model-path]
  (let [[model model-root] (loader/read-model model-path)]
    (loader/load-components-from-model model model-root)
    (ur/init-runtime model-name nil)))

(defn- clear-model-init [model-name]
  (s/remove-inited-component model-name)
  (cn/unregister-model model-name))

(deftest test-migrations-rel-contains
  (let [model-name :Manager
        old-model "test/sample/migrations-rel-contains/old/manager/model.al"
        new-model "test/sample/migrations-rel-contains/model.al"]
    (load-model model-name old-model)
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

(deftest test-migrations-attribute-changes
  (let [model-name :Factory
        old-model "test/sample/migrations-attr-change/old/factory/model.al"
        new-model "test/sample/migrations-attr-change/model.al"]
    (load-model model-name old-model)
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
               (:BuyerName fs) (:Address fs) (:Verified fs))))))
