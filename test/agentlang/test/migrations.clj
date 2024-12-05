(ns agentlang.test.migrations
  "Migration specific tests."
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util.runtime :as ur]
            [agentlang.component :as cn]
            [agentlang.store :as s]
            [agentlang.test.util :as tu]
            [agentlang.lang.tools.build :as build]
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
  ([model-path reset-state model-paths init]
   (when reset-state
     (reset-models-state)) 
   (binding [gs/migration-mode true]
     (let [[model entities]
           (build/load-model-migration nil "local" model-path model-paths)]
       (when init (ur/init-runtime (:Name model) nil))
       [model entities])))
  ([model-path reset-state model-paths]
   (load-model model-path reset-state model-paths true))
  ([model-path reset-state]
   (load-model model-path reset-state nil))
  ([model-path]
   (load-model model-path false)))

(defn- load-model-no-init
  ([model-path reset-state]
   (when reset-state
     (reset-models-state))
   (binding [gs/migration-mode true]
     (let [[model entities]
           (build/load-model-migration nil "local" model-path)]
       [model entities])))
  ([model-path]
   (load-model-no-init model-path false)))

(defn- clear-model-init [model-name]
  (s/remove-inited-component model-name)
  (cn/unregister-model model-name))

(deftest test-same-ent
  (let [model-name :Factory
        old-model "test/sample/migrations/1-same-ent/old/factory"
        new-model "test/sample/migrations/1-same-ent"]
    
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))

    (let [customers (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Customer {}})))]
      (is (= 5 (count customers))))

    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)

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
        old-model "test/sample/migrations/2-attr-change/old/factory"
        new-model "test/sample/migrations/2-attr-change"]
    
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))

    (let [shipments (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Shipment {}})))
          fs (first shipments)]
      (is (= 5 (count shipments)))
      (is (and (:Price fs) (:Quantity fs) (:Country fs)
               (:CustomerFirstName fs) (:CustomerLastName fs))))

    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)

    (let [shipments (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Shipment {}})))
          fs (first shipments)]
      (is (= 5 (count shipments)))
      (is (and (:MinPrice fs) (:MaxPrice fs) (:Amount fs)
               (:BuyerName fs) (:Address fs) (:Verified fs))))
    (clear-model-init model-name)))

(deftest test-rel-change
  (let [model-name :Manager
        old-model "test/sample/migrations/3-rel-change/old/manager"
        new-model "test/sample/migrations/3-rel-change"]

    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Manager/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/GetWorkspaceForUser {:Name "User1"}})))]
      (is (= 3 (count users)))
      (is (= 2 (count ws))))
    (clear-model-init model-name)))

(deftest test-rel-contains
  (let [model-name :Manager
        old-model "test/sample/migrations/4-rel-contains/old/manager"
        new-model "test/sample/migrations/4-rel-contains"]
    
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Manager/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/LookupAll_Workspace {}})))
          ws1 (first (filter #(= (:WorkspaceName %) "WS1") ws))
          ws1-user (first (filter #(= (:__Id__ %) (:User ws1)) users))]
      (is (= 3 (count users)))
      (is (seq (:User (first ws))))
      (is (= (count ws) 3))
      (is (= "User1" (:Name ws1-user))))
    (clear-model-init model-name)))

(deftest test-rel-rename
  (let [model-name :App
        old-model "test/sample/migrations/5-rel-rename/old/app"
        new-model "test/sample/migrations/5-rel-rename"]

    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:App/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:App/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:App/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:App/GetWorkspaceForUser {:Name "User1"}})))]
      (is (= 3 (count users)))
      (is (= 2 (count ws))))
    (clear-model-init model-name)))

(deftest test-rel-to-ref
  (let [model-name :App
        old-model "test/sample/migrations/6-rel-to-ref/old/app"
        new-model "test/sample/migrations/6-rel-to-ref"]

    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:App/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:App/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:App/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:App/GetWorkspaceForUser {:Name "User1"}})))
          ws-first (first ws)]
      (is (= 3 (count users)))
      (is (= 2 (count ws)))
      (is (and (:WorkspaceName ws-first) (:Id ws-first) (not (:User ws-first)))))
    (clear-model-init model-name)))

(deftest test-rel-between
  (let [model-name :Social
        old-model "test/sample/migrations/7-rel-between/old/social"
        new-model "test/sample/migrations/7-rel-between"]
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Social/Init {}}))
    (let [friendships (tu/fresult (ev/eval-all-dataflows
                               (cn/make-instance {:Social/LookupAll_Friendship {}})))]
      (is (= 6 (count friendships))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (ev/eval-all-dataflows
     (cn/make-instance {:Social/LookupAll_Person {}}))
    (let [persons (tu/fresult (ev/eval-all-dataflows
                               (cn/make-instance {:Social/LookupAll_Person {}})))
          friendships (tu/fresult (ev/eval-all-dataflows
                                   (cn/make-instance {:Social/LookupAll_Friendship {}})))]
      (is (= 7 (count persons)))
      (is (= 6 (count friendships))))
    (clear-model-init model-name)))

(deftest test-rel-between-enh
  (let [model-name :Social
        old-model "test/sample/migrations/8-rel-between-enh/old/social"
        new-model "test/sample/migrations/8-rel-between-enh"] 
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Social/Init {}}))
    (let [friendships (tu/fresult (ev/eval-all-dataflows
                               (cn/make-instance {:Social/LookupAll_Friendship {}})))]
      (is (= 6 (count friendships))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (ev/eval-all-dataflows
     (cn/make-instance {:Social/LookupAll_Person {}}))
    (let [persons (tu/fresult (ev/eval-all-dataflows
                               (cn/make-instance {:Social/LookupAll_Person {}})))
          relationships (tu/fresult (ev/eval-all-dataflows
                                     (cn/make-instance {:Social/LookupAll_Relationship {}})))
          fr (first relationships)]
      (is (= 7 (count persons)))
      (is (= 6 (count relationships)))
      (is (and (:Me fr) (:Other fr) (:RelationshipType fr) 
               (not (:From fr)) (not (:To fr)))))
    (clear-model-init model-name)))

(deftest test-rel-type-change
  (let [model-name :Manager
        old-model "test/sample/migrations/9-rel-type-change/old/manager"
        new-model "test/sample/migrations/9-rel-type-change"]
    (load-model old-model true)
    (ev/eval-all-dataflows (cn/make-instance {:Manager/Init {}}))
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))]
      (is (= 3 (count users))))
    (clear-model-init model-name)
    (load-model new-model)
    (ur/invoke-migrations-event)
    (let [users (tu/fresult (ev/eval-all-dataflows
                             (cn/make-instance {:Manager/LookupAll_User {}})))
          ws (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/LookupAll_Workspace {}})))
          bt (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/LookupAll_BelongsTo {}})))
          fbt (first bt)]
      (is (= 3 (count users)))
      (is (= (count ws) 3))
      (is (= (count bt) 3))
      (is (and (:USER fbt) (:WRKSPC fbt))))
    (clear-model-init model-name)))

(deftest test-join-entities
  (let [model-name :Manager
        old-model "test/sample/migrations/10-join-entities/old/manager"
        new-model "test/sample/migrations/10-join-entities"]

    (load-model old-model true)
     (ev/eval-all-dataflows (cn/make-instance {:Manager/Init {}}))
     (let [users (tu/fresult (ev/eval-all-dataflows
                              (cn/make-instance {:Manager/LookupAll_Customer {}})))
           orders (tu/fresult (ev/eval-all-dataflows
                        (cn/make-instance {:Manager/LookupAll_Order {}})))]
       (is (= 3 (count users)))
       (is (= 6 (count orders))))

    (clear-model-init model-name)
    (load-model new-model)
    
    (ur/invoke-migrations-event)
    
    (let [co (tu/fresult (ev/eval-all-dataflows
                          (cn/make-instance {:Manager/LookupAll_CustomerOrder {}})))
          cof (first co)]
      (is (= 6 (count co)))
      (is (and (:CustomerName cof) (:CustomerId cof) (:OrderId cof))))
    (clear-model-init model-name)))

(deftest test-auto-migrate
  (let [model-name :Factory
        model-name-2 :Office
        old-model "test/sample/migrations/11-auto-migrate-entities/old/factory"
        new-model "test/sample/migrations/11-auto-migrate-entities"
        [_ old-entities] (load-model old-model true)]
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))
    (let [customers1 (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Customer1 {}})))]
      (is (= 2 (count customers1))))
    (clear-model-init model-name)
    (clear-model-init model-name-2)
    (let [[_ new-entities] (load-model new-model false nil false)] 
      (ur/rename-db-entity-tables
       new-entities old-entities "current"
       {:no-auto-migration
        #{:Retail :Factory/Customer2}})
      (ur/init-runtime model-name nil) 
      (let [customers1 (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Factory/LookupAll_Customer1 {}})))
            customers2 (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Factory/LookupAll_Customer2 {}})))
            customers3 (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Factory/LookupAll_Customer3 {}})))
            fs (first customers1)
            persons (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Office/LookupAll_Person {}})))
            officerel (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Office/LookupAll_OfficeRel {}})))
            retailuser (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Retail/LookupAll_User {}})))]
        (is (and (= 2 (count customers1)) (and (:Name fs) (:Age fs) (:Gender fs))))
        (is (= 0 (count customers2)))
        (is (= 1 (count customers3)))
        (is (= 2 (count persons)))
        (is (= 1 (count officerel)))
        (is (= 0 (count retailuser))))
      (clear-model-init model-name)
      (clear-model-init model-name-2))))

(deftest test-auto-migrate-dependencies
  (let [model-name :Factory
        model-name-2 :Office
        old-model "test/sample/migrations/12-auto-migrate-with-dependencies/factory/old/factory"
        new-model "test/sample/migrations/12-auto-migrate-with-dependencies/factory"
        new-dependency ["test/sample/migrations/12-auto-migrate-with-dependencies"]
        old-dependency ["test/sample/migrations/12-auto-migrate-with-dependencies/office/old"]
        [_ old-entities] (load-model old-model true old-dependency)]
    (ev/eval-all-dataflows (cn/make-instance {:Factory/Init {}}))
    (let [customers (tu/fresult (ev/eval-all-dataflows
                                 (cn/make-instance {:Factory/LookupAll_Customer {}})))]
      (is (= 5 (count customers))))
    (clear-model-init model-name)
    (clear-model-init model-name-2)
    (let [[_ new-entities] (load-model new-model false new-dependency false)] 
      (ur/rename-db-entity-tables
       new-entities old-entities "current"
       {:no-auto-migration
        #{}}) 
      (ur/init-runtime model-name nil) 
      (let [customers (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Factory/LookupAll_Customer {}})))
            workers (tu/fresult (ev/eval-all-dataflows (cn/make-instance {:Office/LookupAll_Worker {}})))
            fs (first customers)]
        (is (and (= 5 (count customers)) (and (:Name fs) (:Age fs) (:Gender fs))))
        (is (= 2 (count workers))))
      (clear-model-init model-name)
      (clear-model-init model-name-2))))