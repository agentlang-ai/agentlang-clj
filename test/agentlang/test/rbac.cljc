(ns agentlang.test.rbac
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.auth]
            [agentlang.intercept.rbac.internal :as rbac]
            [agentlang.lang.rbac :as lr]
            [agentlang.lang.internal :as li]
            [agentlang.lang
             :refer [component attribute event view
                     entity record relationship dataflow]]
            #?(:clj  [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(defn- with-rbac [defcomponent-fn]
  (lr/reset-events!)
  (let [cname (defcomponent-fn)]
    (is (lr/finalize-events))
    (is (cn/instance-of?
         :Agentlang.Kernel.Rbac/RoleAssignment
         (tu/invoke {(li/make-path cname :InitUsers) {}})))))

(deftest role-management
  (defcomponent :RoleMgmt
    (dataflow
     :RoleMgmt/CreateUsers
     {:Agentlang.Kernel.Identity/User {:Email "abc@abc.com"}}
     {:Agentlang.Kernel.Identity/User {:Email "xyz@xyz.com"}})
    (dataflow
     :RoleMgmt/CreateRoles
     {:Agentlang.Kernel.Rbac/Role {:Name "rm.r1"}}
     {:Agentlang.Kernel.Rbac/Role {:Name "rm.r2"}})
    (dataflow
     :RoleMgmt/AssignPrivileges
     {:Agentlang.Kernel.Rbac/Privilege
      {:Name "rm.p1"
       :Actions [:q# [:read :create :update]]
       :Resource [:q# [:A :B]]}}
     {:Agentlang.Kernel.Rbac/Privilege
      {:Name "rm.p2"
       :Actions [:q# [:read]]
       :Resource [:q# [:C]]}}
     {:Agentlang.Kernel.Rbac/PrivilegeAssignment
      {:Role "rm.r1" :Privilege "rm.p1"}}
     {:Agentlang.Kernel.Rbac/PrivilegeAssignment
      {:Role "rm.r1" :Privilege "rm.p2"}}
     {:Agentlang.Kernel.Rbac/PrivilegeAssignment
      {:Role "rm.r2" :Privilege "rm.p2"}})
    (dataflow
     :RoleMgmt/AssignRoles
     {:Agentlang.Kernel.Rbac/RoleAssignment
      {:Role "rm.r1" :Assignee "abc@abc.com"}}
     {:Agentlang.Kernel.Rbac/RoleAssignment
      {:Role "rm.r2" :Assignee "xyz@xyz.com"}}))
  (let [[r1 r2 r3 r4]
        (mapv tu/invoke [:RoleMgmt/CreateUsers :RoleMgmt/CreateRoles
                               :RoleMgmt/AssignPrivileges :RoleMgmt/AssignRoles])]
    (is (cn/instance-of? :Agentlang.Kernel.Identity/User r1))
    (is (cn/instance-of? :Agentlang.Kernel.Rbac/Role r2))
    (is (cn/instance-of? :Agentlang.Kernel.Rbac/PrivilegeAssignment r3))
    (is (cn/instance-of? :Agentlang.Kernel.Rbac/RoleAssignment r4))
    (let [ps1 (rbac/privileges "abc@abc.com")
          ps2 (rbac/privileges "xyz@xyz.com")
          p2 (first ps2)]
      (is (= (count ps1) 2))
      (is (= (count ps2) 1))
      (is (= [:read] (:Actions p2)))
      (is (= [:C] (:Resource p2))))))

(def ^:private call-with-rbac tu/call-with-rbac)
(def ^:private with-user tu/with-user)

(deftest basic-rbac-dsl
  (with-rbac
    #(defcomponent :Brd
      (entity
       :Brd/E
       {:rbac [{:roles ["brd-user"] :allow [:create]}
               {:roles ["brd-manager"] :allow [:create :update :read]}]
        :Id {:type :Int :id true}
        :X :Int})
      (dataflow
       :Brd/InitUsers
       {:Agentlang.Kernel.Identity/User
        {:Email "u1@brd.com"}}
       {:Agentlang.Kernel.Identity/User
        {:Email "u2@brd.com"}}
       {:Agentlang.Kernel.Rbac/RoleAssignment
        {:Role "brd-user" :Assignee "u2@brd.com"}}
       {:Agentlang.Kernel.Rbac/RoleAssignment
        {:Role "brd-manager" :Assignee "u1@brd.com"}})))
  (let [e? (partial cn/instance-of? :Brd/E)]
    (call-with-rbac
     (fn []
       (let [as-path (fn [id] (li/vec-to-path [:Brd/E id]))
             create-e (fn [id]
                        {:Brd/Create_E
                         {:Instance
                          {:Brd/E {:Id id :X (* id 100)}}}})
             update-e (fn [id new-x]
                        {:Brd/Update_E
                         {:path (as-path id) :Data {:X new-x}}})
             lookup-all-es (constantly {:Brd/LookupAll_E {}})
             lookup-e (fn [id] {:Brd/Lookup_E {:path (as-path id)}})
             check-es (fn [n es]
                        (is (= n (count es)))
                        (is (every? e? es)))
             check-e (fn [id x e]
                       (is (e? e))
                       (is (= x (:X e)))
                       (is (= id (:Id e))))
             delete-e (fn [id] {:Brd/Delete_E {:path (as-path id)}})
             with-u1 (partial with-user "u1@brd.com")
             with-u2 (partial with-user "u2@brd.com")]
         (tu/is-error #(tu/invoke (create-e 1)))
         (is (e? (tu/invoke (with-u1 (create-e 1)))))
         (is (e? (tu/invoke (with-u2 (create-e 2)))))
         (is (e? (tu/invoke (with-u2 (create-e 3)))))
         (is (not (seq (tu/invoke (lookup-all-es)))))
         (check-es 3 (tu/invoke (with-u1 (lookup-all-es))))
         (check-es 2 (tu/invoke (with-u2 (lookup-all-es))))
         (is (nil? (seq (tu/invoke (with-u2 (lookup-e 1))))))
         (check-e 2 200 (first (tu/invoke (with-u2 (lookup-e 2)))))
         (check-e 3 300 (first (tu/invoke (with-u2 (lookup-e 3)))))
         (check-e 1 100 (first (tu/invoke (with-u1 (lookup-e 1)))))
         (check-e 2 200 (first (tu/invoke (with-u1 (lookup-e 2)))))
         (check-e 3 300 (first (tu/invoke (with-u1 (lookup-e 3)))))
         (is (nil? (tu/invoke (with-u2 (update-e 1 200)))))
         (check-e 1 100 (first (tu/invoke (with-u1 (lookup-e 1)))))
         (check-e 1 200 (first (tu/invoke (with-u1 (update-e 1 200)))))
         (check-e 1 200 (first (tu/invoke (with-u1 (lookup-e 1)))))
         (check-e 2 400 (first (tu/invoke (with-u2 (update-e 2 400)))))
         (check-e 2 400 (first (tu/invoke (with-u2 (lookup-e 2)))))
         (check-e 2 600 (first (tu/invoke (with-u1 (update-e 2 600)))))
         (check-e 2 600 (first (tu/invoke (with-u2 (lookup-e 2)))))
         (is (nil? (seq (tu/invoke (with-u2 (delete-e 1))))))
         (check-e 1 200 (first (tu/invoke (with-u1 (lookup-e 1)))))
         (check-e 1 200 (first (tu/invoke (with-u1 (delete-e 1)))))
         (is (nil? (seq (tu/invoke (with-u1 (lookup-e 1))))))
         (check-e 2 600 (first (tu/invoke (with-u2 (delete-e 2)))))
         (is (nil? (seq (tu/invoke (with-u2 (lookup-e 2))))))
         (check-e 3 300 (first (tu/invoke (with-u1 (lookup-all-es)))))
         (check-es 1 (tu/invoke (with-u1 (lookup-all-es))))
         (check-es 1 (tu/invoke (with-u2 (lookup-all-es)))))))))

(deftest rbac-with-contains
  (with-rbac
    #(defcomponent :Rwc
       (dataflow
        :Rwc/InitUsers
        {:Agentlang.Kernel.Identity/User
         {:Email "u1@rwc.com"}}
        {:Agentlang.Kernel.Identity/User
         {:Email "u2@rwc.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "rwc-user" :Assignee "u2@rwc.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "rwc-manager" :Assignee "u1@rwc.com"}})
       (entity :Rwc/A {:Id {:type :Int :id true} :X :Int
                       :rbac [{:roles ["rwc-user"] :allow [:create]}
                              {:roles ["rwc-manager"] :allow [:create :update :read]}]})
       (entity :Rwc/B {:Id {:type :Int :id true} :Y :Int})
       (entity :Rwc/C {:Id {:type :Int :id true} :Z :Int})
       (relationship :Rwc/AB {:meta {:contains [:Rwc/A :Rwc/B]}})
       (relationship :Rwc/BC {:meta {:contains [:Rwc/B :Rwc/C]}})
       (dataflow
        :Rwc/CreateB
        {:Rwc/B {:Id :Rwc/CreateB.Id
                 :Y :Rwc/CreateB.Y}
         :Rwc/AB {:Rwc/A {:Id? :Rwc/CreateB.A}}})
       (dataflow
        :Rwc/LookupAB
        {:Rwc/B? {}
         :Rwc/AB? {:Rwc/A {:Id :Rwc/LookupAB.A}}})
       (dataflow
        :Rwc/LookupABbyX
        {:Rwc/B? {}
         :Rwc/AB? {:Rwc/A {:X :Rwc/LookupABbyX.X}}})
       (dataflow
        :Rwc/CreateC
        {:Rwc/C {:Id :Rwc/CreateC.Id
                 :Z :Rwc/CreateC.Z}
         :Rwc/BC {:Rwc/B {:Id :Rwc/CreateC.B}
                  :Rwc/AB? {:Rwc/A {:Id :Rwc/CreateC.A}}}})
       (dataflow
        :Rwc/LookupABC
        {:Rwc/C? {}
         :Rwc/BC? {:Rwc/B {:Id :Rwc/LookupABC.B}
                   :Rwc/AB {:Rwc/A {:Id :Rwc/LookupABC.A}}}})
       (dataflow
        :Rwc/UpdateC
        {:Rwc/C {:Id? :Rwc/UpdateC.C
                 :Z '(* 10 :Rwc/C.Z)}
         :Rwc/BC? {:Rwc/B {:Id :Rwc/UpdateC.B}
                   :Rwc/AB {:Rwc/A {:Id :Rwc/UpdateC.A}}}})
       (dataflow
        :Rwc/LookupABCbyZ
        {:Rwc/C {:Z? :Rwc/LookupABCbyZ.Z}
         :Rwc/BC? {:Rwc/B {:Id :Rwc/LookupABCbyZ.B}
                   :Rwc/AB {:Rwc/A {:Id :Rwc/LookupABCbyZ.A}}}})
       (dataflow
        :Rwc/LookupBA
        {:Rwc/A? {}
         :Rwc/AB? {:Rwc/B {:Id :Rwc/LookupBA.B}}})
       (dataflow
        :Rwc/LookupCBA
        {:Rwc/A? {}
         :Rwc/AB? {:Rwc/B {}
                   :Rwc/BC {:Rwc/C {:Id :Rwc/LookupCBA.C}}}})))
  (call-with-rbac
   (fn []
     (let [a? (partial cn/instance-of? :Rwc/A)
           b? (partial cn/instance-of? :Rwc/B)
           c? (partial cn/instance-of? :Rwc/C)
           with-u1 (partial with-user "u1@rwc.com")
           with-u2 (partial with-user "u2@rwc.com")
           lookup-bs (fn [with-user id] (tu/invoke (with-user {:Rwc/LookupAB {:A id}})))
           lookup-bs-by-x (fn [with-user x] (tu/invoke (with-user {:Rwc/LookupABbyX {:X x}})))
           check-paths (fn [aid b]
                         (is (b? b))
                         (is (= (cn/instance-path b)
                                (li/vec-to-path (vec (concat [:Rwc/A aid :Rwc/AB :Rwc/B] [(:Id b)]))))))
           check-bs (fn [aid bs]
                      (is (seq bs))
                      (is (every? b? bs))
                      (doseq [b bs] (check-paths aid b)))
           create-c (fn [with-user id z b a]
                      (tu/invoke
                       (with-user
                         {:Rwc/CreateC {:Id id :Z z :B b :A a}})))
           lookup-cs (fn [with-user b a] (tu/invoke (with-user {:Rwc/LookupABC {:B b :A a}})))
           check-cs (fn [n s cs]
                      (is (count cs) n)
                      (is (every? c? cs))
                      (is (= s (apply + (mapv :Z cs)))))
           lookup-all-cs (fn [with-user z b a]
                           (tu/invoke (with-user {:Rwc/LookupABCbyZ {:Z z :B b :A a}})))
           check-a (fn [id inst]
                     (is (a? inst))
                     (is (= id (:Id inst))))
           update-c (fn [with-user c b a] (tu/invoke (with-user {:Rwc/UpdateC {:C c :B b :A a}})))
           lookup-ba (fn [with-user b] (tu/invoke (with-user {:Rwc/LookupBA {:B b}})))
           check-ba (fn [aid res]
                      (is (= 1 (count res)))
                      (check-a aid (first res)))
           check-cba check-ba
           lookup-cba (fn [with-user c] (tu/invoke (with-user {:Rwc/LookupCBA {:C c}})))
           lookup-all-a (fn [with-user] (tu/invoke (with-user {:Rwc/LookupAll_A {}})))
           check-ids (fn [t? ids res]
                       (is (= (count ids) (count res)))
                       (is (every? t? res))
                       (is (= (set ids) (set (mapv :Id res)))))
           check-as (partial check-ids a?)
           delete-a (fn [with-user id]
                      (tu/invoke (with-user {:Rwc/Delete_A {:path (li/vec-to-path [:Rwc/A id])}})))
           lookup-all-b (fn [with-user] (tu/invoke (with-user {:Rwc/LookupAll_B {}})))
           check-bs-ids (partial check-ids b?)
           lookup-all-c (fn [with-user] (tu/invoke (with-user {:Rwc/LookupAll_C {}})))
           check-cs-ids (partial check-ids c?)]
       (is (a? (tu/invoke (with-u1 {:Rwc/Create_A {:Instance {:Rwc/A {:Id 1 :X 100}}}}))))
       (is (a? (tu/invoke (with-u2 {:Rwc/Create_A {:Instance {:Rwc/A {:Id 2 :X 300}}}}))))
       (is (b? (tu/invoke (with-u1 {:Rwc/CreateB {:Id 101 :Y 10 :A 1}}))))
       (tu/is-error #(tu/invoke (with-u2 {:Rwc/CreateB {:Id 102 :Y 11 :A 1}})))
       (is (b? (tu/invoke (with-u1 {:Rwc/CreateB {:Id 102 :Y 11 :A 1}}))))
       (is (b? (tu/invoke (with-u2 {:Rwc/CreateB {:Id 103 :Y 12 :A 2}}))))
       (is (not (seq (lookup-bs with-u2 1))))
       (check-bs 1 (lookup-bs with-u1 1))
       (check-bs 2 (lookup-bs with-u2 2))
       (check-bs 2 (lookup-bs with-u1 2))
       (is (not (seq (lookup-bs-by-x with-u2 1))))
       (check-bs 1 (lookup-bs-by-x with-u1 100))
       (check-bs 2 (lookup-bs-by-x with-u2 300))
       (check-bs 2 (lookup-bs-by-x with-u1 300))
       (tu/is-error #(create-c with-u2 201 30 101 1))
       (is (c? (create-c with-u1 201 30 101 1)))
       (is (c? (create-c with-u1 202 40 101 1)))
       (is (c? (create-c with-u1 203 40 101 1)))
       (is (c? (create-c with-u1 204 50 102 1)))
       (is (c? (create-c with-u1 205 60 102 1)))
       (is (c? (create-c with-u1 206 70 103 2)))
       (is (c? (create-c with-u2 207 80 103 2)))
       (tu/is-error #(lookup-cs with-u2 101 1))
       (check-cs 3 (+ 40 40 30) (lookup-cs with-u1 101 1))
       (tu/is-error #(lookup-cs with-u2 102 1))
       (check-cs 2 (+ 50 60) (lookup-cs with-u1 102 1))
       (check-cs 2 (+ 70 80) (lookup-cs with-u1 103 2))
       (check-cs 1 80 (lookup-cs with-u2 103 2))
       (tu/is-error #(lookup-all-cs with-u2 40 101 1))
       (check-cs 2 80 (lookup-all-cs with-u1 40 101 1))
       (check-cs 1 30 (lookup-all-cs with-u1 30 101 1))
       (check-cs 1 60 (lookup-all-cs with-u1 60 102 1))
       (is (not (seq (lookup-all-cs with-u2 60 102 1))))
       (check-cs 1 70 (lookup-all-cs with-u1 70 103 2))
       (is (not (seq (lookup-all-cs with-u2 70 103 2))))
       (check-cs 1 80  (lookup-all-cs with-u2 80 103 2))
       (is (not (update-c with-u2 201 101 1)))
       (check-cs 1 300 (update-c with-u1 201 101 1))
       (check-cs 3 (+ 40 40 300) (lookup-cs with-u1 101 1))
       (is (nil? (seq (lookup-ba with-u2 101))))
       (check-ba 1 (lookup-ba with-u1 101))
       (is (not (seq (lookup-ba with-u2 102))))
       (check-ba 1 (lookup-ba with-u1 102))
       (check-cba 2 (lookup-cba with-u1 206))
       (check-cba 2 (lookup-cba with-u2 206))
       (check-as [1 2] (lookup-all-a with-u1))
       (check-as [2] (lookup-all-a with-u2))
       (is (not (delete-a with-u2 1)))
       (check-as [1] (delete-a with-u1 1))
       (check-as [2] (lookup-all-a with-u1))
       (check-as [2] (lookup-all-a with-u2))
       (check-bs-ids [103] (lookup-all-b with-u2))
       (check-bs-ids [103] (lookup-all-b with-u1))
       (check-cs-ids [207] (lookup-all-c with-u2))
       (check-cs-ids [206 207] (lookup-all-c with-u1))
       (is (nil? (seq (lookup-bs with-u1 1))))
       (check-bs-ids [103] (lookup-bs with-u2 2))
       (check-bs-ids [103] (lookup-bs with-u1 2))))))

(deftest instance-privs
  (with-rbac
    #(defcomponent :Ipv
       (entity
        :Ipv/E
        {:rbac [{:roles ["ipv-user"] :allow [:create :update :read]}
                {:roles ["ipv-guest"] :allow [:read]}]
         :Id {:type :Int :id true}
         :X :Int})
       (dataflow
        :Ipv/InitUsers
        {:Agentlang.Kernel.Identity/User
         {:Email "u1@ipv.com"}}
        {:Agentlang.Kernel.Identity/User
         {:Email "u2@ipv.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "ipv-user" :Assignee "u1@ipv.com"}})
       {:Agentlang.Kernel.Rbac/RoleAssignment
        {:Role "ipv-guest" :Assignee "u2@ipv.com"}}))
  (call-with-rbac
   (fn []
     (let [e? (partial cn/instance-of? :Ipv/E)
           as-path (fn [id] (li/vec-to-path [:Ipv/E id]))
           create-e (fn [user id]
                      (tu/invoke
                       (with-user
                         user
                         {:Ipv/Create_E
                          {:Instance
                           {:Ipv/E {:Id id :X (* id 100)}}}})))
           update-e (fn [user id x]
                      (first
                       (tu/invoke
                        (with-user
                          user
                          {:Ipv/Update_E
                           {:path (as-path id)
                            :Data {:X x}}}))))
           lookup-e (fn [user id]
                      (first
                       (tu/invoke
                        (with-user
                          user
                          {:Ipv/Lookup_E
                           {:path (as-path id)}}))))
           has-action? (fn [actions action]
                         (if (some #{action} actions)
                           true
                           false))
           inst-priv (fn [owner assignee actions id]
                       (tu/invoke
                        (with-user
                          owner
                          {:Agentlang.Kernel.Rbac/AssignInstancePrivilege
                           {:ResourcePath (as-path id)
                            :Assignee assignee
                            :CanRead (has-action? actions :read)
                            :CanUpdate (has-action? actions :update)
                            :CanDelete (has-action? actions :delete)}})))
           del-inst-priv (fn [owner assignee id]
                           (first
                            (tu/invoke
                             (with-user
                               owner
                               {:Agentlang.Kernel.Rbac/DeleteInstancePrivilegeAssignment
                                {:ResourcePath (as-path id)
                                 :Assignee assignee}}))))
           ip? #(string? (:ResourcePath %))
           e1 (create-e "u1@ipv.com" 1)]
       (is (e? e1))
       (tu/is-error #(create-e "u2@ipv.com" 2))
       (is (cn/same-instance? e1 (lookup-e "u1@ipv.com" 1)))
       (is (not (lookup-e "u2@ipv.com" 1)))
       (is (e? (update-e "u1@ipv.com" 1 3000)))
       (is (not (update-e "u2@ipv.com" 1 5000)))
       (is (ip? (inst-priv "u1@ipv.com" "u2@ipv.com" [:read :update] 1)))
       (tu/is-error #(inst-priv "u2@ipv.com" "u2@ipv.com" [:read :update] 1))
       (let [e (lookup-e "u1@ipv.com" 1)]
         (is (= 3000 (:X e)))
         (is (cn/same-instance? e (lookup-e "u2@ipv.com" 1)))
         (is (e? (update-e "u2@ipv.com" 1 5000)))
         (tu/is-error #(del-inst-priv "u2@ipv.com" "u2@ipv.com" 1))
         (is (ip? (del-inst-priv "u1@ipv.com" "u2@ipv.com" 1)))
         (let [e (lookup-e "u1@ipv.com" 1)]
           (is (= 5000 (:X e)))
           (is (not (update-e "u2@ipv.com" 1 8000)))
           (is (not (lookup-e "u2@ipv.com" 1)))
           (is (cn/same-instance? e (lookup-e "u1@ipv.com" 1)))))))))

(deftest rbac-with-between
  (with-rbac
    #(defcomponent :Rwb
       (entity
        :Rwb/A
        {:Id {:type :Int :id true} :X :Int
         :rbac [{:roles ["rwb-user"] :allow [:create]}
                {:roles ["rwb-guest"] :allow [:read]}]})
       (entity :Rwb/B {:Id {:type :Int :id true} :Y :Int
                       :rbac [{:roles ["rwb-user"] :allow [:create]}
                              {:roles ["rwb-guest"] :allow [:read]}]})
       (entity :Rwb/C {:Id {:type :Int :id true} :Z :Int})

       (relationship :Rwb/AB {:meta {:between [:Rwb/A :Rwb/B]}})
       (relationship :Rwb/BC {:meta {:contains [:Rwb/B :Rwb/C]}})

       (dataflow
        :Rwb/CreateB
        {:Rwb/B {:Id :Rwb/CreateB.Id :Y :Rwb/CreateB.Y}
         :Rwb/AB {:Rwb/A {:Id? :Rwb/CreateB.A}}})

       (dataflow
        :Rwb/CreateC
        {:Rwb/C {:Id :Rwb/CreateC.Id :Z :Rwb/CreateC.Z}
         :Rwb/BC {:Rwb/B {:Id? :Rwb/CreateC.B}}})

       (dataflow
        :Rwb/LookupB
        {:Rwb/B? {}
         :Rwb/AB? {:Rwb/A {:Id :Rwb/LookupB.A}}})

       (dataflow
        :Rwb/LookupC
        {:Rwb/C? {}
         :Rwb/BC? {:Rwb/B {:Id :Rwb/LookupC.B}}})

       (dataflow
        :Rwb/LookupAllCforA
        {:Rwb/C? {}
         :Rwb/BC?
         {:Rwb/B {}
          :Rwb/AB?
          {:Rwb/A {:Id :Rwb/LookupAllCforA.A}}}})

       (dataflow
        :Rwb/LookupAllForA
        {:Rwb/C? {}
         :Rwb/BC?
         {:Rwb/B {}
          :Rwb/AB?
          {:Rwb/A {:Id :Rwb/LookupAllForA.A} :as :A}}
         :into
         {:AX :A.X
          :BY :Rwb/B.Y
          :CZ :Rwb/C.Z}})

       (dataflow
        :Rwb/InitUsers
        {:Agentlang.Kernel.Identity/User
         {:Email "u1@rwb.com"}}
        {:Agentlang.Kernel.Identity/User
         {:Email "u2@rwb.com"}}
        {:Agentlang.Kernel.Identity/User
         {:Email "u3@rwb.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "rwb-user" :Assignee "u1@rwb.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "rwb-user" :Assignee "u2@rwb.com"}}
        {:Agentlang.Kernel.Rbac/RoleAssignment
         {:Role "rwb-guest" :Assignee "u3@rwb.com"}})))
  (call-with-rbac
   (fn []
     (let [with-u1 (partial with-user "u1@rwb.com")
           with-u2 (partial with-user "u2@rwb.com")
           with-u3 (partial with-user "u3@rwb.com")
           create-a (fn [wu id x]
                      (tu/invoke (wu {:Rwb/Create_A
                                      {:Instance
                                       {:Rwb/A {:Id id :X x}}}})))
           create-b (fn [wu id y a]
                      (tu/invoke (wu {:Rwb/CreateB {:Id id :Y y :A a}})))
           create-c (fn [wu id z b]
                      (tu/invoke (wu {:Rwb/CreateC {:Id id :Z z :B b}})))

           lookup-b (fn [wu a]
                      (tu/invoke (wu {:Rwb/LookupB {:A a}})))

           lookup-c-for-a (fn [wu a]
                            (tu/invoke (wu {:Rwb/LookupAllCforA {:A a}})))
           lookup-all-for-a (fn [wu a]
                              (tu/invoke (wu {:Rwb/LookupAllForA {:A a}})))

           a? (partial cn/instance-of? :Rwb/A)
           b? (partial cn/instance-of? :Rwb/B)
           c? (partial cn/instance-of? :Rwb/C)
           check-bs (fn [ids bs]
                      (is (= (count bs) (count ids)))
                      (is (every? b? bs))
                      (doseq [b bs]
                        (is (some (fn [id] (= id (:Id b))) ids))))]
       (tu/is-error #(create-a identity 1 10))
       (tu/is-error #(create-a with-u3 1 10))
       (is (a? (create-a with-u1 1 10)))
       (is (a? (create-a with-u2 2 20)))
       (is (b? (create-b with-u1 11 110 1)))
       (is (b? (create-b with-u1 12 120 1)))
       (is (b? (create-b with-u2 13 130 2)))
       (is (c? (create-c with-u1 20 1010 11)))
       (is (c? (create-c with-u1 21 1030 12)))
       (tu/is-error #(create-c with-u2 21 1030 12))
       (is (c? (create-c with-u2 21 1040 13)))
       (is (= 2 (count (lookup-c-for-a with-u1 1))))
       (tu/is-error #(count (lookup-c-for-a with-u1 2)))
       (tu/is-error #(lookup-all-for-a with-u2 1))
       (let [rs (lookup-all-for-a with-u3 2)
             r (first rs)]
         (is (= 1 (count rs)))
         (is (= 20 (:AX r)))
         (is (= 130 (:BY r)))
         (is (= 1040 (:CZ r))))
       (let [rs (lookup-all-for-a with-u2 2)
             r (first rs)]
         (is (= 1 (count rs)))
         (is (= 20 (:AX r)))
         (is (= 130 (:BY r)))
         (is (= 1040 (:CZ r))))
       (let [rs (lookup-all-for-a with-u1 1)]
         (is (= 2 (count rs)))
         (is (= 20 (apply + (mapv :AX rs))))
         (is (= (+ 110 120) (apply + (mapv :BY rs))))
         (is (= (+ 1010 1030) (apply + (mapv :CZ rs)))))
       (let [rs (lookup-all-for-a with-u3 1)]
         (is (= 2 (count rs)))
         (is (= 20 (apply + (mapv :AX rs))))
         (is (= (+ 110 120) (apply + (mapv :BY rs))))
         (is (= (+ 1010 1030) (apply + (mapv :CZ rs)))))
       (check-bs [11 12] (lookup-b with-u1 1))
       (check-bs [13] (lookup-b with-u3 2))
       (check-bs [13] (lookup-b with-u2 2))))))
