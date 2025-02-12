(component
 :Agentlang.Kernel.Rbac
 {:refer [:Agentlang.Kernel.Lang]
  :clj-import '[(:require [clojure.string :as s]
                          [agentlang.util :as u]
                          [agentlang.store.util :as stu]
                          [agentlang.lang.internal :as li])]})

(entity
 :Role
 {:Name {:type :String
         :unique true
         li/path-identity true}})

(def ^:private oprs li/rbac-oprs)

(defn- crud-list? [xs]
  (let [xs (mapv u/string-as-keyword xs)]
    (every? #(some #{%} oprs) (set xs))))

(entity
 :Privilege
 {:Name {:type :String
         :default u/uuid-string
         :unique true
         li/path-identity true}
  :Actions {:check crud-list?}
  :Resource :Edn})

(entity
 :PrivilegeAssignment
 {:Name {:type :String
         :unique true
         :default u/uuid-string
         li/path-identity true}
  :Role {:ref :Role.Name
         :indexed true}
  :Privilege {:ref :Privilege.Name}
  :meta {:unique [:Role :Privilege]}})

(entity
 :RoleAssignment
 {:Name {:type :String
         :unique true
         :default u/uuid-string
         li/path-identity true}
  :Role {:ref :Role.Name
         :indexed true}
  :Assignee {:type :String ; usually a :Agentlang.Kernel.Identity/User.Name
             :indexed true}
  :meta
  {:unique [:Role :Assignee]}})

(dataflow
 :FindRoleAssignments
 {:RoleAssignment
  {:Assignee? :FindRoleAssignments.Assignee}})

(dataflow
 :DeleteRoleAssignments
 [:delete :RoleAssignment {:Assignee :DeleteRoleAssignments.Assignee}])

(dataflow
 :FindPrivilegeAssignments
 {:PrivilegeAssignment
  {:Role? [:in :Agentlang.Kernel.Rbac/FindPrivilegeAssignments.RoleNames]}})

(dataflow
 :FindPrivileges
 {:Privilege {:Name? [:in :Agentlang.Kernel.Rbac/FindPrivileges.Names]}})

(entity
 {:InstancePrivilegeAssignment
  {:Name {:type :String
          :default u/uuid-string
          li/path-identity true}
   :CanRead {:type :Boolean :default false}
   :CanUpdate {:type :Boolean :default false}
   :CanDelete {:type :Boolean :default false}
   :Resource {:type :Path :indexed true}
   :ResourceId {:type :Any :indexed true}
   :Assignee {:type :String :indexed true}}})
