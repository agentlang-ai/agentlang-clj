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

(dataflow
 :LookupRole
 {:Role {:Name? :LookupRole.Name} :as [:R]}
 :R)

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
 :LookupRoleAssignment
 {:RoleAssignment {:Name? :LookupRoleAssignment.Name} :as [:R]}
 :R)

(dataflow
 :FindRoleAssignments
 {:RoleAssignment
  {:Assignee? :FindRoleAssignments.Assignee}})

(dataflow
 :DeleteRoleAssignments
 [:delete {:RoleAssignment {:Assignee? :DeleteRoleAssignments.Assignee}}])

(dataflow
 :FindPrivilegeAssignments
 {:PrivilegeAssignment
  {:Role? [:in :Agentlang.Kernel.Rbac/FindPrivilegeAssignments.RoleNames]}})

(dataflow
 :FindPrivileges
 {:Privilege {:Name? [:in :Agentlang.Kernel.Rbac/FindPrivileges.Names]}})

(record
 {:InstancePrivilegeAssignment
  {:Name {:type :String
          :default u/uuid-string
          li/path-identity true}
   :IsOwner {:type :Boolean :default true}
   :CanRead {:type :Boolean :default false}
   :CanUpdate {:type :Boolean :default false}
   :CanDelete {:type :Boolean :default false}
   :ResourcePath {:type :String :indexed true}
   :Assignee {:type :String :indexed true}}})

(event
 {:AssignInstancePrivilege
  {:Name {:type :String :default u/uuid-string}
   :CanRead {:type :Boolean :default false}
   :CanUpdate {:type :Boolean :default false}
   :CanDelete {:type :Boolean :default false}
   :ResourcePath :String
   :Assignee :String}})

(dataflow
 :AssignInstancePrivilege
 {:InstancePrivilegeAssignment
  {:Name :AssignInstancePrivilege.Name
   :IsOwner false
   :CanRead :AssignInstancePrivilege.CanRead
   :CanUpdate :AssignInstancePrivilege.CanUpdate
   :CanDelete :AssignInstancePrivilege.CanDelete
   :ResourcePath :AssignInstancePrivilege.ResourcePath
   :Assignee :AssignInstancePrivilege.Assignee}})

(event
 :DeleteInstancePrivilegeAssignment
 {:ResourcePath :String
  :Assignee :String})

(event
 {:AssignOwnership
  {:Name {:type :String :default u/uuid-string}
   :ResourcePath :String
   :Assignee :String}})

(dataflow
 :AssignOwnership
 {:InstancePrivilegeAssignment
  {:Name :AssignOwnership.Name
   :IsOwner true
   :ResourcePath :AssignOwnership.ResourcePath
   :Assignee :AssignOwnership.Assignee}})
