(ns
 agentlang.kernel.rbac
 (:require
  [clojure.string :as s]
  [agentlang.util :as u]
  [agentlang.store.util :as stu]
  [agentlang.lang.internal :as li]
  [agentlang.kernel.lang
   :refer
   [Agentlang_Kernel_Lang___COMPONENT_ID__]]
  [agentlang.lang
   :refer
   [dataflow
    entity
    view
    pattern
    attribute
    rule
    relationship
    component
    resolver
    event
    inference
    record]]))
(component
 :Agentlang.Kernel.Rbac
 {:refer [:Agentlang.Kernel.Lang],
  :clj-import
  '[(:require
     [clojure.string :as s]
     [agentlang.util :as u]
     [agentlang.store.util :as stu]
     [agentlang.lang.internal :as li])]})
(entity
 :Agentlang.Kernel.Rbac/Role
 {:Name {:type :String, :unique true, li/path-identity true}})
(def oprs li/rbac-oprs)
(defn-
 crud-list?
 [xs]
 (let
  [xs (mapv u/string-as-keyword xs)]
  (every? (fn* [p1__603#] (some #{p1__603#} oprs)) (set xs))))
(entity
 :Agentlang.Kernel.Rbac/Privilege
 {:Name
  {:type :String,
   :default u/uuid-string,
   :unique true,
   li/path-identity true},
  :Actions {:check agentlang.kernel.rbac/crud-list?},
  :Resource :Edn})
(entity
 :Agentlang.Kernel.Rbac/PrivilegeAssignment
 {:Name
  {:type :String,
   :unique true,
   :default u/uuid-string,
   li/path-identity true},
  :Role {:ref :Agentlang.Kernel.Rbac/Role.Name, :indexed true},
  :Privilege {:ref :Agentlang.Kernel.Rbac/Privilege.Name},
  :meta
  {:unique
   [:Agentlang.Kernel.Rbac/Role :Agentlang.Kernel.Rbac/Privilege]}})
(entity
 :Agentlang.Kernel.Rbac/RoleAssignment
 {:Name
  {:type :String,
   :unique true,
   :default u/uuid-string,
   li/path-identity true},
  :Role {:ref :Agentlang.Kernel.Rbac/Role.Name, :indexed true},
  :Assignee {:type :String, :indexed true},
  :meta {:unique [:Agentlang.Kernel.Rbac/Role :Assignee]}})
(dataflow
 :Agentlang.Kernel.Rbac/FindRoleAssignments
 #:Agentlang.Kernel.Rbac{:RoleAssignment
                         {:Assignee?
                          :Agentlang.Kernel.Rbac/FindRoleAssignments.Assignee}})
(dataflow
 :Agentlang.Kernel.Rbac/DeleteRoleAssignments
 [:delete
  #:Agentlang.Kernel.Rbac{:RoleAssignment
                          {:Assignee?
                           :Agentlang.Kernel.Rbac/DeleteRoleAssignments.Assignee}}])
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivilegeAssignments
 #:Agentlang.Kernel.Rbac{:PrivilegeAssignment
                         {:Role?
                          [:in
                           :Agentlang.Kernel.Rbac/FindPrivilegeAssignments.RoleNames]}})
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivileges
 #:Agentlang.Kernel.Rbac{:Privilege
                         {:Name?
                          [:in
                           :Agentlang.Kernel.Rbac/FindPrivileges.Names]}})
(record
 #:Agentlang.Kernel.Rbac{:InstancePrivilegeAssignment
                         {:Name
                          {:type :String,
                           :default u/uuid-string,
                           li/path-identity true},
                          :IsOwner {:type :Boolean, :default true},
                          :CanRead {:type :Boolean, :default false},
                          :CanUpdate {:type :Boolean, :default false},
                          :CanDelete {:type :Boolean, :default false},
                          :ResourcePath {:type :String, :indexed true},
                          :Assignee {:type :String, :indexed true}}})
(def
 Agentlang_Kernel_Rbac___COMPONENT_ID__
 "b64e01b9-ffa7-4ce0-ab32-1cc8d915e9a9")
