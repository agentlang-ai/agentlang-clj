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
 {:Name {:type :String, li/path-identity true}})
(def oprs li/rbac-oprs)
(defn-
 crud-list?
 [xs]
 (let
  [xs (mapv u/string-as-keyword xs)]
  (every? (fn* [p1__400#] (some #{p1__400#} oprs)) (set xs))))
(entity
 :Agentlang.Kernel.Rbac/Privilege
 {:Name {:type :String, :default u/uuid-string, li/path-identity true},
  :Actions {:check agentlang.kernel.rbac/crud-list?},
  :Resource :Edn})
(entity
 :Agentlang.Kernel.Rbac/PrivilegeAssignment
 {:Name {:type :String, :default u/uuid-string, li/path-identity true},
  :Role {:ref :Agentlang.Kernel.Rbac/Role.Name, :indexed true},
  :Privilege {:ref :Agentlang.Kernel.Rbac/Privilege.Name},
  :meta
  {:unique
   [:Agentlang.Kernel.Rbac/Role :Agentlang.Kernel.Rbac/Privilege]}})
(entity
 :Agentlang.Kernel.Rbac/RoleAssignment
 {:Name {:type :String, :default u/uuid-string, li/path-identity true},
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
  :Agentlang.Kernel.Rbac/RoleAssignment
  {:Assignee :Agentlang.Kernel.Rbac/DeleteRoleAssignments.Assignee}])
(defn-
 priv-assigns-query
 [env]
 (let
  [role-names
   (env :Agentlang.Kernel.Rbac/FindPrivilegeAssignments.RoleNames)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Agentlang.Kernel.Rbac/PrivilegeAssignment)
   " WHERE ("
   (stu/attribute-column-name :Role)
   " in ("
   (s/join
    ","
    (map (fn* [p1__401#] (str "'" (str p1__401#) "'")) role-names))
   "))")))
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivilegeAssignments
 [:query
  #:Agentlang.Kernel.Rbac{:PrivilegeAssignment?
                          agentlang.kernel.rbac/priv-assigns-query}])
(defn-
 privileges-query
 [env]
 (let
  [names (env :Agentlang.Kernel.Rbac/FindPrivileges.Names)]
  (str
   "SELECT * FROM "
   (stu/entity-table-name :Agentlang.Kernel.Rbac/Privilege)
   " WHERE ("
   (stu/attribute-column-name :Name)
   " in ("
   (s/join
    ","
    (map (fn* [p1__402#] (str "'" (str p1__402#) "'")) names))
   "))")))
(dataflow
 :Agentlang.Kernel.Rbac/FindPrivileges
 [:query
  #:Agentlang.Kernel.Rbac{:Privilege?
                          agentlang.kernel.rbac/privileges-query}])
(entity
 #:Agentlang.Kernel.Rbac{:InstancePrivilegeAssignment
                         {:Name
                          {:type :String,
                           :default u/uuid-string,
                           li/path-identity true},
                          :Actions
                          {:check agentlang.kernel.rbac/crud-list?,
                           :optional true},
                          :Resource :Path,
                          :ResourceId :Any,
                          :Assignee {:type :String, :indexed true}}})
(entity
 #:Agentlang.Kernel.Rbac{:OwnershipAssignment
                         {:Name
                          {:type :String,
                           :default u/uuid-string,
                           li/path-identity true},
                          :Resource :Path,
                          :ResourceId :Any,
                          :Assignee {:type :String, :indexed true}}})
(def
 Agentlang_Kernel_Rbac___COMPONENT_ID__
 "5e1d56e1-a386-4bea-bf3e-40b06076eef4")
