(component :Manager)

(entity
 :User
 {:Name :String})

(entity
 :Workspace
 {:Id :Identity
  :WorkspaceName :String})

(relationship
 :BelongsTo
 {:meta {:contains [:User :Workspace]}})

(dataflow
 :GetWorkspaceForUser
 {:Manager/Workspace? {}
  :-> [[:Manager/BelongsTo?
        {:Manager/User {:Name? :GetWorkspaceForUser.Name}}]]})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Manager/User? {:meta {:version "0.0.3"}} :as :U}
 [:for-each :U
  {:Manager/User {:Name :%.Name}
   :as :U2}
  [:try
   {:Manager/Workspace? {:meta {:version "0.0.3"}}
    :-> [[:Manager/BelongsTo?
          {:Manager/User {:Name? :%.Name :meta {:version "0.0.3"}}}]]
    :as :W}
   :ok
   [:for-each :W
    {:Manager/Workspace
     {:Id :%.Id :WorkspaceName :%.WorkspaceName}
     :-> [[{:Manager/BelongsTo {}} :U2]]}]
   :not-found {}]])
