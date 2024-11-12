(component :App)

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
 {:App/Workspace? {}
  :-> [[:App/BelongsTo?
        {:App/User {:Name? :GetWorkspaceForUser.Name}}]]})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:App/User? {:meta {:version "0.1.1"}} :as :U}
 [:for-each :U
  {:App/User {:Name :%.Name}
   :as :U2}
  [:try
  {:App/Workspace {:User? :%.__Id__ :meta {:version "0.1.1"}} :as :W}  
   :ok
   [:for-each :W
    {:App/Workspace
     {:WorkspaceName :%.WorkspaceName}
     :-> [[{:App/BelongsTo {}} :U2]]}]
   :not-found {}]])
