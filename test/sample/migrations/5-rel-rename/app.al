(component :App)

(entity
 :User
 {:Name {:type :String :guid true}})

(entity
 :Workspace
 {:WSID {:type :UUID :guid true}
  :WorkspaceName :String})

(relationship
 :PartOf
 {:meta {:contains [:User :Workspace]}})

(dataflow
 :GetWorkspaceForUser
 {:App/Workspace? {}
  :-> [[:App/PartOf?
        {:App/User {:Name? :GetWorkspaceForUser.Name}}]]})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:App/User? {:meta {:version "0.0.5"}} :as :U}
 [:for-each :U
  {:App/User {:Name :%.Name}
   :as :U2}
  [:try
   {:App/Workspace? {:meta {:version "0.0.5"}}
    :-> [[:App/BelongsTo?
          {:App/User {:Name? :%.Name :meta {:version "0.0.5"}}}]]
    :as :W}
   :ok
   [:for-each :W
    {:App/Workspace
     {:WSID :%.Id :WorkspaceName :%.WorkspaceName}
     :-> [[{:App/PartOf {}} :U2]]}]
   :not-found {}]])
