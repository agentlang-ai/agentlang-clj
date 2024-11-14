(component :Manager)

(entity
 :User
 {:Name :String})

(entity
 :Workspace
 {:Id :Identity
  :WorkspaceName :String
  :User :UUID})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Manager/User? {:meta {:version "0.0.1"}} :as :U}
 [:for-each :U
  {:Manager/User {:Name :%.Name}
   :as :U2}
  [:try
   {:Manager/Workspace? {:meta {:version "0.0.1"}}
    :-> [[:Manager/BelongsTo?
          {:Manager/User {:Name? :%.Name :meta {:version "0.0.1"}}}]]
    :as :W}
   :ok
   [:for-each :W
    {:Workspace
     {:Id :%.Id :WorkspaceName :%.WorkspaceName :User :U2.__Id__}}]
   :not-found {}]])
