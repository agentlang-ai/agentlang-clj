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
 {:meta {:between [:Manager/User :Manager/Workspace :as [:USER :WRKSPC]]}})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Manager/User? {:meta {:version "0.2.1"}} :as :U}
 [:for-each :U
  {:Manager/User {:Name :%.Name}
   :as :U2}
  [:try
   {:Manager/Workspace? {:meta {:version "0.2.1"}}
    :-> [[:Manager/BelongsTo?
          {:Manager/User {:Name? :%.Name :meta {:version "0.2.1"}}}]]
    :as :W}
   :ok
   [:for-each :W
    {:Manager/Workspace {:Id :%.Id :WorkspaceName :%.WorkspaceName} :as :W2}
    {:Manager/BelongsTo {:USER :U2.__Id__ :WRKSPC :W2.Id}}]
   :not-found {}]])
