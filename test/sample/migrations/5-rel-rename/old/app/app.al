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
 :Init
 {:App/User {:Name "User1"} :as :U1}
 {:App/User {:Name "User2"} :as :U2}
 {:App/User {:Name "User3"} :as :U3}
 {:App/Workspace {:WorkspaceName "WS1"}
  :-> [[:App/BelongsTo :U1]]}
 {:App/Workspace {:WorkspaceName "WS2"}
  :-> [[:App/BelongsTo :U1]]}
 {:App/Workspace {:WorkspaceName "WS3"}
  :-> [[:App/BelongsTo :U2]]})
