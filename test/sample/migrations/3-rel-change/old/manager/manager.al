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
 :Init
 {:Manager/User {:Name "User1"} :as :U1}
 {:Manager/User {:Name "User2"} :as :U2}
 {:Manager/User {:Name "User3"} :as :U3}
 {:Manager/Workspace {:WorkspaceName "WS1"}
  :-> [[:Manager/BelongsTo :U1]]}
 {:Manager/Workspace {:WorkspaceName "WS2"}
  :-> [[:Manager/BelongsTo :U1]]}
 {:Manager/Workspace {:WorkspaceName "WS3"}
  :-> [[:Manager/BelongsTo :U2]]})
