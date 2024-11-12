(component :App)

(entity
 :User
 {:Name :String})

(entity
 :Workspace
 {:WorkspaceName :String
  :User :UUID})

(dataflow
 :Init
 {:App/User {:Name "User1"} :as :U1}
 {:App/User { :Name "User2"} :as :U2}
 {:App/User { :Name "User3"} :as :U3}
 {:App/Workspace {:WorkspaceName "WS1" :User :U1.__Id__}}
 {:App/Workspace {:WorkspaceName "WS2" :User :U1.__Id__}}
 {:App/Workspace {:WorkspaceName "WS3" :User :U3.__Id__}})
