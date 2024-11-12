(component :Social)

(entity
 :Person
 {:Name :String})

(relationship
 :Relationship
 {:RelationshipType
  {:oneof ["Friends" "Family"] :default "Friends"}
  :meta {:between [:Person :Person :as [:Me :Other]]}})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Social/Person? {:meta {:version "0.1.1"}} :as :P}
 [:for-each :P
  {:Social/Person {:Name :%.Name}}]
 {:Social/Friendship? {:meta {:version "0.1.1"}} :as :F}
 [:for-each :F
  {:Social/Relationship {:Me :%.From :Other :%.To}}])
