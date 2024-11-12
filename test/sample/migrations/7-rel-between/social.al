(component :Social)

(entity
 :Person
 {:Name :String})

(relationship
 :Friendship
 {:meta {:between [:Person :Person :as [:From :To]]}})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Social/Person? {:meta {:version "0.0.1"}} :as :P}
 [:for-each :P
  {:Social/Person {:Name :%.Name}}]
 {:Social/Friendship? {:meta {:version "0.0.1"}} :as :F}
 [:for-each :F
  {:Social/Friendship {:From :%.From :To :%.To}}])
