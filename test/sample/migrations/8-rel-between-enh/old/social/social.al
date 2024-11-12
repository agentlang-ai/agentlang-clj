(component :Social)

(entity
 :Person
 {:Name :String})

(relationship
 :Friendship
 {:meta {:between [:Person :Person :as [:From :To]]}})

(dataflow
 :Init
 {:Social/Person {:Name "Person1"} :as :P1}
 {:Social/Person {:Name "Person2"} :as :P2}
 {:Social/Person {:Name "Person3"} :as :P3}
 {:Social/Person {:Name "Person4"} :as :P4}
 {:Social/Person {:Name "Person5"} :as :P5}
 {:Social/Person {:Name "Person6"} :as :P6}
 {:Social/Person {:Name "Person7"} :as :P7}
 {:Friendship {:From :P1.__Id__ :To :P2.__Id__}}
 {:Friendship {:From :P1.__Id__ :To :P3.__Id__}}
 {:Friendship {:From :P1.__Id__ :To :P4.__Id__}}
 {:Friendship {:From :P2.__Id__ :To :P6.__Id__}}
 {:Friendship {:From :P2.__Id__ :To :P7.__Id__}}
 {:Friendship {:From :P3.__Id__ :To :P4.__Id__}})
