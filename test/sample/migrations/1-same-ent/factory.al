(component :Factory)

(entity
 :Customer
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(entity
 :CustomerMale
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M"]}})

(entity
 :Person
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Factory/Customer? {:meta {:version "0.0.3"}} :as :C}
 [:for-each :C
  {:Factory/Customer {:Name :%.Name :Age :%.Age :Gender :%.Gender}}]
 [:for-each :C
  {:Factory/Person {:Name :%.Name :Age :%.Age :Gender :%.Gender}}] 
 {:Factory/Customer {:Gender? "M" :meta {:version "0.0.3"}} :as :CM}
 [:for-each :CM
  {:Factory/CustomerMale {:Name :%.Name :Age :%.Age :Gender :%.Gender}}])
