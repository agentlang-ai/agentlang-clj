(component :Office)

(entity
 :Person
 {:Id {:type :Int :guid true}
  :Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(relationship
 :OfficeRel
 {:meta {:between [:Person :Person :as [:Manager :Reportee]]}})
