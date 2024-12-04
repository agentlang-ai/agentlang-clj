(component :Factory)

(entity
 :Customer
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})
