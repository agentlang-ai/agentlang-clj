(component :Office)

(entity
 :Worker
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})
