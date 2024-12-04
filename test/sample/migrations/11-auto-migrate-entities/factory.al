(component :Factory)

(entity
 :Customer1
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(entity
 :Customer2
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(entity
 :Customer3
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})
