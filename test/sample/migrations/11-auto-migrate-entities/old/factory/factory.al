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

(dataflow
 :Init
 {:Factory/Customer1 {:Name "Dan Smith" :Age 34 :Gender "M"}}
 {:Factory/Customer1 {:Name "Anna Brad" :Age 28 :Gender "F"}}
 {:Factory/Customer2 {:Name "John Jones" :Age 45 :Gender "M"}}
 {:Factory/Customer2 {:Name "Elizabeth Li" :Age 39 :Gender "F"}}
 {:Factory/Customer3 {:Name "Sam Brown" :Age 31 :Gender "M"}}
 {:Retail/User {:Name "Dan Smith" :Age 34 :Gender "M"}}
 {:Retail/User {:Name "Anna Brad" :Age 28 :Gender "F"}}
 {:Office/Person {:Id 10 :Name "Dan Smith" :Age 34 :Gender "M"}}
 {:Office/Person {:Id 15 :Name "Anna Brad" :Age 28 :Gender "F"}}
 {:Office/OfficeRel {:Manager 10 :Reportee 15}})
