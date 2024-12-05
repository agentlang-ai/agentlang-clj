(component :Factory)

(entity
 :Customer
 {:Name :String
  :Age :Int
  :Gender {:oneof ["M" "F"]}})

(dataflow
 :Init
 {:Factory/Customer {:Name "Dan Smith" :Age 34 :Gender "M"}}
 {:Factory/Customer {:Name "Anna Brad" :Age 28 :Gender "F"}}
 {:Factory/Customer {:Name "John Jones" :Age 45 :Gender "M"}}
 {:Factory/Customer {:Name "Elizabeth Li" :Age 39 :Gender "F"}}
 {:Factory/Customer {:Name "Sam Brown" :Age 31 :Gender "M"}}
 {:Office/Worker {:Name "Sam Brown" :Age 31 :Gender "M"}}
 {:Office/Worker {:Name "John Jones" :Age 45 :Gender "M"}})
