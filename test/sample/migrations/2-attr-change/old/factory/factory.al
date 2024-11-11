(component :Factory)

(entity
 :Shipment
 {:Price :Int
  :Quantity :Int
  :Country :String
  :CustomerFirstName :String
  :CustomerLastName :String})

(dataflow
 :Init
 {:Factory/Shipment {:Price 482 :Quantity 4 :Country "USA" :CustomerFirstName "Dan" :CustomerLastName "Smith"}}
 {:Factory/Shipment {:Price 112 :Quantity 3 :Country "Australia" :CustomerFirstName "Anna" :CustomerLastName "Brad"}}
 {:Factory/Shipment {:Price 485 :Quantity 16 :Country "Ireland" :CustomerFirstName "John" :CustomerLastName "Jones"}}
 {:Factory/Shipment {:Price 162 :Quantity 18 :Country "Ireland" :CustomerFirstName "Brad" :CustomerLastName "Davidson"}}
 {:Factory/Shipment {:Price 91 :Quantity 12 :Country "UK" :CustomerFirstName "Sam" :CustomerLastName "Brown"}})