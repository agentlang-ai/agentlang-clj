(component :Manager)

(entity
 :Manager/Customer
 {:Id {:type :Int :guid true}
  :Name :String})

(entity
 :Manager/Order
 {:Id {:type :Int :guid true}
  :CustomerId :Int
  :Date :Now})

(dataflow
 :Init
 {:Manager/Customer {:Id 10 :Name "User1"}}
 {:Manager/Customer {:Id 20 :Name "User2"}}
 {:Manager/Customer {:Id 30 :Name "User3"}}
 {:Manager/Order {:Id 1 :CustomerId 10}}
 {:Manager/Order {:Id 2 :CustomerId 10}}
 {:Manager/Order {:Id 3 :CustomerId 10}}
 {:Manager/Order {:Id 4 :CustomerId 20}}
 {:Manager/Order {:Id 5 :CustomerId 30}}
 {:Manager/Order {:Id 6 :CustomerId 30}})

