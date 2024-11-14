(component :Manager)

(entity
 :Manager/CustomerOrder
 {:CustomerName :String
  :CustomerId :Int
  :OrderId :Int})

(entity
 :Manager/Order
 {:Id {:type :Int :guid true}
  :CustomerId :Int
  :Date :Now})

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Manager/Order? {:meta {:version "0.3.1"}}
  :join [{:Manager/Customer {:Id? :Manager/Order.CustomerId :meta? {:version "0.3.1"}}}]
  :with-attributes {:CustomerName :Manager/Customer.Name
                    :CustomerId :Manager/Customer.Id
                    :OrderId :Manager/Order.Id}
  :as :F}
 [:for-each :F
  {:Manager/CustomerOrder
   {:CustomerName :%.CustomerName
    :CustomerId :%.CustomerId
    :OrderId :%.OrderId}}])
