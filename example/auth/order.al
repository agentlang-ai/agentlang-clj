(component :Acme)

(entity
 :Order
 {:order_id {:type :String :guid true}
  :order_qty :Int
  :order_amount :Decimal
  :tnc_agreement {:type :Boolean :default true}
  :submission_date :Now
  :rbac [{:roles ["order-users"] :allow [:create]}]})

(relationship
 :UserOrder
 {:meta {:between [:Fractl.Kernel.Identity/User :Acme/Order]}
  :rbac [{:roles ["order-users"] :allow [:create]}]})

(dataflow
 :CreateOrder
 {:Order
  {:order_id :CreateOrder.order_id
   :order_qty :CreateOrder.order_qty
   :order_amount :CreateOrder.order_amount}
  :as :O}
 {:UserOrder
  {:User :CreateOrder.EventContext.User
   :Order :O.order_id}}
 :O)
