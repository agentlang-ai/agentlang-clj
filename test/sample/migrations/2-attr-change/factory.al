(component :Factory)

(entity
 :Shipment
 {:MinPrice {:type :Float :default 10}
  :MaxPrice :Float 
  :Amount :Int
  :BuyerName :String
  :Address :String
  :Verified {:type :Boolean :default true}})

(defn concat-names [firstname lastname]
  (str firstname " " lastname))

(dataflow
 :Agentlang.Kernel.Lang/Migrations
 {:Factory/Shipment? {:meta {:version "0.0.1"}} :as :S}
 [:for-each :S
  {:Factory/Shipment
   {:MaxPrice :%.Price
    :Amount :%.Quantity
    :BuyerName (quote (concat-names :%.CustomerFirstName :%.CustomerLastName))
    :Address :%.Country}}])
