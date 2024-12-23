(component :Loyalty.Core)

(entity :Customer
 {:Email {:type :Email :guid true}
  :Name :String
  :Joined :Now})

(entity :Point
 {:Value :Int
  :Created :Now
  :meta {:doc "Loyalty point earned by a customer for each purchase."}})

(relationship :CustomerPoint
 {:meta {:between [:Customer :Point]}})

(entity :Purchase
 {:CustomerEmail :Email
  :InvoiceNo {:type :String :guid true}
  :Date :Now
  :Amount :Decimal})

(defn compute-points
  "Compute the loyalty point based on the purchase amount."
  [amt]
  (cond
    (>= amt 5000.0) 10
    (>= amt 2000.0) 5
    :else 2))

(dataflow [:after :create :Purchase]
 "When a new purchase happens, use its amount to compute a loyalty point for the customer."
 [:match
  [:> :Instance.Amount 1000.0]
  {:Point {:Value '(loyalty.core/compute-points :Instance.Amount)}
   :-> [[{:CustomerPoint {}} {:Customer {:Email? :Instance.CustomerEmail}}]]}
  :Instance])

(defn total-points [ps]
  (reduce + 0 (map :Value ps)))

(dataflow :CustomerLoyaltyPoints
 "Return the total loyalty-points for a customer."
 {:Point? {} :-> [[{:CustomerPoint {:Customer? :CustomerLoyaltyPoints.CustomerEmail}}]]
  :as :Result}
 [:eval '(loyalty.core/total-points :Result)])
