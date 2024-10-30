(component :Logs)

(entity
 :LogEntry
 {:Id {:type :Int :guid true}
  :Message :String
  :Created :Now
  :Level {:oneof ["info" "debug" "warn" "error"]}})

(dataflow
 :RecentErrorLogs
 {:LogEntry {:Level? "error"
             :Created? [:> :RecentErrorLogs.Date]}})

(entity
 :Customer
 {:Id {:type :Int :guid true}
  :Name :String})

(entity
 :Order
 {:Id {:type :Int :guid true}
  :CustomerId :Int
  :Date :Now})

(dataflow
 :CustomerOrders
 {:Order? {}
  :join [{:Customer {:Id? :Order.CustomerId}}]
  :with-attributes {:CustomerName :Customer.Name
                    :CustomerId :Customer.Id
                    :OrderId :Order.Id}})

(view
 :CustomerOrder
 {:CustomerName :Customer.Name
  :CustomerId :Customer.Id
  :OrderId :Order.Id
  :query {:Order? {}
          :join [{:Customer {:Id? :Order.CustomerId}}]}})

(view
 :CustomerName
 {:name :Customer.Name
  :query {:Customer? {}}})

(dataflow
 :Customers
 {:Customer {:Id? [:>= :Customers.Id]}})

(require '[agentlang.resolver.registry :as r])
(use '[agentlang.resolver.click-house])

(r/register-resolver {:name :click-house-logs
                      :type :click-house
                      :paths [:Logs/LogEntry :Logs/Customer :Logs/Order
                              :Logs/CustomerOrder :Logs/CustomerName]})
