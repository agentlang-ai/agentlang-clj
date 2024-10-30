(component
 :Library.Catalog
 {:clj-import '[(:use [agentlang.lang.string]
                      [agentlang.lang.b64])
                (:require [agentlang.component :as cn])]
  :refer [:Library.Identity]})

(def valid-name? (partial string-in-range? 3 120))
(def app-name? valid-name?)
(def app-artifact? (partial string-in-range? 1 10485760)) ; max size - 10MB

(entity {:Book
         {:Name {:check library.catalog/app-name?}
          :ISBN {:type :String :guid true}
          :Publisher {:ref :Library.Identity/User.Email
                      :indexed true}
          :PublishDate :Now
          :LastUpdated :Now
          :IsCheckedout {:type :Boolean
                         :default false}
          :LastCheckout {:type :UUID
                         :optional true}}})

(event {:ListBooks {:Publisher :UUID}})

(dataflow :ListBooks {:Book {:Publisher? :ListBooks.Publisher}})

(event {:ListAllBooks {}})

(dataflow :ListAllBooks :Book?)
