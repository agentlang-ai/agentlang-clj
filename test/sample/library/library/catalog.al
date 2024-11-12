(component
 :Library.Catalog
 {:clj-import '[(:require [agentlang.lang.string :as str])]
  :refer [:Library.Identity]})

(defn app-name? [s]
  (str/string-in-range? 3 120 s))

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
