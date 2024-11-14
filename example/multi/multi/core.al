(component
 :Multi.Core
 {:clj-import '[(:require [agentlang.util.logger :as log])]
  :refer [:Multi.Erp]})

(defn valid-company-name? [s]
  (let [r (and (string? s)
               (<= 5 (count s) 50))]
    (when-not r
      (log/warn (str "invalid company name - " s)))
    r))

(entity
 :Company
 {:Name {:guid true :check multi.core/valid-company-name?}})

(relationship
 :CompanyEmployee
 {:meta {:contains [:Company :Multi.Erp/Employee]}})
