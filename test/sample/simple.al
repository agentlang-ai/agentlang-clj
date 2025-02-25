(ns sample.simple
  (:use [agentlang.lang]))

(component :Sample.Simple)

(entity
 :E1
 {:Id {:type :Int :id true}
  :A :Int
  :B :Int
  :C :Int
  :X {:type :String
      :write-only true}
  :Y :Now
  :rbac [{:roles ["user"] :allow [:create]}]})

(dataflow
 :K
 {:E1 {:Id :K.Id
       :A '(+ 5 :B)
       :B :K.Data.I
       :C '(+ 10 :A)
       :X "secret"
       :Y '(agentlang.lang.datetime/now)}})

(entity :A {:Id {:type :Int :id true} :X :Int})
(entity :B {:Id {:type :Int :id true} :Y :Int})
(relationship :AB {:meta {:contains [:A :B]}})

(entity :C {:Id {:type :Int :id true} :Z :Int})
(relationship :AC {:meta {:between [:A :C]}})

(dataflow
 :FindAC
 {:C? {}
  :AC? {:A {:Id :FindAC.A} :as :A1}
  :into
  {:A :A1.X
   :CZ :C.Z}})

;; Enable for testing auth+rbac
#_(dataflow
 :Agentlang.Kernel.Lang/AppInit
 [:> '(agentlang.util/getenv "SAMPLE_USER") :as :U]
 {:Agentlang.Kernel.Identity/User {:Email :U}}
 {:Agentlang.Kernel.Rbac/RoleAssignment {:Role "user" :Assignee :U}})
