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

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 [:> '(agentlang.util/getenv "SAMPLE_USER") :as :U]
 {:Agentlang.Kernel.Identity/User {:Email :U}}
 {:Agentlang.Kernel.Rbac/RoleAssignment {:Role "user" :Assignee :U}})
