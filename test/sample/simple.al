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

(entity :A {:Id {:type :Int :id true} :X :Int
            :meta {:audit true :actions {:create :CreateA :delete :DeleteA}}
            :rbac [{:roles ["user"] :allow [:create]}]})
(entity :B {:Id {:type :Int :id true} :Y :Int})
(relationship :AB {:meta {:contains [:A :B]}})

(entity :C {:meta {:actions {:update :UpdateC}} :Id {:type :Int :id true} :Z :Int})
(relationship :AC {:meta {:between [:A :C]}})

(defn init-as []
  (mapv (fn [id] {:Id id :X (* id 10)}) (range 10000)))

(dataflow
 :InitAs
 [:call '(sample.simple/init-as) :as :As]
 [:for-each :As
  {:A {} :from :%}])

(dataflow
 :FindAC
 {:C? {}
  :AC? {:A {:Id :FindAC.A} :as :A1}
  :into
  {:A :A1.X
   :CZ :C.Z}})

(dataflow
 :CreateA
 [:call '(println "create-a")]
 {:A {} :from :CreateA.Instance})

(dataflow
 :DeleteA
 [:call '(println "delete-a")]
 [:delete {:A {:__path__? :DeleteA.path}}])

(dataflow
 :UpdateC
 [:call '(println "update-c")]
 {:C {:__path__? :UpdateC.path
      :Z :UpdateC.Data.Z}})

;; Enable for testing auth+rbac
#_(dataflow
 :Agentlang.Kernel.Lang/AppInit
 [:call '(agentlang.util/getenv "SAMPLE_USER") :as :U]
 {:Agentlang.Kernel.Identity/User {:Email :U}}
 {:Agentlang.Kernel.Rbac/RoleAssignment {:Role "user" :Assignee :U}})

#_(dataflow
 :AssignPrivs
 {:B? {}
  :AB?
  {:A {:Id :AssignPrivs.Id}}
  :as :As}
 [:for-each :As
  {:Agentlang.Kernel.Rbac/AssignInstancePrivilege
   {:ResourcePath :%.__path__
    :Assignee :AssignPrivs.Email
    :CanRead :AssignPrivs.CanRead
    :CanUpdate :AssignPrivs.CanUpdate
    :CanDelete :AssignPrivs.CanDelete}}])
