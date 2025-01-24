(ns agentlang.evaluator.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang :as ln]
            [agentlang.evaluator.state :as es]))

(ln/entity
 :Agentlang.Kernel.Eval/PushPattern
 {:Pattern :String})

(ln/entity
 :Agentlang.Kernel.Eval/PopPattern
 {:Result {:type :Any :default es/get-last-eval-result}})

(defn- create-node [instance]
  (case (cn/instance-type-kw instance)
    :Agentlang.Kernel.Eval/PushPattern
    (do (println ">>>> ") (u/pprint (u/parse-string (:Pattern instance))) instance)

    :Agentlang.Kernel.Eval/PopPattern
    (do (println "<<<< ") (u/pprint (:Result instance)) (:Result instance))

    instance))

(ln/resolver
 :Agentlang.Kernel.Eval/ExecGraphResolver
 {:with-methods
  {:create create-node}
  :paths [:Agentlang.Kernel.Eval/PushPattern :Agentlang.Kernel.Eval/PopPattern]})
