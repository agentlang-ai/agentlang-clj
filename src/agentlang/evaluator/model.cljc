(ns agentlang.evaluator.model
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator.exec-graph :as exg]))

(ln/component :Agentlang.Kernel.Eval)

(ln/event
 li/exec-graph-node-event
 {:Pattern :Agentlang.Kernel.Lang/String
  :DfStart {:type :Agentlang.Kernel.Lang/Boolean :default false} ; dataflow-start?
  :DfEnd {:type :Agentlang.Kernel.Lang/Boolean :default false} ; dataflow-end?
  })

(ln/entity
 :Agentlang.Kernel.Eval/ExecGraph
 {:Key {:type :Agentlang.Kernel.Lang/UUID :default u/uuid-string :guid true}
  :Graph :Agentlang.Kernel.Lang/Any})

(defn exec-graph-infos [exec-graphs]
  (mapv (fn [exg]
          (let [g (:Graph exg)]
            {:Key (:Key exg) :Event (exg/graph-event g)}))
        exec-graphs))

(defn trim-graph [exg]
  (let [g (exg/trim-graph (:Graph exg))]
    (assoc exg :Graph g)))

(ln/event :Agentlang.Kernel.Eval/LookupEventsWithGraphs {})

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupEventsWithGraphs
 {:Agentlang.Kernel.Eval/ExecGraph? {} :as :Exgs}
 [:eval '(agentlang.evaluator.model/exec-graph-infos :Exgs)])

(ln/event :Agentlang.Kernel.Eval/GetExecGraph {:Key :Agentlang.Kernel.Lang/String})

(ln/dataflow
 :Agentlang.Kernel.Eval/GetExecGraph
 {:Agentlang.Kernel.Eval/ExecGraph {:Key? :Agentlang.Kernel.Eval/GetExecGraph.Key} :as [:Exg]}
 [:eval '(agentlang.evaluator.model/trim-graph :Exg)])
