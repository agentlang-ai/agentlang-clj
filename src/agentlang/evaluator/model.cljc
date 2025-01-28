(ns agentlang.evaluator.model
  (:require [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]))

(ln/component :Agentlang.Kernel.Eval)

(ln/event
 li/exec-graph-node-event
 {:Pattern :Agentlang.Kernel.Lang/String
  :DfStart {:type :Agentlang.Kernel.Lang/Boolean :default false} ; dataflow-start?
  :DfEnd {:type :Agentlang.Kernel.Lang/Boolean :default false} ; dataflow-end?
  })
