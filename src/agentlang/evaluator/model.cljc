(ns agentlang.evaluator.model
  (:require [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]))

(ln/component :Agentlang.Kernel.Eval)

(ln/event
 li/exec-graph-node-event
 {:Pattern :Agentlang.Kernel.Lang/String})
