(ns agentlang.evaluator.suspend
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.evaluator.state :as gs]))

(ln/component :Agentlang.Kernel.Eval)

(ln/entity
 :Agentlang.Kernel.Eval/Suspension
 {:Id {:type :UUID :default u/uuid-string :guid true}
  :Event :Any
  :OPCC :Int ;; opcode-counter
  :Env :Map})

(ln/dataflow
 :Agentlang.Kernel.Eval/SaveSuspension
 {:Agentlang.Kernel.Eval/Suspension
  {:Event :Agentlang.Kernel.Eval/SaveSuspension.Event
   :OPCC :Agentlang.Kernel.Eval/SaveSuspension.OPCC
   :Env :Agentlang.Kernel.Eval/SaveSuspension.Env}})

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadSuspension
 {:Agentlang.Kernel.Eval/SaveSuspension {:Id? :Agentlang.Kernel.Eval/LoadSuspension.Id}})

(def ^:dynamic suspension-info nil)

(defn restart-suspension [suspension]
  (binding [suspension-info {:env (:Env suspension) :opcc (:OPCC suspension)}]
    (let [r (first ((gs/get-active-evaluator) (:Event suspension)))]
      (:result r))))

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/RestartSuspension.Id} :as [:S]}
 [:eval '(agentlang.evaluator.suspend/restart-suspension :S)])

(defn save-suspension [evaluator event opcc env]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/SaveSuspension
                             {:Event event :OPCC opcc :Env env}}))]
    (when (= :ok (:status r))
      (:Id (first (:result r))))))

(defn load-suspension [evaluator id]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/LoadSuspension {:Id id}}))]
    (when (= :ok (:status r))
      (first (:result r)))))

(ln/entity
 :Agentlang.Kernel.Eval/SuspensionResult
 {:Id {:type :UUID :guid true}
  :Result :Any})

(defn- query-suspension-result [[_ {w :where}]]
  (when (and (= := (first w))
             (= :Id (second w)))
    (when-let [suspension (load-suspension (gs/get-active-evaluator) (nth w 2))]
      (let [r (restart-suspension suspension)]
        [(cn/make-instance
          :Agentlang.Kernel.Eval/SuspensionResult
          {:Id (nth w 2)
           :Result r})]))))

(ln/resolver
 :Agentlang.Kernel.Eval/SuspensionResultResolver
 {:with-methods {:query query-suspension-result}
  :paths [:Agentlang.Kernel.Eval/SuspensionResult]})
