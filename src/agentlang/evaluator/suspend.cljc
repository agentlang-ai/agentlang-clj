(ns agentlang.evaluator.suspend
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.env :as env]
            [agentlang.component :as cn]
            [agentlang.evaluator.state :as gs]))

(ln/component :Agentlang.Kernel.Eval)

(ln/entity
 :Agentlang.Kernel.Eval/Suspension
 {:Id {:type :String :default u/uuid-string :guid true}
  :Event :Any
  :OPCC :Int ;; opcode-counter
  :Env :Map
  :ValueAlias {:type :Keyword :optional true}})

(ln/dataflow
 :Agentlang.Kernel.Eval/SaveSuspension
 {:Agentlang.Kernel.Eval/Suspension
  {:Event :Agentlang.Kernel.Eval/SaveSuspension.Event
   :OPCC :Agentlang.Kernel.Eval/SaveSuspension.OPCC
   :Env :Agentlang.Kernel.Eval/SaveSuspension.Env
   :ValueAlias :Agentlang.Kernel.Eval/SaveSuspension.ValueAlias}})

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadSuspension
 {:Agentlang.Kernel.Eval/SaveSuspension {:Id? :Agentlang.Kernel.Eval/LoadSuspension.Id}})

(defn- maybe-bind-restart-value [env suspension restart-value]
  (if-let [alias (:ValueAlias suspension)]
    (env/bind-to-alias env (keyword alias) restart-value)
    env))

(def ^:dynamic suspension-info nil)

(defn restart-suspension [suspension restart-value]
  (binding [suspension-info {:env (maybe-bind-restart-value (:Env suspension) suspension restart-value)
                             :opcc (:OPCC suspension)}]
    (let [r (first ((gs/get-active-evaluator) (:Event suspension)))]
      (:result r))))

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/RestartSuspension.Id} :as [:S]}
 [:eval '(agentlang.evaluator.suspend/restart-suspension :S :Agentlang.Kernel.Eval/RestartSuspension.Value)])

(defn save-suspension [evaluator event opcc env alias]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/SaveSuspension
                             {:Event event :OPCC opcc :Env env :ValueAlias alias}}))]
    (when (= :ok (:status r))
      (:Id (first (:result r))))))

(defn load-suspension [evaluator id]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/LoadSuspension {:Id id}}))]
    (when (= :ok (:status r))
      (first (:result r)))))

(ln/entity
 :Agentlang.Kernel.Eval/SuspensionResult
 {:Id {:type :String :guid true}
  :Result :Any})

(defn- query-suspension-result [[_ {w :where}]]
  (when (and (= := (first w))
             (= :Id (second w)))
    (let [[id restart-value] (s/split (nth w 2) #"\$")]
      (when-let [result
                 (first
                  ((gs/get-active-evaluator)
                   {:Agentlang.Kernel.Eval/RestartSuspension
                    {:Id id :Value restart-value}}))]
        (when (= :ok (:status result))
          [(cn/make-instance
            :Agentlang.Kernel.Eval/SuspensionResult
            {:Id id
             :Result (:result result)})])))))

(ln/resolver
 :Agentlang.Kernel.Eval/SuspensionResultResolver
 {:with-methods {:query query-suspension-result}
  :paths [:Agentlang.Kernel.Eval/SuspensionResult]})
