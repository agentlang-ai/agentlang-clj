(ns agentlang.evaluator.suspend
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.env :as env]
            [agentlang.component :as cn]
            [agentlang.evaluator.state :as gs]))

(ln/component :Agentlang.Kernel.Eval)

(def ^:private active-suspension-id #?(:clj (ThreadLocal.)
                                       :cljs (atom nil)))

(defn init-suspension-id []
  (let [id (u/uuid-string)]
    #?(:clj (.set active-suspension-id id)
       :cljs (reset! active-suspension-id id))
    id))

(defn get-suspension-id []
  #?(:clj (.get active-suspension-id)
     :cljs @active-suspension-id))

(defn- fetch-suspension-id-once []
  (let [id (get-suspension-id)]
    (and (init-suspension-id) id)))

(ln/entity
 :Agentlang.Kernel.Eval/Suspension
 {:Id {:type :String :guid true}
  :Event :Any
  :OPCC :Int ;; opcode-counter
  :Env :Map
  :ValueAlias {:type :Keyword :optional true}})

(ln/event :Agentlang.Kernel.Eval/LoadSuspension {:Id :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/LoadSuspension.Id}})

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

(ln/event :Agentlang.Kernel.Eval/RestartSuspension {:Id :String :Value :Any})

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/RestartSuspension.Id} :as [:S]}
 [:eval '(agentlang.evaluator.suspend/restart-suspension :S :Agentlang.Kernel.Eval/RestartSuspension.Value)])

(defn save-suspension [evaluator event opcc env alias]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/Create_Suspension
                             {:Instance
                              {:Agentlang.Kernel.Eval/Suspension
                               {:Id (fetch-suspension-id-once)
                                :Event event
                                :OPCC opcc
                                :Env env
                                :ValueAlias alias}}}}))]
    (when (= :ok (:status r))
      (:Id (first (:result r))))))

(defn load-suspension [evaluator id]
  (let [r (first (evaluator {:Agentlang.Kernel.Eval/LoadSuspension {:Id id}}))]
    (when (= :ok (:status r))
      (first (:result r)))))
