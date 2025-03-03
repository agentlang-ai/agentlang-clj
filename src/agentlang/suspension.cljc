(ns agentlang.suspension
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.component :as cn]
            [agentlang.env :as env]            
            [agentlang.lang :as ln]
            [agentlang.resolver.core :as r]
            [agentlang.store :as store]))

(ln/component :Agentlang.Kernel.Eval)

(ln/entity
 :Agentlang.Kernel.Eval/Suspension
 {:Id {:type :String :id true}
  :Patterns :Any
  :Env :Map
  :ValueAlias {:type :Keyword :optional true}})

#?(:clj
   (def ^:private sid (ThreadLocal.))
   :cljs
   (def ^:dynamic sid nil))

(defn set-suspension-id! [id]
  #?(:clj
     (.set sid id)
     :cljs
     (reset! sid id)))

(defn get-suspension-id []
  #?(:clj
     (.get sid)
     :cljs
     @sid))

(defn suspension-id [flag]
  (or (get-suspension-id)
      (let [id (u/uuid-string)]
        (set-suspension-id! id)
        id)))

(defn suspend-dataflow []
  (gs/set-dataflow-suspended! true)
  true)

(defn revoke-dataflow [] (gs/set-dataflow-suspended! false) true)

(def dataflow-suspended? gs/dataflow-suspended?)

(defn as-suspended [obj] (and (suspend-dataflow) obj))

(defn save [env patterns alias]
  (revoke-dataflow)
  (let [r (gs/evaluate-pattern
           {:Agentlang.Kernel.Eval/Suspension
            {:Id (or (suspension-id) (u/uuid-string))
             :Patterns patterns
             :Env (env/cleanup env false)
             :ValueAlias alias}})]
      (set-suspension-id! nil)
      r))

(defn- maybe-bind-restart-value [env suspension restart-value]
  (if-let [alias (:ValueAlias suspension)]
    (env/bind-to-alias env (keyword alias) restart-value)
    env))

(defn restart-suspension [suspension restart-value]
  (let [store (store/get-default-store)
        env (maybe-bind-restart-value (:Env suspension) suspension restart-value)
        patterns (:Patterns suspension)]
    (:result (gs/evaluate-dataflow store env patterns))))

(ln/event :Agentlang.Kernel.Eval/RestartSuspension {:Id :String :Value :Any})

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/RestartSuspension.Id} :as [:S]}
 [:call '(agentlang.evaluator.suspend/restart-suspension :S :Agentlang.Kernel.Eval/RestartSuspension.Value)])

(ln/entity
 :Agentlang.Kernel.Eval/Continue
 {:Id {:type :String :id true}
  :Result {:type :Any :optional true}})

(defn- parse-restarter-id [id]
  (let [[sid vs :as r] (s/split id #"\$")]
    (if vs
      [sid (into {} (mapv #(let [[k v] (s/split % #":")] [(keyword k) (read-string v)]) (s/split vs #",")))]
      r)))

(defn- query-suspension-restarter [params]
  (let [qattrs (r/query-attributes params)
        [_ _ id] (first (vals qattrs))]
    (let [[susp-id value] (parse-restarter-id id)
          result
          (gs/evaluate-dataflow
           {:Agentlang.Kernel.Eval/RestartSuspension
            {:Id susp-id :Value value}})]
      [(cn/make-instance
        :Agentlang.Kernel.Eval/Continue
        {:Id id :Result (:result result)})])))

(ln/resolver
 :Agentlang.Kernel.Eval/SuspensionRestarterResolver
 {:with-methods
  {:create identity
   :query query-suspension-restarter}
  :paths [:Agentlang.Kernel.Eval/Continue]})
