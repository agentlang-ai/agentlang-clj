(ns agentlang.suspension
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.component :as cn]
            [agentlang.env :as env]            
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.core :as r]))

(ln/component :Agentlang.Kernel.Eval)

(ln/entity
 :Agentlang.Kernel.Eval/Suspension
 {:Id {:type :String :id true}
  :Patterns :Any
  :Env :Any
  :ValueAlias {:type :String :optional true}})

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

(defn suspension-id []
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
             :Patterns (li/sealed patterns)
             :Env (li/sealed (env/cleanup env false))
             :ValueAlias (when alias (name alias))}})]
      (set-suspension-id! nil)
      r))

(ln/event :Agentlang.Kernel.Eval/LoadSuspension {:Id :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/LoadSuspension.Id} :as [:S]}
 :S)

(defn- delete-suspension [suspension]
  (gs/evaluate-patterns
   [[:delete {:Agentlang.Kernel.Eval/Suspension {:Id? (:Id suspension)}}]
    [:delete :Agentlang.Kernel.Eval/Suspension :purge]]))

(defn- maybe-bind-restart-value [env suspension restart-value]
  (if-let [alias (:ValueAlias suspension)]
    (let [k (keyword alias)
          v0 (env/lookup-by-alias env k)
          v (if (and v0 (map? v0)) (merge v0 restart-value) restart-value)]
      (env/bind-to-alias env k v))
    env))

(defn restart-suspension [suspension restart-value]
  (when suspension
    (let [env (maybe-bind-restart-value (li/sealed-value (:Env suspension)) suspension restart-value)
          patterns (li/sealed-value (:Patterns suspension))
          r (:result (gs/evaluate-dataflow env patterns))]
      (delete-suspension suspension)
      r)))

(ln/event :Agentlang.Kernel.Eval/RestartSuspension {:Id :String :Value :Any})

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspension
 {:Agentlang.Kernel.Eval/Suspension {:Id? :Agentlang.Kernel.Eval/RestartSuspension.Id} :as [:S]}
 [:call '(agentlang.suspension/restart-suspension :S :Agentlang.Kernel.Eval/RestartSuspension.Value)])

(ln/entity
 :Agentlang.Kernel.Eval/Continue
 {:Id {:type :String :id true}
  :Result {:type :Any :optional true}})

(defn- parse-restarter-id [id]
  (let [[sid vs :as r] (s/split id #"\$")]
    (if vs
      [sid (into {} (mapv #(let [[k v] (s/split % #":")] [(keyword k) (read-string v)]) (s/split vs #",")))]
      r)))

(defn- extract-id-from-path [path]
  (let [idx (s/index-of path ",")]
    (subs path (inc idx))))

(defn- query-suspension-restarter [params]
  (let [qattrs (r/query-attributes params)
        [_ attr val] (first (vals qattrs))
        id (cond
             (= attr :Id) val
             (= attr li/path-attr) (extract-id-from-path val)
             :else (u/throw-ex (str "Cannot query continuation by " attr)))]
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
