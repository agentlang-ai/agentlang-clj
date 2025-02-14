(ns agentlang.intercept.rbac
  (:require [clojure.set :as set]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.store.util :as stu]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            [agentlang.intercept.rbac.internal :as ri]))

(defn- can-do? [predic arg]
  (cond
    gs/audit-trail-mode true
    (gs/rbac-enabled?) (predic (gs/active-user) arg)
    :else true))

(def can-create? (partial can-do? ri/can-create?))
(def can-read? (partial can-do? ri/can-read?))
(def can-update? (partial can-do? ri/can-update?))
(def can-delete? (partial can-do? ri/can-delete?))

(defn find-owners [env inst-priv-entity respath]
  (mapv :Assignee (:result
                   (gs/evaluate-pattern {inst-priv-entity
                                         {:IsOwner? true
                                          :ResourcePath? respath}}))))

(defn handle-instance-privilege-assignment [env inst]
  (let [path (:ResourcePath inst)
        entity-name (li/entity-name-from-path path)
        inst-priv-entity (stu/inst-priv-entity entity-name)
        owners (find-owners env inst-priv-entity path)
        current-user (gs/active-user)]
    (if (some #{current-user} owners)
      (let [attrs0 (cn/instance-attributes inst)
            attrs (if (:IsOwner attrs0)
                    (assoc attrs0 :CanRead true :CanUpdate true :CanDelete true)
                    attrs0)]
        (gs/evaluate-pattern env {inst-priv-entity attrs})
        (u/throw-ex (str "Only an owner can assign instance-privileges to " path))))))
