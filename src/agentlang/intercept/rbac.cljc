(ns agentlang.intercept.rbac
  (:require [clojure.set :as set]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.store.util :as stu]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            [agentlang.intercept.rbac.internal :as ri]))

(defn- can-do? [predic arg]
  (if (gs/rbac-enabled?)
    (predic (gs/active-user) arg)
    true))

(def can-create? (partial can-do? ri/can-create?))
(def can-read? (partial can-do? ri/can-read?))
(def can-update? (partial can-do? ri/can-update?))
(def can-delete? (partial can-do? ri/can-delete?))

(defn find-owners [env inst-priv-entity respath]
  (mapv :Assignee (:result
                   (gs/kernel-call
                    #(gs/evaluate-pattern
                      env {inst-priv-entity
                           {:IsOwner? true
                            :ResourcePath? respath}})))))

(defn- fetch-inst-priv-info [env inst]
  (let [path0 (:ResourcePath inst)
        path (str path0 "%")
        entity-name (li/entity-name-from-path path)
        inst-priv-entity (stu/inst-priv-entity entity-name)
        current-user (gs/active-user)]
    (if (or (ri/superuser-email? current-user)
            (ri/has-role? "admin" current-user)
            (some #{current-user} (find-owners env inst-priv-entity path)))
      [path entity-name inst-priv-entity]
      (u/throw-ex (str "Only an owner can assign or remove instance-privileges on " path0) :forbidden))))

(defn handle-instance-privilege-assignment [env inst]
  (let [[path entity-name inst-priv-entity] (fetch-inst-priv-info env inst)
        attrs0 (assoc (cn/instance-attributes inst) :ResourcePath path)
        attrs (if (:IsOwner attrs0)
                (assoc attrs0 :CanRead true :CanUpdate true :CanDelete true)
                attrs0)
        r (:result (gs/kernel-call #(gs/evaluate-pattern env {inst-priv-entity attrs})))]
    (when-not (cn/instance-of? inst-priv-entity r)
      (u/throw-ex (str "Failed to assign instance privilege to " path)))
    (doseq [child-entity-name (cn/all-contained-children-names entity-name)]
      (let [inst-priv-entity (stu/inst-priv-entity child-entity-name)
            r (:result (gs/kernel-call #(gs/evaluate-pattern env {inst-priv-entity attrs})))]
        (when-not (cn/instance-of? inst-priv-entity r)
          (u/throw-ex (str "Failed to assign instance privilege to " path " for " child-entity-name)))))
    r))

(defn delete-instance-privilege-assignment [env inst]
  (let [[path entity-name inst-priv-entity] (fetch-inst-priv-info env inst)
        attrs0 (cn/instance-attributes inst)
        attrs {:ResourcePath? path :Assignee? (:Assignee attrs0)}
        result (gs/kernel-call #(gs/evaluate-pattern env [:delete {inst-priv-entity attrs}]))]
    (when (seq (:result result))
      (gs/kernel-call #(gs/evaluate-pattern env [:delete inst-priv-entity :purge]))
      (doseq [child-entity-name (cn/all-contained-children-names entity-name)]
        (let [inst-priv-entity (stu/inst-priv-entity child-entity-name)]
          (gs/kernel-call #(gs/evaluate-pattern env [:delete {inst-priv-entity attrs}]))
          (gs/kernel-call #(gs/evaluate-pattern env [:delete inst-priv-entity :purge]))))
      result)))
