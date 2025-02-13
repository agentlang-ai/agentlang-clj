(ns agentlang.intercept.rbac
  (:require [clojure.set :as set]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.intercept.rbac.internal :as ri]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn- handle-read-permission-checks [_ user arg]
  )

(defn- handle-delete-permission-checks [_ user arg]
  )

(defn- handle-create-permission-checks [_ user arg]
  (if (ri/can-create? user arg)
    arg
    (u/throw-ex (str user " has no create permission on " arg))))

(defn- handle-ownership-for-update [user arg]
  )

(defn- handle-update-permission-checks [_ user arg]
  (if (keyword? arg)
    (when (ri/can-update? user arg)
      arg)
    (if (map? arg)
      (handle-ownership-for-update user arg)
      (mapv (partial handle-ownership-for-update user) arg))))

(def ^:private handlers
  {:create handle-create-permission-checks
   :update handle-update-permission-checks
   :read handle-read-permission-checks
   :delele handle-delete-permission-checks})

(defn- run [env opr arg]
  (if gs/audit-trail-mode
    arg
    (if-let [user (gs/active-user)]
      (if (ri/superuser? user)
        arg
        (if-let [handler (opr handlers)]
          (handler env user arg)
          (u/throw-ex (str "rbac-interceptor cannot handle operation " opr))))
      arg)))

(defn make [_] ; config is not used
  {:name :rbac :fn run})

(defn- can-do? [predic arg]
  (cond
    gs/audit-trail-mode true
    (gs/rbac-enabled?) (predic (gs/active-user) arg)
    :else true))

(def can-create? (partial can-do? ri/can-create?))
(def can-read? (partial can-do? ri/can-read?))
(def can-update? (partial can-do? ri/can-update?))
(def can-delete? (partial can-do? ri/can-delete?))
