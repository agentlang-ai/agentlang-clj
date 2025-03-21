(ns agentlang.rbac.core
  (:require [agentlang.global-state :as gs]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.evaluator :as ev]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator.intercept.internal :as ii]))

(defn get-superuser-email []
  (or (get-in (gs/get-app-config) [:authentication :superuser-email])
      "superuser@superuser.com"))

(def ^:private superuser (atom nil))

(defn- find-su-event []
  (cn/make-instance
   {:Agentlang.Kernel.Identity/FindUser
    {:Email (get-superuser-email)}}))

(defn- lookup-superuser []
  (when-let [r (ev/safe-eval (find-su-event))]
    (first r)))

(defn init
  ([config]
   (when-let [su (lookup-superuser)]
     (reset! superuser su))
   true)
  ([] (init nil)))

(defn superuser? [user]
  (cn/same-instance? user @superuser))

(defn superuser-email? [email]
  (when (seq email)
    (when-let [su @superuser]
      (= email (:Email su)))))

#?(:clj
   (def ^:private local-cache (ThreadLocal.)))

(defn- cached [key]
  #?(:clj
     (when-let [c (.get local-cache)]
       (get c key))))

(defn- cache [key val]
  #?(:clj
     (let [c (or (.get local-cache) {})]
       (.set local-cache (assoc c key val))
       val)
     :cljs val))

(def ^:private find-privileges
  (fn [role-names]
    (when (seq role-names)
      (or (cached role-names)
          (cache role-names
                 (ev/safe-eval-internal
                  {:Agentlang.Kernel.Rbac/FindPrivilegeAssignments
                   {:RoleNames role-names}}))))))

(def ^:private role-assignments
  (fn [user-name]
    (or (cached user-name)
        (cache user-name
               (ev/safe-eval-internal
                {:Agentlang.Kernel.Rbac/FindRoleAssignments
                 {:Assignee user-name}})))))

(def ^:private admin-priv [{:Resource [:*] :Actions [:*]}])

(defn has-admin-privilege? [privs]
  (and (seqable? privs)
       (let [apriv (first admin-priv)]
         (first (filter #(= % apriv) privs)))))

(def privileges
  (fn [user-name]
    (when-let [rs (role-assignments user-name)]
      (let [role-names (mapv :Role rs)]
        (if (some #{"admin"} role-names)
          admin-priv
          (let [ps (find-privileges role-names)
                names (mapv :Privilege ps)]
            (when (seq names)
              (or (cached names)
                  (cache names
                         (ev/safe-eval-internal
                          {:Agentlang.Kernel.Rbac/FindPrivileges
                           {:Names names}}))))))))))

(defn- has-priv-on-resource? [resource priv-resource]
  (or (if (or (= :* priv-resource)
              (= resource priv-resource))
        true
        (let [[rc rn :as r] (li/split-path resource)
              [prc prn :as pr] (li/split-path priv-resource)]
          (cond
            (= r pr) true
            (and (= rc prc)
                 (= prn :*)) true
            :else false)))
      (when-let [parents (seq (cn/containing-parents resource))]
        (some (fn [[_ _ p]] (has-priv-on-resource? p priv-resource)) parents))))

(defn- filter-privs [privs action ignore-refs resource]
  (seq
   (filter
    (fn [p]
      (and (some (partial has-priv-on-resource? resource)
                 (map #(if (and ignore-refs (not= % :*))
                         (li/root-path %)
                         %)
                      (:Resource p)))
           (some #{action :*} (:Actions p))))
    privs)))

(defn- has-priv? [action userid arg]
  ;; Assumes - (not (superuser-email? userid))
  (let [resource (:data arg)
        privs (privileges userid)
        predic (partial filter-privs privs action (:ignore-refs arg))]
    (if (ii/attribute-ref? resource)
      (let [rp (li/root-path resource)]
        (or (predic rp)
            (predic resource)))
      (predic resource))))

(def can-read? (partial has-priv? :read))
(def can-create? (partial has-priv? :create))
(def can-update? (partial has-priv? :update))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))

(def ^:private priv-assignment-callbacks (atom nil))

(defn register-privilege-assignment-callback [type-name callback-fn]
  (swap! priv-assignment-callbacks assoc type-name callback-fn)
  type-name)

(defn run-privilege-assignment-callback
  ([tag inst user privs]
   ;; tag - oneof [:ownership :instpriv]
   ;; inst - instance for which privilege assignment happens
   ;; user - assignee
   ;; privs - optional, setof actions in the case of :instpriv
   (let [type-name (cn/instance-type-kw inst)]
     (when-let [callback-fn (get @priv-assignment-callbacks type-name)]
       (callback-fn tag inst user privs))))
  ([tag inst user] (run-privilege-assignment-callback tag inst user nil)))

(def run-instance-privilege-assignment-callback
  (partial run-privilege-assignment-callback :instpriv))

(def run-ownership-assignment-callback
  (partial run-privilege-assignment-callback :ownership))
