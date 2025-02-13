(ns agentlang.intercept.rbac.internal
  (:require [agentlang.global-state :as gs]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]))

(defn get-superuser-email []
  (or (get-in (gs/get-app-config) [:authentication :superuser-email])
      "superuser@superuser.com"))

(defn- find-su-event []
  (cn/make-instance
   {:Agentlang.Kernel.Identity/FindUser
    {:Email (get-superuser-email)}}))

(def ^:private lookup-superuser
  (memoize
   (fn []
     (when-let [r (:result (gs/evaluate-dataflow (find-su-event)))]
       (first r)))))

(defn superuser? [user]
  (cn/same-instance? user (lookup-superuser)))

(defn superuser-email? [email]
  (when (seq email)
    (when-let [su (lookup-superuser)]
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
                 (:result
                  (gs/evaluate-dataflow-internal
                   {:Agentlang.Kernel.Rbac/FindPrivilegeAssignments
                    {:RoleNames role-names}})))))))

(def ^:private role-assignments
  (fn [user-name]
    (or (cached user-name)
        (cache user-name
               (:result
                (gs/evaluate-dataflow-internal
                 {:Agentlang.Kernel.Rbac/FindRoleAssignments
                  {:Assignee user-name}}))))))

(def ^:private admin-priv [{:Resource [:*] :Actions [:*]}])

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
                         (:result
                          (gs/evaluate-dataflow-internal
                           {:Agentlang.Kernel.Rbac/FindPrivileges
                            {:Names names}})))))))))))

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
      (and (some (partial has-priv-on-resource? resource) (:Resource p))
           (some #{action :*} (:Actions p))))
    privs)))

(defn- has-priv? [action userid arg]
  ;; Assumes - (not (superuser-email? userid))
  (let [resource arg
        privs (privileges userid)
        predic (partial filter-privs privs action true)]
    (predic resource)))

(def can-read? (partial has-priv? :read))
(def can-create? (partial has-priv? :create))
(def can-update? (partial has-priv? :update))
(def can-delete? (partial has-priv? :delete))
(def can-eval? (partial has-priv? :eval))
