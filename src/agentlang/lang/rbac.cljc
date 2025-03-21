(ns agentlang.lang.rbac
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]))

(def ^:private postproc-events (u/make-cell []))

(def ^:private inited-roles (u/make-cell #{}))
(def ^:private allow-all [:create :update :delete :read])
(def ^:private admin-rbac-spec {:roles ["admin"] :allow :*})

(defn- valid-perm? [s]
  (if (some #{s} allow-all)
    true
    false))

(defn- validate-perms [alw]
  (if (= :* alw)
    allow-all
    (if (and (seq alw) (every? true? (mapv valid-perm? alw)))
      alw
      (u/throw-ex (str "invalid permissions in " alw)))))

(defn- create-roles [roles spec]
  (when (or (not (seq roles)) (not (every? string? roles)))
    (u/throw-ex (str "invalid roles in " spec)))
  (when-let [roles (seq (set/difference roles (set @inited-roles)))]
    (let [r (mapv
             (fn [r]
               {:Agentlang.Kernel.Rbac/Role {:Name? r}
                li/except-tag
                {:not-found {:Agentlang.Kernel.Rbac/Role {:Name r}}}})
             roles)]
      (u/safe-set inited-roles (set/union roles @inited-roles))
      r)))

(defn- rbac-patterns [recname spec]
  (let [[c n] (li/split-path recname)]
    (mapv
     (fn [{roles :roles allow :allow}]
       (concat
        (create-roles (set roles) spec)
        (let [allow (validate-perms allow)
              pname (str "priv_" (name c) "_" (name n)
                         "_" (s/join "_" roles))]
          (concat
           [{:Agentlang.Kernel.Rbac/Privilege
             {:Name? pname
              :Actions [:q# allow]
              :Resource [:q# [recname]]}
             li/except-tag
             {:not-found
              {:Agentlang.Kernel.Rbac/Privilege
               {:Name pname
                :Actions [:q# allow]
                :Resource [:q# [recname]]}}}}]
           (mapv
            (fn [r]
              {:Agentlang.Kernel.Rbac/PrivilegeAssignment
               {:Role? r :Privilege? pname}
               li/except-tag
               {:not-found {:Agentlang.Kernel.Rbac/PrivilegeAssignment
                            {:Role r :Privilege pname}}}})
            roles)))))
     spec)))

(defn- conj-admin [spec]
  (if (some #{admin-rbac-spec} spec)
    spec
    (conj spec admin-rbac-spec)))

(defn- intern-rbac [evaluator recname spec]
  (when (seq spec)
    (let [spec (conj-admin spec)
          pats (vec (su/nonils (apply concat (rbac-patterns recname spec))))
          [c n] (li/split-path recname)
          event-name (li/make-path c (keyword (str (name n) "_reg_rbac")))]
      (cn/intern-event event-name {})
      (cn/register-dataflow event-name pats)
      (evaluator {event-name {}}))))

(defn- raw-spec [spec]
  (if (map? spec)
    (:spec spec)
    spec))

(defn- verify-rbac-spec [recname spec]
  (when-let [node (:owner spec)]
    (when-not (cn/between-relationship? recname)
      (u/throw-ex (str "rbac-owner can be specified only for between relatonships - " recname)))
    (when-not (cn/maybe-between-node-as-attribute recname node)
      (u/throw-ex (str "invalid rbac-owner - " node))))
  recname)

(defn rbac [recname spec]
  (if (map? spec)
    (verify-rbac-spec recname spec) ; used later by the interceptor
    (let [cont (fn [evaluator]
                 (when-let [spec (or (raw-spec spec) spec)]
                   (intern-rbac evaluator recname spec)))]
      (u/safe-set postproc-events (conj @postproc-events cont))
      recname)))

(defn eval-events [evaluator]
  (su/nonils
   (mapv #(:result (% evaluator)) @postproc-events)))

(defn reset-events! [] (u/safe-set postproc-events []))

(defn- ok? [r]
  (when r
    (cond
      (map? r) (= :ok (:status r))
      (seqable? r) (ok? (first r))
      :else false)))

(defn finalize-events
  ([evaluator]
   (let [rs (eval-events evaluator)]
     (when-not rs
       (u/throw-ex (str "post-process event failed - " rs)))
     (reset-events!)
     rs))
  ([] (finalize-events gs/evaluate-dataflow-internal)))
