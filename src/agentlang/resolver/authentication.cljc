(ns agentlang.resolver.authentication
  (:require [agentlang.util :as u]
            [agentlang.util.http :as uh]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [agentlang.auth.core :as auth]))

(def ^:private crdel-fns
  {:user {:create auth/call-upsert-user
          :delete auth/call-delete-user}
   :role {:create auth/create-role
          :delete auth/delete-role}
   :role-assignment {:create auth/add-user-to-role
                     :delete auth/remove-user-from-role}})

(defn- crdel [tag client config inst]
  (let [n (cn/instance-type-kw inst)]
    (case n
      :Agentlang.Kernel.Rbac/RoleAssignment
      (and ((tag (:role-assignment crdel-fns))
            (assoc config auth/client-key client
                   :role-name (:Role inst)
                   :username (:Assignee inst)))
           inst)
      :Agentlang.Kernel.Rbac/Role
      (and ((tag (:role crdel-fns))
            (assoc config auth/client-key client
                   :role-name (:Name inst)))
           inst)
      ((tag (:user crdel-fns)) client config inst))))

(def ^:private create (partial crdel :create))
(def ^:private delete (partial crdel :delete))

(defn- lookup [auth-service params]
  (let [entity-name (r/query-entity-name params)]
    (when (= (li/split-path entity-name) [:Agentlang.Kernel.Identity :User])
      (let [qattrs (r/query-attributes params)]
        (if (r/query-all? qattrs)
          (auth/lookup-all-users auth-service)
          (auth/lookup-users (assoc auth-service auth/query-key (first (vals qattrs)))))))))

(defmake :authentication
  (fn [resolver-name config]
    (let [config (merge config (when (auth/cognito? config) (uh/get-aws-config)))]
      (if-let [client (auth/make-client config)]
        (r/make-resolver
         resolver-name
         {:create {:handler (partial create client config)}
          :update {:handler (partial auth/call-upsert-user client config :update)}
          :delete {:handler (partial delete client config)}
          :query {:handler (partial lookup (assoc config auth/client-key client))}})
        (u/throw-ex (str "failed to create auth-client for " resolver-name))))))
