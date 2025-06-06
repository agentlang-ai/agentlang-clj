(ns agentlang.user-session
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn- evaluate-dataflow-internal [event]
  (:result (gs/evaluate-dataflow-internal event)))

(defn session-lookup [user]
  (let [result (evaluate-dataflow-internal
                {:Agentlang.Kernel.Identity/LookupUserSession
                 {:User user}})]
    [(cond
       (not (seq result)) :not-found
       (cn/instance-of? :Agentlang.Kernel.Identity/UserSession result) :ok
       :else :error)
     result]))

(defn is-logged-in [user]
  (let [[status session] (session-lookup user)]
    (if (= :ok status)
      (:LoggedIn session)
      (u/throw-ex (str "failed to lookup session for user " user)))))

(defn ok? [session]
  (= :ok (first session)))

(defn session-exists-for? [user] (ok? (session-lookup user)))

(defn session-create [user logged-in]
  (evaluate-dataflow-internal
   {:Agentlang.Kernel.Identity/Create_UserSession
    {:Instance
     {:Agentlang.Kernel.Identity/UserSession
      {:User user :LoggedIn logged-in}}}}))

(defn session-update [user logged-in]
  (evaluate-dataflow-internal
   {:Agentlang.Kernel.Identity/Update_UserSession
    {:path (li/vec-to-path [:Agentlang.Kernel.Identity/UserSession user])
     :Data {:LoggedIn logged-in}}}))

(defn upsert-user-session [user-id logged-in]
  ((if (session-exists-for? user-id)
     session-update
     session-create)
   user-id logged-in))

(defn- normalize-sid [sid]
  (if-let [i (s/index-of sid "=")]
    (subs sid (inc i))
    sid))

(defn session-cookie-create [sid user-data]
  (evaluate-dataflow-internal
   {:Agentlang.Kernel.Identity/Create_SessionCookie
    {:Instance
     {:Agentlang.Kernel.Identity/SessionCookie
      (merge
       {:Id (normalize-sid sid)
        :UserData user-data
        :CreatedTimeMillis #?(:clj (System/currentTimeMillis) :cljs 0)}
       (when-let [ttl (get-in user-data [:authentication-result :expires-in])]
         {:TtlMs (* 1000 ttl)}))}}}))

(defn session-cookie-delete [sid]
  (evaluate-dataflow-internal
   {:Agentlang.Kernel.Identity/Delete_SessionCookie
    {:Id (normalize-sid sid)}}))

(defn session-cookie-replace [sid user-data]
  (session-cookie-delete sid)
  (session-cookie-create sid user-data))

(defn lookup-session-cookie-user-data [sid]
  (let [result (evaluate-dataflow-internal
                {:Agentlang.Kernel.Identity/LookupSessionCookie
                 {:Id (normalize-sid sid)}})]
    (when (and (seq result) (cn/instance-of? :Agentlang.Kernel.Identity/SessionCookie result))
      [(:UserData result) (:CreatedTimeMillis result) (li/path-attr result)])))

(defn session-cookie-update-tokens [sid tokens]
  (when-let [[user-data _ path] (lookup-session-cookie-user-data sid)]
    (let [authr (merge (:authentication-result user-data) tokens)
          user-data (assoc user-data :authentication-result authr)]
      (evaluate-dataflow-internal
       {:Agentlang.Kernel.Identity/Update_SessionCookie
        {:path path :Data {:UserData user-data}}})
      user-data)))

(defn maybe-assign-roles [email user-roles]
  (evaluate-dataflow-internal
   {:Agentlang.Kernel.Rbac/DeleteRoleAssignments
    {:Assignee email}})
  (doseq [role (if (string? user-roles)
                 (s/split user-roles #",")
                 user-roles)]
    (let [role-assignment (str role "-" email)]
      (when-not (evaluate-dataflow-internal
                 {:Agentlang.Kernel.Rbac/LookupRole
                  {:Name role}})
        (when-not (evaluate-dataflow-internal
                   {:Agentlang.Kernel.Rbac/Create_Role
                    {:Instance
                     {:Agentlang.Kernel.Rbac/Role
                      {:Name role}}}})
          (u/throw-ex (str "failed to create role " role))))
      (when-not (evaluate-dataflow-internal
                 {:Agentlang.Kernel.Rbac/LookupRoleAssignment
                  {:Name role-assignment}})
        (when-not (evaluate-dataflow-internal
                   {:Agentlang.Kernel.Rbac/Create_RoleAssignment
                    {:Instance
                     {:Agentlang.Kernel.Rbac/RoleAssignment
                      {:Name role-assignment :Role role :Assignee email}}}})
          (u/throw-ex (str "failed to assign role " role " to " email)))))))

(defn ensure-local-user [email user-roles]
  #?(:clj
     (try
       (let [r (first
                (evaluate-dataflow-internal
                 {:Agentlang.Kernel.Identity/FindUser
                  {:Email email}}))
             user
             (if (and (= :ok (:status r)) (seq (:result r)))
               (first (:result r))
               (let [r (first
                        (evaluate-dataflow-internal
                         {:Agentlang.Kernel.Identity/Create_User
                          {:Instance
                           {:Agentlang.Kernel.Identity/User
                            {:Email email}}}}))]
                 (when (= :ok (:status r))
                   (:result r))))]
         (when user-roles
           (maybe-assign-roles email user-roles))
         user)
       (catch Exception ex
         (log/error (str "ensure-local-user failed: " (.getMessage ex)))))))
