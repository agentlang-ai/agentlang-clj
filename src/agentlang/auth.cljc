(ns agentlang.auth
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            #?(:clj [agentlang.auth.cognito])
            #?(:clj [agentlang.auth.okta])
            #?(:clj [agentlang.auth.keycloak])
            #?(:clj [agentlang.auth.df])
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.authentication :as authn]
            #?(:clj [agentlang.resolver.redis :as cache])))

(defn- maybe-signup-user [evaluator names email password]
  (if-let [user (first
                 (u/safe-ok-result
                  (evaluator
                   {:Agentlang.Kernel.Identity/FindUser
                    {:Email email}})))]
    user
    (u/safe-ok-result
     (evaluator {:Agentlang.Kernel.Identity/SignUp
                 {:User
                  {:Agentlang.Kernel.Identity/User
                   (merge
                    names
                    {:Password password
                     :Email email})}}}))))

(defn- email-to-names [email default-last-name]
  (let [[n _] (s/split email #"@")
        names (s/split n #"\.")
        first-name (first names)
        last-name (or (second names) default-last-name)]
    {:Name email
     :FirstName first-name
     :LastName last-name}))

(defn- setup-cache-resolver [config]
  #?(:clj
     (when config
       (try
         (let [resolver (cache/make :auth-cache config)]
           (rr/override-resolver [:Agentlang.Kernel.Identity/SessionCookie] resolver))
         (catch Exception ex
           (log/error ex))))))

(defn setup-resolver [config evaluator]
  (let [r-ident (authn/make :auth-identity config)
        r-roles (authn/make :auth-roles config)
        admin-email (:superuser-email config)
        admin-password (u/getenv "AGENTLANG_SUPERUSER_PASSWORD" "admin")]
    (when-not admin-email
      (u/throw-ex (str "superuser email not set in auth-config")))
    (when-not admin-password
      (u/throw-ex (str "AGENTLANG_SUPERUSER_PASSWORD not set")))
    ((if (:is-identity-store config) rr/override-resolver rr/compose-resolver)
     [:Agentlang.Kernel.Identity/User]
     r-ident)
    (rr/compose-resolver
     [:Agentlang.Kernel.Rbac/Role
      :Agentlang.Kernel.Rbac/RoleAssignment]
     r-roles)
    (when-not (maybe-signup-user
               evaluator (email-to-names admin-email "superuser")
               admin-email admin-password)
      (log/error (str "failed to create local user for " admin-email)))
    (when-not (setup-cache-resolver (:cache config))
      (log/warn "failed to setup cache for authentication"))
    true))
