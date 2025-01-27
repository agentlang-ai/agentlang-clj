(ns agentlang.connections.client
  (:require [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.lang.internal :as li]
            [agentlang.lang.datetime :as dt]
            [agentlang.global-state :as gs]
            [agentlang.datafmt.json :as json]))

;; A client library for the integration-manager-service.

(def full-connection-name li/make-path)

(def ^:private integration-manager-config
  (memoize (fn [] (:integration-manager (gs/get-app-config)))))

(defn- connections-api-host []
  (or (:host (integration-manager-config))
      "http://localhost:5000"))

(defn- post-handler [response]
  (when (map? response)
    {:status (:status response)
     :body (json/decode (:body response))}))

(def ^:private auth-token (atom nil))

(defn- reset-auth-token []
  (let [conn-config (integration-manager-config)]
    (if-let [token (:token conn-config)]
      (do (reset! auth-token [token nil])
          token)
      (let [username (:username conn-config)
            password (:password conn-config)]
        (when (and username password)
          (let [response (http/do-post
                          (str (connections-api-host) "/login")
                          nil {:Agentlang.Kernel.Identity/UserLogin
                               {:Username username :Password password}}
                          :json post-handler)]
            (when (= 200 (:status response))
              (let [result (get-in (:body response) [:result :authentication-result])
                    token (:id-token result)
                    expiry-secs (when-let [s (:expires-in result)]
                                  (+ (dt/unix-timestamp) s))]
                (reset! auth-token [token expiry-secs])
                (log/debug (str "token refreshed. " (when expiry-secs (str "expires in " expiry-secs " seconds"))))
                token))))))))

(defn token-expired? [expiry-secs]
  (<= (- expiry-secs (dt/unix-timestamp)) 0))

(defn get-auth-token []
  (if-let [[token expiry-secs] @auth-token]
    (if (token-expired? expiry-secs)
      (reset-auth-token)
      token)
    (reset-auth-token)))

(defn- with-auth-token []
  (when-let [token (get-auth-token)]
    {:auth-token token}))

(defn- create-instance
  ([api-url ident inst callback]
   (try
     (let [response (http/do-post (http/url-encode api-url) (with-auth-token) inst :json post-handler)]
       (case (:status response)
         200 (let [r (first (:body response))]
               (if (= "ok" (:status r))
                 (callback (first (:result r)))
                 (log/error (str "failed to create - " ident))))
         401 (do (log/error "authentication required")
                 (reset! auth-token nil))
         (log/error (str "failed to create " ident " with status " (:status response)))))
     (catch Exception ex
       (log/error ex))))
  ([api-url ident inst] (create-instance api-url ident inst identity)))

(defn create-new-integration
  ([integ-name user-data]
   (create-instance
    (str (connections-api-host) "/api/IntegrationManager.Core/Integration")
    integ-name {:IntegrationManager.Core/Integration {:Name integ-name :UserData user-data}}))
  ([integ-name] (create-new-integration integ-name nil)))

(defn configure-new-connection [integ-name conn-name conn-attrs]
  (create-instance
   (str (connections-api-host)
        "/api/IntegrationManager.Core/Integration/"
        integ-name
        "/ConnectionConfigGroup/ConnectionConfig")
   conn-name {:IntegrationManager.Core/ConnectionConfig (merge {:Name conn-name} conn-attrs)}))

(def cached-connections (atom nil))

(defn get-connection [conn-name] (get @cached-connections conn-name))

(defn create-connection [conn-config-name conn-name]
  (or (get-connection conn-name)
      (create-instance
       (str (connections-api-host) "/api/IntegrationManager.Core/Connection")
       conn-name {:IntegrationManager.Core/Connection
                  {:ConnectionId (u/uuid-string)
                   :ConnectionConfigName (name conn-config-name)}}
       (fn [conn]
         (if-not (:Parameter conn)
           (log/error (str "failed to create connection for - " conn-name))
           (do (swap! cached-connections assoc conn-name conn)
               (assoc conn :CacheKey conn-name)))))))

(defn- lookup-connection-configs [integ-name]
  (try
    (let [api-url (str
                   (connections-api-host)
                   "/api/IntegrationManager.Core/Integration/"
                   integ-name
                   "/ConnectionConfigGroup/ConnectionConfig")
          response (http/do-get (http/url-encode api-url) (with-auth-token) :json post-handler)]
      (case (:status response)
        200 (let [r (first (:body response))]
              (when (= "ok" (:status r))
                (:result r)))
        401 (do (log/error "authentication required")
                (reset! auth-token nil))
        (log/error (str "failed to lookup connection-configs for " integ-name " with status " (:status response)))))
    (catch Exception ex
      (log/error ex))))

(defn- get-mapped-integ-name [integ-name]
  (when-let [integs (su/vec-as-map (get-in (gs/get-app-config) [:integration-manager :integrations]))]
    (when-let [n (get integs integ-name)]
      (u/keyword-as-string n))))

(defn find-connection-config-name [integ-name full-conn-name]
  (if-let [mapped-integ-name (get-mapped-integ-name integ-name)]
    (let [n (u/keyword-as-string full-conn-name)]
      (:Name
       (first (filter #(= n (get-in % [:UserData :ConnectionTypeName]))
                      (lookup-connection-configs mapped-integ-name)))))
    (do (log/warn (str "No mapping for integration " integ-name))
        nil)))

(defn open-connection
  ([integration-name connection-name]
   (let [conn-name (full-connection-name integration-name connection-name)]
     (or (get-connection conn-name)
         (if-let [conn-config-name (find-connection-config-name integration-name conn-name)]
           (create-connection conn-config-name conn-name)
           (u/throw-ex (str "Unable to find connection-configurations for " conn-name))))))
  ([conn-name] (apply open-connection (li/split-path conn-name))))

(def connection-parameter :Parameter)

(def cache-connection! create-connection)

(defn close-connection [conn]
  (when (get @cached-connections (:CacheKey conn))
    (try
      (let [response (http/do-request
                      :delete
                      (str (connections-api-host) "/api/IntegrationManager.Core/Connection/" (:ConnectionId conn))
                      (when-let [token (get-auth-token)]
                        {"Authorization" (str "Bearer " token)}))]
        (case (:status response)
          401 (do (log/error "authentication required")
                  (reset! auth-token nil))
          (when (= "ok" (get (first (:body response)) "status"))
            (swap! cached-connections dissoc (:CacheKey conn))
            true)))
      (catch Exception ex
        (log/error ex)))))

(defn refresh-connection [conn]
  (when (close-connection conn)
    (create-connection (:ConnectionConfigName conn) (:CacheKey conn))))
