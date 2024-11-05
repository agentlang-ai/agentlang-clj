(ns agentlang.connections.client
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.global-state :as gs]
            [agentlang.datafmt.json :as json]))

;; A client library for the connection-manager-service.

(def ^:private connection-manager-config
  (memoize (fn [] (:connection-manager (gs/get-app-config)))))

(defn- connections-api-host []
  (or (:host (connection-manager-config))
      "http://localhost:5000"))

(defn- post-handler [response]
  (when (map? response)
    {:status (:status response)
     :body (json/decode (:body response))}))

(def ^:private auth-token (atom nil))

(defn- reset-auth-token []
  (let [conn-config (connection-manager-config)
        username (:username conn-config)
        password (:password conn-config)]
    (when (and username password)
      (let [response (http/do-post
                      (str (connections-api-host) "/login")
                      nil {:Agentlang.Kernel.Identity/UserLogin
                           {:Username username :Password password}}
                      :json post-handler)]
        (when (= 200 (:status response))
          (let [token (get-in (:body response) [:result :authentication-result :id-token])]
            (reset! auth-token token)
            token))))))

(defn- with-auth-token []
  (if-let [token @auth-token]
    {:auth-token token}
    (when (reset-auth-token)
      (with-auth-token))))

(defn configure-new-connection [conn-name conn-attrs]
  (try
    (let [inst {:ConnectionManager.Core/ConnectionConfig (merge {:Name conn-name} conn-attrs)}
          response (http/do-post
                    (str (connections-api-host) "/api/ConnectionManager.Core/Create_ConnectionConfig")
                    (with-auth-token)
                    {:ConnectionManager.Core/Create_ConnectionConfig
                     {:Instance inst}} :json post-handler)]
      (case (:status response)
        200 (let [r (first (:body response))]
              (when (not= "ok" (:status r))
                (log/error (str "failed to configure connection - " conn-name)))
              inst)
        401 (do (log/error "authentication required")
                (reset! auth-token nil))
        :else (log/error (str "failed to configure connection " conn-name " with status " (:status response)))))
    (catch Exception ex
      (log/error ex))))

(def cached-connections (atom nil))

(defn get-connection [conn-name] (get @cached-connections conn-name))

(defn create-connection [conn-config-name conn-name]
  (or (get-connection conn-name)
      (try
        (let [response (http/do-post
                        (str (connections-api-host) "/api/ConnectionManager.Core/Connection")
                        (with-auth-token)
                        {:ConnectionManager.Core/Connection
                         {:ConnectionId (u/uuid-string)
                          :ConnectionConfigName (name conn-config-name)}} :json post-handler)]
          (case (:status response)
            200 (let [r (first (:body response))]
                  (if (= "ok" (:status r))
                    (let [conn (first (:result r))]
                      (if-not (:Parameter conn)
                        (log/error (str "failed to create connection for - " conn-name))
                        (do (swap! cached-connections assoc conn-name conn)
                            (assoc conn :CacheKey conn-name))))
                    (log/error (str "failed to get connection - " conn-name))))
            401 (do (log/error "authentication required")
                    (reset! auth-token nil))
            :else (log/error (str "failed to create connection " conn-name " with status " (:status response)))))
        (catch Exception ex
          (log/error ex)))))

(def connection-parameter :Parameter)

(def cache-connection! create-connection)

(defn close-connection [conn]
  (when (get @cached-connections (:CacheKey conn))
    (try
      (let [response (http/do-request
                      :delete
                      (str (connections-api-host) "/api/ConnectionManager.Core/Connection/" (:ConnectionId conn))
                      (when-let [token @auth-token]
                        {"Authorization" (str "Bearer " token)}))]
        (case (:status response)
          401 (do (log/error "authentication required")
                  (reset! auth-token nil))
          :else
          (when (= "ok" (get (first (:body response)) "status"))
            (swap! cached-connections dissoc (:CacheKey conn))
            true)))
      (catch Exception ex
        (log/error ex)))))

(defn refresh-connection [conn]
  (when (close-connection conn)
    (create-connection (:ConnectionConfigName conn) (:CacheKey conn))))
