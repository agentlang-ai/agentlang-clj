(ns agentlang.connections.client
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.global-state :as gs]))

;; A client library for the connection-manager-service.

(defn- connections-api-host []
  (or (:host (:connection-manager (gs/get-app-config)))
      "http://localhost:5000"))

(defn configure-new-connection [conn-name conn-attrs]
  (try
    (let [inst {:ConnectionManager.Core/ConnectionConfig (merge {:Name conn-name} conn-attrs)}
          r (first
             (http/POST (str (connections-api-host) "/api/ConnectionManager.Core/Create_ConnectionConfig")
                        nil {:ConnectionManager.Core/Create_ConnectionConfig
                             {:Instance inst}} :json))]
      (when (not= "ok" (:status r))
        (log/error (str "failed to configure connection - " conn-name)))
      inst)
    (catch Exception ex
      (log/error ex))))

(def cached-connections (atom nil))

(defn get-connection [conn-name] (get @cached-connections conn-name))

(defn create-connection [conn-config-name conn-name]
  (or (get-connection conn-name)
      (try
        (let [r (first
                 (http/POST (str (connections-api-host) "/api/ConnectionManager.Core/Connection")
                            nil {:ConnectionManager.Core/Connection
                                 {:ConnectionId (u/uuid-string)
                                  :ConnectionConfigName (name conn-config-name)}} :json))]
          (if (= "ok" (:status r))
            (let [conn (first (:result r))]
              (if-not (:Parameter conn)
                (log/error (str "failed to create connection for - " conn-name))
                (do (swap! cached-connections assoc conn-name conn)
                    (assoc conn :CacheKey conn-name))))
            (log/error (str "failed to get connection - " conn-name))))
        (catch Exception ex
          (log/error ex)))))

(def connection-parameter :Parameter)

(def cache-connection! create-connection)

(defn close-connection [conn]
  (when (get @cached-connections (:CacheKey conn))
    (try
      (let [r (http/do-request
               :delete
               (str (connections-api-host) "/api/ConnectionManager.Core/Connection/" (:ConnectionId conn)))]
        (when (= "ok" (get (first (:body r)) "status"))
          (swap! cached-connections dissoc (:CacheKey conn))
          true))
      (catch Exception ex
        (log/error ex)))))

(defn refresh-connection [conn]
  (when (close-connection conn)
    (create-connection (:ConnectionConfigName conn) (:CacheKey conn))))
