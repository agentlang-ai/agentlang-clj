(ns agentlang.connections.client
  (:require [agentlang.util :as u]
            [agentlang.util.http :as http]
            [agentlang.global-state :as gs]))

;; A client library for the connection-manager-service.

(defn- connections-api-host []
  (or (:connections-api-host (gs/get-app-config))
      "http://localhost:5000"))

(defn configure-new-connection [conn-name conn-attrs]
  (let [inst {:ConnectionManager.Core/ConnectionConfig (merge {:Name conn-name} conn-attrs)}
        r (first
           (http/POST (str (connections-api-host) "/api/ConnectionManager.Core/Create_ConnectionConfig")
                      nil {:ConnectionManager.Core/Create_ConnectionConfig
                           {:Instance inst}} :json))]
    (when (not= "ok" (:status r))
      (u/throw-ex (str "failed to configure connection - " conn-name)))
    inst))

(def cached-connection (atom nil))

(defn connection-manager-client-create-connection [conn-name]
  (or @cached-connection
      (let [r (first
               (http/POST (str (connections-api-host) "/api/ConnectionManager.Core/Connection")
                          nil {:ConnectionManager.Core/Connection {:ConnectionConfigName (name conn-name)}} :json))]
        (if (= "ok" (:status r))
          (let [conn (first (:result r))]
            (when-not (:Connection conn)
              (u/throw-ex (str "failed to create connection for - " conn-name)))
            (swap! cached-connection assoc (:ConnectionId conn) conn)
            conn)
          (u/throw-ex (str "failed to get connection - " conn-name))))))

(defn connection-manager-client-mark-connection-for-refresh [conn]
  (let [connid (:ConnectionId conn)]
    (when (get @cached-connection connid)
      (let [r (http/do-request :delete (str (connections-api-host) "/api/ConnectionManager.Core/ActiveConnection/" connid))]
        (swap! cached-connection dissoc connid)
        (= "ok" (get (first (:body r)) "status"))))))
