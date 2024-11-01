(ns agentlang.connections.client
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.global-state :as gs]))

;; A client library for the connection-manager-service.

(defn- connections-api-host []
  (or (:connections-api-host (gs/get-app-config))
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

(defn create-connection
  ([conn-type conn-name]
   (let [k (u/string-as-keyword conn-name)]
     (or (get @cached-connections k)
         (let [r (first
                  (http/POST (str (connections-api-host) "/api/ConnectionManager.Core/Connection")
                             nil {:ConnectionManager.Core/Connection
                                  {:ConnectionId (name conn-name)
                                   :ConnectionConfigName (name conn-type)}} :json))]
           (if (= "ok" (:status r))
             (let [conn (first (:result r))]
               (when-not (:Connection conn)
                 (u/throw-ex (str "failed to create connection for - " conn-name)))
               (swap! cached-connections assoc k conn)
               conn)
             (u/throw-ex (str "failed to get connection - " conn-name)))))))
  ([conn-type] (create-connection conn-type conn-type)))

(defn get-connection
  ([conn-type conn-name]
   (try
     (:Connection (create-connection conn-type conn-name))
     (catch Exception ex
       (do (log/error ex) nil))))
  ([conn-type] (get-connection conn-type conn-type)))

(defn mark-connection-for-refresh [conn]
  (let [connid (:ConnectionId conn)]
    (when (get @cached-connections (u/string-as-keyword connid))
      (let [r (http/do-request :delete (str (connections-api-host) "/api/ConnectionManager.Core/ActiveConnection/" connid))]
        (swap! cached-connections dissoc connid)
        (= "ok" (get (first (:body r)) "status"))))))
