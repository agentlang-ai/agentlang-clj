(ns agentlang.telemetry
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.lang.datetime :as dt]
            [agentlang.datafmt.json :as json]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.connections.client :as cc]))

(def telemetry-config
  (fn []
    #?(:clj
       (when-let [cfg (:telemetry (gs/get-app-config))]
         (assoc cfg :api-url (str (:host cfg) "/api/TelemetryService.Core/WebIngest"))))))

(defn- extract-first-map [r]
  (if (map? r)
    r
    (when (seqable? r)
      (extract-first-map (first r)))))

(defn- post-handler [response]
  (when (map? response)
    {:status (:status response)
     :body (json/decode (:body response))}))

(def ^:private connection-name (u/uuid-string))

(defn log-event [event-instance event-result]
  #?(:clj
     (let [{api-url :api-url} (telemetry-config)]
       (when api-url
         (try
           (let [evt-name (cn/instance-type-kw event-instance)
                 event-result (extract-first-map event-result)
                 error? (not= :ok (:status event-result))
                 conn (cc/create-connection "Agentlang/Telemetry" connection-name)
                 auth (when conn (cc/connection-parameter conn))
                 inst {:TelemetryService.Core/WebIngest
                       {:Data
                        {:AppUuid (u/get-app-uuid)
                         :Timestamp (dt/unix-timestamp)
                         :EventName evt-name
                         :EventData (cn/cleanup-inst event-instance)
                         :ResultType (if error? "ERROR" "VALUE")
                         :ResultValue (when-not error? (cn/cleanup-inst (:result event-result)))
                         :IsPartialValue false
                         :ResultError (when error? (or (:message event-result) (:result event-result)))}}}
                 response (http/do-post api-url auth inst :json post-handler)]
             (case (:status response)
               200 (let [r (first (:body response))]
                     (when (not= "ok" (:status r))
                       (log/error (str "failed to log-event - " event-instance))))
               401 (do (cc/close-connection conn) (log/error "authentication required"))
               (log/error (str "failed to log-event " event-instance " with status " (:status response)
                               " - " (or (:body response) (:message response))))))
           (catch Exception ex
             (log/error ex)))))))
