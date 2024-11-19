(ns agentlang.telemetry
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as http]
            [agentlang.lang.datetime :as dt]
            [agentlang.datafmt.json :as json]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]))

(def telemetry-config
  (fn []
    #?(:clj
       (when-let [cfg (:telemetry (gs/get-app-config))]
         (assoc cfg :api-url (str (:host cfg) "/api/TelemetryService.Core/WebIngest")
                :auth {:auth-token (:token cfg)})))))

(defn- extract-first-map [r]
  (if (map? r)
    r
    (when (seqable? r)
      (extract-first-map (first r)))))

(defn- post-handler [response]
  (when (map? response)
    {:status (:status response)
     :body (json/decode (:body response))}))

(defn log-event [event-instance event-result]
  #?(:clj
     (let [{api-url :api-url auth :auth} (telemetry-config)]
       (when api-url
         (try
           (let [evt-name (cn/instance-type-kw event-instance)
                 _ (println "#######################" event-instance event-result)
                 event-result (extract-first-map event-result)
                 error? (not= :ok (:status event-result))
                 inst {:TelemetryService.Core/WebIngest
                       {:Data
                        {:AppUuid (u/get-app-uuid)
                         :Timestamp (dt/unix-timestamp)
                         :EventName evt-name
                         :EventData {evt-name (cn/user-attributes event-instance)}
                         :ResultType (if error? "ERROR" "VALUE")
                         :ResultValue (when-not error? (:result event-result))
                         :IsPartialValue false
                         :ResultError (when error? (or (:message event-result) (:result event-result)))}}}
                 response (http/do-post api-url (when auth auth) inst :json post-handler)]
             (println "##$" response)
             (case (:status response)
               200 (let [r (first (:body response))]
                     (when (not= "ok" (:status r))
                       (log/error (str "failed to log-event - " event-instance))))
               401 (log/error "authentication required")
               :else (log/error (str "failed to log-event " event-instance " with status " (:status response)))))
           (catch Exception ex
             (log/error ex)))))))
