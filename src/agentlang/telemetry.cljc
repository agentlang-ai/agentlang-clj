(ns agentlang.telemetry
  (:require [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.http :as http]
            [agentlang.lang.datetime :as dt]
            [agentlang.datafmt.json :as json]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.connections.client :as cc])))

(def telemetry-enabled?
  (memoize
   (fn []
     (let [config (gs/get-app-config)]
       (and (get-in config [:telemetry :enabled?])
            (:connection-manager config))))))

(defn- extract-first-map [r]
  (if (map? r)
    r
    (when (seqable? r)
      (extract-first-map (first r)))))

(defn- post-handler [response]
  (when (map? response)
    {:status (:status response)
     :body (json/decode (:body response))}))

(def ^:private result-cap 10)

(defn- trim-result [event-result]
  (let [rs (:result event-result)]
    (if (and (vector? rs) (> (count rs) result-cap))
      [(assoc event-result :result (vec (take rs result-cap))) true]
      [event-result false])))

(def ^:private connection-name (u/uuid-string))

(defn log-event [event-instance event-result]
  #?(:clj
     (when (telemetry-enabled?)
       (try
         (let [evt-name (cn/instance-type-kw event-instance)
               event-result (extract-first-map event-result)
               error? (not= :ok (:status event-result))
               conn (cc/create-connection "Agentlang/Telemetry" connection-name)]
           (when conn
             (let [conn-params (cc/connection-parameter conn)
                   auth (when conn-params (dissoc conn-params :host))
                   api-url (str (:host conn-params) "/api/TelemetryService.Core/WebIngest")
                   [event-result is-partial] (if error? [event-result false] (trim-result event-result))
                   inst {:TelemetryService.Core/WebIngest
                         {:Data
                          {:AppUuid (u/get-app-uuid)
                           :Timestamp (dt/unix-timestamp)
                           :EventName evt-name
                           :EventData (cn/cleanup-inst event-instance)
                           :ResultType (if error? "ERROR" "VALUE")
                           :ResultValue (when-not error? (cn/cleanup-inst (:result event-result)))
                           :IsPartialValue is-partial
                           :ResultError (when error? (or (:message event-result) (:result event-result)))}}}
                   response (http/do-post api-url auth inst :json post-handler)]
               (case (:status response)
                 200 (let [r (first (:body response))]
                       (when (not= "ok" (:status r))
                         (log/error (str "failed to log-event - " event-instance))))
                 401 (do (cc/close-connection conn) (log/error "authentication required"))
                 (log/error (str "failed to log-event " event-instance " with status " (:status response)
                                 " - " (or (:body response) (:message response))))))))
         (catch Exception ex
           (log/error ex))))))
