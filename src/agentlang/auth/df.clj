(ns agentlang.auth.df
  "Authenticate by evaluating a local dataflow."
  (:require [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.util.auth :as au]
            [agentlang.auth.core :as auth]))

(def ^:private tag :dataflow)
(def ^:private token-db (atom {}))

(defmethod auth/make-client tag [config]
  (when-let [auth-event-name (:auth-event config)]
    {:auth-event auth-event-name}))

(defmethod auth/make-authfn tag [config]
  (fn [_ token]
    (get @token-db token)))

(defn- normalize-result [result token]
  (if (map? result)
    (assoc result :access_token token)
    (normalize-result (first result) token)))

(defmethod auth/user-login tag [{event :event evaluate :eval auth-event :auth-event}]
  (when-not (= auth-event (cn/instance-type event))
    (u/throw-ex (str "login event is expected to be of type " auth-event)))
  (let [r (evaluate event)
        result (if (map? r) r (first r))]
    (if (:ok result)
      (let [token (u/uuid-string)
            nr (normalize-result (:result result) token)]
        (swap! token-db assoc token nr)
        (assoc result :result nr))
      (u/throw-ex (str "login failed for event " auth-event)))))

(defmethod auth/user-logout tag [{sub :sub}]
  :bye)

(defn- get-session-value [request k]
  (when-let [token (au/bearer-token request)]
    (if k
      (get-in @token-db [token k])
      (get @token-db token))))

(defmethod auth/session-user tag [{request :request k :auth-identity}]
  (get-session-value request k))

(defmethod auth/session-sub tag [{request :request k :auth-identity}]
  (get-session-value request k))
