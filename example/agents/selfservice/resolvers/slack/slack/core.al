(component
 :Slack.Core
 {:clj-import (quote [(:require [clojure.string :as s]
                                [agentlang.component :as cn]
                                [agentlang.util :as u]
                                [agentlang.util.http :as http]
                                [agentlang.util.logger :as log]
                                [agentlang.datafmt.json :as json]
                                [agentlang.evaluator :as ev]
                                [agentlang.connections.client :as cc]
                                [agentlang.lang.internal :as li])])})

(entity
 :Chat
 {:channel {:type :String :guid true}
  ;;:default (System/getenv "SLACK_CHANNEL_ID")}
  :text :String
  :response {:type :String :default ""}
  :mrkdwn {:type :Boolean :default true}
  :thread {:type :String :optional true}})

(def test-mode (System/getenv "SELFSERVICE_TEST_MODE"))

(defn slack-connection []
  (cc/get-connection :Slack/Connection))

(defn slack-api-key []
  (or (cc/connection-parameter (slack-connection)) (System/getenv "SLACK_API_KEY")))

(def slack-base-url "https://slack.com/api")

(defn get-url [endpoint] (str slack-base-url endpoint))

(defn- handle-response [response result]
  (let [status (:status response)
        body (:body response)]
    (if (<= 200 status 299)
      (let [output-decoded (json/decode body)]
        (if (:ok output-decoded)
          (assoc result :thread (:ts output-decoded))
          (throw (ex-info "Request failed. " output-decoded))))
      (throw (ex-info "Request failed. " {:status status :body body})))))

(defn- http-opts []
  {:headers {"Authorization" (str "Bearer " (slack-api-key))
             "Content-Type" "application/json"}})

(defn- extract-approval [response]
  (let [status (:status response)
        body (:body response)]
    (when (= 200 status)
      (let [output-decoded (json/decode body)]
        (when (:ok output-decoded)
          (let [messages (:messages output-decoded)]
            (when (>= (count messages) 2)
              (s/lower-case (s/trim (:text (second messages)))))))))))

(defn wait-for-reply [{channel :channel ts :thread}]
  (let [url (get-url (str "/conversations.replies?ts=" ts "&channel=" channel))
        f (fn [] (Thread/sleep (* 10 1000)) (http/do-get url (http-opts)))
        r
        (loop [response (f), retries 50]
          (if (zero? retries)
            "reject"
            (if-let [r (extract-approval response)]
              r
              (recur (f) (dec retries)))))]
    (log/debug (str "slack-resolver/wait-for-reply: " r))
    r))

(defn- create-chat [api-name instance]
  (let [data (dissoc instance :-*-type-*- :type-*-tag-*- :thread)
        url (get-url (str "/" api-name))
        response (http/do-post url (http-opts) data)
        new-instance (handle-response response instance)]
    (assoc new-instance :response (wait-for-reply new-instance))))

(defn- print-instance [inst]
  (println "** " (cn/instance-type-kw inst) " **")
  (u/pprint (cn/instance-attributes inst))
  inst)

(defn create-entity [instance]
  (if test-mode
    (print-instance (assoc instance :text (u/trace ">>> " (rand-nth ["this request is approved" "this is rejected"]))))
    (let [[c n] (li/split-path (cn/instance-type instance))]
      (if (= n :Chat)
        (create-chat "chat.postMessage" instance)
        instance))))

(resolver
 :Slack.Core/Resolver
 {:with-methods {:create create-entity}
  :paths [:Slack.Core/Chat]})

(entity
 :ManagerSlackChannel
 {:Manager {:type :String :optional true}
  :SlackChannelId {:type :String :optional true}})

(event
 :LookupManagerSlackChannel
 {:Manager :String})

(dataflow
 :LookupManagerSlackChannel
 {:ManagerSlackChannel {:Manager? :LookupManagerSlackChannel.Manager}})

(defn- make-slack-channel [channel-id]
  (cn/make-instance
   :Slack.Core/ManagerSlackChannel
   {:SlackChannelId channel-id}))

(def default-slack-channel (make-slack-channel
                            (if test-mode
                              "approval-requests"
                              (or (System/getenv "SLACK_CHANNEL_ID") "C07L51XJULV"))))

(def slack-channel-db {"mgr01@acme.com" default-slack-channel})

(defn- get-manager-info [[[_ n] {where :where}]]
  (let [[_ _ v] where]
    (when (= n :ManagerSlackChannel)
      (when-let [ch (get slack-channel-db v default-slack-channel)]
        [(assoc ch :Manager v)]))))

(resolver
 :ManagerResolver
 {:with-methods
  {:query get-manager-info}
  :paths [:Slack.Core/ManagerSlackChannel]})
