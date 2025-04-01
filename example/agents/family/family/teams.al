(component
 :Family.Teams
 {:clj-import (quote [(:require [org.httpkit.client :as http]
                                [clojure.string :as s]
                                [agentlang.util :as u]
                                [agentlang.component :as cn]
                                [agentlang.connections.client :as cc]
                                [agentlang.interpreter :as ip]
                                [agentlang.util.http :as uh]
                                [agentlang.datafmt.json :as json]
                                [agentlang.inference.service.channel.core :as ch])])})

(import (quote (clojure.lang ExceptionInfo)))

(def cached-token (atom nil)) ;; implement ttl

(defn get-access-token
  "Obtain and return access-token string for given credentials. Suitable for Delegate API."
  ([^String username
    ^String password
    ^String tenant-id
    ^String client-id
    ^String client-secret]
   (or @cached-token
       (let [auth-url (str "https://login.microsoftonline.com/" tenant-id "/oauth2/v2.0/token")
             body (format
                   "grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s&scope=https://graph.microsoft.com/.default"
                   client-id, client-secret, username, password)
             {:keys [status headers
                     body error]
              :as response}
             (deref (http/request
                     {:url auth-url
                      :method :post
                      :headers {"Content-Type" "application/x-www-form-urlencoded"}
                      :body body
                      :as :text}))]
         (if (or error (not= 200 status))
           (if error (throw error) (throw (Exception. (str "ERROR get-access-token: " status ", " (or error body)))))
           (let [token (-> (json/decode body)
                           (get :access_token))]
             (reset! cached-token token)
             token)))))
  ([]
   (get-access-token (u/getenv "TEAMS_USER")
                     (u/getenv "TEAMS_PASSWORD")
                     (u/getenv "TEAMS_TENANT_ID")
                     (u/getenv "TEAMS_CLIENT_ID")
                     (u/getenv "TEAMS_CLIENT_SECRET"))))

(defn http-get-with-bearer [url access-token]
  (let [{:keys [status headers
                body error]
         :as response}
        (deref (http/request
                {:url url
                 :method :get
                 :headers {"Authorization" (str "Bearer " access-token)}
                 :as :text}))]
    (cond
      error
      (throw error)

      (not= status 200)
      (throw (ex-info "GET request failed"
                      {:status status
                       :body   body}))
      
      :otherwise body)))

(defn post-json-with-bearer [url access-token data]
  (let [{:keys [status headers
                body error]
         :as response}
        (deref (http/request
                {:url url
                 :method :post
                 :headers {"Authorization" (str "Bearer " access-token)
                           "Content-Type"  "application/json"}
                 :body (json/encode data)
                 :as :text}))]
    (cond
      error
      (throw error)

      (not= status 201)
      (throw (ex-info "POST request failed"
                      {:status status
                       :body   body}))
      
      :otherwise body)))

(defn send-message-to-channel [^String access-token ^String team-id ^String channel-id ^String chat-message]
  (let [message-url (str "https://graph.microsoft.com/v1.0/teams/" team-id "/channels/" channel-id "/messages")
        upload-data {:body {:content chat-message}}
        result-text (post-json-with-bearer message-url access-token upload-data)]
    result-text))

(defn do-get [url]
  (let [result (uh/do-get url {:auth-token (get-access-token)})]
    (json/decode (:body result))))

(defn get-old-chat-id [access-token recipient-user-id]
  ;; This implementation is wrong, there seems to be no API `users/<user-id>/conversations`.
  #_(let [message-url (str "https://graph.microsoft.com/v1.0/users/" recipient-user-id "/conversations")
        result-text (http-get-with-bearer message-url access-token)
        result-data (json/decode result-text)
        _ (u/pprint result-data)
        first-chat-id (get-in result-data [:value 0 :id])]
    (or first-chat-id
        (throw (ex-info "Could not find chat ID" {:result-data result-data})))))

(def cached-user-ids (atom {}))

(defn- get-cached-user-id [user-id]
  (get @cached-user-ids user-id))

(defn- set-user-id-cache [user-id obj-id]
  (swap! cached-user-ids assoc user-id obj-id)
  obj-id)

(defn get-user-object-id
  ([access-token user-id]
   (if (s/index-of user-id "@")
     (or (get-cached-user-id user-id)
         (let [message-url (str "https://graph.microsoft.com/v1.0/users/" user-id)
               result-text (http-get-with-bearer message-url access-token)
               result-data (json/decode result-text)]
           (set-user-id-cache user-id (get result-data :id))))
     user-id))
  ([user-id] (get-user-object-id (get-access-token) user-id)))

(defn create-new-chat-id [access-token sender-user-id recipient-user-id]
  (let [sender-user-object-id (get-user-object-id access-token sender-user-id)
        recipient-user-object-id (get-user-object-id access-token recipient-user-id)
        generate-member-data (fn [user-object-id]
                               {"@odata.type" "#microsoft.graph.aadUserConversationMember"
                                "roles" ["owner"]
                                "user@odata.bind" (format "https://graph.microsoft.com/v1.0/users('%s')"
                                                          user-object-id)})
        request-payload-data {"chatType" "oneOnOne"
                              "members" [(generate-member-data sender-user-object-id)
                                         (generate-member-data recipient-user-object-id)]}
        message-url "https://graph.microsoft.com/v1.0/chats"
        result-text (post-json-with-bearer message-url access-token request-payload-data)
        result-data (json/decode result-text)]
    (get result-data :id)))

(defn get-or-create-chat-id [access-token sender-user-id recipient-user-id]
  #_(try
      (get-old-chat-id access-token recipient-user-id)
      (catch Exception ex
        (create-new-chat-id access-token sender-user-id recipient-user-id)))
  (create-new-chat-id access-token sender-user-id recipient-user-id))

(defn send-message-to-user
  ([^String access-token ^String sender-user-id ^String recipient-user-id ^String chat-message]
   (let [chat-id (get-or-create-chat-id access-token sender-user-id recipient-user-id)
         message-url (str "https://graph.microsoft.com/v1.0/chats/" chat-id "/messages")
         upload-data {:body {:content chat-message}}
         result-text (post-json-with-bearer message-url access-token upload-data)]
     {:chat-id chat-id :result result-text}))
  ([sender-user-id recipient-user-id chat-message]
   (send-message-to-user (get-access-token) sender-user-id recipient-user-id chat-message))
  ([recipient-user-id chat-message]
   (send-message-to-user (u/getenv "TEAMS_USER") recipient-user-id chat-message)))

(defn get-last-message [chat-id]
  (do-get (str "https://graph.microsoft.com/v1.0/chats/" chat-id "/messages?$top=1")))

(defn get-replies [chat-id message-id]
  (do-get (str "https://graph.microsoft.com/v1.0/chats/" chat-id "/messages/" message-id)))

(defn sync-chats [user]
  (let [user-id (get-user-object-id user)]
    (do-get (str "https://graph.microsoft.com/v1.0/users/" user-id "/chats/getAllMessages/delta?$top=2"))))

(defn fetch-next-message [user skip-token]
  (let [user-id (get-user-object-id user)]
    (do-get (str "https://graph.microsoft.com/v1.0/users/" user-id "/chats/getAllMessages/delta?&%24skiptoken=" skip-token))))

(def ^:private tag :teams)

(def ^:private run-flags (atom {}))

(defn- can-run? [channel-name]
  (get @run-flags channel-name))

(defn- extract-msg-ids [r]
  (let [chat-id (:chat-id r)
        result (json/decode (:result r))
        msg-id (:id result)]
    [chat-id msg-id]))

(defmethod ch/channel-start tag [{channel-name :name agent-name :agent}]
  (swap! run-flags assoc channel-name true)
  (let [send (partial ch/send-instruction-to-agent channel-name agent-name (name channel-name))
        can-run? #(can-run? channel-name)
        to (u/getenv "TEAMS_RECIPIENT")
        send-msg (fn [msg]
                   (let  [r (send-message-to-user to msg)]
                     (extract-msg-ids r)))]
    (u/parallel-call
     {:delay-ms 2000}
     (fn []
       (let [[chat-id msg-id] (send-msg "Hello, I'm the joke chat-agent")]
         (loop [last-msg-id msg-id]
           (when (can-run?)
             (Thread/sleep 10000)
             (let [r (get-last-message chat-id)
                   msg (first (:value r))
                   id (:id msg)]
               (if (not= last-msg-id id)
                 (let [resp (send (get-in msg [:body :content]))
                       [_ last-msg-id] (send-msg resp)]
                   (recur last-msg-id))
                 (recur last-msg-id)))))))))
  channel-name)

(defmethod ch/channel-shutdown tag [{channel-name :name}]
  (swap! run-flags dissoc channel-name)
  channel-name)
