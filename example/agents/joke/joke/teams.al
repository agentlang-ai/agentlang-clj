(component
 :Joke.Teams
 {:clj-import (quote [(:require [clojure.pprint :as pp]
                                [agentlang.util :as u]
                                [agentlang.component :as cn]
                                [agentlang.connections.client :as cc]
                                [agentlang.interpreter :as ip]
                                [org.httpkit.client :as http]
                                [agentlang.util.http :as uh]
                                [cheshire.core :as json])])})

(import (quote (clojure.lang ExceptionInfo)))

(defn get-access-token
  "Obtain and return access-token string for given credentials. Suitable for Delegate API."
  ([^String username ^String password
    ^String tenant-id
    ^String client-id ^String client-secret]
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
       (-> (json/parse-string body)
           (get "access_token")))))
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
                 :body (json/generate-string data)
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

(defn get-old-chat-id [access-token recipient-user-id]
  (let [message-url (str "https://graph.microsoft.com/v1.0/users/" recipient-user-id "/conversations")
        result-text (http-get-with-bearer message-url access-token)
        result-data (json/parse-string result-text)
        first-chat-id (get-in result-data ["value" 0 "id"])]
    (or first-chat-id
        (throw (ex-info "Could not find chat ID" {:result-data result-data})))))

(defn get-user-object-id [access-token user-id]
  (let [message-url (str "https://graph.microsoft.com/v1.0/users/" user-id)
        result-text (http-get-with-bearer message-url access-token)
        result-data (json/parse-string result-text)]
    (get result-data "id")))

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
        result-data (json/parse-string result-text)]
    (get result-data "id")))

(defn get-or-create-chat-id [access-token sender-user-id recipient-user-id]
  (try
    (get-old-chat-id access-token recipient-user-id)
    (catch ExceptionInfo ei
      (create-new-chat-id access-token sender-user-id recipient-user-id))))

(defn send-message-to-user [^String access-token ^String sender-user-id ^String recipient-user-id ^String chat-message]
  (let [chat-id (get-or-create-chat-id access-token sender-user-id recipient-user-id)
        message-url (str "https://graph.microsoft.com/v1.0/chats/" chat-id "/messages")
        upload-data {:body {:content chat-message}}
        result-text (post-json-with-bearer message-url access-token upload-data)]
    {:chat-id chat-id :result result-text}))

(defn send-test-message []
  (let [token (get-access-token)]
    (send-message-to-user token "vijay@fractlhq.onmicrosoft.com" "hasnain@fractlhq.onmicrosoft.com" "hello")))

(defn get-test-messages [chat-id]
  (let [url "https://graph.microsoft.com/v1.0/chats/" chat-id "/messages?$top=1"
        result (uh/do-get url {:auth-token (get-access-token)})]
    (u/pprint result)
    "ok"))

(dataflow :SendText [:call '(joke.teams/send-test-message)])
(dataflow :GetText [:call '(joke.teams/get-test-messages :GetText.ChatId)])
