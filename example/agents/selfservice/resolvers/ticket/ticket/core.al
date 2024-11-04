(component
 :Ticket.Core
 {:clj-import '[(:require [agentlang.util :as u]
                          [agentlang.util.logger :as log]
                          [agentlang.util.http :as http]
                          [agentlang.util.seq :as us]
                          [agentlang.datafmt.json :as json]
                          [agentlang.component :as cn]
                          [agentlang.connections.client :as cc]
                          [agentlang.lang.b64 :as b64])]})
(entity
 :JiraConnectionConfig
 {:rooturl :String
  :user :String
  :token :String})

(entity
 :Ticket
 {:Id {:type :Any :guid true}
  :Title :String
  :Content {:type :String :optional true}})

(entity
 :TicketComment
 {:TicketId :Any
  :Body :String})

(entity
 :GithubMember
 {:Org :String
  :Email :Email})

(defn- print-instance [inst]
  (println "** " (cn/instance-type-kw inst) " **")
  (u/pprint (cn/instance-attributes inst))
  inst)

(defn- maybe-parse-paragraphs [content]
  (if (vector? content)
    (let [f (first content)]
      (if (and (map? f) (:text f))
        {:text (apply str (mapv :text content))}
        content))
    content))

(defn- extract-contents [content]
  (let [content (maybe-parse-paragraphs content)]
    (cond
      (vector? content)
      (apply #(if (seq %) (str "\n" %) "") (mapv extract-contents content))

      (map? content)
      (if-let [cnts (:content content)]
        (extract-contents cnts)
        (or (:text content) ""))
    
      :else "")))

(defn- lookup-ticket [url basic-auth id]
  (let [{status :status body :body}
        (http/do-get (str url id)
         {:headers {"Authorization" basic-auth "Accept" "application/json"}})]
    (if (= 200 status)  
      (let [obj (json/decode body)
            fields (:fields obj)]
        (cn/make-instance
         :Ticket.Core/Ticket
         {:Id id :Title (:summary fields) :Content (extract-contents (:content (:description fields)))}))
      (log/warn (str "failed to lookup ticket " id ", status: " status)))))

(defn- fetch-individual-tickets [url basic-auth picker-response]
  (when-let [secs (seq (:sections picker-response))]
    (when-let [ids (seq (apply concat (mapv (fn [sec] (mapv :id (:issues sec))) secs)))]
      (vec (us/nonils (mapv (partial lookup-ticket url basic-auth) ids))))))

(defn- ticket-basic-auth [connection]
  (str "Basic " (b64/encode-string (str (:user connection) ":" (:token connection)))))

(defn- make-headers [connection]
  (let [basic-auth (ticket-basic-auth connection)]
    {:headers {"Authorization" basic-auth "Accept" "application/json"}}))

(def test-mode (System/getenv "SELFSERVICE_TEST_MODE"))

(def ^:private default-tickets [(cn/make-instance
                                 :Ticket.Core/Ticket
                                 {:Id "10000"
                                  :Title "Request to join org"
                                  :Content "Please add moe@acme.com to the github org acme"})])

(defn ticket-query [get-connection _]
  (if test-mode
    default-tickets
    (let [connection (get-connection)
          cparam (cc/connection-parameter connection)
          url (str (:rooturl cparam) "/rest/api/3/issue/")
          headers (make-headers cparam)
          {status :status body :body} (http/do-get (str url "picker") headers)
          basic-auth (get-in headers [:headers "Authorization"])]
      (if (= 200 status)
        (fetch-individual-tickets url basic-auth (json/decode body))
        (log/warn (str "lookup tickets failed with status: " status))))))

(defn- make-comment-body [text]
  {"content"
   [{"content" [{"text" text "type" "text"}]
     "type" "paragraph"}]
   "type" "doc"
   "version" 1})

(defn ticket-comment-create [get-connection instance]
  (if test-mode
    (print-instance instance)
    (let [connection (get-connection)
          cparam (cc/connection-parameter connection)
          url (str (:rooturl cparam) "/rest/api/3/issue/" (:TicketId instance) "/comment")
          headers (make-headers cparam)
          body {:body (make-comment-body (:Body instance))}
          {status :status :as response} (http/do-post url headers body)]
      (if (or (= 201 status) (= 200 status))
        instance
        (log/warn (str "create ticket-comment returned status: " status))))))

(defn get-tickets-connection []
  (when-not test-mode
    (or (cc/get-connection :selfservice-jira-connection)
        {:rooturl (u/getenv "TICKETS_ROOT_URL")
         :user (u/getenv "TICKETS_USER")
         :token (u/getenv "TICKETS_TOKEN")})))

(resolver
 :TicketResolver
 {:with-methods
  {:query (partial ticket-query get-tickets-connection)
   :create (partial ticket-comment-create get-tickets-connection)}
  :paths [:Ticket.Core/Ticket :Ticket.Core/TicketComment]})

(defn- github-member-post [get-token inst]
  (if test-mode
    (print-instance inst)
    (let [api-token (get-token)
          result (http/do-post
                  (str "https://api.github.com/orgs/" (:Org inst) "/invitations")
                  {:headers
                   {"Accept" "application/vnd.github+json"
                    "Authorization" (str "Bearer " api-token)
                    "X-GitHub-Api-Version" "2022-11-28"}}
                  {:email (:Email inst) :role "direct_member"})
          status (:status result)]
      (if (<= 200 status 300)
        inst
        (u/throw-ex (str "failed to add user " (:Email inst)
                         " to github org " (:Org inst)
                         ", with status " status " and reason " (:body result)))))))

(defn- get-github-token []
  (or (cc/connection-parameter (cc/get-connection :selfservice-github-connection))
      (u/getenv "GITHUB_API_TOKEN")))

(resolver
 :GithubResolver
 {:with-methods
  {:create (partial github-member-post get-github-token)}
  :paths [:Ticket.Core/GithubMember]})

(defn as-json [result]
  (json/encode (mapv cn/instance-attributes result)))

(entity
 :TicketManager
 {:TicketId {:type :Any :optional true}
  :Manager :String})

(entity
 :ManagerSlackChannel
 {:Manager {:type :String :optional true}
  :SlackChannelId {:type :String :optional true}})

(event
 :LookupTicketManagerByTicketId
 {:TicketId :Any})

(dataflow
 :LookupTicketManagerByTicketId
 {:TicketManager {:TicketId? :LookupTicketManagerByTicketId.TicketId}})

(event
 :LookupManagerSlackChannel
 {:Manager :String})

(dataflow
 :LookupManagerSlackChannel
 {:ManagerSlackChannel {:Manager? :LookupManagerSlackChannel.Manager}})

(defn- make-slack-channel [channel-id]
  (cn/make-instance
   :Ticket.Core/ManagerSlackChannel
   {:SlackChannelId channel-id}))

(defn- make-ticket-manager [n]
  (cn/make-instance
   :Ticket.Core/TicketManager
   {:Manager n}))

(def default-manager (make-ticket-manager "admin@acme.com"))
(def default-slack-channel (make-slack-channel (if test-mode "approval-requests" (System/getenv "SLACK_CHANNEL_ID"))))

(def manager-db {"10000" (make-ticket-manager "mgr01@acme.com")})
(def slack-channel-db {"mgr01@acme.com" default-slack-channel})

(defn- get-manager-info [[[_ n] {where :where}]]
  (let [[_ _ v] where]
    (case n
      :TicketManager
      (when-let [mgr (get manager-db (str v) default-manager)]
        [(assoc mgr :TicketId v)])

      :ManagerSlackChannel
      (when-let [ch (get slack-channel-db v default-slack-channel)]
        [(assoc ch :Manager v)]))))

(resolver
 :ManagerResolver
 {:with-methods
  {:query get-manager-info}
  :paths [:Ticket.Core/TicketManager
          :Ticket.Core/ManagerSlackChannel]})
