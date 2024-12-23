(ns agentlang.inference.service.model
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [agentlang.lang
             :refer [component
                     dataflow
                     inference
                     entity
                     event
                     record
                     attribute
                     relationship]
             :as ln]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.util.http :as http]
            [agentlang.evaluator :as e]
            [agentlang.datafmt.json :as json]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            [agentlang.inference.service.planner :as planner]
            [agentlang.inference.service.agent-gen :as agent-gen]))

(component :Agentlang.Core {:model :Agentlang})

(entity
 :Agentlang.Core/LLM
 {:Type {:type :String :default "openai"} ; e.g "openai"
  :Name {:type :String :guid true :default #(us/generate-code 5)}
  :Config {:type :Map :optional true}
  ;; example config for openai:
  ;; {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
  ;;  :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
  ;;  :EmbeddingModel "text-embedding-3-small"
  ;;  :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
  ;;  :CompletionModel "gpt-3.5-turbo"}
  })

(dataflow
 :Agentlang.Core/FindLLM
 {:Agentlang.Core/LLM
  {:Name? :Agentlang.Core/FindLLM.Name}})

(ln/install-standalone-pattern-preprocessor!
 :Agentlang.Core/LLM
 (fn [pat]
   (let [attrs (li/record-attributes pat)
         tp (:Type attrs)
         nm (:Name attrs)]
     (assoc pat :Agentlang.Core/LLM
            (-> attrs
                (cond->
                    tp (assoc :Type (u/keyword-as-string tp))
                    nm (assoc :Name (u/keyword-as-string nm))))))))

;; The document must be specified using the scheme: "rs://<store>/<document_name>.<extn>",
;; where "rs" means retrieval-service. E.g: "rs://local/Companies_new.pdf".
;; Example entry in config.edn:
;; {:retrieval-service {:host "https://retrieval-service.fractl.io/pratik@fractl.io" :token "<token>"}}
(defn- read-from-retrieval-service [file-name]
  (let [config (:retrieval-service (gs/get-app-config))
        token (or (:token config)
                  (u/getenv "RETRIEVAL_SERVICE_TOKEN" ""))
        url (str (or (:host config)
                     (u/getenv "RETRIEVAL_SERVICE_HOST"))
                 "/" file-name "/chunks")
        options (when (seq token) {:headers {"Token" token}})
        response (http/do-get url options)]
    (if (= 200 (:status response))
      (let [body (json/decode (:body response))]
        (apply str (concat (:chunks body))))
      (u/throw-ex (str "failed to load document from: " url ", status: " (:status response))))))

(def ^:private doc-scheme-handlers {"file" slurp
                                    "rs" read-from-retrieval-service})
(def ^:private doc-schemes (keys doc-scheme-handlers))
(def ^:private scheme-suffix "://")

(defn- document-resource-scheme [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s 0 idx)))

(defn- document-uri? [s]
  (and (string? s)
       (when-let [scm (document-resource-scheme s)]
         (some #{scm} doc-schemes))))

(defn- document-resource-name [s]
  (when-let [idx (s/index-of s scheme-suffix)]
    (subs s (+ idx 3))))

(defn read-document-resource [uri]
  (when-let [h (get doc-scheme-handlers (document-resource-scheme uri))]
    (h (document-resource-name uri))))

(entity
 :Agentlang.Core/Document
 {:Id {:type :UUID :default u/uuid-string :guid true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :Agent {:type :String :optional true}
  :Uri {:check document-uri?}
  :Title :String
  :Content '(agentlang.inference.service.model/read-document-resource :Uri)})

(defn- agent-messages? [xs]
  (if (seq xs)
    (every? #(and (map? %)
                  (= 2 (count (keys %)))
                  (some #{(:role %)} #{:system :user :assistant})
                  (string? (:content %)))
            xs)
    true))

(defn- tool-components-list? [xs]
  (and (vector? xs) (every? #(or (string? %) (li/name? %)) xs)))

(def ^:private ft-chain-of-thought (str "In your response please include the step-by-step thought-process "
                                        "that led you to your answer or conclusion."))
(def ^:private ft-self-critique (str "In your response please include a self-criticism of your answer or conclusion."
                                     "You may use questions like \"what's a different way to solve this problem?\","
                                     "\"Is there an unconventional solution?\" etc for formulating your self-critique."))

(def ^:private feature-set {"chain-of-thought" ft-chain-of-thought
                            "self-critique" ft-self-critique})

(def ^:private feature-set-keys (set (keys feature-set)))

(defn- feature-list? [xs]
  (when (seq xs)
    (and (vector? xs)
         (= feature-set-keys (set/union feature-set-keys (set xs))))))

(defn get-feature-prompt [ft] (get feature-set ft ""))

(record
 :Agentlang.Core/Inference
 {:UserInstruction :String
  :ChatId {:type :String :optional true}})

(entity
 :Agentlang.Core/Agent
 {:Name {:type :String :guid true}
  :Type {:type :String :default "chat"}
  :Features {:check feature-list? :optional true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :UserInstruction {:type :String :optional true}
  :ToolComponents {:check tool-components-list? :optional true}
  :Input {:type :String :optional true}
  :Context {:type :Map :optional true}
  :Response {:type :Any :read-only true}
  :Integrations {:listof :String :optional true}
  :CacheChatSession {:type :Boolean :default true}})

(defn- agent-of-type? [typ agent-instance]
  (= typ (:Type agent-instance)))

(def ocr-agent? (partial agent-of-type? "ocr"))
(def classifier-agent? (partial agent-of-type? "classifier"))
(def planner-agent? (partial agent-of-type? "planner"))
(def agent-gen-agent? (partial agent-of-type? "agent-gen"))
(def eval-agent? (partial agent-of-type? "eval"))

(defn- preproc-agent-tools-spec [tools]
  (when tools
    (mapv (fn [x]
            (cond
              (map? x) x
              (string? x) {:name x}
              (keyword? x) {:name (subs (str x) 1)}
              :else (u/throw-ex (str "Invalid tool: " x))))
          tools)))

(defn- preproc-agent-input-spec [input]
  (when input
    (cond
      (string? input) input
      (keyword? input) (subs (str input) 1)
      :else (u/throw-ex (str "Invalid agent input: " input)))))

(defn- preproc-agent-delegate [d]
  (let [from (:From d) to (:To d)]
    (-> d
        (cond->
            from (assoc :From (u/keyword-as-string from))
            to (assoc :To (u/keyword-as-string to))))))

(defn- preproc-agent-delegates [delegs]
  (when delegs
    (cond
      (map? delegs) (preproc-agent-delegate delegs)
      (vector? delegs) (mapv preproc-agent-delegates delegs)
      :else delegs)))

(defn- preproc-agent-docs [docs]
  (mapv (fn [spec]
          (if-let [agent (:Agent spec)]
            (assoc spec :Agent (u/keyword-as-string agent))
            spec))
        docs))

(defn- classifier-with-instructions [agent-instance]
  (if-let [s (when-let [delegates (seq (mapv :To (:Delegates agent-instance)))]
               (str "Classify the following user query into one of the categories - "
                    (s/join ", " delegates)
                    (when-let [ins (:UserInstruction agent-instance)]
                      (str "\n"
                           "The user query is: \"" ins "\"\n"
                           "Return only the category name and nothing else.\n"))))]
    (assoc agent-instance :UserInstruction s)
    agent-instance))

(defn- verify-name [n]
  (when (or (s/index-of n "/") (s/index-of n "."))
    (u/throw-ex (str "Invalid name " n ", cannot contain `/` or `.`")))
  n)

(ln/install-standalone-pattern-preprocessor!
 :Agentlang.Core/Agent
 (fn [pat]
   (let [attrs (li/record-attributes pat)
         nm (:Name attrs)
         input (preproc-agent-input-spec (:Input attrs))
         tools (preproc-agent-tools-spec (:Tools attrs))
         delegates (preproc-agent-delegates (:Delegates attrs))
         tp (:Type attrs)
         llm (or (:LLM attrs) {:Type "openai"})
         docs (:Documents attrs)
         new-attrs
         (-> attrs
             (cond->
                 nm (assoc :Name (verify-name (u/keyword-as-string nm)))
                 input (assoc :Input input)
                 tools (assoc :Tools tools)
                 delegates (assoc :Delegates delegates)
                 docs (assoc :Documents (preproc-agent-docs docs))
                 tp (assoc :Type (u/keyword-as-string tp))
                 llm (assoc :LLM (u/keyword-as-string llm))))]
     (assoc pat :Agentlang.Core/Agent
            (cond
              (planner-agent? new-attrs) (planner/with-instructions new-attrs)
              (agent-gen-agent? new-attrs) (agent-gen/with-instructions new-attrs)
              (classifier-agent? new-attrs) (classifier-with-instructions new-attrs)
              :else new-attrs)))))

(defn maybe-define-inference-event [event-name]
  (if (cn/find-schema event-name)
    (if (cn/event? event-name)
      event-name
      (u/throw-ex (str "not an event - " event-name)))
    (event {event-name {:meta {:inherits :Agentlang.Core/Inference}}})))

(defn maybe-input-as-inference [agent]
  (when-let [input (:Input agent)]
    (let [n (u/string-as-keyword input)]
      (when (maybe-define-inference-event n)
        (inference n {:agent (:Name agent)}))))
  agent)

(dataflow
 [:before :create :Agentlang.Core/Agent]
 [:eval '(agentlang.inference.service.model/maybe-input-as-inference :Instance)])

(def ^:private agent-callbacks (atom nil))

(defn- set-agent-callback! [tag agent-name f]
  (let [cbs (assoc (get @agent-callbacks tag) agent-name f)]
    (swap! agent-callbacks assoc tag cbs)))

(defn- get-agent-callback [tag agent-instance]
  (get-in @agent-callbacks [tag (:Name agent-instance)]))

(def agent-response-handler (partial get-agent-callback :rh))
(def agent-prompt-fn (partial get-agent-callback :pfn))
(def set-agent-response-handler! (partial set-agent-callback! :rh))
(def set-agent-prompt-fn! (partial set-agent-callback! :pfn))

(relationship
 :Agentlang.Core/AgentDelegate
 {:meta {:between [:Agentlang.Core/Agent
                   :Agentlang.Core/Agent
                   :as [:From :To]]}
  :Preprocessor {:type :Boolean :default false}})

(attribute
 :Agentlang.Core/Delegates
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/AgentDelegate
  :relationship :Agentlang.Core/AgentDelegate})

(defn concat-results [rs] (vec (apply concat rs)))

(dataflow
 :Agentlang.Core/FindAgentDelegates
 {:Agentlang.Core/AgentDelegate
  {:From? :Agentlang.Core/FindAgentDelegates.Agent
   :Preprocessor? :Agentlang.Core/FindAgentDelegates.Preprocessor}
  :as :Delegates}
 [:for-each :Delegates
  {:Agentlang.Core/Agent
   {:Name? :%.To}}
  :as :Rs]
 [:eval '(agentlang.inference.service.model/concat-results :Rs)])

(entity
 :Agentlang.Core/ChatSession
 {:Id {:type :String :guid true :default u/uuid-string}
  :Messages {:check agent-messages?}})

(relationship
 :Agentlang.Core/AgentChatSession
 {:meta {:contains [:Agentlang.Core/Agent :Agentlang.Core/ChatSession]}})

(attribute
 :Agentlang.Core/Chat
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/ChatSession
  :relationship :Agentlang.Core/AgentChatSession})

(dataflow
 :Agentlang.Core/LookupAgentChatSessions
 {:Agentlang.Core/ChatSession? {}
  :-> [[:Agentlang.Core/AgentChatSession?
        :Agentlang.Core/LookupAgentChatSessions.Agent]]})

(dataflow
 :Agentlang.Core/CreateAgentChatSession
 {:Agentlang.Core/ChatSession
  {:Id :Agentlang.Core/CreateAgentChatSession.ChatId
   :Messages :Agentlang.Core/CreateAgentChatSession.Messages}
  :-> [[:Agentlang.Core/AgentChatSession :Agentlang.Core/CreateAgentChatSession.Agent]]})

(dataflow
 :Agentlang.Core/ResetAgentChatSessions
 [:eval '(agentlang.inference.service.model/reset-agent-chat-session
          :Agentlang.Core/ResetAgentChatSessions.Agent)])

(defn- tool-param? [x]
  (and (map? x)
       (string? (:type x))
       (map? (:properties x))
       (if-let [r (:required x)]
         (every? string? r)
         true)))

(entity
 :Agentlang.Core/Tool
 {:id {:type :UUID :guid true :default u/uuid-string}
  :name :String
  :type {:type :String :default "function"}
  :description {:type :String :optional true}
  :parameters {:check tool-param? :optional true}})

(relationship
 :Agentlang.Core/AgentLLM
 {:meta {:between [:Agentlang.Core/Agent :Agentlang.Core/LLM]}})

(attribute
 :Agentlang.Core/LLM
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/LLM
  :relationship :Agentlang.Core/AgentLLM
  :order 0})

(relationship
 :Agentlang.Core/AgentTool
 {:meta {:between [:Agentlang.Core/Agent :Agentlang.Core/Tool]}})

(attribute
 :Agentlang.Core/Tools
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/Tool
  :relationship :Agentlang.Core/AgentTool})

(relationship
 :Agentlang.Core/AgentDocument
 {:meta {:between [:Agentlang.Core/Agent :Agentlang.Core/Document]}})

(dataflow
 :Agentlang.Core/AddAgentDocument
 {:Agentlang.Core/Document
  {:Agent :Agentlang.Core/AddAgentDocument.Agent
   :Uri :Agentlang.Core/AddAgentDocument.Uri
   :Title :Agentlang.Core/AddAgentDocument.Title}
  :as :Doc}
 {:Agentlang.Core/AgentDocument {:Agent :Doc.Agent :Document :Doc.Id}}
 :Doc)

(attribute
 :Agentlang.Core/Documents
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/Document
  :relationship :Agentlang.Core/AgentDocument})

(dataflow
 :Agentlang.Core/LLMsForAgent
 {:Agentlang.Core/AgentLLM
  {:Agent? :Agentlang.Core/LLMsForAgent.Agent}})

(dataflow
 :Agentlang.Core/AgentTools
 {:Agentlang.Core/AgentTool
  {:Agent? :Agentlang.Core/AgentTools.Agent} :as :R}
 [:for-each :R
  {:Agentlang.Core/Tool
   {:id? :%.Tool}}])

(dataflow
 :Agentlang.Core/HasAgentDocuments
 {:Agentlang.Core/AgentDocument
  {:Agent? :Agentlang.Core/HasAgentDocuments.Agent}})

(dataflow
 :Agentlang.Core/AgentDocuments
 {:Agentlang.Core/AgentDocument
  {:Agent? :Agentlang.Core/AgentDocuments.Agent} :as :R}
 [:for-each :R
  {:Agentlang.Core/Document
   {:Id? :%.Document}}])

(defn- eval-event
  ([event callback atomic?]
   (when-let [result (first ((if atomic?
                               e/eval-all-dataflows-atomic
                               e/eval-all-dataflows)
                             event))]
     (when (= :ok (:status result))
       (callback (:result result)))))
  ([event callback] (eval-event event callback false))
  ([event] (eval-event event identity)))

(defn- eval-internal-event [event & args]
  (apply eval-event (e/mark-internal (cn/make-instance event)) args))

(defn- maybe-agent-pattern [p]
  (when (and (map? p)
             (= :Agentlang.Core/Agent (li/record-name p)))
    p))

(defn lookup-agent-by-name [agent-name]
  #?(:clj
     (eval-event
      {:Agentlang.Core/Lookup_Agent
       {:Name (u/keyword-as-string agent-name)}}
      first)
     :cljs
     (let [valid-pats (us/nonils
                       (map (fn [p]
                              (cond
                                (and (vector? p) (= :try (first p))) (maybe-agent-pattern (second p))
                                (map? p) (maybe-agent-pattern p)
                                :else nil))
                            @gs/standalone-patterns))
           n (u/keyword-as-string agent-name)]
       (when-let [agent-pat (first
                             (filter (fn [p]
                                       (and (= :Agentlang.Core/Agent (li/record-name p))
                                            (= n (:Name (li/record-attributes p)))))
                                     valid-pats))]
         (cn/make-instance agent-pat)))))

(defn agent-input-type [agent-instance]
  (when-let [input (:Input agent-instance)]
    (u/string-as-keyword input)))

(defn- find-agent-delegates [preproc agent-instance]
  (eval-event
   {:Agentlang.Core/FindAgentDelegates
    {:Agent (:Name agent-instance)
     :Preprocessor preproc}}))

(def find-agent-pre-delegates (partial find-agent-delegates true))
(def find-agent-post-delegates (partial find-agent-delegates false))

(defn- lookup-for-agent [event-name proc agent-instance]
  (eval-event
   {event-name
    {:Agent (:Name agent-instance)}}
   #(mapv proc %)))

(def lookup-llms-for-agent (partial lookup-for-agent :Agentlang.Core/LLMsForAgent :LLM))

(defn ensure-llm-for-agent [agent-instance]
  (if-let [llm (first (lookup-llms-for-agent agent-instance))]
    llm
    (u/throw-ex (str "No LLM attached to agent " (:Name agent-instance)))))

(defn- normalize-tool [tool]
  (let [tool (if (map? tool) tool (first tool))
        attrs (cn/instance-attributes tool)
        tp (:type tool)]
    {:type tp (keyword tp) (dissoc attrs :type)}))

(def lookup-agent-tools (partial lookup-for-agent :Agentlang.Core/AgentTools normalize-tool))

(defn- normalize-docchunk [docchunk]
  (let [docchunk (if (map? docchunk) docchunk (first docchunk))]
    (str (:Title docchunk) " " (:Content docchunk))))

(def lookup-agent-docs (partial lookup-for-agent :Agentlang.Core/AgentDocuments normalize-docchunk))
(def has-agent-docs? (partial lookup-for-agent :Agentlang.Core/HasAgentDocuments seq))

(defn add-agent-document [agent-name doc-title doc-uri]
  (eval-event
   {:Agentlang.Core/AddAgentDocument
    {:Agent agent-name
     :Uri doc-uri
     :Title doc-title}}))

(defn- context-chat-id [agent-instance]
  (get-in agent-instance [:Context :ChatId]))

(defn lookup-agent-chat-session [agent-instance]
  (when-let [chat-sessions (seq (eval-event
                                 {:Agentlang.Core/LookupAgentChatSessions
                                  {:Agent agent-instance}}))]
    (if-let [chat-id (context-chat-id agent-instance)]
      (first (filter #(= (:Id %) chat-id) chat-sessions))
      (let [n (:Name agent-instance)]
        (first (filter #(= (:Id %) n) chat-sessions))))))

(defn create-agent-chat-session [agent-instance alt-instruction]
  (let [ins (or (:UserInstruction agent-instance) alt-instruction)]
    (when ins
      (eval-internal-event
       {:Agentlang.Core/CreateAgentChatSession
        {:ChatId (or (context-chat-id agent-instance) (:Name agent-instance))
         :Messages [{:role :system :content ins}]
         :Agent agent-instance}}
       identity true))))

(defn maybe-init-agent-chat-session [agent-instance alt-instruction]
  (or (when (:CacheChatSession agent-instance)
        (when-not (lookup-agent-chat-session agent-instance)
          (create-agent-chat-session agent-instance alt-instruction)))
      agent-instance))

(defn update-agent-chat-session [chat-session messages]
  (eval-internal-event
   {:Agentlang.Core/Update_ChatSession
    {li/path-attr (li/path-attr chat-session)
     :Data {:Messages messages}}}
   identity true))

(defn reset-agent-chat-session [agent]
  (if (string? agent)
    (when-let [agent-instance (lookup-agent-by-name agent)]
      (reset-agent-chat-session agent-instance))
    (when-let [sess (lookup-agent-chat-session agent)]
      (let [msgs (vec (filter #(= :system (:role %)) (:Messages sess)))]
        (update-agent-chat-session sess msgs)))))

(defn open-entities [] ; entities that's open to be read by all users
  (set/difference (set (cn/entity-names :Agentlang.Core false)) #{:Agentlang.Core/Document}))
