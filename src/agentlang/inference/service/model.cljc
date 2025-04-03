(ns agentlang.inference.service.model
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [camel-snake-kebab.core :as csk]
            [agentlang.lang
             :refer [component
                     dataflow
                     inference
                     entity
                     event
                     event-internal
                     record
                     attribute
                     relationship]
             :as ln]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.util.http :as http]
            [agentlang.datafmt.json :as json]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            [agentlang.inference.service.planner :as planner]
            [agentlang.inference.service.agent-gen :as agent-gen]
            [agentlang.inference.service.tools :as tools]
            [agentlang.inference.service.channel.core :as ch]
            #?(:clj [agentlang.connections.client :as connections])
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(component :Agentlang.Core {:model :Agentlang})

(def subscription-events (atom {}))

(defn register-subscription-event [resolver-model input-event]
  (swap! subscription-events assoc resolver-model input-event)
  resolver-model)

(defn get-subscription-event [resolver-model]
  (get @subscription-events resolver-model))

(entity
 :Agentlang.Core/LLM
 {:Type {:type :String :default "openai"} ; e.g "openai"
  :Name {:type :String :id true :default #(us/generate-code 5)}
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
  {:Name? :Agentlang.Core/FindLLM.Name} :as [:LLM]}
 :LLM)

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
        token #?(:clj (connections/get-auth-token)
                 :cljs (u/getenv "RETRIEVAL_SERVICE_TOKEN" ""))
        url (str (or (:host config)
                     (u/getenv "RETRIEVAL_SERVICE_HOST"))
                 "/" file-name "/chunks")
        options (when (seq token) {:headers {"Token" token}})
        response (http/do-get url options)]
    (if (= 200 (:status response))
      (let [body (json/decode (:body response))]
        (apply str (concat (:chunks body))))
      (u/throw-ex (str "failed to load document from: " url ", status: " (:status response))))))

#?(:clj
   (def ^:private doc-scheme-handlers {"file" slurp
                                      "rs" read-from-retrieval-service})
   :cljs
   (def ^:private doc-scheme-handlers {"rs" read-from-retrieval-service}))
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
 {:Id {:type :UUID :default u/uuid-string :id true}
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

(def ^:private feature-set-keys (set (concat (keys feature-set) ["planner" "ocr"])))

(defn- feature-list? [xs]
  (when (seq xs)
    (and (vector? xs)
         (= feature-set-keys (set/union feature-set-keys (set xs))))))

(defn- has-feature? [x xs]
  (some #{x} (map u/keyword-as-string xs)))

(def ^:private has-planner? (partial has-feature? "planner"))
(def ^:private has-ocr? (partial has-feature? "ocr"))
(def ^:private has-interactive? (partial has-feature? "interactive"))

(defn get-feature-prompt [ft] (get feature-set ft ""))

(defn notify-channel [ch-type ch-name msg]
  (ch/channel-send {:channel-type (u/string-as-keyword ch-type)
                    :name ch-name
                    :message msg}))

(event
 :Agentlang.Core/NotifyChannel
 {:ChannelType :String
  :ChannelName :String
  :Message :String})

(dataflow
 :Agentlang.Core/NotifyChannel
 [:call '(agentlang.inference.service.model/notify-channel
          :Agentlang.Core/NotifyChannel.ChannelType
          :Agentlang.Core/NotifyChannel.ChannelName
          :Agentlang.Core/NotifyChannel.Message)])

(record
 :Agentlang.Core/Inference
 {:UserInstruction :String
  :ChatId {:type :String :optional true}})

(entity
 :Agentlang.Core/Agent
 {:Name {:type :String :id true}
  :Type {:type :String :default "chat"}
  :Features {:check feature-list? :optional true}
  :AppUuid {:type :UUID :default u/get-app-uuid}
  :UserInstruction {:type :String :optional true}
  :ToolComponents {:check tool-components-list? :optional true}
  :Input {:type :String :optional true}
  :Context {:type :Map :optional true :dynamic true}
  :Response {:type :Any :read-only true}
  :Integrations {:listof :String :optional true}
  :Delegates {:listof :String :optional true}
  :Channels {:listof :Any :optional true}
  :CacheChatSession {:type :Boolean :default true}})

(defn- agent-of-type? [typ agent-instance]
  (= typ (:Type agent-instance)))

(def ocr-agent? (partial agent-of-type? "ocr"))
(def planner-agent? (partial agent-of-type? "planner"))
(def interactive-planner-agent? (partial agent-of-type? "interactive-planner"))
(def agent-gen-agent? (partial agent-of-type? "agent-gen"))

(defn- eval-event
  ([event callback atomic?]
   (when-let [result ((if atomic?
                        gs/evaluate-dataflow-atomic
                        gs/evaluate-dataflow)
                      event)]
     (callback (:result result))))
  ([event callback] (eval-event event callback true))
  ([event] (eval-event event identity)))

(defn- eval-internal-event [event & args]
  (gs/kernel-call
   #(apply eval-event (cn/make-instance event) args)))

(defn- preproc-agent-tools-spec [tools]
  (when tools
    (mapv (fn [x]
            (cond
              (map? x) x
              (string? x) {:name x}
              (keyword? x) {:name (subs (str x) 1)}
              :else (u/throw-ex (str "Invalid tool: " x))))
          tools)))

(defn- as-event-name [agent-name]
  (if (and (keyword? agent-name)
           (= 2 (count (li/split-path agent-name))))
    agent-name
    (let [n (csk/->PascalCase agent-name)]
      (cn/canonical-type-name n))))

(def ^:private inference-event-schema {:meta {:inherits :Agentlang.Core/Inference}})

(defn- preproc-agent-input-spec [agent-name input]
  (if input
    (cond
      (string? input) input
      (keyword? input) (subs (str input) 1)
      :else (u/throw-ex (str "Invalid agent input: " input)))
    (let [event-name (as-event-name agent-name)]
      (and (event-internal event-name inference-event-schema)
           (preproc-agent-input-spec nil event-name)))))

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

(defn- fetch-channel-tools [channel]
  (when-let [tools (get-in (cn/fetch-model channel) [:channel :tools])]
    (preproc-agent-tools-spec tools)))

(defn- maybe-register-subscription-handlers! [channels input]
  (doseq [channel channels]
    (when (get-in (cn/fetch-model channel) [:channel :subscriptions])
      (register-subscription-event channel input))))

(defn- preproc-kws-vect [delegs]
  (when (seq delegs)
    (mapv u/keyword-as-string delegs)))

(def ^:private preproc-agent-delegates preproc-kws-vect)
(def ^:private preproc-agent-tool-components preproc-kws-vect)

(defn- maybe-cast-to-planner [attrs]
  (let [tp (u/string-as-keyword (:Type attrs))]
    (cond
      (or (= :planner tp) (= :interactive-planner tp))
      attrs

      (seq (:Features attrs))
      (let [fs (:Features attrs)]
        (cond
          (has-planner? fs)
          (if (has-interactive? fs)
            (assoc attrs :Type :interactive-planner)
            (assoc attrs :Type :planner))

          (has-ocr? fs)
          (assoc attrs :Type :ocr)

          (has-interactive? fs)
          (assoc attrs :Type :interactive-planner)

          :else attrs))

      (or (seq (:Tools attrs))
          (seq (:Delegates attrs)))
      (assoc attrs :Type :planner)

      :else attrs)))

(defn- maybe-start-channel [agent-name tool-docs ch]
  (when (map? ch)
    (when (not= :default (ch/channel-type-tag ch))
      (ch/channel-start (assoc ch :schema-doc tool-docs :agent (or (ch/channel-agent-name ch) agent-name)))))
  ch)

(defn- start-channels [agent-name tool-docs channels]
  (mapv (partial maybe-start-channel agent-name tool-docs) channels))

(def ^:private agent-info-cache (atom {}))

(defn- cache-agent-info [agent-name tools llm]
  (swap! agent-info-cache assoc agent-name [tools llm]))

(ln/install-standalone-pattern-preprocessor!
 :Agentlang.Core/Agent
 (fn [pat]
   (let [attrs (maybe-cast-to-planner (li/record-attributes pat))
         nm (:Name attrs)
         agent-name (u/keyword-as-string nm)
         input (preproc-agent-input-spec nm (:Input attrs))
         tools0 (:Tools attrs)
         tools (preproc-agent-tools-spec tools0)
         delegates (preproc-agent-delegates (:Delegates attrs))
         tool-components (preproc-agent-tool-components (:ToolComponents attrs))
         features (when-let [ftrs (:Features attrs)] (mapv u/keyword-as-string ftrs))
         tp (:Type attrs)
         llm (or (:LLM attrs) {:Type "openai"})
         _ (cache-agent-info agent-name tools0 llm)
         docs (:Documents attrs)
         channels (:Channels attrs)
         _ (when (seq channels) (start-channels agent-name (tools/documentation tools tool-components) channels))
         integs (when-let [xs (:Integrations attrs)] (mapv u/keyword-as-string xs))
         tools0 (vec (concat tools (flatten (us/nonils (mapv fetch-channel-tools channels)))))
         tools (if (seq channels)
                 (vec (conj tools0 {:name "Agentlang.Core/NotifyChannel"}))
                 tools0)
         new-attrs
         (-> attrs
             (cond->
                 nm (assoc :Name agent-name)
                 input (assoc :Input input)
                 tools (assoc :Tools tools)
                 delegates (assoc :Delegates delegates)
                 docs (assoc :Documents (preproc-agent-docs docs))
                 tp (assoc :Type (u/keyword-as-string tp))
                 features (assoc :Features features)
                 integs (assoc :Integrations integs)
                 channels (assoc :Channels channels)
                 tool-components (assoc :ToolComponents tool-components)
                 llm (assoc :LLM (u/keyword-as-string llm))))]
     (when (seq channels)
       (maybe-register-subscription-handlers! channels (keyword input)))
     (assoc pat :Agentlang.Core/Agent
            (cond
              (planner-agent? new-attrs) (planner/with-instructions new-attrs)
              (interactive-planner-agent? new-attrs) (planner/with-interactive-instructions new-attrs)
              (agent-gen-agent? new-attrs) (agent-gen/with-instructions new-attrs)
              :else new-attrs)))))

(defn maybe-define-inference-event [event-name]
  (if (cn/find-schema event-name)
    (if (cn/event? event-name)
      event-name
      (u/throw-ex (str "not an event - " event-name)))
    (event {event-name inference-event-schema})))

(defn maybe-input-as-inference [agent]
  (when-let [input (:Input agent)]
    (let [n (u/string-as-keyword input)]
      (when (maybe-define-inference-event n)
        (inference n {:agent (:Name agent)}))))
  agent)

(defn- maybe-assoc-channel-name [ch agent-name]
  (if (:name ch)
    ch
    (assoc ch :name (u/string-as-keyword agent-name))))

(defn maybe-create-channel-agents [agent]
  (when (planner-agent? agent)
    (doseq [ch (:Channels agent)]
      (when-let [helper-agent-name (ch/channel-agent-name ch)]
        (let [[tools llm] (get @agent-info-cache (:Name agent))
              ins (str "Analyse requests based on the definition(s) of " (s/join ", " tools) ".\n")
              _ (preproc-agent-input-spec helper-agent-name nil)
              result (try
                       (:result (gs/evaluate-pattern
                                 {:Agentlang.Core/Agent
                                  (planner/with-interactive-instructions
                                    {:Name helper-agent-name
                                     :LLM (u/keyword-as-string llm)
                                     :Type "interactive-planner"
                                     :Channels [(maybe-assoc-channel-name ch helper-agent-name)]
                                     :Delegates [(u/keyword-as-string (:Name agent))]
                                     :UserInstruction ins
                                     :Input helper-agent-name
                                     :Tools (preproc-agent-tools-spec tools)
                                     :ToolComponents (:ToolComponents agent)})}))
                       (catch #?(:clj Exception :cljs :default) ex
                         (log/warn #?(:clj (.getMessage ex) :cljs ex))
                         nil))]
          (when (and result (not (cn/instance-of? :Agentlang.Core/Agent result)))
            (log/warn (str "Failed to create channel agent " helper-agent-name)))))))
  agent)

(dataflow
 [:before :create :Agentlang.Core/Agent]
 [:call '(agentlang.inference.service.model/maybe-input-as-inference :Instance)]
 [:call '(agentlang.inference.service.model/maybe-create-channel-agents :Instance)])

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

(defn concat-results [rs] (vec (apply concat rs)))

(dataflow
 :Agentlang.Core/FindAgentDelegates
 {:Agentlang.Core/Agent
  {:Name? [:in :Agentlang.Core/FindAgentDelegates.DelegateNames]}})

(entity
 :Agentlang.Core/ChatSession
 {:Id {:type :String :id true :default u/uuid-string}
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
  :Agentlang.Core/AgentChatSession? :Agentlang.Core/LookupAgentChatSessions.Agent})

(dataflow
 :Agentlang.Core/CreateAgentChatSession
 [:try
  {:Agentlang.Core/ChatSession
   {:Id :Agentlang.Core/CreateAgentChatSession.ChatId
    :Messages :Agentlang.Core/CreateAgentChatSession.Messages}
   :Agentlang.Core/AgentChatSession :Agentlang.Core/CreateAgentChatSession.Agent}
  :error {:Agentlang.Core/LookupAgentChatSessions {:Agent :Agentlang.Core/CreateAgentChatSession.Agent}}])

(dataflow
 :Agentlang.Core/ResetAgentChatSessions
 [:call '(agentlang.inference.service.model/reset-agent-chat-session
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
 {:id {:type :UUID :id true :default u/uuid-string}
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
  :Agentlang.Core/AgentDocument
  {:Agentlang.Core/Agent {:Name? :Agentlang.Core/AddAgentDocument.Agent}}})

(attribute
 :Agentlang.Core/Documents
 {:extend :Agentlang.Core/Agent
  :type :Agentlang.Core/Document
  :relationship :Agentlang.Core/AgentDocument})

(dataflow
 :Agentlang.Core/LLMsForAgent
 {:Agentlang.Core/LLM? {}
  :Agentlang.Core/AgentLLM? :Agentlang.Core/LLMsForAgent.Agent})

(dataflow
 :Agentlang.Core/AgentTools
 {:Agentlang.Core/Tool? {}
  :Agentlang.Core/AgentTool? :Agentlang.Core/AgentTools.Agent})

(dataflow
 :Agentlang.Core/AgentDocuments
 {:Agentlang.Core/Document? {}
  :Agentlang.Core/AgentDocument? :Agentlang.Core/AgentDocuments.Agent})

(dataflow
 :Agentlang.Core/LookupAgentByName
 {:Agentlang.Core/Agent {:Name? :Agentlang.Core/LookupAgentByName.Name}})

(defn- maybe-agent-pattern [p]
  (when (and (map? p)
             (= :Agentlang.Core/Agent (li/record-name p)))
    p))

(defn lookup-agent-by-name [agent-name]
  #?(:clj
     (eval-event
      {:Agentlang.Core/LookupAgentByName
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

(defn find-agent-delegates [agent-instance]
  (when-let [ds (seq (:Delegates agent-instance))]
    (eval-event
     {:Agentlang.Core/FindAgentDelegates
      {:DelegateNames (vec ds)}})))

(defn- lookup-for-agent [event-name proc agent-instance]
  (eval-event
   {event-name {:Agent agent-instance}}
   #(when (seq %) (mapv proc %))))

(def lookup-llms-for-agent (memoize (partial lookup-for-agent :Agentlang.Core/LLMsForAgent :Name)))

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
(def has-agent-docs? (memoize (partial lookup-for-agent :Agentlang.Core/AgentDocuments seq)))

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
    {:path (li/path-attr chat-session)
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
