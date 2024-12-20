(ns agentlang.inference.service.logic
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.datafmt.json :as json]
            [agentlang.global-state :as gs]
            [agentlang.evaluator :as e]
            [agentlang.inference.provider :as provider]
            [agentlang.inference.provider.core :as p]
            [agentlang.inference.embeddings.core :as ec]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.tools :as tools]
            [agentlang.inference.service.planner :as planner]
            [agentlang.inference.service.lib.agent :as agent]
            [agentlang.inference.service.lib.prompt :as prompt])
  (:import (clojure.lang ExceptionInfo)))

(def ^:private generic-agent-handler (atom nil))

(defn set-generic-agent-handler! [f]
  (reset! generic-agent-handler f))

(defn handle-doc-chunk [operation instance]
  (when (= :add operation)
    (let [doc-chunk (cn/instance-attributes instance)
          app-uuid (:AppUuid doc-chunk)
          doc-name (:Title doc-chunk)
          chunk-text (:Content doc-chunk)]
      (log/debug (u/pretty-str "Ingesting doc chunk" doc-chunk))
      (ec/embed-document-chunk app-uuid doc-chunk)
      instance)))

(defn- assoc-tool-id [instance]
  (str (:AppUuid instance) "__"
       (:Tag instance) "__"
       (:Type instance)))

(defn- parse-tool-id [instance]
  (let [[app-uuid tag type] (s/split (:Id instance) #"__")]
    {:app-uuid app-uuid :tag tag :type type}))

(defn answer-question [app-uuid question-text
                       qcontext {:keys [use-docs?
                                        use-schema?]
                                 :as options}
                       agent-config]
  (let [agent-args {:user-question question-text
                    :background qcontext
                    :use-docs? use-docs?
                    :app-uuid app-uuid
                    :agent-config agent-config}]
    (try
      (if use-schema?
        (-> (agent/make-planner-agent agent-args)
            (apply [(dissoc agent-args :agent-config)])
            (select-keys [:answer-text
                          :patterns
                          :errormsg]))
        (-> (agent/make-docs-rag-agent agent-args)
            (apply [(dissoc agent-args :agent-config)])
            (select-keys [:answer-text])))
      (catch ExceptionInfo e
        (log/error e)
        {:errormsg (u/pretty-str (ex-message e) (ex-data e))})
      (catch Exception e
        (log/error e)
        {:errormsg (.getMessage e)}))))

(defn answer-question-analyze [question-text qcontext agent-config]
  (let [agent-args (merge {:user-statement question-text
                           :payload qcontext}
                          agent-config)]
    (try
      (-> (agent/make-analyzer-agent agent-args)
          (apply [agent-args])
          (select-keys [:answer-text
                        :patterns
                        :errormsg]))
      (catch ExceptionInfo e
        (log/error e)
        {:errormsg (u/pretty-str (ex-message e) (ex-data e))})
      (catch Exception e
        (log/error e)
        {:errormsg (.getMessage e)}))))

(defn- log-trigger-agent! [instance]
  (log/info (str "Triggering " (:Type instance) " agent - " (:Name instance))))

(defn- verify-analyzer-extension [ext]
  (when ext
    (when-not (u/keys-in-set? ext #{:Comment :OutputEntityType
                                    :OutputAttributes :OutputAttributeValues})
      (u/throw-ex (str "Invalid keys in analyzer agent extension")))
    ext))

(defn handle-analysis-agent [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [question (:UserInstruction instance)
          qcontext (:Context instance)
          ext (verify-analyzer-extension (:Extension instance))
          out-type (:OutputEntityType ext)
          out-scm (cn/ensure-schema out-type)
          pfn (model/agent-prompt-fn instance)
          agent-config
          (assoc
           (if pfn
             {:make-prompt (partial pfn instance)}
             {:information-type (:Comment ext)
              :output-keys (or (:OutputAttributes ext)
                               (vec (cn/user-attribute-names out-scm)))
              :output-key-values (or (:OutputAttributeValues ext)
                                     (cn/schema-as-string out-scm))})
           :result-entity out-type)]
      (answer-question-analyze question (or qcontext {}) agent-config))))

(defn- format-as-agent-response [agent-instance result]
  ;; TODO: response parsing should also move to agent-registry,
  ;; one handler will be needed for each type of agent.
  (log/debug (str "### " (:Name agent-instance) "\n\n" result))
  (if-let [response
           (cond
             (string? result) result
             (map? result) (first (:Response result))
             (vector? result) (first result))]
    response
    result))

(defn- respond-with-agent [agent-name agents user-instruction]
  (if-let [agent (first (filter #(= agent-name (:Name %)) agents))]
    (:Response (@generic-agent-handler (assoc agent :UserInstruction user-instruction)))
    [(str "No delegate with name " agent-name) nil]))

(defn- compose-agents [agent-instance result]
  (if (vector? result)
    (let [[response model-info] result
          delegates (model/find-agent-post-delegates agent-instance)
          ins (:UserInstruction agent-instance)]
      (log/debug (str "Response from agent " (:Name agent-instance) " - " response))
      (if-let [agent-name (when (model/classifier-agent? agent-instance) response)]
        (respond-with-agent agent-name delegates (or (get-in agent-instance [:Context :UserInstruction]) ins))
        (if (seq delegates)
          (let [n (:Name agent-instance)
                rs (mapv #(let [ins (str (or (:UserInstruction %) "") "\n" response)
                                ctx (assoc (:Context ins) :ParentResponse response)]
                            (format-as-agent-response % (@generic-agent-handler (assoc % :UserInstruction ins :Context ctx))))
                         delegates)]
            [(apply str rs) model-info])
          result)))
    result))

(defn- update-delegate-user-instruction [delegate agent-instance]
  (if (model/ocr-agent? agent-instance)
    (assoc delegate :Context (:Context agent-instance))
    (assoc delegate
           :Context (:Context agent-instance)
           :UserInstruction (str (or (:UserInstruction delegate) "")
                                 "\n"
                                 (:UserInstruction agent-instance)))))

(defn- call-preprocess-agents [agent-instance]
  (when-let [delegates (seq (model/find-agent-pre-delegates agent-instance))]
    (let [d (first delegates)
          [response model-info]
          (:Response (@generic-agent-handler (update-delegate-user-instruction d agent-instance)))]
      response)))

(defn- maybe-add-docs [docs user-ins]
  (if (seq docs)
    (str user-ins "\n Make use of the following knowledge-base:\n" (json/encode docs))
    user-ins))

(def ^:private agent-documents-limit 20)

(defn- maybe-lookup-agent-docs [agent-instance]
  (when (model/has-agent-docs? agent-instance)
    (let [embedding (provider/get-embedding {:text-content
                                             (json/encode {:Agent (:Name agent-instance)
                                                           :Content (:UserInstruction agent-instance)})})]
      (ec/find-similar-objects
       {:classname (ec/get-document-classname (:AppUuid agent-instance))
        :embedding embedding}
       agent-documents-limit))))

(defn handle-chat-agent [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [ins (:UserInstruction instance)
          docs (maybe-lookup-agent-docs instance)
          preprocessed-instruction (call-preprocess-agents instance)
          ins (if (or (model/planner-agent? instance) (model/eval-agent? instance))
                (str ins (if preprocessed-instruction (str "\n" preprocessed-instruction) ""))
                (or preprocessed-instruction ins))
          final-instruction (maybe-add-docs docs ins)
          instance (assoc instance :UserInstruction final-instruction)]
      (compose-agents instance (provider/make-completion instance)))))

(defn handle-agent-gen-agent [instance]
  (let [s (str (:UserInstruction instance) "\nGenerate an agent with `core.al` file contents and `model.al` file contents.\n")]
    (handle-chat-agent (assoc instance :UserInstruction s))))

(defn handle-classifier-agent [instance]
  (let [s (str (:UserInstruction instance) "\nReturn only the category name and nothing else.\n")]
    (handle-chat-agent (assoc instance :UserInstruction s))))

(defn- start-chat [agent-instance]
  ;; TODO: integrate messaging resolver
  (println (str (:Name agent-instance) ": " (:UserInstruction agent-instance)))
  (:ChatUuid agent-instance))

(defn- get-next-chat-message [_]
  ;; TODO: integrate messaging resolver
  (print " ? ")
  (flush)
  (read-line))

(defn- make-chat-completion [instance]
  (let [agent-name (:Name instance)
        chat-id (start-chat instance)]
    (loop [iter 0, instance instance]
      (if (< iter 5)
        (let [[result _ :as r] (provider/make-completion instance)
              chat-session (model/lookup-agent-chat-session instance)
              msgs (:Messages chat-session)]
          (log/debug (str "Response " iter " from " agent-name " - " result))
          (if (= \{ (first (s/trim result)))
            (do (println (str agent-name ": Thanks, your request is queued for processing."))
                r)
            (do (println (str agent-name ": " result))
                (model/update-agent-chat-session
                 chat-session
                 (vec (concat msgs [{:role :user :content (get-next-chat-message chat-id)}])))
                (recur (inc iter) (if (zero? iter) (dissoc instance :UserInstruction) instance)))))
        (do (println (str agent-name ": session expired"))
            [(json/encode {:error "chat session with agent " agent-name " has expired."}) "agentlang"])))))

(defn handle-orchestrator-agent [instance]
  (log-trigger-agent! instance)
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [ins (:UserInstruction instance)
          docs (maybe-lookup-agent-docs instance)
          preprocessed-instruction (call-preprocess-agents instance)
          final-instruction (maybe-add-docs docs (or preprocessed-instruction ins))
          instance (assoc instance :UserInstruction final-instruction)]
      (compose-agents instance (make-chat-completion instance)))))

(defn- maybe-eval-patterns [[response _]]
  (if (string? response)
    (if-let [pats
             (let [exp (read-string response)]
               (cond
                 (vector? exp) exp
                 (map? exp) [exp]))]
      (mapv e/safe-eval-pattern pats)
      response)
    response))

(defn handle-eval-agent [instance]
  (maybe-eval-patterns (handle-chat-agent instance)))

(defn- maybe-add-tool-params [tool-instance]
  (let [f ((keyword (:type tool-instance)) tool-instance)]
    (if-not (:parameters f)
      (let [n (keyword (:name f))]
        (if (cn/entity? n)
          (tools/entity-to-tool n)
          (tools/event-to-tool n)))
      tool-instance)))

(defn- format-planner-result [r]
  (cond
    (or (vector? r) (map? r) (string? r)) r
    (seqable? r) (vec r)
    :else r))

(defn- trim-till-first-expr [s]
  (if-let [i (s/index-of s "(")]
    (subs s i (inc (s/last-index-of s ")")))
    s))

(defn- normalize-planner-expressions [s]
  (if-let [exprs (if (string? s)
                   (u/safe-read-string (trim-till-first-expr s))
                   s)]
    (cond
      (planner/maybe-expressions? exprs) exprs
      (planner/maybe-an-expression? exprs) `(do ~exprs)
      :else exprs)
    s))

(defn- instance-results? [xs]
  (every? cn/an-instance? xs))

(defn- dataflow-patterns? [xs]
  (if (vector? xs)
    (if (instance-results? xs)
      false
      true)
    false))

(defn- block-expressions [exprs]
  (when (planner/maybe-expressions? exprs)
    (rest exprs)))

(defn- splice-parent-expressions [instance result]
  (let [orig-exprs (if (string? result)
                     (read-string result)
                     result)]
    (if-let [parent-response (:ParentResponse (:Context instance))]
      (if-let [pexprs (block-expressions
                       (if (string? parent-response)
                         (u/safe-read-string parent-response)
                         parent-response))]
        `(~'do ~@pexprs ~@(block-expressions orig-exprs))
        orig-exprs)
      orig-exprs)))

(defn handle-planner-agent [instance]
  (log-trigger-agent! instance)
  (let [tools [] #_(vec (concat
                         (apply concat (mapv tools/all-tools-for-component (:ToolComponents instance)))
                         (mapv maybe-add-tool-params (model/lookup-agent-tools instance))))
        has-tools (seq tools)
        [orig-result model-name] (handle-chat-agent
                                  (if has-tools
                                    (assoc instance :tools tools)
                                    instance))
        _ (log/debug (str "Planner " (:Name instance) " raw result: " orig-result))
        insts? (instance-results? orig-result)
        result (if insts? orig-result (splice-parent-expressions instance (normalize-planner-expressions orig-result)))
        _ (log/debug (str "Planner " (:Name instance) " final result: " result))
        patterns (cond
                   has-tools (mapv tools/tool-call-to-pattern result)
                   insts? result
                   :else (planner/expressions-to-patterns
                          (planner/validate-expressions
                           (if (string? result)
                             (read-string result)
                             result))))]
    (cond
      insts? [patterns model-name]
      (seq patterns)
      (do (log/debug (str "Patterns generated by " (:Name instance) ": " (u/pretty-str patterns)))
          [(format-planner-result
            (if (dataflow-patterns? patterns)
              (u/safe-ok-result (e/eval-patterns :Agentlang.Core patterns))
              patterns))
           model-name])
      :else
      [result model-name])))

(defn handle-ocr-agent [instance]
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(provider/make-ocr-completion instance)))
