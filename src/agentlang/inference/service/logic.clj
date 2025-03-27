(ns agentlang.inference.service.logic
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.util.logger :as log]
            [agentlang.datafmt.json :as json]
            [agentlang.global-state :as gs]
            [agentlang.exec-graph :as exg]
            [agentlang.inference.provider :as provider]
            [agentlang.inference.provider.core :as p]
            [agentlang.inference.embeddings.core :as ec]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.tools :as tools]
            [agentlang.inference.service.planner :as planner])
  (:import (clojure.lang ExceptionInfo)))

(def ^:private generic-agent-handler (atom nil))

(defn set-generic-agent-handler! [f]
  (reset! generic-agent-handler f))

(defn handle-doc-chunk [operation instance]
  (when (= :add operation)
    (let [doc-chunk (cn/instance-attributes instance)
          app-uuid (:AppUuid doc-chunk)]
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
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(let [ins (:UserInstruction instance)
          docs (maybe-lookup-agent-docs instance)
          final-instruction (maybe-add-docs docs ins)
          instance (assoc instance :UserInstruction final-instruction)]
      (provider/make-completion instance))))

(defn handle-agent-gen-agent [instance]
  (let [s (str (:UserInstruction instance) "\nGenerate an agent with `core.al` file contents and `model.al` file contents.\n")]
    (handle-chat-agent (assoc instance :UserInstruction s))))

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

(defn- add-delegates-as-tools [instance delegate-events]
  (let [ins (:UserInstruction instance)
        tools (tools/as-raw-tools delegate-events)]
    (assoc instance :UserInstruction
           (str "You can use these additional definitions:\n" tools "\n\n" ins))))

(defn handle-planner-agent [instance]
  (let [deleg-events (su/nonils (mapv #(when-let [s (:Input %)] (keyword s)) (model/find-agent-delegates instance)))
        instance (if (seq deleg-events)
                   (add-delegates-as-tools instance deleg-events)
                   instance)
        [orig-result model-name] (handle-chat-agent instance)
        _ (log/debug (str "Planner " (:Name instance) " raw result: " orig-result))
        insts? (instance-results? orig-result)
        result (if insts? orig-result (splice-parent-expressions instance (normalize-planner-expressions orig-result)))
        _ (log/debug (str "Planner " (:Name instance) " final result: " result))
        patterns (cond
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
              (do (exg/add-agent-node (:Name instance))
                  (exg/exit-node (:result (gs/evaluate-patterns patterns))))
              patterns))
           model-name])
      :else
      [result model-name])))

(defn handle-interactive-planner-agent [instance]
  (let [[resp model :as r] (handle-chat-agent instance)]
    (if (= "OK" (s/upper-case resp))
      (if-let [delegate (keyword (first (:Delegates instance)))]
        (let [ins (or (get-in instance [:Context :UserInstruction]) (:UserInstruction instance))]
          (:result (gs/evaluate-pattern {delegate {:UserInstruction ins}})))
        r)
      r)))

(defn handle-ocr-agent [instance]
  (p/call-with-provider
   (model/ensure-llm-for-agent instance)
   #(provider/make-ocr-completion instance)))
