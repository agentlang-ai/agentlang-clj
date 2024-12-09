(ns agentlang.inference.provider
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.provider.protocol :as p]
            [agentlang.inference.provider.openai]
            [agentlang.inference.provider.anthropic]
            [agentlang.inference.provider.registry :as r]))

(defn- make-provider-request [pfn spec]
  (if-let [active-provider (r/fetch-active-provider)]
    (pfn active-provider spec)
    (u/throw-ex "No active LLM provider")))

(def make-embedding (partial make-provider-request p/make-embedding))

(defn- inference-agent? [x] (when x (cn/instance-of? :Agentlang.Core/Agent x)))

(defn- preproc-messages [msgs]
  (mapv
   #(into
     {}
     (mapv
      (fn [[k v]]
        (let [k (u/string-as-keyword k)]
          [k (if (= k :role) (keyword v) v)]))
      %))
   msgs))

(defn- add-user-instruction [agent-instance msgs]
  (if-let [ins (:UserInstruction agent-instance)]
    (vec (concat msgs [{:role :user :content ins}]))
    msgs))

(defn- fetch-messages [agent-instance]
  (if-let [sess (model/lookup-agent-chat-session agent-instance)]
    [(add-user-instruction agent-instance (preproc-messages (:Messages sess))) sess]
    [(add-user-instruction agent-instance nil) nil]))

(defn- maybe-agent-to-spec [obj]
  (if (inference-agent? obj)
    (let [[msgs chat-session] (fetch-messages obj)]
      [{:messages msgs :tools (:tools obj)} obj chat-session])
    (if-let [agent (:agent obj)]
      (if (inference-agent? agent)
        (let [[msgs chat-session] (fetch-messages agent)]
          [(assoc (dissoc obj :agent) :messages msgs) agent chat-session])
        [obj nil nil])
      [obj nil nil])))

(defn make-completion [agent-instance]
  (let [[spec agent-inst chat-session] (maybe-agent-to-spec agent-instance)
        msgs (:messages spec)
        result (make-provider-request p/make-completion spec)]
    (when (and chat-session (:CacheChatSession agent-instance) (seq msgs))
      (model/update-agent-chat-session
       chat-session
       (vec (concat msgs [{:role :assistant :content (first result)}]))))
    result))

(defn make-ocr-completion [agent-instance]
  (make-provider-request
   p/make-ocr-completion
   {:user-instruction (:UserInstruction agent-instance)
    :image-url (get-in agent-instance [:Context :UserInstruction])}))

(def get-embedding (comp first make-embedding))
(def get-completion (comp first make-completion))
