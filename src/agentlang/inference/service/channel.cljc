(ns agentlang.inference.service.channel
  (:require [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.inference :as i]
            [agentlang.inference.service.model :as model]))

(def channel-type-tag :channel-type)

(defmulti channel-start channel-type-tag)
(defmulti channel-shutdown channel-type-tag)

(defn send-instruction-to-agent [channel-name agent-name chat-id message]
  (try
    (if-let [agent (model/force-find-agent-by-name agent-name)]
      (if-not (some #{channel-name} (:Channels agent))
        (str "Channel " channel-name " is not attached to agent " agent-name)
        (if-let [input (:Input agent)]
          (i/run-inference-for-event
           (cn/make-instance input {:ChatId chat-id :UserInstruction message})
           agent)
          (str "No input-event defined for agent " agent-name)))
      (str "Agent " agent-name " not found"))
    (catch #?(:clj Exception :cljs :default) ex
      (str "Error invoking agent " agent-name " - " #?(:clj (.getMessage ex) :cljs ex)))))
