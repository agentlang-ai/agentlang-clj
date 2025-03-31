(ns agentlang.inference.service.channel.core
  (:require [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]))

(def channel-type-tag :channel-type)

(defn channel-agent-name [ch]
  (when-let [agent (:via ch)]
    (u/keyword-as-string agent)))

(defn dissoc-agent [ch] (dissoc ch :via))

;; The argument-map of `channel-start` should contain the following keys:
;; :channel-type - [keyword]
;; :name - channel-name [string]
;; :config - channel configuration [map]
;; `channel-start` may never return. If this function finishes without an error,
;; return a truth value.
(defmulti channel-start channel-type-tag)

;; The argument-map of `channel-shutdown should contain the following keys:
;; :channel-type - [keyword]
;; :name - channel-name [string]
;; On success, return a truth value.
(defmulti channel-shutdown channel-type-tag)

(defn- find-agent-by-name [agent-name]
  (first
   (:result
    (gs/kernel-call
     #(gs/evaluate-pattern
       {:Agentlang.Core/Agent {:Name? agent-name}})))))

(defn send-instruction-to-agent [channel-name agent-name chat-id message]
  (try
    (if-let [agent (find-agent-by-name agent-name)]
      (if-not (some #{channel-name} (map :name (:Channels agent)))
        (str "Channel " channel-name " is not attached to agent " agent-name)
        (if-let [input (:Input agent)]
          (gs/run-inference
           (cn/make-instance input {:ChatId chat-id :UserInstruction message})
           agent)
          (str "No input-event defined for agent " agent-name)))
      (str "Agent " agent-name " not found"))
    (catch #?(:clj Exception :cljs :default) ex
      (str "Error invoking agent " agent-name " - " #?(:clj (.getMessage ex) :cljs ex)))))
