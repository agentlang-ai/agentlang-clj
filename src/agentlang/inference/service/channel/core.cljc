(ns agentlang.inference.service.channel.core
  (:require [agentlang.util :as u]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(def channel-type-tag :channel-type)

(defn channel-agent-name [ch]
  (let [n (:name ch)]
    (when (= 2 (count (li/split-path n)))
      (u/keyword-as-string n))))

;; The argument-map of `channel-start` should contain the following keys:
;; :channel-type - [keyword]
;; :name - channel-name [string]
;; :config - channel configuration [map]
;; `channel-start` may never return. If this function finishes without an error,
;; return a truth value.
(defmulti channel-start channel-type-tag)

;; The argument-map of `channel-shutdown` should contain the following keys:
;; :channel-type - [keyword]
;; :name - channel-name [string]
;; On success, return a truth value.
(defmulti channel-shutdown channel-type-tag)

;; Send a message over the channel.
;; The argument-map of `channel-send` should contain the following keys:
;; :channel-type - [keyword]
;; :name - channel-name [string]
;; :message - message to send, usually a text [any]
;; The argument-map may contain other values as required by the implementation.
;; On success, return a truth value.
(defmulti channel-send channel-type-tag)

(def find-agent-by-name
  (memoize
   (fn [agent-name]
     (first
      (:result
       (gs/kernel-call
        #(gs/evaluate-pattern
          {:Agentlang.Core/Agent {:Name? agent-name}})))))))

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
      (log/warn (str "failed to send instruction to agent - " #?(:clj (.getMessage ex) :cljs ex)))
      (str "Error invoking agent " agent-name " - " #?(:clj (.getMessage ex) :cljs ex)))))
