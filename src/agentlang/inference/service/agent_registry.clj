(ns agentlang.inference.service.agent-registry
  (:require [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.service.logic :as logic]))

(def ^:private agent-registry (atom {}))

(defn register-agent-handler [agent-type handler]
  (swap! agent-registry assoc agent-type handler)
  agent-type)

(defn fetch-agent-handler [agent-type]
  (get @agent-registry agent-type))

(register-agent-handler "planner" logic/handle-planner-agent)
(register-agent-handler "interactive-planner" logic/handle-interactive-planner-agent)
(register-agent-handler "chat" logic/handle-chat-agent)
(register-agent-handler "ocr" logic/handle-ocr-agent)
(register-agent-handler "agent-gen" logic/handle-agent-gen-agent)

(defn- cleanup-agent [inst]
  (dissoc inst :Context))

(defn- log-trigger-agent! [instance]
  (log/debug (str "Triggering " (:Type instance) " agent - " (:Name instance))))

(defn handle-generic-agent [instance]
  (if-let [handler (fetch-agent-handler (:Type instance))]
    (do (log-trigger-agent! instance)
        (cleanup-agent (assoc instance :Response (handler instance))))
    (u/throw-ex (str "No handler for agent type " (:Type instance)))))

(logic/set-generic-agent-handler! handle-generic-agent)
