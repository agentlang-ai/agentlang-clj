(ns agentlang.inference.service.channel.cmdline
  (:require [agentlang.util :as u]
            [agentlang.inference.service.channel.core :as ch]))

(def ^:private tag :cmdline)

(def ^:private run-flags (atom {}))

(defn- can-run? [channel-name]
  (get @run-flags channel-name))

(defmethod ch/channel-start tag [{channel-name :name agent-name :agent}]
  (swap! run-flags assoc channel-name true)
  (let [send (partial ch/send-instruction-to-agent channel-name agent-name (name channel-name))]
    (u/parallel-call
     {:delay-ms 2000}
     (fn []
       (loop [run? (can-run? channel-name)]
         (when run?
           (print (str channel-name "> "))
           (flush)
           (let [s (read-line)]
             (println (str agent-name ">>> " (send s))))
           (recur (can-run? channel-name))))
       (println "Bye."))))
  channel-name)

(defmethod ch/channel-shutdown tag [{channel-name :name}]
  (swap! run-flags dissoc channel-name)
  channel-name)
