(ns agentlang.inference.service.channel.cmdline
  (:require [agentlang.util :as u]
            [agentlang.inference.service.channel.core :as ch]))

(def ^:private tag :cmdline)

(def ^:private run-flags (atom {}))

(defn- can-run? [channel-name]
  (get @run-flags channel-name))

(defn- stop [channel-name]
  (swap! run-flags dissoc channel-name)
  channel-name)

(defmethod ch/channel-start tag [{channel-name :name agent-name :agent doc :doc schema-doc :schema-doc}]
  (swap! run-flags assoc channel-name true)
  (let [send (partial ch/send-instruction-to-agent channel-name agent-name (name channel-name))]
    (u/parallel-call
     {:delay-ms 2000}
     (fn []
       (when (seq doc) (println) (println doc))
       (when (seq schema-doc)
         (println "You can refer to the following definitions while interacting with me:")
         (println)
         (println schema-doc))
       (loop [run? (can-run? channel-name)]
         (when run?
           (print (str channel-name "> "))
           (flush)
           (let [s (read-line)]
             (if (= s "bye")
               (stop channel-name)
               (println (str agent-name ">>> " (send s)))))
           (recur (can-run? channel-name))))
       (println "Bye."))))
  channel-name)

(defmethod ch/channel-shutdown tag [{channel-name :name}]
  (stop channel-name))

(defmethod ch/channel-send tag [{msg :message}]
  (println msg)
  true)
