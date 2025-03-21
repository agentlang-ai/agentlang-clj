(ns agentlang.inference.service.channel.ttl
  (:require [agentlang.inference.service.channel.core :as cc]))

(def ^:private tag :ttl)

(def ^:private run-flags (atom {}))

(defn- can-run? [channel-name]
  (get @run-flags channel-name))

(defmethod cc/channel-start tag [{channel-name :name}]
  (swap! run-flags assoc channel-name true)
  (loop [run? (can-run? channel-name)]
    (when run?
      (print (str channel-name "> "))
      (flush)
      (println (read-line))
      (recur (can-run? channel-name))))
  (println "Bye.")
  channel-name)

(defmethod cc/channel-shutdown tag [{channel-name :name}]
  (reset! run-flags dissoc channel-name)
  channel-name)
