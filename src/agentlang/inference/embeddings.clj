(ns agentlang.inference.embeddings
  (:require [agentlang.inference.embeddings.internal.queue :as q]))

(defn embed-schema [spec] (q/enqueue spec))
