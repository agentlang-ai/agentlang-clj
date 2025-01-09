(ns agentlang.inference.service.core
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.evaluator :as ev]
            [agentlang.global-state :as gs]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.resolver :as api-resolver]
            [agentlang.inference.provider.core :as p]))

(defn init [] (api-resolver/register-resolver))

(defn- preproc-doc-spec [doc]
  (cond
    (map? doc) doc
    (string? doc) {:Title (or (last (s/split doc #"/")) "")  :Uri doc}
    :else (u/throw-ex (str "Invalid document value - " doc))))

(defn- agent-documents [config]
  (when-let [agents (:agents config)]
    (us/nonils
     (mapv (fn [[agent-name spec]]
             (when-let [docs (:Documents spec)]
               [(if (string? agent-name)
                  agent-name
                  (subs (str agent-name) 1))
                (mapv preproc-doc-spec docs)]))
           agents))))

(defn setup-agent-documents []
  (doseq [[agent-name docs] (agent-documents (gs/get-app-config))]
    (mapv #(model/add-agent-document agent-name (:Title %) (:Uri %)) docs))
  true)
