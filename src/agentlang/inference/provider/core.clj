(ns agentlang.inference.provider.core
  (:require [agentlang.util :as u]
            [agentlang.global-state :as gs]))

(def ^:dynamic active-llm nil)

(def lookup-provider-by-name
  (memoize
   (fn [provider-name]
     (when provider-name
       (:result
        (gs/evaluate-dataflow-internal
         {:Agentlang.Core/FindLLM
          {:Name provider-name}}))))))

(defn find-first-provider []
  (if-let [provider (first (:result (gs/evaluate-dataflow-internal
                                     {:Agentlang.Core/LookupAll_LLM {}})))]
    provider
    (u/throw-ex "No default LLM provider found")))

(defn call-with-provider [provider-name f]
  (if-let [provider (if provider-name
                      (lookup-provider-by-name provider-name)
                      (find-first-provider))]
    (binding [active-llm provider]
      (f))
    (u/throw-ex (str "LLM provider " provider-name " not found"))))
