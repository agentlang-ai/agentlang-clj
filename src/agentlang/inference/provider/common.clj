(ns agentlang.inference.provider.common
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.provider.registry :as r]))            

(defn make-embedding-fn [{default-embedding-endpoint :default-embedding-endpoint
                          default-embedding-model :default-embedding-model
                          get-api-key :get-api-key}]
  (fn [{text-content :text-content
        model-name :model-name
        api-key :api-key
        embedding-endpoint :embedding-endpoint :as args}]
    (let [config (r/fetch-active-provider-config)
          model-name (or model-name (:EmbeddingModel config) default-embedding-model)
          embedding-endpoint (or embedding-endpoint (:EmbeddingApiEndpoint config) default-embedding-endpoint)
          api-key (or api-key (:ApiKey config) (get-api-key))
          options {:headers {"Authorization" (str "Bearer " api-key)
                             "Content-Type" "application/json"}
                   :body (json/generate-string {"input" text-content
                                                "model" model-name
                                                "encoding_format" "float"})}
          response @(http/post embedding-endpoint options)
          status (:status response)]
      (if (<= 200 status 299)
        (or (when-let [r (-> (:body response)
                             json/parse-string
                             (get-in ["data" 0 "embedding"]))]
              [r model-name])
            (do
              (log/error
               (u/pretty-str
                (format "Failed to extract AI embedding (status %s):" status)
                response))
              nil))
        (do
          (log/error
           (u/pretty-str
            (format "Failed to generate OpenAI embedding (status %s):" status)
            response))
          nil)))))

(defn make-completion-fn [{default-completion-endpoint :default-completion-endpoint
                           make-request :make-request}]
  (fn [{completion-endpoint :completion-endpoint
        tools :tools
        :as args}]
    (let [config (r/fetch-active-provider-config)
          completion-endpoint (or completion-endpoint (:CompletionApiEndpoint config) default-completion-endpoint)
          [options chat-completion-response] (make-request config args)
          response @(http/post completion-endpoint options)]
      (chat-completion-response (and tools true) response))))

(defn make-ocr-completion-fn [{default-completion-endpoint :default-completion-endpoint
                               make-request :make-request}]
  (fn [{completion-endpoint :completion-endpoint :as args}]
    (let [config (r/fetch-active-provider-config)
          [options chat-completion-response] (make-request config args)
          completion-endpoint (or completion-endpoint (:CompletionApiEndpoint config) default-completion-endpoint)          
          response @(http/post completion-endpoint options)]
      (chat-completion-response response))))
