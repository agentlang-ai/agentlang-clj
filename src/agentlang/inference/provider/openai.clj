(ns agentlang.inference.provider.openai
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.provider.common :as common]
            [agentlang.inference.provider.protocol :as p]
            [agentlang.inference.provider.registry :as r]))

(def ^:private default-embedding-endpoint "https://api.openai.com/v1/embeddings")
(def ^:private default-embedding-model "text-embedding-3-small")

(defn- get-openai-api-key [] (u/getenv "OPENAI_API_KEY"))

(def make-openai-embedding (common/make-embedding-fn
                            {:default-embedding-endpoint default-embedding-endpoint
                             :default-embedding-model default-embedding-model
                             :get-api-key get-openai-api-key}))

(def ^:private default-temperature 0)
(def ^:private default-max-tokens 500)

(defn- chat-completion-response
  ([model-name with-tools response]
   (let [status (:status response)]
     (if (<= 200 status 299)
       [(-> (:body response)
            (json/parse-string)
            (get-in ["choices" 0 "message" (if with-tools "tool_calls" "content")]))
        model-name]
       (do (log/error
            (u/pretty-str (str "OpenAI chat-competion failed with status: " status)
                          response))
           nil))))
  ([model-name response] (chat-completion-response model-name false response)))

(def ^:private default-completion-endpoint "https://api.openai.com/v1/chat/completions")
(def ^:private default-ocr-completion-model "gpt-4o")
(def ^:private default-completion-model "gpt-3.5-turbo")

(def make-openai-completion
  (common/make-completion-fn
   {:default-completion-endpoint default-completion-endpoint
    :make-request
    (fn [config {messages :messages
                 tools :tools
                 temperature :temperature
                 max-tokens :max-tokens
                 api-key :api-key
                 model-name :model-name}]
      (let [openai-api-key (or api-key (:ApiKey config) (get-openai-api-key))
            model-name (or model-name (:CompletionModel config) default-completion-model)]
        [{:headers {"Content-type"  "application/json"
                    "Authorization" (str "Bearer " openai-api-key)}
          :body (json/generate-string
                 (merge
                  {:model model-name
                   :messages messages
                   :temperature (or temperature default-temperature)
                   :max_tokens (or max-tokens default-max-tokens)}
                  (when tools
                    {:tools tools
                     :tool_choice "auto"})))}
         (partial chat-completion-response model-name)]))}))

(def make-openai-ocr-completion
  (common/make-ocr-completion-fn
   {:default-completion-endpoint default-completion-endpoint
    :make-request
    (fn [config {user-instruction :user-instruction
                 image-url :image-url
                 api-key :api-key
                 model-name :model-name
                 max-tokens :max-tokens}]
      (let [messages
            [{"role" "user"
              "content"
              [{"type" "text"
                "text" user-instruction}
               {"type" "image_url"
                "image_url" {"url" image-url}}]}]
            openai-api-key (or api-key (:ApiKey config) (get-openai-api-key))
            model-name (or model-name (:CompletionModel config) default-ocr-completion-model)
            max-tokens (or max-tokens default-max-tokens)]
        [{:headers {"Content-type"  "application/json"
                    "Authorization" (str "Bearer " openai-api-key)}
          :body (json/generate-string {:model model-name
                                       :messages messages
                                       :max_tokens max-tokens})}
         (partial chat-completion-response model-name)]))}))

(r/register-provider
 :openai
 (reify p/AiProvider
   (make-embedding [_ spec]
     (make-openai-embedding spec))
   (make-completion [_ spec]
     (make-openai-completion spec))
   (make-ocr-completion [_ spec]
     (make-openai-ocr-completion spec))))
