(ns agentlang.inference.provider.anthropic
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.provider.protocol :as p]
            [agentlang.inference.provider.registry :as r]))

;; Since, Anthropic doesn't have support for embedding,
;; using OpenAI embeddings for this usage.
(def ^:private default-embedding-endpoint "https://api.openai.com/v1/embeddings")
(def ^:private default-embedding-model "text-embedding-3-small")

(defn- get-openai-api-key [] (u/getenv "OPENAI_API_KEY"))

(defn make-openai-embedding [{text-content :text-content
                              model-name :model-name
                              openai-api-key :openai-api-key
                              embedding-endpoint :embedding-endpoint :as args}]
  (let [openai-config (r/fetch-active-provider-config)
        model-name (or model-name (:EmbeddingModel openai-config) default-embedding-model)
        embedding-endpoint (or embedding-endpoint (:EmbeddingApiEndpoint openai-config) default-embedding-endpoint)
        openai-api-key (or openai-api-key (:EmbeddingApiKey openai-config) (get-openai-api-key))
        options {:headers {"Authorization" (str "Bearer " openai-api-key)
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
              (format "Failed to extract OpenAI embedding (status %s):" status)
              response))
            nil))
      (do
        (log/error
         (u/pretty-str
          (format "Failed to generate OpenAI embedding (status %s):" status)
          response))
        nil))))

(defn- get-anthropic-api-key [] (u/getenv "ANTHROPIC_API_KEY"))

(def ^:private default-temperature 1)
(def ^:private default-max-tokens 1024)

(defn- assert-message! [message]
  (when-not (and (map? message)
                 (some #{(:role message)} #{:user :assistant :system})
                 (string? (:content message)))
    (u/throw-ex (str "invalid message: " message))))

(defn- chat-completion-response
  ([model-name with-tools response]
   (let [status (:status response)]
     (if (<= 200 status 299)
       [(-> (:body response)
            (json/parse-string)
            (get-in ["content" 0 (if with-tools "input" "text")]))
        model-name]
       (do (log/error
            (u/pretty-str (str "AnthropicAI chat-completion failed with status: " status)
                          response))
           nil))))
  ([model-name response] (chat-completion-response model-name false response)))

(def ^:private default-completion-endpoint "https://api.anthropic.com/v1/messages")
(def ^:private default-completion-model "claude-3-5-sonnet-latest")
(def ^:private default-ocr-completion-model "claude-3-5-sonnet-latest")
(def ^:private default-anthropic-version "2023-06-01")

(defn make-anthropic-message [{messages :messages
                               model-name :model-name
                               anthropic-api-key :anthropic-api-key
                               anthropic-version :anthropic-version
                               completion-endpoint :completion-endpoint
                               temperature :temperature
                               max-tokens :max-tokens
                               tools :tools}]
  (doseq [m messages] (assert-message! m))
  (let [anthropic-config (r/fetch-active-provider-config)
        model-name (or model-name (:CompletionModel anthropic-config) default-completion-model)
        completion-endpoint (or completion-endpoint (:CompletionApiEndpoint anthropic-config) default-completion-endpoint)
        temperature (or temperature (:Temperature anthropic-config) default-temperature)
        max-tokens (or max-tokens (:MaxTokens anthropic-config) default-max-tokens)
        anthropic-api-key (or anthropic-api-key (:ApiKey anthropic-config) (get-anthropic-api-key))
        anthropic-version (or anthropic-version (:AnthropicVersion anthropic-config) default-anthropic-version)
        system-message (first (filterv #(= (:role %) :system) messages))
        messages (into [] (remove #(= % system-message) messages))
        formatted-system-message (get system-message :content)
        options {:headers {"content-type" "application/json"
                           "x-api-key" anthropic-api-key
                           "anthropic-version" anthropic-version}
                 :body (json/generate-string
                        {:model model-name
                         :system formatted-system-message
                         :temperature temperature
                         :messages messages
                         :max_tokens max-tokens})}
        response @(http/post completion-endpoint options)]
    (chat-completion-response model-name (and tools true) response)))

(defn make-anthropic-ocr-completion [{user-instruction :user-instruction
                                      image-media-type :image-media-type
                                      image-encoded-data :image-encoded-data
                                      anthropic-version :anthropic-version}]
  (let [anthropic-config (r/fetch-active-provider-config)
        model-name default-ocr-completion-model
        completion-endpoint (or (:CompleteApiEndpoint anthropic-config) default-completion-endpoint)
        max-tokens 1024
        anthropic-version (or anthropic-version (:AnthropicVersion anthropic-config) default-anthropic-version)
        anthropic-api-key (or (:ApiKey anthropic-config) (get-anthropic-api-key))
        messages
        [{"role" "user"
          "content"
          [{"type" "text"
            "text" user-instruction}
           {"type" "image"
            "source"
            {"type" "base64"
             "media_type" image-media-type
             "data" image-encoded-data}}]}]
        options {:headers {"content-type" "application/json"
                           "x-api-key" anthropic-api-key
                           "anthropic-version" anthropic-version}
                 :body (json/generate-string
                        {:model model-name
                         :messages messages
                         :max_tokens max-tokens})}
        response @(http/post completion-endpoint options)]
    (chat-completion-response model-name response)))

(r/register-provider
 :anthropic
 (reify p/AiProvider
   (make-embedding [_ spec]
     (make-openai-embedding spec))
   (make-completion [_ spec]
     (make-anthropic-message spec))
   (make-ocr-completion [_ spec]
     (make-anthropic-ocr-completion spec))))
