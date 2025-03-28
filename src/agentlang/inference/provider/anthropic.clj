(ns agentlang.inference.provider.anthropic
  (:require [cheshire.core :as json]
            [org.httpkit.client :as http]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.provider.common :as common]
            [agentlang.inference.provider.protocol :as p]
            [agentlang.inference.provider.registry :as r]))

(def make-anthropic-embedding (fn [_] (u/throw-ex "Embeddings not supported in Anthropic")))

(defn- get-anthropic-api-key [] (u/getenv "ANTHROPIC_API_KEY"))

(def ^:private default-temperature 1)
(def ^:private default-max-tokens 1024)

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

(def make-anthropic-completion
  (common/make-completion-fn
   {:default-completion-endpoint default-completion-endpoint
    :make-request
    (fn [config {:keys [messages tools temperature max-tokens api-key version model-name cache]}]
      (let [anthropic-api-key (or api-key (:ApiKey config) (get-anthropic-api-key))
            model-name (or model-name (:CompletionModel config) default-completion-model)
            anthropic-version (or version (:Version config) default-anthropic-version)
            cache? (or cache (:Cache config) true)
            temperature (or temperature (:Temperature config) default-temperature)
            max-tokens (or max-tokens (:MaxTokens config) default-max-tokens)

            system-message (first (filterv #(= (:role %) :system) messages))
            user-messages (into [] (remove #(= % system-message) messages))

            formatted-messages (if cache?
                                 [{:role "user"
                                   :content [{:type "text"
                                              :text (get (first user-messages) :content)
                                              :cache_control {:type "ephemeral"}}]}]
                                 [{:role "user"
                                   :content [{:type "text"
                                              :text (get (first user-messages) :content)}]}])
            formatted-system-message (when system-message
                                       (if cache?
                                         [{:type "text"
                                           :text (get system-message :content)
                                           :cache_control {:type "ephemeral"}}]
                                         [{:type "text"
                                           :text (get system-message :content)}]))

            options {:headers {"content-type" "application/json"
                               "x-api-key" anthropic-api-key
                               "anthropic-version" anthropic-version}
                     :body (json/generate-string
                            {:model model-name
                             :system (if (or (nil? formatted-system-message)
                                             (empty? formatted-system-message))
                                       []
                                       formatted-system-message)
                             :temperature temperature
                             :messages formatted-messages
                             :max_tokens max-tokens})}]
        [options (partial chat-completion-response model-name)]))}))

(defn- fetch-image-data [image-url])
  ;; TODO: fetch image mime-type and binary-data


(def make-anthropic-ocr-completion
  (common/make-ocr-completion-fn
   {:default-completion-endpoint default-completion-endpoint
    :make-request
    (fn [config {user-instruction :user-instruction
                 image-url :image-url
                 version :version
                 api-key :api-key}]
      (let [model-name default-ocr-completion-model
            completion-endpoint (or (:CompleteApiEndpoint config) default-completion-endpoint)
            max-tokens 1024
            anthropic-version (or version (:Version config) default-anthropic-version)
            anthropic-api-key (or api-key (:ApiKey config) (get-anthropic-api-key))
            [image-media-type image-encoded-data] (fetch-image-data image-url)
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
                             :max_tokens max-tokens})}]
        [options (partial chat-completion-response model-name)]))}))

(r/register-provider
 :anthropic
 (reify p/AiProvider
   (make-embedding [_ spec]
     (make-anthropic-embedding spec))
   (make-completion [_ spec]
     (make-anthropic-completion spec))
   (make-ocr-completion [_ spec]
     (make-anthropic-ocr-completion spec))))
