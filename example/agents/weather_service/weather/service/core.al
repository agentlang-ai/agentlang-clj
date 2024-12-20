(component :Weather.Service.Core)

(entity
 :Weather
 {:Id :Identity
  :Date :Now
  :City {:type :String :indexed true}
  :Temperature :Double
  :Description {:type :String :optional true}})

(event
 :GetWeatherForCity
 {:meta {:doc "Get the latest weather report for a given city."}
  :City :String})

(dataflow
 :GetWeatherForCity
 {:Weather?
  {:where [:= :City :GetWeatherForCity.City]
   :order-by [:Date]}
  :as [:Result]}
 :Result)

{:Agentlang.Core/LLM
 {:Type :openai
  :Name :llm01
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name :weather-planner-agent
  :Type :planner
  :Tools [:Weather.Service.Core/GetWeatherForCity]
  :UserInstruction "You are an agent that figures out which tool to use to answer a user query."
  :LLM :llm01
  :Input :Weather.Service.Core/InvokePlanner}}

;; Usage:
;; POST api/Weather.Service.Core/InvokePlanner
;; {"Weather.Service.Core/InvokePlanner": {"UserInstruction": "What's the weather for Boston today?"}}

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 {:Weather.Service.Core/Weather
  {:City "Boston" :Temperature 70.4}}
 {:Weather.Service.Core/Weather
  {:City "NY" :Temperature 72.0}})
