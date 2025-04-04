(component :Joke.Core)

{:Agentlang.Core/LLM
 {:Type "openai",
  :Name "llm-openai"}}

{:Agentlang.Core/LLM
 {:Type "anthropic",
  :Config {:MaxTokens 8192
           :Cache false},
  :Name "llm-anthropic"}}

{:Agentlang.Core/Agent
 {:Name :joke-agent
  :LLM "llm-anthropic"
  :UserInstruction "I am an AI bot who tell jokes"
  :Input :Joke.Core/TellAJoke}}

;; Usage:
;; POST api/Joke.Core/TellAJoke
;; {"Joke.Core/TellAJoke": {"UserInstruction": "OK, tell me a joke about AGI?"}}

;; To start a new session, add a session-identifier to the request:
;; {"Joke.Core/TellAJoke": {"UserInstruction": "OK, tell me a joke about AGI?" "ChatId": "my-new-chat-session"}}
