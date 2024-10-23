(component :Joke.Core)

{:Agentlang.Core/Agent
 {:Name :joke-agent
  :UserInstruction "I am an AI bot who tell jokes"
  :Input :Joke.Core/TellAJoke}}

;; Usage:
;; POST api/Joke.Core/TellAJoke
;; {"Joke.Core/TellAJoke": {"UserInstruction": "OK, tell me a joke about AGI?"}}

;; To start a new session, add a session-identifier to the request:
;; {"Joke.Core/TellAJoke": {"UserInstruction": "OK, tell me a joke about AGI?" "ChatId": "my-new-chat-session"}}
