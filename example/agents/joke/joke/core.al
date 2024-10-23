(component :Joke.Core)

{:Agentlang.Core/Agent
 {:Name :joke-agent
  :UserInstruction "I am an AI bot who tell jokes"
  :Input :Joke.Core/TellAJoke}}

;; Usage:
;; POST api/Joke.Core/TellAJoke
;; {"Joke.Core/TellAJoke": {"UserInstruction": "OK, tell me a joke about AGI?"}}
