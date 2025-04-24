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

;;
;; A simple agent that uses an MCP client to fetch its tools.
;;
(require '[agentlang.lang.tools.mcp.client :as cl])
(def t (cl/make-client-transport "npx" ["-y", "@modelcontextprotocol/server-everything", "dir"]))
(def cl (cl/init :ServerEverything t))

{:Agentlang.Core/Agent
 {:Name :Joke.Core/EchoAgent
  :LLM "llm-openai"
  :Tools [:ServerEverything/echo]
  :UserInstruction "Try to have the request echoed"}}

;; Usage:
;; POST api/Joke.Core/EchoAgent
;; {"Joke.Core/EchoAgent": {"UserInstruction": "hello there"}}

;; The agent should invoke the :ServerEverything/echo tool which will response - "Echo: hello there"
