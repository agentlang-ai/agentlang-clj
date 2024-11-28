(ns agentlang.inference.service.agent-gen
  (:require
    [agentlang.util :as u]))

(def generic-agent-gen-instructions
  (str "Consider this generation of agent in Agentlang in `core.al` file.\n"
       (u/pretty-str
        '(component :Joke.Core)
        "\n\n"
        '{:Agentlang.Core/Agent
          {:Name :joke-agent
           :UserInstruction "I am an AI bot who tell jokes"
           :Input :Joke.Core/TellAJoke}})
       "\n\nThis is an agent generated to tell jokes. Specifically, Agentlang, has following structure where it has component on the top of the file with `(component <name>)`, you can assume\n"
       "that the file is core.al and for generation of a Joke agent, it can be `Joke.Core`. Now, the Agent declaration is interesting.\n"
       "Agentlang defines such structure of map with name as `:Agentlang.Core/Agent`."
       "\nIt should have a value of another map with key `:Name`, `:UserInstruction` and `:Input`."
       "\nFor the `Name`, make it a keyword of agentname-agent, e.g. `:joke-agent`."
       "\nFor the `UserInstruction`, you can generate an string for whatever your use-case is, or whatever you've been asked to be an agent of."
       "\nFor the `:Input`, have it as a full qualifer name of `<component-name>/<good-generated-functionality-name>`"
       "For, e.g. `:Joke.Core/TellAJoke`, the name suits well as, the agent is for telling a joke and the description aligns with it."
       "\n\n When all of this is in place, user, can send a POST request to `api/Joke.Core/TellAJoke` with body as, {\"Joke.Core/TellAJoke\": {\"UserInstruction\": \"OK, tell me a joke about AGI?\"}} for LLM to generate a joke."
       "\nFor this reason, you have to generate proper key and value for the data."
       "\n\n\nNow, let's look at the `model.al` that needs to be generated along with `core.al` file above."
       (u/pretty-str
        '{:name :Joke
          :agentlang-version "current"
          :components [:Joke.Core]})
       "\n\nThis is the `model.al` file for the Joke agent."
       "The `:name` is, `:Joke` without the `.Core` from the component-name."
       "For simplicity, you can always keep `:agentlang-version` as \"current\"."
       "The `:components` must be a vector of the component name, hence `[:Joke.Core]`"
       "\n\n When a prompt asks you to generate an agent, you must understand the use case of the agent and"
       "generate these two files."))

(defn with-instructions [instance]
  (assoc instance :UserInstruction
         (str generic-agent-gen-instructions
              "Additional agent generation specific instruction from the user follows:\n\n" (:UserInstruction instance))))
