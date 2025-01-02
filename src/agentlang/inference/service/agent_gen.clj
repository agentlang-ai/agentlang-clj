(ns agentlang.inference.service.agent-gen
  (:require
    [agentlang.util :as u]))

(def generic-agent-gen-instructions
  (str "Consider this generation of agent in Agentlang in `core.al` file.\n"
       (u/pretty-str
        '(component :Joke.Core))
       "\n\n"
       (u/pretty-str
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
       "\n\n When a prompt asks you to generate an agent, you must understand the use case of the agent and"
       "generate this file."
       "\n\n\n Remember, you don't need to provide description, you just provide this core.al file contents."
       "\n\n Also, don't provide any backticks."
       "\n\n Let's try another example."
       "\nFollowing is an example of an expense report generator agent with support of reading bill image using ocr agent provided with agentlang."
       "\n\n This is the `core.al` for `Expense`.\n"
       (u/pretty-str
        '(component :Expense.Core))
       "\n\n"
       (u/pretty-str
        '(entity
          :Expense.Core/Expense
          {:Id :Identity
           :Title :String
           :Amount :Double}))
       "\n\n"
       (u/pretty-str
         '{:Agentlang.Core/Agent
           {:Name :receipt-ocr-agent
            :Type :ocr
            :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                                  "No need to include sub-totals, totals and other data.")}})
       "\n\n"
       (u/pretty-str
        '{:Agentlang.Core/Agent
          {:Name :expense-agent
           :Type :planner
           :UserInstruction "Convert an expense report into individual instances of the expense entity."
           :Tools [:Expense.Core/Expense]
           :Input :Expense.Core/SaveExpenses
           :Delegates {:To :receipt-ocr-agent :Preprocessor true}}})
       "\n\n\n"
       "\n This is a slighly more complicated example which has multiple agents and also, has Agentlang entity defined.\n"
       "First, we have an `:Expense.Core/Expense` entity to store `Expense` information, it contains `:Id`, `:Title` and `:Amount` attributes.\n"
       "Then, an `Agent` is defined with `:receipt-ocr-agent` which is of type `:ocr` which can analyze the image of a receipt and return items and amounts.\n"
       "There are following types of `Agent` that is supported on `Agentlang`: `:ocr`, `:classifier`, `:planner`, `:eval`, `:chat`.\n"
       "The descriptions for these various types of agents that you can create are: \n"
       "`ocr` - agents that can extract text from images \n"
       "`classifier` - agents that can return the classification of provided text and no additional descriptions\n"
       "`planner` - an agent that can generate dataflow patterns using tools and text.\n"
       "`eval` - agents that can evaluate dataflow patterns\n"
       "`chat` - simple text chat agents that answer based on user instruction\n"
       "\n Next there is another `Agent` named `:expense-agent` which is of type `:planner`, a `:planner` agent can have tools.\n"
       "The `:Tools` used here is the entity `:Expense.Core/Expense`, the input is `:Expense.Core/SaveExpenses`.\n"
       "It delegeates to `:receipt-ocr-agent` for generating items and amounts from image and then, it converts the expense report into individual instances of expense entity.\n"
       "\n The entry of `Agent` and POST request will be sent to `:Expense.Core/SaveExpenses` and it will trigger the `receipt-ocr-agent` for the operation.\n\n"
       "\n Let's look at another example of an agent that can generate weather info for any city.\n"
       "This is the `core.al` for `Weather.Service`.\n\n"
       (u/pretty-str
        '(component :Weather.Service.Core))
       "\n\n"
       (u/pretty-str
        '(entity
          :Weather.Service.Core/Weather
          {:Id :Identity
           :Date :Now
           :City {:type :String :indexed true}
           :Temperature :Double
           :Description {:type :String :optional true}}))
       "\n\n"
       (u/pretty-str
        '(event
          :Weather.Service.Core/GetWeatherForCity
          {:meta {:doc "Get the latest weather report for a given city."}
           :City :String}))
       "\n\n"
       (u/pretty-str
        '(dataflow
          :Weather.Service.Core/GetWeatherForCity
          {:Weather?
           {:where [:= :City :Weather.Service.Core/GetWeatherForCity.City]
            :order-by [:Date]}
           :as [:Result]}
          :Result))
       "\n\n"
       (u/pretty-str
        '{:Agentlang.Core/LLM
          {:Type "openai"
           :Name "llm01"
           :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
                    :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
                    :EmbeddingModel "text-embedding-3-small"
                    :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
                    :CompletionModel "gpt-3.5-turbo"}}})
       "\n\n"
       (u/pretty-str
        '{:Agentlang.Core/Agent
          {:Name "weather-planner-agent"
           :Type "planner"
           :ToolComponents ["Weather.Service.Core"]
           :UserInstruction "You are an agent that figures out which tool to use to answer a user query."
           :LLM "llm01"}})
       "\n\n\n\n"
       "\n There are more things happening here than previous examples.\n"
       "First, we define an entity like, previous example. We also define an agentlang event, which has `:City` attributes\n"
       "Now, if there is event, means there will be dataflow, in this dataflow, we find the weather of a city and order it by Date.\n"
       "There's new description of `Agentlang.Core/LLM`, we can use this to define a custom name for llm and supply that to `Agent` like, we did.\n"
       "\n This helps us customize the LLM models and APIs\n"
       "Finally, we define, an `Agent` called, `weather-planner-agent` which is of type `planner`, this uses, the `:ToolComponents` which means it can use the whole component as tool.\n"
       "It also, uses the custom LLM defined as llm01.\n"
       "\n\n Following are important things to note when you are generating agents.\n"
       "Do not reutrn any plain text in your response, if required only have clojure comments.\n"
       "Read the information and don't try to deviate from the above described structures of agents.\n"
       "Remeber the format should be a clojure format and must be a proper clojure code. It should be validated. Don't generate YAML format or any other language code, it should be Agentlang agent described above and don't deviate from the format and example provided.\n\n"
       "Just provide a properly formatted agentlang core file contents which is clojure code wrapped in string without any backticks at all."))

(defn with-instructions [instance]
  (assoc instance :UserInstruction
         (str generic-agent-gen-instructions
              "Additional agent generation specific instruction from the user follows:\n\n" (:UserInstruction instance))))
