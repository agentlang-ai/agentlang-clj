(component :Complexplanner.Core)

(entity
 :Income
 {:Description :String
  :Date :Now
  :Amount :Decimal})

(entity
 :Expense
 {:Description :String
  :Date :Now
  :Amount :Decimal})

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name "llm01"}}

{:Agentlang.Core/Agent
 {:Name "complex-planner"
  :Type "planner"
  :Tools [{:name "Complexplanner.Core/Income"}
          {:name "Complexplanner.Core/Expense"}]
  :UserInstruction (str "The user may ask you to do one of the following: \n"
                        "1. Save a list of income and expense descriptions into the database.\n"
                        "2. Query incomes by some criteria.\n"
                        "3. Query expense by some criteria.\n")
  :LLM "llm01"}}

(inference :InvokePlanner {:agent "complex-planner"})
