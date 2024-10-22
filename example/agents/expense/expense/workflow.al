(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

{:Agentlang.Core/LLM {:Type :openai :Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :receipt-ocr-agent
  :Type :ocr
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")
  :LLM :llm01}}

{:Agentlang.Core/Agent
 {:Name :expense-agent
  :Type :planner
  :LLM :llm01
  :UserInstruction "Convert an expense report into individual instances of the expense entity."
  :Tools [:Expense.Workflow/Expense]
  :Input :Expense.Workflow/SaveExpenses
  :Delegates {:To :receipt-ocr-agent :Preprocessor true}}} ; preprocess the bill-image with the ocr-agent.

;; Usage:
;; POST api/Expense.Workflow/SaveExpenses
;; {"Expense.Workflow/SaveExpenses": {"UserInstruction": "https://acme.com/bill/myexpense.jpg"}}
