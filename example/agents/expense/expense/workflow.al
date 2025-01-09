(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

{:Agentlang.Core/LLM {:Type :openai :Name :llm01}}

(event
 :ReceiptImageToExpenseReport
 {:meta {:doc "Returns a textual expense report"}
  :Url :String})

{:Agentlang.Core/Agent
 {:Name :receipt-ocr-agent
  :Type :ocr
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")
  :Input :Expense.Workflow/ReceiptImageToExpenseReport
  :LLM :llm01}}

{:Agentlang.Core/Agent
 {:Name :convert-report-to-expense-agent
  :Type :planner
  :UserInstruction "Convert an expense report to individual instances of the expense entity."
  :Tools [:Expense.Workflow/Expense]}}

{:Agentlang.Core/Agent
 {:Name :expense-agent
  :Type :planner
  :LLM :llm01
  :UserInstruction (str "1. Extract an expense report from the given receipt image url.\n"
                        "2. Convert this report to individual expenses.")
  :Input :Expense.Workflow/SaveExpenses
  :Delegates [:receipt-ocr-agent :convert-report-to-expense-agent]}}

;; Usage:
;; POST api/Expense.Workflow/SaveExpenses
;; {"Expense.Workflow/SaveExpenses": {"UserInstruction": "https://acme.com/bill/myexpense.jpg"}}
