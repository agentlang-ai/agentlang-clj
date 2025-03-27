(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

{:Agentlang.Core/Agent
 {:Name :Expense.Workflow/AnalyzeInstruction
  :Type :interactive-planner
  :ToolComponents [:Expense.Workflow]
  :Input :Expense.Workflow/AnalyzeInstruction
  :LLM :llm01}}

{:Agentlang.Core/LLM {:Type :openai :Name :llm01}}

(event
 :ReceiptImageToExpenseReport
 {:meta {:doc (str "Returns a textual expense report, so do not try "
                   "to access attributes like `UserInstruction` on the return value.")}
  :Url :String})

{:Agentlang.Core/Agent
 {:Name :Expense.Workflow/ReceiptImageToExpenseReportAgent
  :Features [:ocr]
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")
  :Input :Expense.Workflow/ReceiptImageToExpenseReport
  :LLM :llm01}}

{:Agentlang.Core/Agent
 {:Name :Expense.Workflow/ConvertReportToExpenseAgent
  :UserInstruction "Convert an expense report to individual instances of the expense entity."
  :Tools [:Expense.Workflow/Expense]}}

{:Agentlang.Core/Agent
 {:Name :Expense.Workflow/SaveExpensesAgent
  :LLM :llm01
  :UserInstruction (str "1. Extract an expense report from the given receipt image url.\n"
                        "2. Convert this report to individual expenses.")
  :Delegates [:Expense.Workflow/ReceiptImageToExpenseReportAgent
              :Expense.Workflow/ConvertReportToExpenseAgent]}}

;; Usage:
;; POST api/Expense.Workflow/SaveExpensesAgent
;; {"Expense.Workflow/SaveExpensesAgent": {"UserInstruction": "https://acme.com/bill/myexpense.jpg"}}
