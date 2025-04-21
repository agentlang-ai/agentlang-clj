(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

(def expenses-queue (atom [{:Title "rent" :Amount 456.0}
                           {:Title "salary" :Amount 2300.0}
                           {:Title "water bill" :Amount 884.33}]))

(defn poll-expenses []
  (Thread/sleep (+ 1000 (rand-int 2500)))
  (when-let [exp (first @expenses-queue)]
    (reset! expenses-queue (rest @expenses-queue))
    (agentlang.component/make-instance :Expense.Workflow/Expense exp)))

(resolver
 :ExpenseListenerResolver
 {:paths [:Expense.Workflow/Expense]
  :with-methods
  {:listener {:source poll-expenses
              :sink :Expense.Workflow/ClassifyExpense}}})

{:Agentlang.Core/LLM {:Type :openai :Name :llm01}}

(event
 :ReportExpense
 {:Type {:oneof ["major" "minor"]}
  :Title :String
  :Amount :Double})

(dataflow
 :ReportExpense
 [:call '(println (str :ReportExpense.Type " expense of $" :ReportExpense.Amount " for " :ReportExpense.Title))])

{:Agentlang.Core/Agent
 {:Name :Expense.Workflow/ClassifyExpense
  :UserInstruction (str "If the expense amount is above 1000.0, report it as major "
                        "otherwise report it as minor.")
  :Tools [:Expense.Workflow/Expense :Expense.Workflow/ReportExpense]}}

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
