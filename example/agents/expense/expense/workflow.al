(component :Expense.Workflow)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

(def n-expenses (atom (inc (rand-int 5))))

(defn poll-expenses []
  (Thread/sleep (+ 1000 (rand-int 2500)))
  (if (pos? @n-expenses)
    (let [exp {:Title (rand-nth ["rent" "salary" "water bill" "electricity"])
               :Amount (* 1.0 (inc (rand-int 1500)))}]
      (swap! n-expenses dec)
      (agentlang.component/make-instance :Expense.Workflow/Expense exp))
    (println "poll-expenses: done")))

(resolver
 :ExpenseListenerResolver
 {:paths [:Expense.Workflow/Expense]
  :with-methods
  {:listener {:source poll-expenses}}})

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

(dataflow
 [:after :create-source :Expense]
 {:Expense.Workflow/ClassifyExpense {:UserInstruction '(pr-str :Instance)}})

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
