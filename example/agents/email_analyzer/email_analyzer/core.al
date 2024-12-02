(component :EmailAnalyzer.Core)

(entity
 :Company
 {:Name {:type :String :guid true}})

(entity
 :AccountEntry
 {:Company :String
  :Id :Identity
  :Description :String
  :Type {:oneof ["income" "expense"]}
  :Amount :Decimal
  :Date :String})

{:Agentlang.Core/Agent
 {:Name :EmailHandlingAgent
  :Type :planner
  :Tools [:EmailAnalyzer.Core/AccountEntry]
  :UserInstruction "Convert an email message to instances of account entry."
  :LLM :llm01
  :Input :EmailAnalyzer.Core/InvokeEmailHandler}}

{:EmailAnalyzer.Core/Company {:Name "acme"}}
{:EmailAnalyzer.Core/Company {:Name "abc"}}
