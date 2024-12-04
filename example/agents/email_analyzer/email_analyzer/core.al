(component :EmailAnalyzer.Core)

(entity
 :Company
 {:Name {:type :String :guid true}})

(entity
 :AccountEntry
 {:Id :Identity
  :Description :String
  :Type {:oneof ["income" "expense"]}
  :Amount :Decimal
  :Date :String})

(relationship
 :CompanyAccountEntry
 {:meta {:contains [:Company :AccountEntry]}})

{:Agentlang.Core/Agent
 {:Name :EmailHandlingAgent
  :Type :planner
  :Tools [:EmailAnalyzer.Core/AccountEntry
          :EmailAnalyzer.Core/CompanyAccountEntry]
  :UserInstruction (str "Convert an email message to instances of account entry under a specific company. "
                        "The company already exists, do not try to create it.")
  :LLM :llm01
  :Input :EmailAnalyzer.Core/InvokeEmailHandler}}

{:EmailAnalyzer.Core/Company {:Name "acme"}}
{:EmailAnalyzer.Core/Company {:Name "abc ltd"}}
