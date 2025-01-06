(component :Verification.Core)

{:Agentlang.Core/LLM {:Name :llm01}}

(event
 :VerifyAnswer
 {:meta {:doc "Returns one of the strings: \"YES\" or \"NO\""}
  :UserInstruction :String})

;; Demonstrates agent-composition, where the answer of one agent is verified by another.
{:Agentlang.Core/Agent
 {:Name :verification-agent
  :LLM :llm01
  :Input :Verification.Core/VerifyAnswer
  :UserInstruction
  (str "You are an agent who verifies the answer returned by another agent. "
       "Analyse the reasoning and ANSWER returned by the other agent and return YES "
       "if its conlusion is correct. Otherwise return NO. "
       "You must reply either `YES` or `NO` and nothing else.")}}

(event :FindAnswerAsText {:UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :chain-of-thought-agent
  :LLM :llm01
  :UserInstruction (str "You are an agent who answer user queries by taking advantage of "
                        "a chain-of-thought. That means, you will take a step-by-step approach "
                        "in your response, cite sources and give reasoning before sharing the final answer in the ."
                        "format, ANSWER: <your-conclusion>")
  :Input :Verification.Core/FindAnswerAsText}}

{:Agentlang.Core/Agent
 {:Name :answer-verification-agent
  :LLM :llm01
  :Type :planner
  :UserInstruction (str "Find the answer for the following user question and have the answer verified. "
                        "Return both the answer and the verification result.")
  :Delegates [:chain-of-thought-agent :verification-agent]
  :Input :Verification.Core/AnswerWithVerification}}

;; Usage:
;; POST api/Verification.Core/AnswerWithVerification
;; {"Verification.Core/AnswerWithVerification":
;;  {"UserInstruction": "Who was the most decorated (maximum medals) individual athlete in the Olympic games that were held at Sydney?"}}
