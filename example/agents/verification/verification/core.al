(component :Verification.Core)

{:Agentlang.Core/LLM {:Name :llm01}}

;; Demonstrates agent-composition, where the answer of one agent is verified by another.
{:Agentlang.Core/Agent
 {:Name :verification-agent
  :LLM :llm01
  :UserInstruction
  (str "You are an agent who verifies the answer returned by another agent. "
       "Analyse the chain-of-thought returned by the other agent and return YES "
       "if its conlusion is correct. Otherwise return NO. The final answer will be "
       "encoded by the other agent as - ANSWER is: <some-text>")}}

{:Agentlang.Core/Agent
 {:Name :chain-of-thought-agent
  :LLM :llm01
  :UserInstruction (str "You are an agent who answer user queries by taking advantage of "
                        "a chain-of-thought. That means, you will take a step-by-step approach "
                        "in your response, cite sources and give reasoning before sharing final answer "
                        "in the below format: ANSWER is: <name>")
  :Delegates {:To "verification-agent"}
  :Input :Verification.Core/AnswerWithVerification}}

;; Usage:
;; POST api/Verification.Core/AnswerWithVerification
;; {"Verification.Core/AnswerWithVerification":
;;  {"UserInstruction": "Who was the most decorated (maximum medals) individual athlete in the Olympic games that were held at Sydney?"}}
