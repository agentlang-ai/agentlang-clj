(component :Features)

{:Agentlang.Core/LLM {:Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :cot-agent
  :LLM :llm01
  :UserInstruction "You are an agent who counsels people on their life-problems"
  :Features ["chain-of-thought"]
  :Input :Features/Chat01}}

{:Agentlang.Core/Agent
 {:Name :cot-sc-agent
  :LLM :llm01
  :UserInstruction "You are an agent who counsels people on their life-problems"
  :Features ["chain-of-thought" "self-critique"]
  :Input :Features/Chat02}}
