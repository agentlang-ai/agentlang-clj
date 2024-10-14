(component
 :Selfservice.Core
 {:refer [:Slack.Core :Ticket.Core]})

(record
 :Request
 {:Org :String
  :Email :Email
  :Id :String})

{:Agentlang.Core/LLM
 {:Type :openai
  :Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :ResponseClassifierAgent
  :Type :chat
  :LLM :llm01
  :UserInstruction
  "Classify the input text to one of the categories - approve or reject.
For example if the input is `you can join the team`, your response must be `approve`.
If the input is `sorry, can't allow`, your response must be `reject`.
If you are unable to classify the text, simply return `reject`.
(Do not include the ticks (`) in your response).
Now please classify the following text based on these rules.\n\n"
  :Input :Selfservice.Core/InvokeResponseClassifierAgent}}

{:Agentlang.Core/Agent
 {:Name :WorkflowAgent
  :Type :planner
  :Tools [:Selfservice.Core/Request
          :Selfservice.Core/InvokeResponseClassifierAgent
          :Slack.Core/Chat
          :Ticket.Core/Ticket
          :Ticket.Core/TicketComment
          :Ticket.Core/GithubMember
          :Ticket.Core/TicketManager
          :Ticket.Core/ManagerSlackChannel]
  :UserInstruction "You'll receive some tickets with requests from users to join GitHub organizations. Follow the following steps:
1. Find the manager for the ticket, you can query on the ticket Id.
2. Find the slack-channel for the manager.
3. For each ticket, send an approval request as a slack message on the manager's channel. This message must include the user's email, github org name and the ticket Id.
4. Get the slack chat's response and classify it as either approve or reject.
5. If the classification result is to approve the request, then
     a. create a ticket comment with the text \"approved\".
     b. add the user as a member to the github org.
   If the response is not to approve, then create a ticket comment - \"rejected\"."
  :LLM :llm01}}

{:Agentlang.Core/Agent
 {:Name :SelfServiceAgent
  :Type :planner
  :LLM :llm01
  :Tools [:Selfservice.Core/Request]
  :UserInstruction
  "You are an agent that identifies a self-service ticket for adding a user to a github organization.
Tickets will be passed to you as a JSON payload. Analyze the tickets and return instances of Request with the
github org, email and ticket id as attributes. If the org or email is empty, ignore that ticket."
  :Delegates {:To :WorkflowAgent}
  :Input :Selfservice.Core/InvokeSelfService}}

(dataflow
 :ProcessTickets
 {:Ticket.Core/Ticket? {} :as :Result}
 [:eval (quote (ticket.core/as-json :Result)) :as :S]
 [:eval '(println "Processing tickets:" :S)]
 {:Selfservice.Core/InvokeSelfService {:UserInstruction :S}})

(dataflow
 :ProcessWebhook
 [:eval (quote (ticket.core/as-json :ProcessWebhook.Tickets)) :as :S]
 [:eval '(println "Processing tickets:" :S)]
 {:Selfservice.Core/InvokeSelfService {:UserInstruction :S}})
