{:name :Ticket
 :agentlang-version "current"
 :components [:Ticket.Core]
 :connection-types
 [{:selfservice-jira-connection :Ticket.Core/JiraConnectionConfig}
  {:selfservice-github-connection :BearerToken}]}
