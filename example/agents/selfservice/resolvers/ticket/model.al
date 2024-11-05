{:name :Ticket
 :agentlang-version "current"
 :components [:Ticket.Core]
 :connection-types
 [{:Ticket/JiraConnection
   {:type :Ticket.Core/JiraConnectionConfig
    :title "Configure Jira Connection"
    :description "provide user-name, base-url and token for connecting to your jira org"}
   :Ticket/GithubConnection
   {:type :BearerToken
    :title "Configure Github Connection"
    :description "add your bearer-token from github"}}]}
