{:name :Selfservice
 :agentlang-version "0.6.1-alpha8"
 :dependencies [[prismatic/schema "1.4.1"]
                [:git "https://github.com/fractl-dev/slack.git" {:model :Slack}]
                [:git "https://github.com/fractl-dev/ticket.git" {:model :Ticket}]]
 :components [:Selfservice.Core]}
