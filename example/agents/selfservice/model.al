{:name :Selfservice
 :agentlang-version "current"
 ;; :dependencies '[[ticket "0.0.1"] [slack "0.0.1"]]
 :dependencies [[:git "https://github.com/fractl-dev/slack.git"]
                [:git "https://github.com/fractl-dev/ticket.git"]]
 :components [:Selfservice.Core]}
