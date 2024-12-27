(component
  :GithubIssueAutolabel.Core
  {:clj-import (quote [(:require [cheshire.core :as json])])})


{:Agentlang.Core/Agent
 {:Name :autolabel-agent
  :Type :planner
  :Tools [:GithubIssueAutolabel.Resolver/Issue
          :GithubIssueAutolabel.Resolver/IssueTriage]
  :UserInstruction "Find issue analysis for the given Github project issue.
Output attributes are: [\"Url\", \"Severity\", \"Priority\"]
Severity: Either of [Critical, Major, Minor, Low]
Priority: Either of [Urgent, High, Moderate, Low, Negligible]

Result output should be an instance of :GithubIssueAutolabel.Resolver/IssueTriage
"
  :Input :AnalyseIssue}}

(event
 :AnalyseIssue
 {:meta {:inherits :Agentlang.Core/Inference}})

(dataflow [:after :create :GithubIssueAutolabel.Resolver/Issue]
  {:AnalyseIssue {:UserInstruction '(cheshire.core/generate-string :Instance)}})

;; timer
(dataflow :GithubIssueAutolabel.Core/Sleep
  {:Agentlang.Kernel.Lang/Timer
   {:Name "timer-autolabel"
    :Expiry 60
    :ExpiryUnit "Minutes" ; one of ["Seconds" "Minutes" "Hours" "Days"]
    :ExpiryEvent [:q# {:GithubIssueAutolabel.Core/FetchIssues {}}]}})

(dataflow :GithubIssueAutolabel.Core/FetchIssues
  [:eval '(println "Fetching issues")]
  {:GithubIssueAutolabel.Resolver/Issue? {} :as :Issues}
  [:for-each :Issues
   {:AnalyseIssue {:UserInstruction '(cheshire.core/generate-string :%)}}]
  {:GithubIssueAutolabel.Core/Sleep {}}
  :Issues)

;; start the timer
(dataflow
 :Agentlang.Kernel.Lang/AppInit
 {:GithubIssueAutolabel.Core/FetchIssues {}})

