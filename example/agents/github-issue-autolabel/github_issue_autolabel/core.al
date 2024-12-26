(component
  :GithubIssueAutolabel.Core)


{:Agentlang.Core/Agent
 {:Name :autolabel-agent
  :Type :planner
  :Tools [:GithubIssueAutolabel.Resolver/Issue]
  :UserInstruction "Find issue analysis for the given Github project issue.
Output attributes are: [\"Url\", \"Severity\", \"Priority\"]
Severity: Either of [Critical, Major, Minor, Low]
Priority: Either of [Urgent, High, Moderate, Low, Negligible]
"
  :Input :GithubIssueAutolabel.Resolver/IssueTriage}}

(event
 :AnalyseIssue
 {:meta {:inherits :Agentlang.Core/Inference}})

(dataflow [:after :create :GithubIssueAutolabel.Resolver/Issue]
  {:AnalyseIssue :Instance})

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
   {:AnalyseIssue :%}]
  {:GithubIssueAutolabel.Core/Sleep {}}
  :Issues)

;; start the timer
(dataflow
 :Agentlang.Kernel.Lang/AppInit
 {:GithubIssueAutolabel.Core/FetchIssues {}})

