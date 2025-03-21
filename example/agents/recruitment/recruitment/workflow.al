(component :Recruitment.Workflow)

(entity
 :InterviewSlot
 {:Id :Identity
  :Date :String
  :Time :String
  :AssignedTo {:type :Email :optional true}
  :Candidate {:type :Email :optional true}
  :Open {:type :Boolean :default true}})

(entity
 :Candidate
 {:Email {:type :Email :id true}
  :Name :String
  :Profile :String})

(event
 :ScheduleInterview
 {:CandidateEmail :Email
  :CandidateName :String
  :CandidateProfile :String
  :AssignedTo :Email})

(dataflow
 :NotifyInterviewer
 [:call '(println "To: " :NotifyInterviewer.Slot.AssignedTo)]
 [:call '(println "Interview scheduled with " :NotifyInterviewer.Candidate.Name " on "
                  :NotifyInterviewer.Slot.Date " " :NotifyInterviewer.Slot.Time)]
 [:call '(println "Candidate profile: " :NotifyInterviewer.Candidate.Profile)])

(dataflow
 :NotifyCandidate
 [:call '(println "To: " :NotifyCandidate.Slot.Candidate)]
 [:call '(println "Interview scheduled on " :NotifyCandidate.Slot.Date " " :NotifyCandidate.Slot.Time)])

(dataflow
 :ScheduleInterview
 {:InterviewSlot {:Open? true} :as [:FreeSlot]}
 {:Candidate
  {:Email :ScheduleInterview.CandidateEmail
   :Name :ScheduleInterview.CandidateName
   :Profile :ScheduleInterview.CandidateProfile}
  :as :Candidate}
 {:InterviewSlot
  {:Id? :FreeSlot.Id
   :AssignedTo :ScheduleInterview.AssignedTo
   :Candidate :ScheduleInterview.CandidateEmail
   :Open false} :as [:UpdatedSlot]}
 {:NotifyInterviewer
  {:Candidate :Candidate
   :Slot :UpdatedSlot}}
 {:NotifyCandidate
  {:Candidate :Candidate
   :Slot :UpdatedSlot}}
 :UpdatedSlot)

(event :RejectProfile {:CandidateEmail :Email :Reason :String})

(dataflow
 :RejectProfile
 [:call '(println "Rejected profile: " :RejectProfile.CandidateEmail)]
 :RejectProfile.Reason)

{:Agentlang.Core/LLM {:Name :llm01}}

(event
 :CreateProfileSummaryTextFromResume
 {:meta {:doc "Returns profile summary as text."}
  :UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :Recruitment.Workflow/CreateProfileSummaryTextFromResumeAgent
  :Input :Recruitment.Workflow/CreateProfileSummaryTextFromResume
  :UserInstruction (str "You are a recruiter that analyses a resume and provides a summary of the "
                        "skills and experience of the candidate. The summary must be in the format - "
                        "Skills: <major-skills-of-the-candidate>, Experience in years: <years>, "
                        "Name: <candidate-name>, Email: <candidate-email>")
  :LLM :llm01}}

(event
 :CheckProfileSummary
 {:meta {:doc "Returns either \"yes\" or \"no\"."}
  :UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :Recruitment.Workflow/CheckProfileSummaryAgent
  :LLM :llm01
  :Input :Recruitment.Workflow/CheckProfileSummary
  :UserInstruction
  (str "If the profile passed to you is for an experienced C++ programmer, return `yes`, otherwise return `no`.")}}

{:Agentlang.Core/Agent
 {:Name :Recruitment.Workflow/InterviewDirectorAgent
  :LLM :llm01
  :UserInstruction (str "1. Create a profile-summary text from the given resume.\n"
                        "2. Check the profile-summary text from step (1) by calling the CheckProfileSummary event.\n"
                        "3. If the result of step (2) is \"yes\", then schedule an interview for the candidate. Otherwise, reject the candidate's profile. "
                        "(An interview may be assigned to one of sam@acme.com, joe@amce.com and susan@acme.com).\n"
                        "Do not skip any of the steps 1, 2 and 3.\n")
  :Tools [:Recruitment.Workflow/ScheduleInterview :Recruitment.Workflow/RejectProfile]
  :Delegates [:Recruitment.Workflow/CreateProfileSummaryTextFromResumeAgent
              :Recruitment.Workflow/CheckProfileSummaryAgent]}}

;; Usage:
;; POST api/Recruitment.Workflow/InterviewDirectorAgent
;; {"Recruitment.Workflow/InterviewDirectorAgent":
;;  {"UserInstruction": "Here's a resume: <some-resume-text>"}}

(def slot-data
  [{:Date "2024-08-10"
    :Time "11:30 AM"}
   {:Date "2024-08-11"
    :Time "04:00 PM"}])

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 [:for-each slot-data {:InterviewSlot {} :from :%}])
