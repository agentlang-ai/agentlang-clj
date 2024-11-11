(component :Recruitment.Workflow)

;; TODO: add doc-search support
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
 {:Email {:type :Email :guid true}
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
 [:eval '(println "To: " :NotifyInterviewer.Slot.AssignedTo)]
 [:eval '(println "Interview scheduled with " :NotifyInterviewer.Candidate.Name " on "
                  :NotifyInterviewer.Slot.Date " " :NotifyInterviewer.Slot.Time)]
 [:eval '(println "Candidate profile: " :NotifyInterviewer.Candidate.Profile)])

(dataflow
 :NotifyCandidate
 [:eval '(println "To: " :NotifyCandidate.Slot.Candidate)]
 [:eval '(println "Interview scheduled on " :NotifyCandidate.Slot.Date " " :NotifyCandidate.Slot.Time)])

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

(event :ProfileRejected {:CandidateEmail :Email :Reason :String})

(dataflow
 :ProfileRejected
 [:eval '(println "Rejected profile: " :ProfileRejected.CandidateEmail)])

{:Agentlang.Core/LLM {:Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :summary-agent
  :UserInstruction (str "You are a recruiter that analyses a resume and provides a summary of the "
                        "skills and experience of the candidate. The summary must be in the format - "
                        "Skills: <major-skills-of-the-candidate>, Experience in years: <years>, "
                        "Name: <candidate-name>, Email: <candidate-email>")
  :LLM :llm01}}

{:Agentlang.Core/Agent
 {:Name :interview-scheduler-agent
  :Type :eval
  :LLM :llm01
  :Input :Recruitment.Workflow/InvokeAgent
  :UserInstruction
  (str "You are an intelligent agent who schedules an interview if the profile summary of a candidate meets "
       "the specified requirements. For example, if the profile summary is "
       "\"Python programmer with 3+ years experience. Name: Sam, Email: sam@me.com\" and the requirement is "
       "\"We need to hire python programmers with 2+ years experience. Possible interviewers are ravi@acme.com, "
       "joe@acme.com and sally@acme.com.\" then schedule an interview as: "
       "[{:Recruitment.Workflow/ScheduleInterview {:CandidateEmail \"sam@me.com\" :CandidateName \"Sam\" :CandidateProfile \"Python programmer with 3+ years experience.\" :AssignedTo \"ravi@acme.com\"}}]."
       "Make sure to distribute interview assignments as evenly as possible. If the profile summary does not match "
       "the requirement, the return: "
       "[{:Recruitment.Workflow/ProfileRejected {:CandidateEmail \"sam@me.com\", :Reason \"not enough experience\"}}]"
       "\n"
       "In the current application, you need to review summarized resumes of C++ programmers "
       "and schedule interviews  with candidates with good experience.")
  :Delegates {:To :summary-agent :Preprocessor true}}}

;; Usage:
;; POST api/Recruitment.Workflow/InvokeAgent
;; {"Recruitment.Workflow/InvokeAgent":
;;  {"UserInstruction": "Here's a resume: <some-resume-text>"}}

(def slot-data
  [{:Date "2024-08-10"
    :Time "11:30 AM"}
   {:Date "2024-08-11"
    :Time "04:00 PM"}])

(dataflow
 :Agentlang.Kernel.Lang/AppInit
 [:for-each slot-data {:InterviewSlot {} :from :%}])
