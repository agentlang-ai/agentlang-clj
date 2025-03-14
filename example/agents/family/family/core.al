(component :Family.Core)

(entity
 :Family
 {:Name {:type :String :id true}})

(entity
 :Member
 {:Email {:type :Email :id true}
  :Name :String})

(relationship
 :FamilyMember
 {:meta {:contains [:Family :Member]}})

(relationship
 :Siblings
 {:meta {:between [:Member :Member :as [:Sibling1 :Sibling2]]}})

(dataflow
 :FindSiblings
 {:Family.Core/Member? {}
  :Family.Core/Siblings? {:Family.Core/Member {:Email "joe@abc.com"} :as :Sibling1}
  :as :siblings})

{:Agentlang.Core/LLM {:Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :Family.Core/HelperAgent
  :LLM :llm01
  :Tools [:Family.Core/Family :Family.Core/Member
          :Family.Core/FamilyMember :Family.Core/ParentChild]
  :UserInstruction (str "Based on the user request, either\n"
                        "1. Create a new Family.\n"
                        "2. Create a Member and add that Member to a Family.\n"
                        "3. Lookup all Members in a Family.\n"
                        "4. Create a Member as a child of another Member.\n"
                        "5. Lookup all children of a Member.\n\n"
                        "As an example, the following expression adds a new member named \"sam\" to the \"scotts\" family:\n"
                        "(make-child :Family.Core/Member {:Name \"sam\" :Email \"sam@family.org\"} :Family.Core/FamilyMember \"scotts\")\n"
                        "Another example of creating a sibling relationship:\n"
                        "(make :Family.Core/Siblings {:Sibling1 \"mary@family.org\" :Sibling2 \"sam@family.org\"})\n")}}

;; Usage:
;; POST api/Family.Core/HelperAgent
;; {"Family.Core/HelperAgent": {"UserInstruction": "Create a new family named ABC"}}
;; Examples of other instructions:
;;  1. "Create a member named Joe with email joe@abc.com in the ABC family"
;;  2. "Add Emil with email emil@abc.com to the ABC family"
;;  3. "Who are the members of the ABC family?"
;;  4. "Make joe@abc.com a sibling emil@abc.com."
;;  5. "Who are siblings of emil@abc.com?"
