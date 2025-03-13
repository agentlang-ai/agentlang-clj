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
 {:meta {:between [:Family :Member]}})

(relationship
 :ParentChild
 {:meta {:contains [:Member :Member :as [:Parent :Child]]}})


{:Agentlang.Core/LLM {:Name :llm01}}

{:Agentlang.Core/Agent
 {:Name :Family.Core/HelperAgent
  :LLM :llm01
  :Tools [:Family.Core/Family :Family.Core/Member
          :Family.Core/FamilyMember :Family.Core/ParentChild]
  :UserInstruction (str "Based on the user request, either\n"
                        "1. Create a new Family, or\n"
                        "2. Create a Member and add that Member to a Family, or\n"
                        "3. Lookup all Members in a Family, or\n"
                        "4. Create a Member as a child of another Member, or\n"
                        "5. Lookup all children of a Member.")}}

;; Usage:
;; POST api/Family.Core/HelperAgent
;; {"Family.Core/HelperAgent": {"UserInstruction": "Create a new family named ABC"}}
