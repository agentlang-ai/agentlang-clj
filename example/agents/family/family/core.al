(component
 :Family.Core
 {:refer [:Family.Schema :Family.Teams]
  :clj-import [(:use [agentlang.inference.service.channel.cmdline])]})

{:Agentlang.Core/LLM {:Name :llm01}}

(def agent-msg "I'm an intelligent agent who will help you manage the family database.")

{:Agentlang.Core/Agent
 {:Name :Family.Core/HelperAgent
  :LLM :llm01
  :Channels [{:channel-type :default
              :name :Family.Core/HttpChannel}
             {:channel-type :cmdline
              :name :Family.Core/ReplChannel
              :doc agent-msg}
             {:channel-type :teams
              :name :Family.Core/TeamsChannel
              :doc agent-msg}]
  :Tools [:Family.Schema/Family
          :Family.Schema/Member
          :Family.Schema/FamilyMember
          :Family.Schema/Siblings
          :Family.Schema/FindSiblings]
  :UserInstruction (str "Based on the user request, either\n"
                        "1. Create a new Family.\n"
                        "2. Create a Member and add that Member to a Family.\n"
                        "3. Lookup all Members in a Family.\n"
                        "4. Create a Member as a child of another Member.\n"
                        "5. Lookup all children of a Member.\n\n"
                        "As an example, the following expression adds a new member named \"sam\" to the \"scotts\" family:\n"
                        "(make-child :Family.Schema/Member {:Name \"sam\" :Email \"sam@family.org\"} :Family.Schema/FamilyMember \"scotts\")\n"
                        "Another example of creating a sibling relationship:\n"
                        "(make :Family.Schema/Siblings {:Sibling1 \"mary@family.org\" :Sibling2 \"sam@family.org\"})\n")}}

;; Usage:
;; POST api/Family.Core/HelperAgent
;; {"Family.Core/HelperAgent": {"UserInstruction": "Create a new family named ABC"}}
;; Examples of other instructions:
;;  1. "Create a member named Joe with email joe@abc.com in the ABC family"
;;  2. "Add Emil with email emil@abc.com to the ABC family"
;;  3. "Who are the members of the ABC family?"
;;  4. "Make joe@abc.com a sibling emil@abc.com."
;;  5. "Who are siblings of emil@abc.com?"
