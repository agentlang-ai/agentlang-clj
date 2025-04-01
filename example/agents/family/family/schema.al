(component :Family.Schema)

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

(event
 :FindSiblings
 {:meta {:doc "Invoke this event to find the siblings of a member"}
  :Member :Email})

(dataflow
 :FindSiblings
 {:Member? {}
  :Siblings? {:Member {:Email :FindSiblings.Member} :as :Sibling1}
  :as :siblings})
