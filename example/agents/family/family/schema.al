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

(dataflow
 :FindSiblings
 {:Member? {}
  :Siblings? {:Member {:Email "joe@abc.com"} :as :Sibling1}
  :as :siblings})
