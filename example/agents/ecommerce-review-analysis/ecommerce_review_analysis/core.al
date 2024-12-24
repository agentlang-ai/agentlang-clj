(component
  :EcommerceReviewAnalysis.Core)

(entity
  :Review
  {:Marketplace      :String
   :CustomerId       :Int
   :ReviewId         :String
   :ProductId        :String
   :ProductParent    :String
   :ProductTitle     :String
   :ProductCategory  :String
   :StarRating       :Int
   :HelpfulVotes     :Int
   :TotalVotes       :Int
   :Vine             :Boolean
   :VerifiedPurchase :Boolean
   :ReviewHeadline   :String
   :ReviewBody       :String
   :ReviewDate       :String})

(entity
  :Analysis
  {:ReviewId :String
   :Quality {:type :String
             :optional true
             :oneof ["Bad"
                     "Terrible"
                     "Neutral"
                     "Good"
                     "Excellent"
                     ]}
   :Price   {:type :String
             :optional true
             :oneof ["Cheap"
                     "Affordable"
                     "Neutral"
                     "Overpriced"
                     "Expensive"
                     ]}
   :Overall {:type :String
             :optional true
             :oneof ["Bad"
                     "Terrible"
                     "Neutral"
                     "Good"
                     "Excellent"
                     ]}
   :Summary {:type :String
             :optional true}})

(event
 :AnalyseReview
 {:meta {:inherits :Agentlang.Core/Inference}})

{:Agentlang.Core/Agent
 {:Name :analyzer-agent
  :Type :planner
  :Tools [:EcommerceReviewAnalysis.Core/Analysis]
  :UserInstruction "Create a new Analysis instance initialized with values based on the following review."
  :Input :AnalyseReview}}

(dataflow
 [:after :create :Review]
 {:AnalyseReview {:UserInstruction '(str "The review instance is: \n" :Instance)}}
 :Instance)
