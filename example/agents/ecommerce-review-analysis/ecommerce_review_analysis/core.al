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

{:Agentlang.Core/LLM
 {:Type "openai"
  :Name :llm01
  :Config {:ApiKey (agentlang.util/getenv "OPENAI_API_KEY")
           :EmbeddingApiEndpoint "https://api.openai.com/v1/embeddings"
           :EmbeddingModel "text-embedding-3-small"
           :CompletionApiEndpoint "https://api.openai.com/v1/chat/completions"
           :CompletionModel "gpt-3.5-turbo"}}}

{:Agentlang.Core/Agent
 {:Name :analyzer-agent
  :LLM :llm01
  :UserInstruction "Find review analysis for the given review"
  :Input :EcommerceReviewAnalysis/AnalyseReview}}

(dataflow [:after :create :EcommerceReviewAnalysis.Core/Review]
  {:EcommerceReviewAnalysis/AnalyseReview :Instance
   :as :A}
  {:EcommerceReviewAnalysis.Core/Analysis :A})

