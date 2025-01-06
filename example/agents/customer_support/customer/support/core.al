(component :Customer.Support.Core)

;; Note: the following settings should be in config.edn
;; { ; ...
;;  :inference-service-enabled true
;;  :publish-schema {:vectordb :pgvector
;;                   :config {:llm-provider "llm01"
;;                            :host #$ [PGVECTOR_DB_HOST "localhost"]
;;                            :port #$ [PGVECTOR_DB_PORT 5432]
;;                            :dbname #$ [PGVECTOR_DB_NAME "postgres"]
;;                            :user #$ [PGVECTOR_DB_USERNAME "postgres"]
;;                            :password #$ [PGVECTOR_DB_PASSWORD "postgres"]}}}

{:Agentlang.Core/LLM {:Name :llm01}}

(event
 :CallTechnicalHelp
 {:meta {:doc "UserInstruction must be set to the original user request."}
  :UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :technical-support
  :LLM :llm01
  :Input :Customer.Support.Core/CallTechnicalHelp
  :UserInstruction
  (str "You are a support agent for a Camera store. "
       "You are supposed to handle technical queries that customers ask on camera gear. "
       "Please use the documentation from the appropriate "
       "camera manufacturer to answer these queries. "
       "If you get a query on the pricing of camera gear, respond with the text: NA")}}

(event
 :CallPriceLookup
 {:meta {:doc "UserInstruction must be set to the original user request."}
  :UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :price-enquiry
  :LLM :llm01
  :Input :Customer.Support.Core/CallPriceLookup
  :UserInstruction
  (str "You are a support agent for a Camera store. "
       "Customers will raise price enquiries for camera gear. "
       "Please use the price-list from the appropriate camera "
       "manufacturer to answer the query. If you get a technical question, "
       "please respond with the simple text: NA")}}

;;;; If not provided in config, documents maybe attached to an agent as the `:Documents` attribute:
;; {
;;  :Documents
;;   [{:Title "ABC Price List"
;;     :Uri "file://./docs/abc_prices.txt"
;;     :Agent price-enquiry-agent}
;;    {:Title  "XYZ Price List"
;;     :Uri "file://./docs/xyz_prices.txt"
;;     :Agent price-enquiry-agent}]}

(event
 :ClassifyRequest
 {:meta {:doc "Returns a text, either \"technical-support\" or \"price-enquiry\""}
  :UserInstruction :String})

{:Agentlang.Core/Agent
 {:Name :request-classifier-agent
  :LLM :llm01
  :UserInstruction "Classify the request as either one of \"technical-support\" or \"price-enquiry\"."
  :Input :Customer.Support.Core/ClassifyRequest}}

{:Agentlang.Core/Agent
 {:Name :camera-support-agent
  :Type :planner
  :LLM :llm01
  :Delegates [:request-classifier-agent :technical-support :price-enquiry]
  :UserInstruction (str "1. Get the user request classified.\n"
                        "2. If the classification is \"technical-support\", call technical help. "
                        "Otherwise, call price lookup.")
  :Input :Customer.Support.Core/CameraStore}}

;; Usage:
;; POST api/Customer.Support.Core/CameraStore
;; {"Customer.Support.Core/CameraStore": {"UserInstruction": "What's the price of Panasonic G9?"}}
