(ns agentlang.test.large
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [agentlang.util :as u]
            [agentlang.util.hash :as sh]
            [agentlang.component :as cn]
            [agentlang.lang
             :as ln
             :refer [component attribute event relationship
                     entity record dataflow inference]]
            [agentlang.test.util :as tu]))

(component :ResourceAllocation.Core)

(entity
 :ResourceAllocation.Core/Config
 {:Name {:type :String :indexed true}
  :ExUnderAllocatedThres
  {:type :Double
   :check (fn [num] (and (number? num) (<= 0 num 2)))
   :default 0.6}
  :ExOverAllocatedThres
  {:type :Double
   :check (fn [num] (and (number? num) (<= 0 num 2)))
   :default 1.2}})

(entity
 :ResourceAllocation.Core/DirectoryUser
 {:dn                          {:type :String :id true}
  :cn                          :String
  :sn                          :String
  :c                           :String
  :l                           :String
  :st                          :String
  :title                       :String
  :physicalDeliveryOfficeName  :String
  :telephoneNumber             :String
  :facsimileTelephoneNumber    :String
  :givenName                   :String
  :instanceType                :Int
  :whenCreated                 :DateTime
  :whenChanged                 :DateTime
  :displayName                 :String
  :uSNCreated                  :Int
  :uSNChanged                  :Int
  :co                          :String
  :department                  :String
  :company                     :String
  :employeeNumber              :String
  :employeeType                :String
  :name                        :String
  :objectGUID                  :String
  :userAccountControl          :Int
  :badPwdCount                 :Int
  :codePage                    :Int
  :countryCode                 :Int
  :badPasswordTime             :Int
  :lastLogoff                  :Int
  :lastLogon                   :Int
  :pwdLastSet                  :Int
  :primaryGroupID              :Int
  :objectSid                   :String
  :adminCount                  :Int
  :accountExpires              :Int
  :logonCount                  :Int
  :sAMAccountName              :String
  :division                    :String
  :sAMAccountType              :Int
  :userPrincipalName           :String
  :lockoutTime                 :Int
  :ipPhone                     :String
  :objectCategory              :String
  :dSCorePropagationData       :DateTime
  :mS_DS_ConsistencyGuid       :String
  :lastLogonTimestamp          :Int
  :msDS_AuthenticatedAtDC      :String
  :msDS_SupportedEncryptionTypes :Int
  :mail                        :String
  :manager                     :String
  :mobile                      :String
  :pager                       :String
  :isManager                   :Boolean
  :workSchedule                :String
  :AssociateID                 :String
  :StartDate                   :DateTime
  :preferredNameFamilyName     :String
  :preferredNameGivenName      :String
  :CostCenterNumber            :Int
  :workerTypeCode              :String
  :costCenterName              :String
  :Gender                      :String
  :meta
  {:base "ou=Users,ou=Company-Users,dc=mydomain,dc=com"
   :objectClass "inetOrgPerson"}})

(entity
 :ResourceAllocation.Core/Team
 {:Id {:type :String :id true}
  :Name {:type :String
         :unique true}
  :Status {:oneof ["Active" "Inactive"]
           :default "Active"}})

(entity
 :ResourceAllocation.Core/Organization
 {:Id {:type :String :id true}
  :Name {:type :String
         :unique true}
  :Status {:oneof ["Active" "Inactive"]
           :default "Active"}})

(entity
 :ResourceAllocation.Core/Resource
 {:Id {:type :String :id true}
  :FirstName :String
  :LastName :String
  :FullName :String
  :Email {:type :Email :unique true}
  :PhoneNumber :String
  :Department :String
  :Role :String
  :HRLevel {:oneof ["1" "2" "3" "4" "5" "6" "7" "8" "9"]
            :default "1"}
  :Type {:oneof ["FTE" "Contractor - FT" "Contractor - PT" "Intern" "Temp" "Vendor"]
         :default "FTE"}
  :HourlyRate {:type :Int :optional true}
  :HourlyRateCurrency {:type :String :default "USD"}
  :AverageWeeklyHours {:type :Int :default 40}
  ;; Add CalculatedAnnualRate
  :Manager :String
  :Status {:oneof ["Active" "Inactive" "Terminated"]
           :default "Active"}
  :StartDate :Date
  :EndDate {:type :Date
            :optional true}
  :WorkLocation {:type :String
                 :optional true}
  :LocationCategory {:oneof ["Onsite" "Offsite" "Offshore"]
                     :default "Onsite"}})

(relationship
 :ResourceAllocation.Core/DirectoryUserResource
 {:meta {:between [:ResourceAllocation.Core/DirectoryUser :ResourceAllocation.Core/Resource]}})

(relationship
 :ResourceAllocation.Core/OrganizationResource
 {:meta {:between [:ResourceAllocation.Core/Organization :ResourceAllocation.Core/Resource]}})

(relationship
 :ResourceAllocation.Core/TeamManager
 {:meta {:between [:ResourceAllocation.Core/Team :ResourceAllocation.Core/Resource]}})

(relationship
 :ResourceAllocation.Core/TeamResource
 {:meta {:between [:ResourceAllocation.Core/Team :ResourceAllocation.Core/Resource]}})

(entity
 :ResourceAllocation.Core/Project
 {:Id {:type :String :id true}
  :Name {:type :String
         :unique true}
  :Owner :String
  :Manager :String
  :Description :String
  :Status {:oneof ["Active" "Completed" "Paused" "Proposed" "Terminated"]
           :default "Active"}
  :Type {:oneof ["Key Initiative" "RTB" "Ongoing"]}
  :Location {:type :String :optional true}
  :StartDate :Date
  :EndDate {:type :Date :optional true}
  :Cost {:type :Float :optional true}
  :CostCurrency {:type :String :default "USD"}
  :AllowOvertime {:type :Boolean :default true}})

(entity
 :ResourceAllocation.Core/Allocation
 {:Id {:type :String :id true}
  :Resource :String
  :Project :String
  :ProjectName :String
  :Period {:type :String :indexed true}
  :Duration {:oneof ["day" "week" "month" "year"]
             :default "week"}
  :AllocationEntered {:type :Double
                      :check (fn [num] (and (number? num) (<= 0 num 2)))}
  :Notes {:type :String :optional true}
  :ExOverAllocated {:type :Boolean :default false}
  :ExUnderAllocated {:type :Boolean :default false}})

(relationship
 :ResourceAllocation.Core/ResourceAllocation
 {:meta {:contains [:ResourceAllocation.Core/Resource :ResourceAllocation.Core/Allocation]}})

(record
 :ResourceAllocation.Core/AllocationInfo
 {:Allocation :String
  :Project :String
  :ProjectName :String
  :Resource :String
  :ResourceName :String
  :Period :String
  :Duration {:oneof ["day" "week" "month" "year"]
             :default "week"}
  :AllocationEntered :Double})

(defn flatten-allocs
  [xs]
  (println "flatten-allocs - xs: " xs)
  (apply concat xs))

(dataflow
 :ResourceAllocation.Core/AllocationForResource
 {:ResourceAllocation.Core/Allocation
  {:Id                 :ResourceAllocation.Core/AllocationForResource.Id
   :Resource           :ResourceAllocation.Core/AllocationForResource.Resource
   :Project            :ResourceAllocation.Core/AllocationForResource.Project
   :ProjectName        :ResourceAllocation.Core/AllocationForResource.ProjectName
   :Period             :ResourceAllocation.Core/AllocationForResource.Period
   :Duration           :ResourceAllocation.Core/AllocationForResource.Duration
   :AllocationEntered  :ResourceAllocation.Core/AllocationForResource.AllocationEntered
   :Notes              :ResourceAllocation.Core/AllocationForResource.Notes
   :ExOverAllocated    :ResourceAllocation.Core/AllocationForResource.ExOverAllocated
   :ExUnderAllocated   :ResourceAllocation.Core/AllocationForResource.ExUnderAllocated}
  :ResourceAllocation.Core/ResourceAllocation
  {:ResourceAllocation.Core/Resource
   {:Id? :ResourceAllocation.Core/AllocationForResource.Resource}}})

(dataflow
 :ResourceAllocation.Core/QueryTeamResource
 {:ResourceAllocation.Core/TeamResource? {}})

(dataflow
 :ResourceAllocation.Core/CreateResourceForTeam
 {:ResourceAllocation.Core/Resource
  {:Id :ResourceAllocation.Core/CreateResourceForTeam.Id
   :FirstName :ResourceAllocation.Core/CreateResourceForTeam.FirstName
   :LastName :ResourceAllocation.Core/CreateResourceForTeam.LastName
   :FullName :ResourceAllocation.Core/CreateResourceForTeam.FullName
   :Email :ResourceAllocation.Core/CreateResourceForTeam.Email
   :PhoneNumber :ResourceAllocation.Core/CreateResourceForTeam.PhoneNumber
   :Department :ResourceAllocation.Core/CreateResourceForTeam.Department
   :Role :ResourceAllocation.Core/CreateResourceForTeam.Role
   :HRLevel :ResourceAllocation.Core/CreateResourceForTeam.HRLevel
   :Type :ResourceAllocation.Core/CreateResourceForTeam.Type
   :HourlyRate :ResourceAllocation.Core/CreateResourceForTeam.HourlyRate
   :HourlyRateCurrency :ResourceAllocation.Core/CreateResourceForTeam.HourlyRateCurrency
   :AverageWeeklyHours :ResourceAllocation.Core/CreateResourceForTeam.AverageWeeklyHours
   :Manager :ResourceAllocation.Core/CreateResourceForTeam.Manager
   :Status :ResourceAllocation.Core/CreateResourceForTeam.Status
   :StartDate :ResourceAllocation.Core/CreateResourceForTeam.StartDate
   :WorkLocation :ResourceAllocation.Core/CreateResourceForTeam.WorkLocation
   :LocationCategory :ResourceAllocation.Core/CreateResourceForTeam.LocationCategory}
  :ResourceAllocation.Core/TeamResource {:ResourceAllocation.Core/Team {:Id? :ResourceAllocation.Core/CreateResourceForTeam.TeamId}}})

(dataflow
 :ResourceAllocation.Core/GetProjectAllocationsForResource
 {:ResourceAllocation.Core/Allocation?
  {}
  :ResourceAllocation.Core/ResourceAllocation? 
  {:ResourceAllocation.Core/Resource
   {:Id :ResourceAllocation.Core/GetProjectAllocationsForResource.ResourceId}}})

(dataflow
 :ResourceAllocation.Core/GetAllocations
 {:ResourceAllocation.Core/Allocation
  {:Period? [:between
             :ResourceAllocation.Core/GetAllocations.StartDate
             :ResourceAllocation.Core/GetAllocations.EndDate]}})


(event :ResourceAllocation.Core/GetAllAllocations {})

(dataflow
 :ResourceAllocation.Core/GetAllAllocations
 {:ResourceAllocation.Core/Allocation? {}})

(event :ResourceAllocation.Core/GetTeamResources {:TeamId :String})

(dataflow
 :ResourceAllocation.Core/GetTeamResources
 {:ResourceAllocation.Core/Resource {}
  :ResourceAllocation.Core/TeamResource?
  {:ResourceAllocation.Core/Team
   {:Id :ResourceAllocation.Core/GetTeamResources.TeamId}}})

(dataflow
 :ResourceAllocation.Core/InitUsers
 {:Agentlang.Kernel.Rbac/Role
  {:Name "user"}}
 {:Agentlang.Kernel.Rbac/Role
  {:Name "admin"}}

 {:Agentlang.Kernel.Identity/User
  {:Email "u1@email.com"}}
 {:Agentlang.Kernel.Identity/User
  {:Email "u2@email.com"}}
 {:Agentlang.Kernel.Identity/User
  {:Email "u3@email.com"}}

 {:Agentlang.Kernel.Rbac/RoleAssignment
  {:Role "admin" :Assignee "u1@email.com"}}
 {:Agentlang.Kernel.Rbac/RoleAssignment
  {:Role "user" :Assignee "u2@email.com"}}
 {:Agentlang.Kernel.Rbac/RoleAssignment
  {:Role "user" :Assignee "u3@email.com"}}

 {:Agentlang.Kernel.Rbac/Privilege
  {:Name "p1"
   :Actions [:q# [:read :create :update]]
   :Resource [:q# [:ResourceAllocation.Core/Team]]}}
 {:Agentlang.Kernel.Rbac/Privilege
  {:Name "p2"
   :Actions [:q# [:create]]
   :Resource [:q# [:ResourceAllocation.Core/Team]]}}

 {:Agentlang.Kernel.Rbac/PrivilegeAssignment
  {:Role "admin" :Privilege "p1"}}
 {:Agentlang.Kernel.Rbac/PrivilegeAssignment
  {:Role "user" :Privilege "p2"}})

(dataflow
 :ResourceAllocation.Core/TeamForResource
 {:ResourceAllocation.Core/Team
  {:Id :ResourceAllocation.Core/TeamForResource.Id
   :Name :ResourceAllocation.Core/TeamForResource.Name
   :Status :ResourceAllocation.Core/TeamForResource.Status}})

(tu/finalize-component :ResourceAllocation.Core)

(tu/invoke {:ResourceAllocation.Core/InitUsers {}})

(def with-user tu/with-user)
(def call-with-rbac tu/call-with-rbac)

(deftest resource-queries
  (testing "Testing resource creation-query"
    (tu/invoke
     {:ResourceAllocation.Core/Create_Resource
      {:Instance
       {:ResourceAllocation.Core/Resource
        {:Id "r01"
         :FirstName "John"
         :LastName "Doe"
         :FullName "John Doe"
         :Email "john.doe@email.com"
         :PhoneNumber "123-456-7890"
         :Department "Engineering"
         :Role "Software Engineer"
         :HRLevel "3"
         :Type "FTE"
         :HourlyRate 50
         :HourlyRateCurrency "USD"
         :AverageWeeklyHours 40
         :Manager "Jane Smith"
         :Status "Active"
         :StartDate "2024-01-01"
         :WorkLocation "New York"
         :LocationCategory "Onsite"}}}})

    (tu/invoke
     {:ResourceAllocation.Core/Create_Resource
      {:Instance
       {:ResourceAllocation.Core/Resource
        {:Id "r02"
         :FirstName "Sarah"
         :LastName "Connor"
         :FullName "Sarah Connor"
         :Email "sarah.connor@email.com"
         :PhoneNumber "098-765-4321"
         :Department "Marketing"
         :Role "Marketing Manager"
         :HRLevel "7"
         :Type "Contractor - FT"
         :HourlyRate 70
         :HourlyRateCurrency "EUR"
         :AverageWeeklyHours 35
         :Manager "John Smith"
         :Status "Active"
         :StartDate "2025-03-15"
         :EndDate "2025-09-15"
         :WorkLocation "San Francisco"
         :LocationCategory "Offsite"}}}})

    (let [res (tu/invoke {:ResourceAllocation.Core/LookupAll_Resource {}})
          r1  (some #(when (= "r01" (:Id %)) %) res)
          keys-r1 [:Id :FirstName :LastName :FullName :Email :PhoneNumber
                   :Department :Role :HRLevel :Type :HourlyRate
                   :HourlyRateCurrency :AverageWeeklyHours :Manager
                   :Status :StartDate :WorkLocation :LocationCategory]]
      (is (= 2 (count res)))
      (is (= {:Id                   "r01"
              :FirstName            "John"
              :LastName             "Doe"
              :FullName             "John Doe"
              :Email                "john.doe@email.com"
              :PhoneNumber          "123-456-7890"
              :Department           "Engineering"
              :Role                 "Software Engineer"
              :HRLevel              "3"
              :Type                 "FTE"
              :HourlyRate           50
              :HourlyRateCurrency   "USD"
              :AverageWeeklyHours   40
              :Manager              "Jane Smith"
              :Status               "Active"
              :StartDate            "2024-01-01"
              :WorkLocation         "New York"
              :LocationCategory     "Onsite"}
             (select-keys r1 keys-r1))))))

(deftest project-queries
  (testing "Testing project creation/query"
    (tu/invoke
     {:ResourceAllocation.Core/Create_Project
      {:Instance
       {:ResourceAllocation.Core/Project
        {:Id            "p01"
         :Name          "Project 1"
         :Description   "Our first big initiative"
         :Type          "Key Initiative"
         :Owner         "John Doe"
         :Manager       "Jane Smith"
         :AllowOvertime true
         :Cost          15000
         :CostCurrency  "USD"
         :StartDate     "2025-01-01"
         :EndDate       "2025-06-30"
         :Location      "New York"
         :Status        "Active"}}}})

    (tu/invoke
     {:ResourceAllocation.Core/Create_Project
      {:Instance
       {:ResourceAllocation.Core/Project
        {:Id            "p02"
         :Name          "Project 2"
         :Description   "Quarterly maintenance project"
         :Type          "RTB"
         :Owner         "Alice Doe"
         :Manager       "Bob Smith"
         :AllowOvertime false
         :Cost          5000
         :CostCurrency  "USD"
         :StartDate     "2025-03-15"
         :EndDate       "2025-09-01"
         :Location      "Los Angeles"
         :Status        "Proposed"}}}})

    (let [res (tu/invoke {:ResourceAllocation.Core/LookupAll_Project {}})
          p1  (some #(when (= "p01" (:Id %)) %) res)
          p2  (some #(when (= "p02" (:Id %)) %) res)

          project-keys  [:Id :Name :Description :Type :Owner :Manager :AllowOvertime
                         :Cost :CostCurrency :StartDate :EndDate :Location :Status]

          expected-p1   {:Id            "p01"
                         :Name          "Project 1"
                         :Description   "Our first big initiative"
                         :Type          "Key Initiative"
                         :Owner         "John Doe"
                         :Manager       "Jane Smith"
                         :AllowOvertime true
                         :Cost          15000.0
                         :CostCurrency  "USD"
                         :StartDate     "2025-01-01"
                         :EndDate       "2025-06-30"
                         :Location      "New York"
                         :Status        "Active"}

          expected-p2   {:Id            "p02"
                         :Name          "Project 2"
                         :Description   "Quarterly maintenance project"
                         :Type          "RTB"
                         :Owner         "Alice Doe"
                         :Manager       "Bob Smith"
                         :AllowOvertime false
                         :Cost          5000.0
                         :CostCurrency  "USD"
                         :StartDate     "2025-03-15"
                         :EndDate       "2025-09-01"
                         :Location      "Los Angeles"
                         :Status        "Proposed"}]

      (is (= 2 (count res)))
      (is (= expected-p1 (select-keys p1 project-keys)))
      (is (= expected-p2 (select-keys p2 project-keys))))))

(deftest allocation-queries
  (testing "Testing Allocation creation/query"

    (tu/invoke
     {:ResourceAllocation.Core/Create_Resource
      {:Instance
       {:ResourceAllocation.Core/Resource
        {:Id "r03"
         :FirstName "user"
         :LastName "01"
         :FullName "user 02"
         :Email "user01@acme.com"
         :PhoneNumber "111-222-3333"
         :Department "Engineering"
         :Role "Software Engineer"
         :HRLevel "3"
         :Type "FTE"
         :HourlyRate 45
         :HourlyRateCurrency "USD"
         :AverageWeeklyHours 38
         :Manager "manager01"
         :Status "Active"
         :StartDate "2025-04-10"
         :WorkLocation "San Francisco"
         :LocationCategory "Offsite"}}}})

    (tu/invoke
     {:ResourceAllocation.Core/Create_Resource
      {:Instance
       {:ResourceAllocation.Core/Resource
        {:Id "r04"
         :FirstName "user"
         :LastName "02"
         :FullName "user 02"
         :Email "user02@acme.com"
         :PhoneNumber "222-333-4444"
         :Department "Finance"
         :Role "Analyst"
         :HRLevel "2"
         :Type "FTE"
         :HourlyRate 50
         :HourlyRateCurrency "USD"
         :AverageWeeklyHours 40
         :Manager "manager02"
         :Status "Active"
         :StartDate "2024-09-01"
         :WorkLocation "New York"
         :LocationCategory "Onsite"}}}})    

      (tu/invoke
       {:ResourceAllocation.Core/AllocationForResource
        {:Id "alloc01"
         :Resource "r03"
         :Project "p01"
         :ProjectName "Project 1"
         :Period "2025-02-01"
         :Duration "week"
         :AllocationEntered  1.5
         :Notes "test 1"
         :ExOverAllocated false
         :ExUnderAllocated false}})

      (tu/invoke
       {:ResourceAllocation.Core/AllocationForResource
        {:Id "alloc02"
         :Resource "r03"
         :Project "p02"
         :ProjectName "Project 2"
         :Period "2025-04-12"
         :Duration "week"
         :AllocationEntered 2
         :Notes "test 2"
         :ExOverAllocated false
         :ExUnderAllocated false}})

      (tu/invoke
       {:ResourceAllocation.Core/AllocationForResource
        {:Id "alloc03"
         :Resource "r03"
         :Project "p01"
         :ProjectName "Project 1"
         :Period "2025-01-01"
         :Duration "week"
         :AllocationEntered 1.4
         :Notes "test 3"
         :ExOverAllocated false
         :ExUnderAllocated false}})
      
      (tu/invoke
       {:ResourceAllocation.Core/AllocationForResource
        {:Id "alloc04"
         :Resource "r04"
         :Project "p02"
         :ProjectName "Project 2"
         :Period "2025-01-01"
         :Duration "week"
         :AllocationEntered 2
         :Notes "test 4"
         :ExOverAllocated false
         :ExUnderAllocated false}})

      (let [a? (partial cn/instance-of? :ResourceAllocation.Core/Allocation)
            as? #(every? a? %)
            res0 (tu/invoke {:ResourceAllocation.Core/GetProjectAllocationsForResource
                             {:Project "p01"
                              :StartDate "2025-01-01"
                              :EndDate "2025-04-01"
                              :ResourceId "r03"}})
            res1 (tu/invoke {:ResourceAllocation.Core/GetProjectAllocationsForResource
                             {:Project "p01"
                              :StartDate "2025-01-01"
                              :EndDate "2025-04-01"
                              :ResourceId "r04"}})            
            res2 (tu/invoke {:ResourceAllocation.Core/GetAllocations
                             {:StartDate "2025-02-01"
                              :EndDate "2025-05-01"}})
            res3 (tu/invoke {:ResourceAllocation.Core/GetAllocations
                             {:StartDate "2025-01-01"
                              :EndDate "2025-03-01"}})            
            res4 (tu/invoke {:ResourceAllocation.Core/GetAllAllocations
                             {}})]
        (doseq [res [res0 res1 res2 res3 res4]] (is (as? res)))
        (is (= (count res0) 3))
        (is (= (count res1) 1))        
        (is (= (count res2) 2))
        (is (= (count res3) 3))        
        (is (= (count res4) 4)))))

#_(deftest team-allocation-queries
  (testing "Testing team allocation"

    (create-team-for-resource "0957e76d-507d-4788-a0de-a11ababd10cf"
                              "Team1"
                              "Active")

    (create-team-for-resource "ba2b0846-5227-4402-a98b-5f8850116e30"
                              "Team2"
                              "Active")

    (create-resource-for-team "836d9185-62bd-4df3-80b9-a9277c90c0f6" "user1" "One" "user1 One" "user1@example.com"
                              "111-222-3333" "Engineering" "Software Engineer" "3" "FTE" 45 "USD" 38 "manager1"
                              "Active" "2025-04-10" "San Francisco" "Offsite" "0957e76d-507d-4788-a0de-a11ababd10cf")

    (create-resource-for-team "9f838ac3-45ca-4ab8-8cbf-03f5d0c1a3f0" "user2" "Two" "user2 Two" "user2@example.com"
                              "222-333-4444" "Finance" "Financial Analyst" "2" "FTE" 50 "USD" 40 "manager2"
                              "Active" "2024-09-01" "New York" "Onsite" "ba2b0846-5227-4402-a98b-5f8850116e30")

    (create-allocation-for-resource "3bb3717e-7e80-4dcc-874d-95a59868b60e"
                                    "414afbd2-a2f5-4c44-8de7-ae4ef2227f7b"
                                    "836d9185-62bd-4df3-80b9-a9277c90c0f6"
                                    "a42d9283-3c58-49c0-9287-db56145fabc0"
                                    "Cool Initiative" "week" "2025-01-01"
                                    1.5 "Some initial notes" false false)
    
    (create-allocation-for-resource "75dfa1b3-5dc9-4f5f-adcf-744a1a6e8b15"
                                    "0d26be41-c844-4585-8dbe-1bdd956fe067"
                                    "836d9185-62bd-4df3-80b9-a9277c90c0f6"
                                    "a42d9283-3c58-49c0-9287-db56145fabc0"
                                    "Cool Initiative" "week" "2023-01-03"
                                    1.5 "Some initial notes" false false)

    (create-allocation-for-resource "5baed1e9-c16c-40ce-a330-94a760c1f056"
                                    "8b187069-080f-420d-babf-49f3fcc5051f"
                                    "9f838ac3-45ca-4ab8-8cbf-03f5d0c1a3f0"
                                    "a42d9283-3c58-49c0-9287-db56145fabc0"
                                    "Cool Initiative" "week" "2020-01-12"
                                    1.5 "Some initial notes" false false)

    (create-allocation-for-resource "c472d18c-3b78-4c44-9bc5-0a68d75125d9"
                                    "f5e10c5f-de44-4672-8584-dedcce3d891d"
                                    "9f838ac3-45ca-4ab8-8cbf-03f5d0c1a3f0"
                                    "fb68f161-cff5-4e5a-a6ae-5c39db9a6f66"
                                    "Cool Initiative" "week" "2023-01-12"
                                    1.5 "Some initial notes" false false)

    (let [res (tu/invoke {:ResourceAllocation.Core/GetTeamAllocations {:TeamId "0957e76d-507d-4788-a0de-a11ababd10cf"}})] 
      (is (= (count res) 2)))))

(deftest rbac-simple
  (testing "Simple rbac creation/reading" 
    (call-with-rbac
     (fn []
       (tu/invoke (with-user "u1@email.com"
                    {:ResourceAllocation.Core/Create_Team
                     {:Instance {:ResourceAllocation.Core/Team
                                 {:Id "52f4ff68-167f-491b-88d9-db2d4470b404"
                                  :Name "Team 1"
                                  :Status "Active"}}}}))

       (tu/invoke (with-user "u2@email.com"
                    {:ResourceAllocation.Core/Create_Team
                     {:Instance {:ResourceAllocation.Core/Team
                                 {:Id "903b5917-2c07-467f-995f-0b9cd23bfdc6"
                                  :Name "Team 2"
                                  :Status "Active"}}}}))

       (let [res-admin (tu/invoke (with-user
                                    "u1@email.com"
                                    {:ResourceAllocation.Core/LookupAll_Team {}}))
             res-user (tu/invoke (with-user
                                   "u2@email.com"
                                   {:ResourceAllocation.Core/LookupAll_Team {}}))
             t1 (first (filter #(= (:Id %) "903b5917-2c07-467f-995f-0b9cd23bfdc6") res-user))]
         (is (= 2 (count res-admin)))
         (is (= 1 (count res-user)))
         (is (= "Team 2" (:Name t1)))
         (is (= "Active" (:Status t1))))))))


;; (deftest rbac-updates
;;   (testing "rbac update"
;;     (call-with-rbac
;;      (fn []
;;        (tu/invoke (with-user "u2@email.com"
;;                        {:ResourceAllocation.Core/Create_Team
;;                         {:Instance {:ResourceAllocation.Core/Team
;;                                     {:Id "6fe902e3-1499-4ccb-bcee-d8d6ec61edb8"
;;                                      :Name "Team 2"
;;                                      :Status "Active"}}}}))
;;        (tu/invoke (with-user "u2@email.com"
;;                        {:ResourceAllocation.Core/Update_Team
;;                         {:Id "6fe902e3-1499-4ccb-bcee-d8d6ec61edb8"
;;                          :Data {:Status "Inactive"}}}))))))
