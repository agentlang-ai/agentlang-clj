#_(do (ns agentlang.test.graphql
  (:require [clojure.test :refer :all]
            [agentlang.util :as fu]
            [agentlang.util.errors :refer :all]
            [agentlang.test.util :as test-util]
            [clojure.test :refer [deftest is testing]]
            [agentlang.test.util :as tu :refer [defcomponent]]
            [agentlang.api :as api]
            [agentlang.evaluator :as e]
            [com.walmartlabs.lacinia :refer [execute]]
            [agentlang.component :as cn]
            [agentlang.graphql.generator :as gg]
            [agentlang.graphql.core :as graphql]
            [agentlang.lang
             :as ln
             :refer [component attribute event entity record dataflow relationship]]
            [clojure.walk :as walk]
            [clojure.string :as str]
            [java-time :as t])
  (:import (clojure.lang IPersistentMap)))

(defn simplify
  "Converts all ordered maps nested within the map into standard hash maps, and
   sequences into vectors, which makes for easier constants in the tests, and eliminates ordering problems."
  [m]
  (walk/postwalk
    (fn [node]
      (cond
        (instance? IPersistentMap node)
        (into {} node)

        (seq? node)
        (vec node)

        :else
        node))
    m))

(defn graphql-handler
  ([component-name query variables]
   (let [schema (cn/schema-info component-name)
         contains-graph-map (gg/generate-contains-graph schema)
         [uninjected-graphql-schema injected-graphql-schema entity-metadatas] (graphql/compile-graphql-schema schema contains-graph-map)]
    (let [context {:auth-config nil :core-component component-name :contains-graph contains-graph-map :entity-metas entity-metadatas}
          result (simplify (execute injected-graphql-schema query variables context))]
       (:data result))))
  ([component-name query]
   (graphql-handler component-name query nil)))

(defn filter-event-attrs [event]
  "Removes internal attrs from event."
    (dissoc (agentlang.component/instance-user-attributes event) :EventContext :__path__ :__parent__))

(defn transform-address [address]
  (if (and (map? address)
           (contains? address :type-*-tag-*-)
           (contains? address :-*-type-*-))
    (let [address-data (dissoc address :type-*-tag-*- :-*-type-*-)]
      {:WordCount.Core/Address address-data})
    address))

(defn transform-user-data [user-data]
  (let [transformed (update user-data :Addresses
                            (fn [addresses]
                              (when (seq addresses)
                                (mapv transform-address addresses))))]
    (if (empty? (:Addresses transformed))
      (dissoc transformed :Addresses)
      transformed)))

(defn build-sample-app []
  (cn/remove-component :GraphQL.Test)
  (api/component :GraphQL.Test)
  (api/entity
    :GraphQL.Test/F
    {:Id :Identity
     :Name {:type :String :id true}
     :Y :Int})
  (tu/finalize-component :GraphQL.Test))

(deftest test-create-user-mutation
  (build-sample-app)
  (let [query (str "mutation {
                    CreateF(
                        input: {
                            Name: \"hasnain\"
                            Id: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\"
                            Y: 1
                        }
                    ) {
                        Name
                        Id
                        Y
                    }
                }")
        expected {:Name "hasnain"
                  :Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Y 1}
        results (graphql-handler :GraphQL.Test query)]
   (is (= expected (:CreateF results)))))

(defn build-word-count-app []
  (cn/remove-component :WordCount.Core)
  (defcomponent :WordCount.Core

    (record :WordCount.Core/Address
            {:City :String
             :Zip :String
             :StreetNumber :Int
             :MoreDetails {:type :String :optional true}})

    (entity :WordCount.Core/User
            {:Id :Identity
             :Email {:type :Email}
             :Name :String
             :MemberSince {:type :Date :optional true}
             :Addresses {:listof :WordCount.Core/Address :optional true}})

    (entity :WordCount.Core/Hero
           {:Id {:type :Int :guid true}
            :Name :String
            :HomePlanet :String
            :Age :Int
            :ForceSensitive :Boolean})

    (entity :WordCount.Core/Order
        {:Id        :Int
         :Details   :String
         :CreatedAt :DateTime})

    (entity :WordCount.Core/Profile
            {:Id :Identity
             :Name :String
             :Bio :String})

    (entity :WordCount.Core/Document
            {:Id :Identity
             :Name {:type :String :id true}
             :Content :String
             :Summary :String
             :LastUpdated {:type :DateTime :optional true}})

    (entity :WordCount.Core/Tag
            {:Id :Identity
             :Name :String})

    (entity :WordCount.Core/Page
            {:Id :Identity
             :Name :String})

    (entity :WordCount.Core/Index
            {:Id :Identity
             :Name :String})

   (entity :WordCount.Core/Tasks
            {:Id :Identity
             :Title :String
             :Completed :Boolean})

    (record :WordCount.Core/SubSubAttribute {:name :String :value :String})

    (record :WordCount.Core/SubAttribute {:name :String :value :String :SubSubAttribute :WordCount.Core/SubSubAttribute})

    (record :WordCount.Core/Attribute {:name :String :value :String :SubAttribute :WordCount.Core/SubAttribute})

    (entity
     :WordCount.Core/Customer
     {:Id {:type :Int :guid true}
      :Name :String
      :ListOfNames {:listof :String}
      :FavoriteIntNumbers {:listof :Int :optional true}
      :FavoriteFloatNumbers {:listof :Float :optional true}
      :Attributes {:listof :WordCount.Core/Attribute}})

    (record :WordCount.Core/DetailValue {:key :String :data :String})

    (record :WordCount.Core/NestedDetail {:key :String :data :String :DetailValue :WordCount.Core/DetailValue})

    (record :WordCount.Core/ProfileDetail {:key :String :data :String :NestedDetail :WordCount.Core/NestedDetail})

    (entity
     :WordCount.Core/UserProfileDetails
     {:UserId {:type :Int :guid true}
      :DisplayName :String
      :ProfileDetails :WordCount.Core/ProfileDetail})

    ;; RELATIONSHIPS

    (relationship :WordCount.Core/UserDocument
                  {:meta {:contains [:WordCount.Core/User :WordCount.Core/Document]}})

    (relationship :WordCount.Core/UserProfile
                  {:meta {:contains [:WordCount.Core/User :WordCount.Core/Profile]}})

    (relationship :WordCount.Core/DocumentPage
                  {:meta {:contains [:WordCount.Core/Document :WordCount.Core/Page]}})

    (relationship :WordCount.Core/DocumentIndex
                  {:meta {:contains [:WordCount.Core/Document :WordCount.Core/Index]}})

    (relationship :WordCount.Core/UserTag
                  {:meta {:between [:WordCount.Core/User :WordCount.Core/Tag]}
                   :Awesome {:type :String}})

    (relationship :WordCount.Core/DocumentTag
                  {:meta {:between [:WordCount.Core/Document :WordCount.Core/Tag]}
                   :MetaDetails :String})

    (relationship :WordCount.Core/DocumentDocuments
                  {:meta {:between [:WordCount.Core/Document :WordCount.Core/Document :as [:Document1 :Document2]]}
                   :Details {:type :String :optional true}})

    (dataflow
     :WordCount.Core/LookupTags
     {:WordCount.Core/DocumentTag
      {:Document? :WordCount.Core/LookupTags.Document
       :Tag? :WordCount.Core/LookupTags.Tag}})))

(defn compare-instance-maps [subset superset id-attr]
    "Checks if all maps in 'subset' exist in 'superset' with exact equality on all nested keys and values.
    Uniquely identifies instances using given Id attribute."
  (let [subset-map (into {} (map (juxt id-attr identity) subset))
        matching-pairs (for [super-item superset
                             :let [subset-item (get subset-map (id-attr super-item))]
                             :when subset-item]
                         [subset-item super-item])]
    (if (= (count matching-pairs) (count subset))
      (every? (fn [[subset-item super-item]] (= subset-item super-item)) matching-pairs)
      false)))

(deftest test-queries-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "Sample Document"
                       :Content "This is a sample document content."
                       :Summary "Summary of the document."}

        user-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "user17@example.com"
                   :Name "John Doe"}

        heroes [{:Id 1 :Name "Luke Skywalker" :HomePlanet "Tatooine" :Age 23 :ForceSensitive true}
               {:Id 2 :Name "Leia Organa" :HomePlanet "Alderaan" :Age 23 :ForceSensitive true}
               {:Id 3 :Name "Han Solo" :HomePlanet "Corellia" :Age 32 :ForceSensitive false}
               {:Id 4 :Name "Chewbacca" :HomePlanet "Kashyyyk" :Age 200 :ForceSensitive false}]

        parent-user-data {:Email "user17@example.com"
                         :Name "John Doe"}

        tag-data {:Id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "Nice"}

        query-by-id-pattern "query {
                               User(attributes: {Id: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\"}) {
                                   Id
                                   Email
                                   Name
                               }
                             }"

        query-by-email-pattern "query {
                                   User(attributes: {Email: \"user17@example.com\"}) {
                                       Email
                                       Name
                                   }
                                 }"

        query-by-name-pattern "query {
                                  User(attributes: {Name: \"John Doe\"}) {
                                      Email
                                      Name
                                  }
                                }"

        parent-user-email "user17@example.com"
        child-document-name "Sample Document"
        parent-user-id "0e977860-5cd4-4bc3-8323-f4f71a66de6d"
        child-document-id "0e977860-5cd4-4bc3-8323-f4f71a66de6e"

        query-all-docs-for-user-pattern (str "query {
                                                User(attributes: { Email: \"" parent-user-email "\" }) {
                                                    Email
                                                    UserDocument {
                                                        Document {
                                                            Name
                                                            Content
                                                            Summary
                                                            LastUpdated
                                                        }
                                                    }
                                                }
                                            }")

        query-by-email-and-doc-id-pattern (str "query {
                                                    User(attributes: { Email: \"" parent-user-email "\" }) {
                                                        Email
                                                        Name
                                                        UserDocument {
                                                            Document(attributes: { Id: \"" child-document-id "\" }) {
                                                                Name
                                                                Content
                                                                Summary
                                                                LastUpdated
                                                            }
                                                        }
                                                    }
                                                }")

        query-by-user-id-and-doc-name-pattern (str "query {
                                                  User(attributes: { Id: \"" parent-user-id "\" }) {
                                                      Name
                                                      UserDocument {
                                                          Document(attributes: { Name: \"" child-document-name "\" }) {
                                                              Name
                                                              Content
                                                              Summary
                                                              LastUpdated
                                                          }
                                                      }
                                                  }
                                              }")

        query-tag-by-all-attributes "query {
                                        UserTag(attributes: {
                                            User: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Tag: \"0e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Awesome: \"Nice\"
                                        }) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"

        query-tag-by-one-attribute "query {
                                        UserTag(attributes: {Awesome: \"Nice\"}) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"
        ]

    ;; CREATE AND QUERY PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Query Parent by GUID"
        (let [results (graphql-handler :WordCount.Core query-by-id-pattern)
              result-data (first (:User results))]
          (is (= user-data result-data))))

    (testing "Query Parent by ID"
      (let [results (graphql-handler :WordCount.Core query-by-email-pattern)
            result-data (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    (testing "Query Parent by Non-ID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-name-pattern)
            result-data  (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

(testing "Multi-Condition Filter Query to Test Booleans and String Filters"
  (let [tasks [{:Title "Complete project report Task" :Completed false}
               {:Title "Attend team meeting" :Completed true}
               {:Title "Review code changes Task" :Completed false}
               {:Title "Update project timeline" :Completed true}
               {:Title "Prepare for client presentation Task" :Completed false}
               {:Title "Cancelled meeting" :Completed true}
               {:Title "Submit expense report Task" :Completed false}
               {:Title "Plan new project phases" :Completed true}
               {:Title "Follow up with stakeholders Task" :Completed false}
               {:Title "Conduct code review meeting" :Completed true}]
        multi-condition-query "query getMultiConditionFilteredTasks($filter: TasksFilter) {
                                  Tasks (filter: $filter) {
                                    Title
                                    Completed
                                  }
                                }"
        multi-condition-variables
        {:filter
         {:and [
                {:Completed {:eq false}}
                {:or [
                      {:Title {:contains "project"}}
                      {:Title {:contains "Task"}}
                     ]}
                {:not {:Title {:eq "Cancelled meeting"}}}
                ]}}]

  ;; Create task instances
  (mapv
    (fn [task]
      (tu/fresult
        (e/eval-all-dataflows
          (cn/make-instance
            :WordCount.Core/Create_Tasks
            {:Instance
             (cn/make-instance :WordCount.Core/Tasks task)}))))
    tasks)

  (let [results (graphql-handler :WordCount.Core multi-condition-query multi-condition-variables)
        results (:Tasks results)
        expected-results (filter
                           (fn [task]
                             (and
                               (not (:Completed task))
                               (or (clojure.string/includes? (:Title task) "project")
                                   (clojure.string/includes? (:Title task) "Task"))
                               (not= (:Title task) "Cancelled meeting")))
                           tasks)]
    (is (= (set results) (set expected-results))
        (str "Mismatch in filtered results. Expected: " expected-results ", Got: " results)))))

    (testing "Multi-Condition Filter Query"
      (let [multi-condition-query "query getMultiConditionFilteredHeros($filter: HeroFilter) {
                                      Hero (filter: $filter) {
                                        Id
                                        Name
                                        HomePlanet
                                        Age
                                        ForceSensitive
                                      }
                                    }"
            multi-condition-variables
            {:filter
             {:and [
                    {:or [
                          {:Age {:gte 30 :lt 100}}
                          {:and [
                                 {:Age {:gte 20 :lt 30}}
                                 {:ForceSensitive {:eq true}}
                                 ]}
                          ]}
                    {:not {:HomePlanet {:eq "Kashyyyk"}}}
                    {:or [
                          {:Name {:contains "a"}}
                          {:HomePlanet {:in ["Tatooine" "Alderaan"]}}
                          ]}
                    ]}}]

      (mapv
        (fn [hero]
          (tu/fresult
            (e/eval-all-dataflows
              (cn/make-instance
                :WordCount.Core/Create_Hero
                {:Instance
                 (cn/make-instance :WordCount.Core/Hero hero)}))))
        heroes)
      (let [results (graphql-handler :WordCount.Core multi-condition-query multi-condition-variables)
            results (:Hero results)]
        (doseq [[result expected] (map vector results heroes)]
          (is (= result expected) (str "Mismatch for hero " (:Name expected)))))))

    (testing "Filtered Heroes Query"
      (let [filtered-heroes-query "query getFilteredHeroes($filter: HeroFilter, $limit: Int, $offset: Int) {
                                     Hero(filter: $filter, limit: $limit, offset: $offset) {
                                       Id Name HomePlanet Age ForceSensitive
                                     }
                                   }"
            filtered-heroes-variables {:filter {:and [{:Name {:startsWith "L"}}
                                                      {:HomePlanet {:contains "oo"}}
                                                      {:Age {:between [20 30]}}
                                                      {:ForceSensitive {:eq true}}]}
                                       :limit  2
                                       :offset 0}
            results (graphql-handler :WordCount.Core filtered-heroes-query filtered-heroes-variables)
            results (:Hero results)]

        (is (= 1 (count results)) "Filtered heroes query returned wrong number of results")
          (is (= "Luke Skywalker" (:Name (first results))) "Filtered heroes query returned wrong hero")))

    (testing "Complex Filtered Heroes Query"
      (let [complex-filter-query "query getComplexFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            complex-filter-variables {:filter {:or [{:and [{:HomePlanet {:endsWith "ne"}}
                                                           {:ForceSensitive {:eq true}}]}
                                                    {:and [{:Name {:contains "a"}}
                                                           {:Age {:gt 30}}]}
                                                    {:and [{:Age {:gte 100}}
                                                           {:ForceSensitive {:eq false}}]}]}}]

        (let [results (graphql-handler :WordCount.Core complex-filter-query complex-filter-variables)
              results (:Hero results)]
          (is (= 3 (count results)) "Complex filtered heroes query returned wrong number of results")
          (is (some #(= "Chewbacca" (:Name %)) results) "Complex filtered heroes query should include Chewbacca")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Complex filtered heroes query should include Luke Skywalker"))))

    (testing "Deep Nested Filtered Heroes Query"
      (let [deep-nested-filter-query "query getDeepNestedFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            deep-nested-filter-variables {:filter
                                          {:and [
                                                 {:or [
                                                       {:and [
                                                              {:not {:Age {:lt 23}}}
                                                              {:HomePlanet {:eq "Tatooine"}}
                                                              ]}
                                                       {:and [
                                                              {:ForceSensitive {:eq true}}
                                                              {:not {:or [
                                                                          {:Name {:contains "Solo"}}
                                                                          {:Name {:contains "Chewbacca"}}
                                                                          ]}}
                                                              ]}
                                                       ]}
                                                 {:or [
                                                       {:not {:and [
                                                                    {:Age {:gte 30}}
                                                                    {:Age {:lte 100}}
                                                                    ]}}
                                                       {:HomePlanet {:in ["Alderaan" "Tatooine"]}}
                                                       ]}
                                                 ]}}]

        (let [results (graphql-handler :WordCount.Core deep-nested-filter-query deep-nested-filter-variables)
              results (:Hero results)]
          (is (= 2 (count results)) "Deep nested filtered heroes query returned wrong number of results")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Deep nested filtered heroes query should include Luke Skywalker")
          (is (some #(= "Leia Organa" (:Name %)) results) "Deep nested filtered heroes query should include Leia Organa"))))

    (testing "Very Complex Filtered Heroes Query"
      (let [very-complex-filter-query "query getVeryComplexFilteredHeros($filter: HeroFilter) {
              Hero(filter: $filter) {
                Id Name HomePlanet Age ForceSensitive
              }
            }"
            very-complex-filter-variables
            {:filter
             {:or [
                   {:and [
                          {:not {:or [
                                      {:Age {:lt 20}}
                                      {:Age {:gt 50}}
                                      ]}}
                          {:HomePlanet {:in ["Tatooine" "Alderaan" "Corellia"]}}
                          {:or [
                                {:Name {:startsWith "L"}}
                                {:Name {:endsWith "lo"}}
                                {:not {:Name {:contains "Chew"}}}
                                ]}
                          ]}
                   {:not {:and [
                                {:ForceSensitive {:eq false}}
                                {:or [
                                      {:Age {:between [30 50]}}
                                      {:not {:HomePlanet {:in ["Kashyyyk" "Tatooine"]}}}
                                      ]}
                                ]}}
                   {:and [
                          {:Age {:gt 100}}
                          {:not {:or [
                                      {:Name {:startsWith "L"}}
                                      {:Name {:endsWith "a"}}
                                      ]}}
                          {:or [
                                {:HomePlanet {:eq "Kashyyyk"}}
                                {:ForceSensitive {:eq false}}
                                ]}
                          ]}
                   ]}}]

        (let [results (graphql-handler :WordCount.Core very-complex-filter-query very-complex-filter-variables)
              results (:Hero results)]
          (is (= 4 (count results)) "Very complex filtered heroes query returned wrong number of results")
          (is (some #(= "Luke Skywalker" (:Name %)) results) "Very complex filtered heroes query should include Luke Skywalker")
          (is (some #(= "Leia Organa" (:Name %)) results) "Very complex filtered heroes query should include Leia Organa")
          (is (some #(= "Han Solo" (:Name %)) results) "Very complex filtered heroes query should include Han Solo")
          (is (some #(= "Chewbacca" (:Name %)) results) "Very complex filtered heroes query should include Chewbacca"))))

    ;; CREATE AND QUERY CHILD
    (testing "Manually create instances of parent and child entities"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                           (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Query All Documents for User"
      (let [results (graphql-handler :WordCount.Core query-all-docs-for-user-pattern)]
        (is (not-empty (get-in results [:User 0 :UserDocument])))))

    (testing "Query Child by GUID Attribute"
        (let [results (graphql-handler :WordCount.Core query-by-email-and-doc-id-pattern)]
          (is (= child-document-name (get-in results [:User 0 :UserDocument 0 :Document 0 :Name])))))

    (testing "Query Child by Non-GUID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-user-id-and-doc-name-pattern)
            user-documents (get-in results [:User 0 :UserDocument])]
        (is (= "Sample Document" (get-in user-documents [0 :Document 0 :Name])))))

     ;; CREATE AND QUERY BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))
            tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Query between instance by all attributes"
      (let [results (graphql-handler :WordCount.Core query-tag-by-all-attributes)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by one attribute"
      (let [results (graphql-handler :WordCount.Core query-tag-by-one-attribute)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Date and DateTime Comparison Operators"
      (let [base-date "2024-07-17T10:00"
            orders (vec (for [i (range 1 11)]
                          (let [date-time (-> (java.time.LocalDateTime/parse base-date)
                                              (.plusDays (dec i))
                                              (.truncatedTo java.time.temporal.ChronoUnit/MINUTES)
                                              .toString)
                                order (cn/make-instance
                                        :WordCount.Core/Order
                                        {:Id        i
                                         :Details   (str "Order #" i)
                                         :CreatedAt date-time})]
                            (first (tu/fresult
                                     (e/eval-all-dataflows
                                       (cn/make-instance
                                         :WordCount.Core/Create_Order
                                         {:Instance order})))))))
            query-template "query getOrders($dateFilter: String!) {
                              Order(filter: {
                                CreatedAt: {
                                  %s: $dateFilter
                                }
                              }) {
                                Id
                                Details
                                CreatedAt
                              }
                            }"]

        (testing "Less than or equal (lte) operator"
          (let [lte-query (format query-template "lte")
                lte-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core lte-query lte-variables))]
            (is (= 5 (count results)))
            (is (every? #(<= (:Id %) 5) results))))

        (testing "Less than (lt) operator"
          (let [lt-query (format query-template "lt")
                lt-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core lt-query lt-variables))]
            (is (= 4 (count results)))
            (is (every? #(< (:Id %) 5) results))))

        (testing "Greater than or equal (gte) operator"
          (let [gte-query (format query-template "gte")
                gte-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core gte-query gte-variables))]
            (is (= 6 (count results)))
            (is (every? #(>= (:Id %) 5) results))))

        (testing "Greater than (gt) operator"
          (let [gt-query (format query-template "gt")
                gt-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core gt-query gt-variables))]
            (is (= 5 (count results)))
            (is (every? #(> (:Id %) 5) results))))

        (testing "Edge case: Exact date match"
          (let [eq-query (format query-template "eq")
                eq-variables {:dateFilter "2024-07-21T10:00"}
                results (:Order (graphql-handler :WordCount.Core eq-query eq-variables))]
            (is (= 1 (count results)))
            (is (= 5 (:Id (first results))))))

        (testing "No results case"
          (let [gt-query (format query-template "gt")
                gt-variables {:dateFilter "2024-07-27T10:00"}
                results (:Order (graphql-handler :WordCount.Core gt-query gt-variables))]
            (is (empty? results))))

        (testing "All results case"
          (let [gte-query (format query-template "gte")
                gte-variables {:dateFilter "2024-07-17T10:00"}
                results (:Order (graphql-handler :WordCount.Core gte-query gte-variables))]
            (is (= 10 (count results)))
            (is (= (set (range 1 11)) (set (map :Id results))))))))

    (testing "Query deeply nested records"
      (let [customer-data-1 {:Id          10000,
                             :Name        "Muhammad Hasnain Naeem",
                             :ListOfNames ["Name1" "Name2" "Name3"],
                             :Attributes
                             [{:WordCount.Core/Attribute {:name  "Personal",
                                                          :value "Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Details",
                                                                                         :value           "More Info",
                                                                                         :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Age", :value "30"}}}}}}
                              {:WordCount.Core/Attribute {:name  "Professional",
                                                          :value "Work Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Job",
                                                                                         :value           "Details",
                                                                                         :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Occupation", :value "Software Engineer"}}}}}}]}

            customer-data-2 {:Id          3001,
                             :Name        "Hasnain Naeem",
                             :ListOfNames ["Alice" "Bob" "Charlie"],
                             :Attributes
                             [{:WordCount.Core/Attribute {:name  "Personal",
                                                          :value "Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Details",
                                                                                         :value           "More Info",
                                                                                         :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Age", :value "30"}}}}}}
                              {:WordCount.Core/Attribute {:name  "Professional",
                                                          :value "Work Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Job",
                                                                                         :value           "Details",
                                                                                         :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Occupation", :value "Engineer"}}}}}}]}


            graphql-customer-data-1 {:Id          10000,
                                     :Name        "Muhammad Hasnain Naeem",
                                     :ListOfNames ["Name1" "Name2" "Name3"],
                                     :Attributes
                                     [{:name  "Personal",
                                       :value "Info",
                                       :SubAttribute
                                       {:name            "Details",
                                        :value           "More Info",
                                        :SubSubAttribute {:name "Age", :value "30"}}}
                                      {:name  "Professional",
                                       :value "Work Info",
                                       :SubAttribute
                                       {:name            "Job",
                                        :value           "Details",
                                        :SubSubAttribute {:name "Occupation", :value "Software Engineer"}}}]}
            graphql-customer-data-2 {:Id          3001,
                                     :Name        "Hasnain Naeem",
                                     :ListOfNames ["Alice" "Bob" "Charlie"],
                                     :Attributes
                                     [{:name  "Personal",
                                       :value "Info",
                                       :SubAttribute
                                       {:name            "Details",
                                        :value           "More Info",
                                        :SubSubAttribute {:name "Age", :value "30"}}}
                                      {:name  "Professional",
                                       :value "Work Info",
                                       :SubAttribute
                                       {:name            "Job",
                                        :value           "Details",
                                        :SubSubAttribute {:name "Occupation", :value "Engineer"}}}]}

            instance1 (e/eval-all-dataflows
                        (cn/make-instance
                          :WordCount.Core/Create_Customer
                          {:Instance
                           (cn/make-instance :WordCount.Core/Customer customer-data-1)}))
            instance2 (e/eval-all-dataflows
                        (cn/make-instance
                          :WordCount.Core/Create_Customer
                          {:Instance
                           (cn/make-instance :WordCount.Core/Customer customer-data-2)}))

            fetch-all-customers-query "query Customer {
                                            Customer {
                                                Id
                                                ListOfNames
                                                Name
                                                Attributes {
                                                    SubAttribute {
                                                        name
                                                        value
                                                        SubSubAttribute {
                                                            name
                                                            value
                                                        }
                                                    }
                                                    name
                                                    value
                                                }
                                            }
                                        }"
            results (graphql-handler :WordCount.Core fetch-all-customers-query)
            customers (get results :Customer)]
        (let [subset [graphql-customer-data-1 graphql-customer-data-2]
              superset customers]
          (compare-instance-maps subset superset :Id))))

    (testing "Filters for String and Integer list attributes: contains, containsAll, containsAny, isEmpty"
      (let [customer-data-1 {:Id          10001,
                             :Name        "Muhammad Hasnain Naeem",
                             :ListOfNames ["Alice" "Bob" "Charlie"],
                             :FavoriteIntNumbers [1 2 3 4 5]
                             :Attributes
                             [{:WordCount.Core/Attribute {:name  "Personal",
                                                          :value "Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Details",
                                                                                         :value           "More Info",
                                                                                         :SubSubAttribute
                                                                                         {:WordCount.Core/SubSubAttribute
                                                                                          {:name "Age",
                                                                                           :value "30"}}}}}}
                              {:WordCount.Core/Attribute {:name  "Professional",
                                                          :value "Work Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Job",
                                                                                         :value           "Details",
                                                                                         :SubSubAttribute
                                                                                         {:WordCount.Core/SubSubAttribute
                                                                                          {:name "Occupation",
                                                                                           :value "Software Engineer"}}}}}}]}

            customer-data-2 {:Id          1000000,
                             :Name        "Hasnain Naeem",
                             :ListOfNames ["David" "Eve" "Frank"],
                             :FavoriteIntNumbers [3 4 5 6 7],
                             :Attributes
                             [{:WordCount.Core/Attribute {:name  "Personal",
                                                          :value "Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Details",
                                                                                         :value           "More Info",
                                                                                         :SubSubAttribute
                                                                                         {:WordCount.Core/SubSubAttribute
                                                                                          {:name "Age",
                                                                                           :value "25"}}}}}}
                              {:WordCount.Core/Attribute {:name  "Professional",
                                                          :value "Work Info",
                                                          :SubAttribute
                                                          {:WordCount.Core/SubAttribute {:name            "Job",
                                                                                         :value           "Details",
                                                                                         :SubSubAttribute
                                                                                         {:WordCount.Core/SubSubAttribute
                                                                                          {:name "Occupation",
                                                                                           :value "Engineer"}}}}}}]}


            graphql-customer-data-1 {:Id          10001,
                                     :Name        "Muhammad Hasnain Naeem",
                                     :ListOfNames ["Alice" "Bob" "Charlie"],
                                     :FavoriteIntNumbers [1 2 3 4 5],
                                     :Attributes
                                     [{:name  "Personal",
                                       :value "Info",
                                       :SubAttribute
                                       {:name            "Details",
                                        :value           "More Info",
                                        :SubSubAttribute {:name "Age", :value "30"}}}
                                      {:name  "Professional",
                                       :value "Work Info",
                                       :SubAttribute
                                       {:name            "Job",
                                        :value           "Details",
                                        :SubSubAttribute {:name "Occupation", :value "Software Engineer"}}}]}

            graphql-customer-data-2 {:Id          1000000,
                                     :Name        "Hasnain Naeem",
                                     :ListOfNames ["David" "Eve" "Frank"],
                                     :FavoriteIntNumbers [3 4 5 6 7],
                                     :Attributes
                                     [{:name  "Personal",
                                       :value "Info",
                                       :SubAttribute
                                       {:name            "Details",
                                        :value           "More Info",
                                        :SubSubAttribute {:name "Age", :value "25"}}}
                                      {:name  "Professional",
                                       :value "Work Info",
                                       :SubAttribute
                                       {:name            "Job",
                                        :value           "Details",
                                        :SubSubAttribute {:name "Occupation", :value "Engineer"}}}]}

            instance1 (e/eval-all-dataflows
                        (cn/make-instance
                          :WordCount.Core/Create_Customer
                          {:Instance
                           (cn/make-instance :WordCount.Core/Customer customer-data-1)}))
            instance2 (e/eval-all-dataflows
                        (cn/make-instance
                          :WordCount.Core/Create_Customer
                          {:Instance
                           (cn/make-instance :WordCount.Core/Customer customer-data-2)}))

            int-list-query "query Customer {
                              Customer(filter: {
                                or: [
                                  {and: [
                                    {Id: {gte: 10000}},
                                    {Id: {lte: 100000}},
                                    {FavoriteIntNumbers: {contains: 3}},
                                    {FavoriteIntNumbers: {containsAll: [4, 5]}},
                                    {FavoriteIntNumbers: {containsAny: [1, 3, 7]}},
                                    {FavoriteIntNumbers: {isEmpty: false}}
                                  ]}
                                ]
                              }) {
                                Id
                                Name
                                ListOfNames
                                FavoriteIntNumbers
                                Attributes {
                                  name
                                  value
                                  SubAttribute {
                                    name
                                    value
                                    SubSubAttribute {
                                      name
                                      value
                                    }
                                  }
                                }
                              }
                            }"

            string-list-query "query Customer {
                                  Customer(filter: {
                                    or: [
                                      {and: [
                                        {Id: {gte: 1000000}},
                                        {ListOfNames: {contains: \"David\"}},
                                        {ListOfNames: {containsAll: [\"Eve\", \"Frank\"]}},
                                        {ListOfNames: {containsAny: [\"Alice\", \"David\"]}},
                                        {ListOfNames: {isEmpty: false}}
                                      ]}
                                    ]
                                  }) {
                                    Id
                                    Name
                                    ListOfNames
                                    FavoriteIntNumbers
                                    Attributes {
                                      name
                                      value
                                      SubAttribute {
                                        name
                                        value
                                        SubSubAttribute {
                                          name
                                          value
                                        }
                                      }
                                    }
                                  }
                                }"

            no_result_int_list_query "query Customer {
                                    Customer(filter: {
                                      and: [
                                        {Id: {gte: 10000}},
                                        {Id: {lte: 100000}},
                                        {FavoriteIntNumbers: {contains: 10}},
                                        {FavoriteIntNumbers: {containsAll: [11, 12]}},
                                        {FavoriteIntNumbers: {containsAny: [13, 14, 15]}},
                                        {FavoriteIntNumbers: {isEmpty: false}}
                                      ]
                                    }) {
                                      Id
                                      Name
                                      ListOfNames
                                      FavoriteIntNumbers
                                    }
                                  }"

            no_result_string_list_query "query Customer {
                                       Customer(filter: {
                                         and: [
                                           {Id: {gte: 1000000}},
                                           {ListOfNames: {contains: \"Zack\"}},
                                           {ListOfNames: {containsAll: [\"Yvonne\", \"Xavier\"]}},
                                           {ListOfNames: {containsAny: [\"Walter\", \"Victor\"]}},
                                           {ListOfNames: {isEmpty: false}}
                                         ]
                                       }) {
                                         Id
                                         Name
                                         ListOfNames
                                         FavoriteIntNumbers
                                       }
                                     }"


            int-list-results (graphql-handler :WordCount.Core int-list-query)
            string-list-results (graphql-handler :WordCount.Core string-list-query)
            int-list-customers (get int-list-results :Customer)
            string-list-customers (get string-list-results :Customer)

            no_result_int_list_results (graphql-handler :WordCount.Core no_result_int_list_query)
            no_result_string_list_results (graphql-handler :WordCount.Core no_result_string_list_query)
            no_result_int_list_customers (get no_result_int_list_results :Customer)
            no_result_string_list_customers (get no_result_string_list_results :Customer)]

        (testing "Integer list filters"
          (let [expected-int-subset [graphql-customer-data-1]]
            (is (= 1 (count int-list-customers)))
            (is (compare-instance-maps expected-int-subset int-list-customers :Id))))

        (testing "String list filters"
          (let [expected-string-subset [graphql-customer-data-2]]
            (is (= 1 (count string-list-customers)))
            (is (compare-instance-maps expected-string-subset string-list-customers :Id))))

        (testing "No results for Integer list filters"
          (is (empty? no_result_int_list_customers) "Expected no results for impossible integer list filter conditions"))

        (testing "No results for String list filters"
          (is (empty? no_result_string_list_customers) "Expected no results for impossible string list filter conditions"))))

    (testing "Create instances of user profiles and query them with nested profile detail filters"
      (let [profile-data-1 {:UserId      10010,
                            :DisplayName "Muhammad Hasnain Naeem",
                            :ProfileDetails
                            {:WordCount.Core/ProfileDetail
                             {:key  "Personal",
                              :data "Info",
                              :NestedDetail
                              {:WordCount.Core/NestedDetail
                               {:key         "Details",
                                :data        "More Info",
                                :DetailValue {:WordCount.Core/DetailValue
                                              {:key "Age", :data "30"}}}}}}}

            profile-data-2 {:UserId      40000,
                            :DisplayName "Hasnain Naeem",
                            :ProfileDetails
                            {:WordCount.Core/ProfileDetail
                             {:key  "Personal",
                              :data "Info",
                              :NestedDetail
                              {:WordCount.Core/NestedDetail
                               {:key         "Details",
                                :data        "More Info",
                                :DetailValue {:WordCount.Core/DetailValue
                                              {:key "Age", :data "25"}}}}}}}

            profile1-instance (e/eval-all-dataflows
                                (cn/make-instance
                                  :WordCount.Core/Create_UserProfileDetails
                                  {:Instance
                                   (cn/make-instance :WordCount.Core/UserProfileDetails profile-data-1)}))

            profile2-instance (e/eval-all-dataflows
                                (cn/make-instance
                                  :WordCount.Core/Create_UserProfileDetails
                                  {:Instance
                                   (cn/make-instance :WordCount.Core/UserProfileDetails profile-data-2)}))

            nested-filter-query "query UserProfileDetailsWithNestedFilter($filter: UserProfileDetailsFilter) {
                           UserProfileDetails(filter: $filter) {
                             UserId
                             DisplayName
                             ProfileDetails {
                               key
                               data
                               NestedDetail {
                                 key
                                 data
                                 DetailValue {
                                   key
                                   data
                                 }
                               }
                             }
                           }
                         }"
            nested-filter-variables {:filter
                                     {:ProfileDetails
                                       {:NestedDetail
                                        {:DetailValue
                                         {:data {:eq "30"}}}}}}

            results (graphql-handler :WordCount.Core nested-filter-query nested-filter-variables)
            filtered-profiles (get results :UserProfileDetails)]

        (is (= 1 (count filtered-profiles))
            "Expected only one user profile to match the nested filter")

        (let [filtered-profile (first filtered-profiles)]
          (is (= 10010 (:UserId filtered-profile))
              "Expected the filtered profile to have UserId 10010")
          (is (= "Muhammad Hasnain Naeem" (:DisplayName filtered-profile))
              "Expected the filtered profile to be Muhammad Hasnain Naeem")
          (is (= "30" (get-in filtered-profile [:ProfileDetails :NestedDetail :DetailValue :data]))
              "Expected the filtered profile to have a Personal detail with Age 30"))))

    (testing "Filters on list of addresses nested inside user entities"
      (let [user1-data {:Id        (fu/uuid-string)
                        :Email     "user1@example.com"
                        :Name      "John Doe"
                        :Addresses [{:WordCount.Core/Address
                                     {:City         "New York"
                                      :Zip          "10001"
                                      :StreetNumber 123
                                      :MoreDetails  "Apartment 4B"}}
                                    {:WordCount.Core/Address
                                     {:City         "Los Angeles"
                                      :Zip          "90001"
                                      :StreetNumber 456}}]}
            user2-data {:Id        (fu/uuid-string)
                        :Email     "user2@example.com"
                        :Name      "Jane Smith"
                        :Addresses [{:WordCount.Core/Address
                                     {:City         "Chicago"
                                      :Zip          "60601"
                                      :StreetNumber 789}}]}
            user3-data {:Id        (fu/uuid-string)
                        :Email     "user3@example.com"
                        :Name      "Bob Johnson"
                        :Addresses [{:WordCount.Core/Address
                                     {:City         "San Francisco"
                                      :Zip          "94105"
                                      :StreetNumber 101
                                      :MoreDetails  "Suite 500"}}
                                    {:WordCount.Core/Address
                                     {:City         "Seattle"
                                      :Zip          "98101"
                                      :StreetNumber 202}}
                                    {:WordCount.Core/Address
                                     {:City         "Portland"
                                      :Zip          "97201"
                                      :StreetNumber 303}}]}
            user4-data {:Id    (fu/uuid-string)
                        :Email "user4@example.com"
                        :Name  "Alice Brown"}
            user5-data {:Id        (fu/uuid-string)
                        :Email     "user5@example.com"
                        :Name      "Charlie Davis"
                        :Addresses [{:WordCount.Core/Address
                                     {:City         "Miami"
                                      :Zip          "33101"
                                      :StreetNumber 555
                                      :MoreDetails  "Beach House"}}]}
            users-data [user1-data user2-data user3-data user4-data user5-data]
            user-instances (mapv #(first (tu/fresult
                                           (e/eval-all-dataflows
                                             (cn/make-instance
                                               :WordCount.Core/Create_User
                                               {:Instance
                                                (cn/make-instance :WordCount.Core/User %)}))))
                                 users-data)]

        ;; Verify instances are created correctly
        (doseq [[user-instance user-data] (map vector user-instances users-data)]
          (is (cn/instance-of? :WordCount.Core/User user-instance))
          (is (= (filter-event-attrs (transform-user-data user-instance)) user-data)))

        (testing "String attribute filter - Filter users with zip codes starting with 9"
          (let [query "query {
              User(filter: { Addresses: { some: { Zip: { startsWith: \"9\" } } } }) {
                Id
                Name
                Addresses {
                  City
                  Zip
                }
              }
            }"
                results (graphql-handler :WordCount.Core query)
                filtered-users (:User results)]
            (is (= 2 (count filtered-users)))
            (is (every? #(some (fn [addr] (str/starts-with? (:Zip addr) "9")) (:Addresses %)) filtered-users))))

        (testing "Nested conditions - Users with addresses in either Los Angeles or Chicago"
          (let [query "query {
                    User(filter: {
                      Addresses: {
                        some: {
                          or: [
                            { City: { eq: \"Los Angeles\" } },
                            { City: { eq: \"Chicago\" } }
                          ]
                        }
                      }
                    }) {
                      Id
                      Name
                      Addresses {
                        City
                        Zip
                      }
                    }
                  }"
                results (graphql-handler :WordCount.Core query)
                filtered-users (get results :User)]
            (is (= 2 (count filtered-users)))
            (is (every? #(some (fn [addr] (contains? #{"Los Angeles" "Chicago"} (:City addr))) (:Addresses %)) filtered-users)))))

      (testing "Count, gt operators on attrs of record in list - Users with at least three addresses,
                where one address has a StreetNumber greater than 200"
        (let [query "query {
            User(filter: {
              and: [
                { Addresses: { count: { gte: 3 } } },
                { Addresses: { some: { StreetNumber: { gt: 200 } } } }
              ]
            }) {
              Id
              Name
              Addresses {
                City
                StreetNumber
                MoreDetails
              }
            }
          }"
              results (graphql-handler :WordCount.Core query)
              filtered-users (:User results)]

          (is (= 1 (count filtered-users)) "Expected exactly one user to match the criteria")

          (when (= 1 (count filtered-users))
            (let [user (first filtered-users)
                  addresses (:Addresses user)]
              (is (= "Bob Johnson" (:Name user)) "Expected user to be Bob Johnson")
              (is (>= (count addresses) 3) "User should have at least three addresses")
              (is (some #(> (:StreetNumber %) 200) addresses) "At least one address should have StreetNumber > 200")))))

      (testing "Complex address filter - Users with at least one address in zip code range 90000-95000 and street number > 400"
        (let [query "query {
                  User(filter: {
                    Addresses: {
                      some: {
                        and: [
                          { Zip: { gte: \"90000\", lt: \"95000\" } },
                          { StreetNumber: { gt: 400 } }
                        ]
                      }
                    }
                  }) {
                    Id
                    Name
                    Addresses {
                      City
                      Zip
                      StreetNumber
                    }
                  }
                }"
              results (graphql-handler :WordCount.Core query {})
              filtered-users (:User results)]

          (is (= 1 (count filtered-users)) "Expected exactly one user to match the criteria")

          (when (= 1 (count filtered-users))
            (let [user (first filtered-users)
                  matching-addresses (filter #(and (>= (compare (:Zip %) "90000") 0)
                                                   (< (compare (:Zip %) "95000") 0)
                                                   (> (:StreetNumber %) 400))
                                             (:Addresses user))]
              (is (= "John Doe" (:Name user)) "Expected user to be John Doe")
              (is (seq matching-addresses) "User should have at least one address matching the criteria")
              (is (every? #(and (>= (compare (:Zip %) "90000") 0)
                                (< (compare (:Zip %) "95000") 0)
                                (> (:StreetNumber %) 400))
                          matching-addresses)
                  "All matching addresses should satisfy the filter conditions")))))

      (testing "Complex user and address filter - Users with specific email domain, all addresses in certain states,
                at least one address with high street number, and name containing specific substring"
        (let [query "query {
                  User(filter: {
                    and: [
                      { Email: { endsWith: \"@example.com\" } },
                      { Addresses: {
                          every: {
                            or: [
                              { City: { in: [\"New York\", \"Los Angeles\", \"Chicago\", \"Houston\"] } },
                              { Zip: { startsWith: \"9\" } }
                            ]
                          },
                          some: { StreetNumber: { gt: 1000 } },
                          count: { gte: 2 }
                        }
                      },
                      { Name: { contains: \"oh\" } },
                      { or: [
                          { MemberSince: { lt: \"2020-01-01\" } },
                          { Addresses: { containsAll: [
                            { City: { eq: \"New York\" } },
                            { MoreDetails: { isNull: false } }
                          ] } }
                        ]
                      }
                    ]
                  }) {
                    Id
                    Name
                    Email
                    MemberSince
                    Addresses {
                      City
                      Zip
                      StreetNumber
                      MoreDetails
                    }
                  }
                }"
              results (graphql-handler :WordCount.Core query {})
              filtered-users (:User results)]

          (is (<= (count filtered-users) 1) "Expected at most one user to match these complex criteria")

          (when (= 1 (count filtered-users))
            (let [user (first filtered-users)
                  addresses (:Addresses user)]
              (is (str/ends-with? (:Email user) "@example.com") "User's email should end with @example.com")
              (is (str/includes? (:Name user) "oh") "User's name should contain 'oh'")
              (is (>= (count addresses) 2) "User should have at least two addresses")
              (is (every? #(or (contains? #{"New York" "Los Angeles" "Chicago" "Houston"} (:City %))
                               (str/starts-with? (:Zip %) "9"))
                          addresses)
                  "All addresses should be in specified cities or have zip starting with 9")
              (is (some #(> (:StreetNumber %) 1000) addresses) "At least one address should have StreetNumber > 1000")
              (is (or (when-let [member-since (:MemberSince user)]
                        (t/before? (t/local-date member-since)
                                   (t/local-date "2020-01-01")))
                      (and (some #(= (:City %) "New York") addresses)
                           (some #(some? (:MoreDetails %)) addresses)))
                  "User should either be a member before 2020 or have a New York address with MoreDetails")))))
      )))

(deftest test-create-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "1e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "1Sample Document"
                       :Content "1This is a sample document content."
                       :Summary "1Summary of the document."}

        user-data {:Id "1e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "1user17@example.com"
                   :Name "1John Doe"}

        parent-user-data {:Email "1user17@example.com"
                         :Name "1John Doe"}

        tag-data {:Id "11977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "1Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "1Nice"}

        customer-data {:Id          1000,
                       :Name        "Hasnain Naeem",
                       :ListOfNames ["Alice" "Bob" "Charlie"],
                       :Attributes
                       [{:name  "Personal",
                         :value "Info",
                         :SubAttribute
                         {:name            "Details",
                          :value           "More Info",
                          :SubSubAttribute {:name "Age", :value "30"}}}
                        {:name  "Professional",
                         :value "Work Info",
                         :SubAttribute
                         {:name            "Job",
                          :value           "Details",
                          :SubSubAttribute {:name "Occupation", :value "Engineer"}}}]}

        create-user-pattern "mutation {
                                CreateUser(input: {
                                    Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                    Email: \"1user17@example.com\",
                                    Name: \"1John Doe\",
                                }) {
                                    Id
                                    Email
                                    Name
                                }
                            }"

        query-by-id-pattern "query {
                               User(attributes: {Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\"}) {
                                   Id
                                   Email
                                   Name
                               }
                             }"

        query-by-email-pattern "query {
                                   User(attributes: {Email: \"1user17@example.com\"}) {
                                       Email
                                       Name
                                   }
                                 }"

        query-by-name-pattern "query {
                                  User(attributes: {Name: \"1John Doe\"}) {
                                      Email
                                      Name
                                  }
                                }"

        parent-user-email "1user17@example.com"
        child-document-name "1Sample Document"
        parent-user-id "1e977860-5cd4-4bc3-8323-f4f71a66de6d"
        child-document-id "1e977860-5cd4-4bc3-8323-f4f71a66de6e"

        create-child-document-mutation "mutation {
                                          CreateUserDocument(input: {
                                            UserId: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Id: \"1e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                            Name: \"1Sample Document\",
                                            Content: \"1This is a sample document content.\",
                                            Summary: \"1Summary of the document.\"
                                          }) {
                                            UserId
                                            Id
                                            Name
                                            Content
                                            Summary
                                          }
                                        }"

        query-all-docs-for-user-pattern (str "query {
                                                User(attributes: { Email: \"" parent-user-email "\" }) {
                                                    Email
                                                    UserDocument {
                                                        Document {
                                                            Name
                                                            Content
                                                            Summary
                                                            LastUpdated
                                                        }
                                                    }
                                                }
                                            }")

        query-by-email-and-doc-id-pattern (str "query {
                                                    User(attributes: { Email: \"" parent-user-email "\" }) {
                                                        Email
                                                        Name
                                                        UserDocument {
                                                            Document(attributes: { Id: \"" child-document-id "\" }) {
                                                                Name
                                                                Content
                                                                Summary
                                                                LastUpdated
                                                            }
                                                        }
                                                    }
                                                }")

        query-by-user-id-and-doc-name-pattern (str "query {
                                                  User(attributes: { Id: \"" parent-user-id "\" }) {
                                                      Name
                                                      UserDocument {
                                                          Document(attributes: { Name: \"" child-document-name "\" }) {
                                                              Name
                                                              Content
                                                              Summary
                                                              LastUpdated
                                                          }
                                                      }
                                                  }
                                              }")

        create-tag-mutation  "mutation {
                                CreateTag(input: {
                                  Id: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                  Name: \"1Tag 1\"
                                }) {
                                  Id
                                  Name
                                }
                              }"

        create-user-tag-mutation "mutation {
                                    CreateUserTag(input: {
                                      Tag: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      User: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"1Nice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        create-customer-mutation "mutation CreateCustomer {
                                    CreateCustomer(input: {
                                      Id: 1000,
                                      Name: \"Hasnain Naeem\",
                                      ListOfNames: [\"Alice\", \"Bob\", \"Charlie\"],
                                      Attributes: [
                                        {
                                          name: \"Personal\",
                                          value: \"Info\",
                                          SubAttribute: {
                                            name: \"Details\",
                                            value: \"More Info\",
                                            SubSubAttribute: {
                                              name: \"Age\",
                                              value: \"30\"
                                            }
                                          }
                                        },
                                        {
                                          name: \"Professional\",
                                          value: \"Work Info\",
                                          SubAttribute: {
                                            name: \"Job\",
                                            value: \"Details\",
                                            SubSubAttribute: {
                                              name: \"Occupation\",
                                              value: \"Engineer\"
                                            }
                                          }
                                        }
                                      ]
                                    }) {
                                      Id
                                      Name
                                      ListOfNames
                                      Attributes {
                                        name
                                        value
                                        SubAttribute {
                                          name
                                          value
                                          SubSubAttribute {
                                            name
                                            value
                                          }
                                        }
                                      }
                                    }
                                  }"

        query-tag-by-all-attributes "query {
                                        UserTag(attributes: {
                                            User: \"1e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Tag: \"11977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                            Awesome: \"1Nice\"
                                        }) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"

        query-tag-by-one-attribute "query {
                                        UserTag(attributes: {Awesome: \"1Nice\"}) {
                                            User
                                            Tag
                                            Awesome
                                        }
                                    }"
        ]

    ;; MUTATE AND QUERY PARENT
    (testing "Create instance of parent entity"
      (let [results (graphql-handler :WordCount.Core create-user-pattern)
            result-data (:CreateUser results)]
        (is (= user-data result-data))))

    (testing "Query Parent by GUID"
        (let [results (graphql-handler :WordCount.Core query-by-id-pattern)
              result-data (first (:User results))]
          (is (= user-data result-data))))

    (testing "Query Parent by ID"
      (let [results (graphql-handler :WordCount.Core query-by-email-pattern)
            result-data (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    (testing "Query Parent by Non-ID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-name-pattern)
            result-data  (first (:User results))]
        (is (= (dissoc user-data :Id) result-data))))

    ;; MUTATE AND QUERY CHILD
    (testing "Create Child Document Instance"
      (let [results (graphql-handler :WordCount.Core create-child-document-mutation)
            result-data (:CreateUserDocument results)]
        (is (= (assoc document-data :UserId (:Id user-data)) result-data))))

    (testing "Query All Documents for User"
      (let [results (graphql-handler :WordCount.Core query-all-docs-for-user-pattern)]
        (is (not-empty (get-in results [:User 0 :UserDocument])))))

    (testing "Query Child by GUID Attribute"
        (let [results (graphql-handler :WordCount.Core query-by-email-and-doc-id-pattern)]
          (is (= child-document-name (get-in results [:User 0 :UserDocument 0 :Document 0 :Name])))))

    (testing "Query Child by Non-GUID Attribute"
      (let [results (graphql-handler :WordCount.Core query-by-user-id-and-doc-name-pattern)
            user-documents (get-in results [:User 0 :UserDocument])]
        (is (= "1Sample Document" (get-in user-documents [0 :Document 0 :Name])))))

     ;; MUTATE AND QUERY BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      ;; create tag
      (graphql-handler :WordCount.Core create-tag-mutation)
      ;; create between instance
      (let [results (graphql-handler :WordCount.Core create-user-tag-mutation)
            result-data (:CreateUserTag results)]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by all attributes"
      (let [results (graphql-handler :WordCount.Core query-tag-by-all-attributes)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    (testing "Query between instance by one attribute"
      (let [results (graphql-handler :WordCount.Core query-tag-by-one-attribute)
            result-data (first (:UserTag results))]
        (is (= user-tag-data result-data))))

    ;; MUTATE A DEEP ENTITY WITH SEVERAL LEVELS OF RECORDS AND LIST ATTRIBUTES
    (testing "Create Customer Instance Having Several Levels of Records and List Attributes"
      (let [results (graphql-handler :WordCount.Core create-customer-mutation)
            result-data (:CreateCustomer results)]
        (is (= customer-data result-data))))))

(deftest test-update-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "2SampleDocument"
                       :Content "2This is a sample document content."
                       :Summary "2Summary of the document."}

        updated-document-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6e"
                               :Name "2SampleDocument"
                               :Content "new2This is a sample document content."
                               :Summary "new2Summary of the document."}

        user-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "2user17@example.com"
                   :Name "2John Doe"}

        customer-data {:Id          1001,
                       :Name        "Hasnain Naeem",
                       :ListOfNames ["Alice" "Bob" "Charlie"],
                       :Attributes
                       [{:WordCount.Core/Attribute {:name  "Personal",
                                                    :value "Info",
                                                    :SubAttribute
                                                    {:WordCount.Core/SubAttribute {:name            "Details",
                                                                                   :value           "More Info",
                                                                                   :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Age", :value "30"}}}}}}
                        {:WordCount.Core/Attribute {:name  "Professional",
                                                    :value "Work Info",
                                                    :SubAttribute
                                                    {:WordCount.Core/SubAttribute {:name            "Job",
                                                                                   :value           "Details",
                                                                                   :SubSubAttribute {:WordCount.Core/SubSubAttribute {:name "Occupation", :value "Engineer"}}}}}}]}

        updated-customer-data {:Id          1001,
                               :Name        "Muhammad Hasnain Naeem", ;; changed
                               :ListOfNames ["Name1" "Name2" "Name3"], ;; changed
                               :Attributes
                               [{:name  "Personal",
                                 :value "Info",
                                 :SubAttribute
                                 {:name            "Details",
                                  :value           "More Info",
                                  :SubSubAttribute {:name "Age", :value "30"}}}
                                {:name  "Professional",
                                 :value "Work Info",
                                 :SubAttribute
                                 {:name            "Job",
                                  :value           "Details",
                                  :SubSubAttribute {:name "Occupation", :value "Software Engineer"}}}]} ;; changed

        updated-user-data {:Id    "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                           :Email "newuser17@example.com"
                           :Name "newJohn Doe"}

        parent-user-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66d100"
                          :Email "2user17@example.com"
                          :Name "2John Doe"}

        tag-data {:Id "2e977860-5cd4-4bc3-8323-f4f71a66de6d"
                  :Name "2Tag 1"}

        user-tag-data {:User (:Id user-data)
                       :Tag (:Id tag-data)
                       :Awesome "2Nice"}

        updated-user-tag-data {:User (:Id user-data)
                               :Tag (:Id tag-data)
                               :Awesome "NewNice"}

        update-user-with-guid-pattern "mutation {
                                          UpdateUser(input: {
                                              Id: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                              Email: \"newuser17@example.com\",
                                              Name: \"newJohn Doe\",
                                          }) {
                                              Id
                                              Email
                                              Name
                                          }
                                      }"

        update-child-document-using-id-mutation "mutation {
                                                  UpdateUserDocument(input: {
                                                    UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",
                                                    Name: \"2SampleDocument\",
                                                    Content: \"new2This is a sample document content.\",
                                                    Summary: \"new2Summary of the document.\"
                                                  }) {
                                                    UserId
                                                    Id
                                                    Name
                                                    Content
                                                    Summary
                                                  }
                                                }"

        update-child-document-using-guid-mutation "mutation {
                                                    UpdateUserDocument(input: {
                                                      UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",
                                                      Content: \"guid2This is a sample document content.\",
                                                      Summary: \"guid2Summary of the document.\"
                                                    }) {
                                                      UserId
                                                      Name
                                                      Content
                                                      Summary
                                                    }
                                                  }"

        update-child-document-using-no-user-id-mutation "mutation {
                                                          UpdateUserDocument(input: {
                                                            Name: \"noid2SampleDocument\",
                                                            Content: \"noid2This is a sample document content.\",
                                                            Summary: \"noid2Summary of the document.\"
                                                          }) {
                                                            UserId
                                                            Id
                                                            Name
                                                            Content
                                                            Summary
                                                          }
                                                        }"

        update-child-document-using-no-id-mutation "mutation {
                                                    UpdateUserDocument(input: {
                                                      UserId: \"2e977860-5cd4-4bc3-8323-f4f71a66d100\",\n
                                                      Content: \"noid2This is a sample document content.\",
                                                      Summary: \"noid2Summary of the document.\"
                                                    }) {
                                                      UserId
                                                      Id
                                                      Name
                                                      Content
                                                      Summary
                                                    }
                                                  }"

        update-user-tag-mutation "mutation {
                                    UpdateUserTag(input: {
                                      Tag: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      User: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"NewNice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        update-user-tag-mutation-without-tag-id "mutation {
                                                  UpdateUserTag(input: {
                                                    User: \"2e977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Awesome: \"NewNice\"
                                                  }) {
                                                    Tag
                                                    User
                                                    Awesome
                                                  }
                                                }"

        update-customer-mutation "mutation UpdateCustomer {
                                    UpdateCustomer(input: {
                                      Id: 1001,
                                      Name: \"Muhammad Hasnain Naeem\",
                                      ListOfNames: [\"Name1\", \"Name2\", \"Name3\"],
                                      Attributes: [
                                        {
                                          name: \"Personal\",
                                          value: \"Info\",
                                          SubAttribute: {
                                            name: \"Details\",
                                            value: \"More Info\",
                                            SubSubAttribute: {
                                              name: \"Age\",
                                              value: \"30\"
                                            }
                                          }
                                        },
                                        {
                                          name: \"Professional\",
                                          value: \"Work Info\",
                                          SubAttribute: {
                                            name: \"Job\",
                                            value: \"Details\",
                                            SubSubAttribute: {
                                              name: \"Occupation\",
                                              value: \"Software Engineer\"
                                            }
                                          }
                                        }
                                      ]
                                    }) {
                                      Id
                                      Name
                                      ListOfNames
                                      Attributes {
                                        name
                                        value
                                        SubAttribute {
                                          name
                                          value
                                          SubSubAttribute {
                                            name
                                            value
                                          }
                                        }
                                      }
                                    }
                                  }"
        ]

    ;; CREATE AND UPDATE PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Update Parent by GUID"
        (let [results (graphql-handler :WordCount.Core update-user-with-guid-pattern)
              result-data (:UpdateUser results)]
          (is (= updated-user-data result-data))))

    ;; CREATE AND UPDATE CHILD
    (testing "Manually create instances of parent and child entities"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                           (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Update Child Document of User using ID"
      (let [results (graphql-handler :WordCount.Core update-child-document-using-id-mutation)]
        (is (= updated-document-data (dissoc (:UpdateUserDocument results) :UserId)))))

    (testing "Fail to Update Child Document of User When No User ID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-no-user-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) ":Id not provided for :WordCount.Core/User"))))

    (testing "Fail to Update Child Document of User using GUID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-guid-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Name not provided for :WordCount.Core/Document"))))

    (testing "Fail to Update Child Document of User When No Document ID"
      (let [result (try
                     (graphql-handler :WordCount.Core update-child-document-using-no-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Name not provided for :WordCount.Core/Document"))))

    ;; CREATE AND UPDATE BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))
            tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Update Between Instance using GUIDs"
        (let [results (graphql-handler :WordCount.Core update-user-tag-mutation)
              result-data (:UpdateUserTag results)]
          (is (= updated-user-tag-data result-data))))

    (testing "Fail to Update Between Instance When Entity GUIDs Missing"
      (let [result (try
                     (graphql-handler :WordCount.Core update-user-tag-mutation-without-tag-id)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Error: GUID for ':WordCount.Core/Tag' not provided."))))

    ;; CREATE AND UPDATE A DEEP ENTITY WITH SEVERAL LEVELS OF RECORDS AND LIST ATTRIBUTES
    (testing "Create instance of customer - an entity with deeply nested attributes"
      (let [customer-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_Customer
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Customer customer-data)}))))]
        (is (cn/instance-of? :WordCount.Core/Customer customer-instance))))

    (testing "Update Customer Instance Having Several Levels of Records and List Attributes"
      (let [results (graphql-handler :WordCount.Core update-customer-mutation)
            result-data (:UpdateCustomer results)]
        (is (= (:Id updated-customer-data) (:Id result-data)))
        (is (= (:Name updated-customer-data) (:Name result-data)))
        (is (= (:ListOfNames updated-customer-data) (:ListOfNames result-data)))
        (is (= (count (:Attributes updated-customer-data)) (count (:Attributes result-data))))))))

(deftest test-delete-mutations-for-word-count-app
  (build-word-count-app)
  (let [document-data {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "3Sample Document"
                       :Content "3This is a sample document content."
                       :Summary "3Summary of the document."}

        document-data2 {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6e"
                       :Name "3Sample Document"
                       :Content "3This is a sample document content."
                       :Summary "3Summary of the document."}

        user-data {:Id "3e977860-5cd4-4bc3-8323-f4f71a66de6d"
                   :Email "3user17@example.com"
                   :Name "3John Doe"}

        parent-user-data {:Id "32977860-5cd4-4bc3-8323-f4f71a66de6d"
                          :Email "3user17@example.com"
                          :Name "3John Doe"}

        tag-data {:Id "32977860-5cd9-4bc3-8323-f4f71a66de6d"
                  :Name "3Tag 1"}

        user-tag-data {:User (:Id parent-user-data)
                       :Tag (:Id tag-data)
                       :Awesome "3Nice"}

        delete-user-by-id "mutation {
                              DeleteUser(input: { Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6d\" }) {
                                Id
                                Email
                                Name
                              }
                            }"

        delete-child-document-using-id-mutation "mutation {
                                                  DeleteUserDocument(input: {
                                                    UserId: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                  }) {
                                                    Id
                                                    Name
                                                    Content
                                                    Summary
                                                  }
                                                }"

        delete-child-document-using-several-attrs-mutation "mutation {
                                                              DeleteUserDocument(input: {
                                                                UserId: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                                Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                                Name: \"3Sample Document\",
                                                                Content: \"3This is a sample document content.\",
                                                                Summary: \"3Summary of the document.\"
                                                              }) {
                                                                UserId
                                                                Id
                                                                Name
                                                                Content
                                                                Summary
                                                              }
                                                            }"

        delete-child-document-without-parent-id-mutation "mutation {
                                                            DeleteUserDocument(input: {
                                                              Id: \"3e977860-5cd4-4bc3-8323-f4f71a66de6e\",
                                                            }) {
                                                              Id
                                                              Name
                                                              Content
                                                              Summary
                                                            }
                                                          }"

        delete-user-tag-mutation "mutation {
                                    DeleteUserTag(input: {
                                      Tag: \"32977860-5cd9-4bc3-8323-f4f71a66de6d\",
                                      User: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                      Awesome: \"3Nice\"
                                    }) {
                                      Tag
                                      User
                                      Awesome
                                    }
                                  }"

        delete-user-tag-mutation-without-tag-id "mutation {
                                                  DeleteUserTag(input: {
                                                    User: \"32977860-5cd4-4bc3-8323-f4f71a66de6d\",
                                                    Awesome: \"3Nice\"
                                                  }) {
                                                    Tag
                                                    User
                                                    Awesome
                                                  }
                                                }"
        ]

    ;; CREATE AND DELETE PARENT
    (testing "Create instance of parent entity"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User user-data)}))))]
        (is (cn/instance-of? :WordCount.Core/User user-instance))
        (is (= (filter-event-attrs user-instance) user-data))))

    (testing "Delete Parent by GUID"
      (let [delete-results (first (:DeleteUser (graphql-handler :WordCount.Core delete-user-by-id)))]
        (is (= delete-results user-data))))

    ;; CREATE AND DELETE CHILD
    (testing "Manually create instance of child document"
      (let [user-instance (first (tu/fresult
                                   (e/eval-all-dataflows
                                     (cn/make-instance
                                      :WordCount.Core/Create_User
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/User parent-user-data)}))))]
        (api/dataflow
         :WordCount.Core/CreateTestDocument
         {:WordCount.Core/User
          {:Id? (:Id user-instance)} :as :U}
          {:WordCount.Core/Document document-data
           :-> [[:WordCount.Core/UserDocument :U]]})

        (tu/finalize-component :WordCount.Core)
        (let [result (:result (first (e/eval-all-dataflows
                                       (cn/make-instance {:WordCount.Core/CreateTestDocument {}}))))]
          (is (cn/instance-of? :WordCount.Core/Document result))
          (is (= (filter-event-attrs result) document-data)))))

    (testing "Delete Child by GUID"
      (let [delete-results (first (:DeleteUserDocument (graphql-handler :WordCount.Core delete-child-document-using-id-mutation)))]
        (is (= delete-results document-data))))

    (testing "Manually create instance of another child document"
      (api/dataflow
       :WordCount.Core/CreateTestDocument2
       {:WordCount.Core/User
        {:Id? (:Id parent-user-data)} :as :U}
        {:WordCount.Core/Document document-data2
         :-> [[:WordCount.Core/UserDocument :U]]})

      (tu/finalize-component :WordCount.Core)
      (let [result (:result (first (e/eval-all-dataflows
                                     (cn/make-instance {:WordCount.Core/CreateTestDocument2 {}}))))]
        (is (cn/instance-of? :WordCount.Core/Document result))
        (is (= (filter-event-attrs result) document-data))))

    (testing "Delete Child by Several Attributes"
      ;; delete
      (let [delete-results (first (:DeleteUserDocument (graphql-handler :WordCount.Core delete-child-document-using-several-attrs-mutation)))]
        (is (= delete-results (assoc document-data :UserId (:Id parent-user-data))))))

    (testing "Fail to Delete Child When Parent GUID Missing"
      (let [result (try
                     (graphql-handler :WordCount.Core delete-child-document-without-parent-id-mutation)
                     (catch Exception e e))]
        (is (instance? Exception result))
        (is (.contains (.getMessage result) "Error: UserId not provided for :WordCount.Core/User. It is needed to identify the parent entity."))))

     ;; CREATE AND DELETE BETWEEN INSTANCE
    (testing "Create instance of between relationship"
      (let [tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_Tag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/Tag tag-data)}))))
            user-tag-instance (first (tu/fresult
                                  (e/eval-all-dataflows
                                    (cn/make-instance
                                      :WordCount.Core/Create_UserTag
                                      {:Instance
                                       (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))))]
        (is (cn/instance-of? :WordCount.Core/UserTag user-tag-instance))
        (is (= (filter-event-attrs user-tag-instance) user-tag-data))))

    (testing "Delete Between Instance using GUIDs"
        (let [results (graphql-handler :WordCount.Core delete-user-tag-mutation)
              result-data (first (:DeleteUserTag results))]
          (is (= user-tag-data result-data))))

    (testing "Delete Between Instance using GUIDs"
      (e/eval-all-dataflows
        (cn/make-instance
          :WordCount.Core/Create_UserTag
          {:Instance
           (cn/make-instance :WordCount.Core/UserTag user-tag-data)}))
      (let [results (graphql-handler :WordCount.Core delete-user-tag-mutation-without-tag-id)
            result-data (first (:DeleteUserTag results))]
        (is (= user-tag-data result-data)))))))
