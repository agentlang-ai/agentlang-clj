(ns agentlang.test.fixes01
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.lang.datetime :as dt]
            [agentlang.lang.internal :as li]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.inference.service.planner :as planner]
            #?(:clj  [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-1691
  (defcomponent :I1691
    (entity :I1691/E {:Id {:type :Int :id true} :X :Int})
    (dataflow
     :I1691/Get
     [:try
      {:I1691/E {:Id? :I1691/Get.E}}
      "a"
      :not-found "b"]))
  (let [g #(tu/invoke {:I1691/Get {:E %}})]
    (is (= "b" (g 1)))
    (is (cn/instance-of?
         :I1691/E
         (tu/invoke
          {:I1691/Create_E
           {:Instance {:I1691/E {:Id 1 :X 10}}}})))
    (is (= "a" (g 1)))))

(deftest map-pattern-bug
  (defcomponent :Mpb
    (event
     :Mpb/E
     {:X :Map})
    (entity
     :Mpb/A
     {:Y :Map})
    (dataflow
     :Mpb/E
     {:Mpb/A {:Y {:default :Mpb/E.X}}}))
  (let [x {:a 1 :b "hello"}
        r (tu/invoke {:Mpb/E {:X x}})]
    (is (cn/instance-of? :Mpb/A r))
    (is (= {:default x} (:Y r)))))

(deftest issue-195
  (defcomponent :I195
    (entity
     :I195/E1
     {:A :Int
      :B :Int
      :C :Int
      :Y :DateTime})
    (dataflow
     :I195/K
     {:I195/E1 {:A '(+ 5 :B)
                :B 10
                :C '(+ 10 :A)
                :Y '(agentlang.lang.datetime/now)}})
    (entity {:I195/E2 {:Y :DateTime}})
    (dataflow :I195/KK {:I195/E2 {:Y '(agentlang.lang.datetime/now)}}))
  (let [evt (cn/make-instance :I195/K {})
        r (tu/invoke evt)]
    (is (cn/instance-of? :I195/E1 r))
    (is (dt/parse-default-date-time (:Y r)))
    (is (= 10 (:B r)))
    (is (= 15 (:A r)))
    (is (= 25 (:C r))))
  (let [evt (cn/make-instance :I195/KK {})
        r (tu/invoke evt)]
    (is (cn/instance-of? :I195/E2 r))
    (is (dt/parse-default-date-time (:Y r)))))

(deftest issue-352-datetime-index
  (defcomponent :I352DtIndex
    (entity
     :I352DtIndex/E
     {:A {:type :DateTime
          :indexed true}
      :B :Int})
    (dataflow
     :I352DtIndex/FindByDateTime
     {:I352DtIndex/E
      {:A? :I352DtIndex/FindByDateTime.Input}})
    (dataflow
     :I352DtIndex/FindBetween
     {:I352DtIndex/E
      {:? {:where [:and
                   [:> :A :I352DtIndex/FindBetween.Start]
                   [:< :A :I352DtIndex/FindBetween.End]]}}}))
  (let [dt "2021-12-30T03:30:24"
        r1 (tu/invoke
            {:I352DtIndex/Create_E
             {:Instance
              {:I352DtIndex/E
               {:A dt
                :B 100}}}})
        r2 (first
            (tu/invoke
             {:I352DtIndex/FindByDateTime
              {:Input dt}}))
        r3 (first
            (tu/invoke
             {:I352DtIndex/FindBetween
              {:Start "2021-11-30T00:00:00"
               :End "2022-01-30T00:00:00"}}))
        r4 (first
            (tu/invoke
             {:I352DtIndex/FindBetween
              {:Start "2022-11-30T00:00:00"
               :End "2023-01-30T00:00:00"}}))]
    (is (cn/instance-of? :I352DtIndex/E r1))
    (is (cn/instance-of? :I352DtIndex/E r2))
    (is (cn/instance-of? :I352DtIndex/E r3))
    (is (cn/same-instance? r1 r2))
    (is (cn/same-instance? r1 r3))
    (is (nil? r4))))

(deftest issue-352-date-time-formats
  (let [dates [["MMMM d, yyyy" "January 8, 2021"]
               ["yyyy-MMM-dd" "2021-Jan-08"]
               ["MMM-dd-yyyy" "Jan-08-2021"]
               ["dd-MMM-yyyy" "08-Jan-2021"]
               ["yyyyMMdd" "20210108"]]
        times [["HH:mm:ss.SSS" "04:05:06.789"]
               ["HH:mm:ss" "04:05:06"]
               ["HH:mm" "04:05"]
               ["HHmmss" "040506"]
               ["HH:mm:ss z" "04:05:06 America/New_York"]]
        date-times [["yyyy-MM-dd HH:mm:ss" "2021-01-08 04:05:06"]
                    ["yyyy-MM-dd HH:mm" "2021-01-08 04:05"]
                    ["yyyy-MM-dd HH:mm:ss.SSS" "2021-01-08 04:05:06.789"]
                    ["yyyyMMddHHmmss" "20210108040506"]
                    ["yyyy-MM-dd HH:mm:ss z" "2021-01-08 04:05:06 America/New_York"]]]
    (is (every? (fn [[f s]] ((dt/date-parser f) s)) dates))
    (is (every? (fn [[f s]] ((dt/time-parser f) s)) times))
    (is (every? (fn [[f s]] ((dt/date-time-parser f) s)) date-times))))

(deftest issue-352-date-time-upserts
  (defcomponent :I352Dtu
    (entity
     :I352Dtu/E
     {:A :Date
      :B :Time}))
  (let [r1 (tu/invoke
            {:I352Dtu/Create_E
             {:Instance
              {:I352Dtu/E
               {:A "2021-08-26"
                :B "14:24:30.000"}}}})
        r2 (first
            (tu/invoke
             {:I352Dtu/Lookup_E
              {:path (li/path-attr r1)}}))]
    (is (cn/same-instance? r1 r2))))

(deftest issue-1703-commas-in-ids
  (defcomponent :I1703
    (entity
     :I1703/E
     {:Id {:type :String :id true}
      :X :Int}))
  (let [[cre e?] (tu/make-create :I1703/E)
        _ (tu/is-error "Comma in Id" #(cre {:Id "101,100" :X 1}))
        e (cre {:Id "101-100" :X 1})]
    (is (e? e))
    (is (= "101-100" (:Id e)))
    (is (= [":I1703/E" "101-100"] (li/path-to-vec (li/path-attr e))))))

(deftest planner-expressions
  (defcomponent :PE
    (entity :PE/Assignee {:IncidentCategory :String
                          :ServiceNowUsername :String})
    (entity
     :PE/IncidentTriage
     {:SysId :String
      :Category :String
      :AssignedTo :String
      :Urgency :String
      :Severity :String})
    (entity :PE/ChatMessageRequest {:MessageText :String}))
  (let [exprs '(do
                 (def incident-category "Networking")
                 (def
                   assignee
                   (lookup-one
                    :PE/Assignee
                    {:IncidentCategory incident-category}))
                 (def
                   triage
                   (make
                    :PE/IncidentTriage
                    {:SysId "471bfbc7a9fe198101e77a3e10e5d47f",
                     :Category incident-category,
                     :AssignedTo (:ServiceNowUsername assignee),
                     :Urgency "1",
                     :Severity "1"}))
                 (def
                   teams-message
                   (make
                    :PE/ChatMessageRequest
                    {:MessageText
                     (str
                      "\nSysId: 471bfbc7a9fe198101e77a3e10e5d47f\n\nDescription: Unable to access Oregon mail server. Is it down?\n\nCategory: "
                      incident-category
                      "\n\nUrgency: 1\n\nSeverity: 1\n\nAssigned to: "
                      (:ServiceNowUsername assignee)
                      "\n\n")}))
                 triage)]
    (is (cn/instance-of? :PE/Assignee (tu/invoke {:PE/Create_Assignee
                                                  {:Instance
                                                   {:PE/Assignee
                                                    {:ServiceNowUsername "joe"
                                                     :IncidentCategory "Networking"}}}})))
    (is (cn/instance-of? :PE/IncidentTriage (:result (gs/evaluate-dataflow (planner/expressions-to-patterns exprs)))))
    (let [msg (first (tu/invoke {:PE/LookupAll_ChatMessageRequest {}}))]
      (is (cn/instance-of? :PE/ChatMessageRequest msg))
      (is (= (:MessageText msg)
             (str
              "\nSysId: 471bfbc7a9fe198101e77a3e10e5d47f\n\nDescription: Unable to access Oregon mail server. Is it down?\n\nCategory: "
              "Networking"
              "\n\nUrgency: 1\n\nSeverity: 1\n\nAssigned to: "
              "joe"
              "\n\n"))))))
