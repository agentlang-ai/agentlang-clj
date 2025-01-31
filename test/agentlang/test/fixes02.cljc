(ns agentlang.test.fixes02
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow inference]]
            [agentlang.lang.raw :as lr]
            [agentlang.evaluator :as e]
            [agentlang.lang.datetime :as dt]
            [clojure.java.io :as io]
            #?(:clj [agentlang.datafmt.csv :as csv])
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-352-datetime-index
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I352DtIndex
     (entity
      {:I352DtIndex/E
       {:A {:type :DateTime
            :indexed true}
        :B :Int}})
     (dataflow :I352DtIndex/FindByDateTime
               {:I352DtIndex/E
                {:A? :I352DtIndex/FindByDateTime.Input}})
     (dataflow :I352DtIndex/FindBetween
               {:I352DtIndex/E
                {:A? [:and
                      [:> :I352DtIndex/FindBetween.Start]
                      [:< :I352DtIndex/FindBetween.End]]}}))
   (let [dt "2021-12-30T03:30:24"
         r1 (tu/first-result
             {:I352DtIndex/Create_E
              {:Instance
               {:I352DtIndex/E
                {:A dt
                 :B 100}}}})
         r2 (tu/first-result
             {:I352DtIndex/FindByDateTime
              {:Input dt}})
         r3 (tu/first-result
             {:I352DtIndex/FindBetween
              {:Start "2021-11-30T00:00:00"
               :End "2022-01-30T00:00:00"}})
         r4 (first
             (e/eval-all-dataflows
              {:I352DtIndex/FindBetween
               {:Start "2022-11-30T00:00:00"
                :End "2023-01-30T00:00:00"}}))]
     (is (cn/instance-of? :I352DtIndex/E r1))
     (is (cn/instance-of? :I352DtIndex/E r2))
     (is (cn/instance-of? :I352DtIndex/E r3))
     (is (cn/same-instance? r1 r2))
     (is (cn/same-instance? r1 r3))
     (is (= :not-found (:status r4))))))

(deftest issue-352-date-time-formats
  (#?(:clj do
      :cljs cljs.core.async/go)
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
     (is (every? (fn [[f s]] ((dt/date-time-parser f) s)) date-times)))))

(deftest issue-352-date-time-upserts
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I352Dtu
     (entity
      :I352Dtu/E
      {:A :Date
       :B :Time}))
   (let [r1 (tu/first-result
             {:I352Dtu/Create_E
              {:Instance
               {:I352Dtu/E
                {:A "2021-08-26"
                 :B "14:24:30"}}}})
         r2 (tu/first-result
             {:I352Dtu/Lookup_E
              {cn/id-attr (cn/id-attr r1)}})]
     (is (cn/same-instance? r1 r2)))))

#?(:clj
   (deftest issue-358-csv-import
     (defcomponent :I358Csv01
       (entity
        :I358Csv01/Employee
        {:FirstName :String
         :LastName :String
         :Salary :Decimal})
       (dataflow
        :I358Csv01/ImportEmployees
        {:Agentlang.Kernel.Lang/DataSync
         {:Source
          {:Agentlang.Kernel.Lang/DataSource
           {:Uri "file://test/sample/emp.csv"
            :Entity "I358Csv01/Employee"
            :AttributeMapping
            {"first_name" "FirstName"
             "last_name" "LastName"
             "salary" "Salary"}}}}})
       (dataflow
        :I358Csv01/ExportEmployees
        {:Agentlang.Kernel.Lang/DataSync
         {:Source
          {:Agentlang.Kernel.Lang/DataSource
           {:Entity "I358Csv01/Employee"
            :AttributeMapping
            {"FirstName" "first_name"
             "LastName" "last_name"
             "Salary" "salary"}}}
          :DestinationUri "file://test/sample/emp2.csv"}}))
     (let [result (first
                   (e/eval-all-dataflows
                    {:I358Csv01/ImportEmployees {}}))]
       (is (= :ok (:status result)))
       (is (partial cn/instance-of? :I358Csv01/Employee) (:result result))
       (let [id (cn/id-attr (:result result))
             r (tu/first-result
                {:I358Csv01/Lookup_Employee
                 {cn/id-attr id}})]
         (is (cn/same-instance? r (:result result))))
       (let [result (first
                     (e/eval-all-dataflows
                      {:I358Csv01/ExportEmployees {}}))
             csv-file "test/sample/emp2.csv"]
         (is (= :ok (:status result)))
         (is (= csv-file (:result result)))
         (let [csv (csv/read-csv csv-file)]
           (io/delete-file csv-file true)
           (is (= ["first_name" "last_name" "salary"] (first csv)))
           (doseq [row (rest csv)]
             (is (some #{row} [["robert" "k" "2400"] ["jane" "a" "5600"]]))))))))

(deftest issue-372-range-query
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I372
     (entity
      :I372/E
      {:X {:type :Int
           :indexed true}})
     (dataflow
      :I372/Lookup
      {:I372/E
       {:X? [:and
             [:>= :I372/Lookup.A]
             [:< :I372/Lookup.B]]}}))
   (let [e1 (tu/first-result
             {:I372/Create_E
              {:Instance {:I372/E {:X 10}}}})
         e2 (tu/first-result
             {:I372/Create_E
              {:Instance {:I372/E {:X 20}}}})
         r1 (:result
             (first
              (e/eval-all-dataflows
               {:I372/Lookup {:A 10 :B 20}})))
         r2 (:result
             (first
              (e/eval-all-dataflows
               {:I372/Lookup {:A 10 :B 21}})))]
     (is (= (count r1) 1))
     (is (cn/same-instance? e1 (first r1)))
     (is (= (count r2) 2))
     (doseq [inst r2]
       (is (or (cn/same-instance? e1 inst)
               (cn/same-instance? e2 inst)))))))

(deftest issue-377-multi-query
  (#?(:clj do)
   (when-not (tu/windows?)
     (defcomponent :I377.Test1
       (entity
        :I377.Test1/Defect
        {:SiteLocation :String
         :DefectType :String
         :Timestamp {:type :DateTime
                     :default dt/now
                     :indexed true}
         :MarkedAsDeleted {:type :Boolean
                           :default false}})

       (event
        :I377.Test1/GetDefectsByDateAndSiteLocation
        {:From :String
         :To :String
         :SiteLocation :String})

       (dataflow
        :I377.Test1/GetDefectsByDateAndSiteLocation
        {:I377.Test1/Defect
         {:Timestamp? [:and
                       [:> :I377.Test1/GetDefectsByDateAndSiteLocation.From]
                       [:< :I377.Test1/GetDefectsByDateAndSiteLocation.To]]
          :SiteLocation? [:= :I377.Test1/GetDefectsByDateAndSiteLocation.SiteLocation]
          :MarkedAsDeleted? [:= false]}}))

     (let [s (dt/now)
           _ (Thread/sleep 1000)
           e1 (cn/make-instance
               {:I377.Test1/Defect
                {:SiteLocation "a"
                 :Timestamp "2021-10-20T11:39:55.539551"
                 :DefectType "fatal"}})
           er1 (tu/first-result
                {:I377.Test1/Create_Defect {:Instance e1}})
           e2 (cn/make-instance
               {:I377.Test1/Defect
                {:SiteLocation "b"
                 :Timestamp "2021-10-20T11:39:20.539551"
                 :DefectType "serious"}})
           er2 (tu/first-result
                {:I377.Test1/Create_Defect {:Instance e2}})
           e3 (cn/make-instance
               {:I377.Test1/Defect
                {:SiteLocation "b"
                 :DefectType "normal"}})
           er3 (tu/first-result
                {:I377.Test1/Create_Defect {:Instance e3}})
           e4 (cn/make-instance
               {:I377.Test1/Defect
                {:SiteLocation "a"
                 :DefectType "fatal"}})
           er4 (tu/first-result
                {:I377.Test1/Create_Defect {:Instance e4}})
           evt (cn/make-instance
                {:I377.Test1/GetDefectsByDateAndSiteLocation
                 {:From s
                  :To (dt/now)
                  :SiteLocation "b"}})
           r (:result (first (e/eval-all-dataflows evt)))]
       (is (= 1 (count r)))
       (is (= (cn/id-attr (first r)) (cn/id-attr er3)))))))

(deftest issue-379-compound-query
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I379
     (entity
      :I379/P
      {:A :Int})
     (entity
      :I379/E
      {:P {:ref :I379/P.__Id__}
       :X {:type :Int
           :indexed true}
       :Y {:type :Int
           :expr '(+ 10 :X :P.A)}})
     (dataflow
      :I379/Q
      {:I379/E {:X? :I379/Q.X}}))
   (let [p (cn/make-instance
            {:I379/P {:A 20}})
         pr (tu/first-result
             {:I379/Create_P {:Instance p}})
         e (cn/make-instance
            {:I379/E
             {:P (cn/id-attr pr)
              :X 100}})
         r1 (tu/first-result
             {:I379/Create_E
              {:Instance e}})
         r2 (tu/first-result
             {:I379/Lookup_E
              {cn/id-attr (cn/id-attr e)}})
         r3 (tu/first-result
             {:I379/Q {:X 100}})]
     (is (= (:Y r1) 130))
     (is (cn/same-instance? r1 r2))
     (is (= (:Y r2) 130))
     (is (cn/same-instance? r1 r3))
     (is (= (:Y r3) 130)))))

(deftest issue-391-complex-queries
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I391
     (entity
      :I391/E
      {:X {:type :Int
           :indexed true}
       :Y :Int})
     (dataflow
      :I391/Query01
      {:I391/E?
       {:where [:>= :X :I391/Query01.X]
        :order-by [:Y]}})
     (dataflow
      :I391/Query02
      {:I391/E?
       {:where [:>= :X :I391/Query02.X]
        :order-by [:Y]
        :limit 3}}))
   (let [es (mapv
             #(cn/make-instance
               {:I391/E
                {:X %1 :Y %2}})
             [12 89 101 32 40]
             [7 2 0 100 15])
         insts (mapv
                #(tu/first-result
                  (cn/make-instance
                   {:I391/Create_E
                    {:Instance %}}))
                es)
         r1 (:result
             (first
              (e/eval-all-dataflows
               (cn/make-instance
                {:I391/Query01
                 {:X 15}}))))
         r2 (:result
             (first
              (e/eval-all-dataflows
               (cn/make-instance
                {:I391/Query02
                 {:X 15}}))))]
     (is (every? (partial cn/instance-of? :I391/E) insts))
     (is (= 4 (count r1)))
     (is (every? #(>= (:X %) 15) r1))
     (is (apply < (mapv :Y r1)))
     (is (= 3 (count r2)))
     (is (every? #(>= (:X %) 15) r2))
     (is (apply < (mapv :Y r2))))))

(deftest issue-427-all-query-alias
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I427
     (entity
      :I427/A
      {:X :Int})
     (record
      :I427/B
      {:Result :Any})
     (dataflow
      :I427/E
      {:I427/A? {} :as :R}
      {:I427/B {:Result :R}}))
   (let [xs (mapv #(tu/first-result
                    {:I427/Create_A
                     {:Instance
                      {:I427/A
                       {:X %}}}})
                  [1 2 3])
         r (tu/first-result
            {:I427/E {}})]
     (is (every? (partial cn/instance-of? :I427/A) xs))
     (is (every? (partial cn/instance-of? :I427/A) (:Result r)))
     (is (= (sort (mapv :X xs)) (sort (mapv :X (:Result r))))))))

(deftest issue-427-cond-query-alias
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I427b
     (entity
      :I427b/A
      {:X :Boolean})
     (record
      :I427b/B
      {:Result :Any})
     (dataflow
      :I427b/E
      {:I427b/A {:X? true} :as :R}
      {:I427b/B {:Result '(identity :R)}}))
   (let [xs (mapv #(tu/first-result
                    {:I427b/Create_A
                     {:Instance
                      {:I427b/A
                       {:X %}}}})
                  [true true false true])
         r (e/eval-all-dataflows
            {:I427b/E {}})
         ys (:Result (first (:result (first r))))]
     (is (= 3 (count ys)))
     (is (every? #(true? (:X %)) ys)))))

(defn- make-long-string [n]
  (let [cs (seq "abcdefghijklmnopqrstuvwxyz")]
    (clojure.string/join
     (loop [s [], i 0]
       (if (< i n)
         (recur (conj s (rand-nth cs)) (inc i))
         s)))))

(defn- make-object [api-token]
  {:ApiToken api-token,
   :Email "testuser@ventur8.io",
   :AuthDomain "agentlang.us.auth0.com",
   cn/id-attr "8dd4b088-1e51-4efe-9385-018783b96eb4"
   :-*-name-*- [:Agentlang.Kernel :OAuthAnyRequest],
   :UserName "testuser",
   :Password "P@s$w0rd123",
   :ClientSecret "DSiQSiVT7Sd0RJwxdQ4gCfjLUA495PjlVNKhkgB6yFgpH2rgt9kpRbxJLPOcAaXH",
   :ClientID "Zpd3u7saV3Y7tebdzJ1Vo0eFALWyxMnR",
   :type-*-tag-*- :entity})

(deftest issue-442-long-string-field
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I442
     (entity
      :I442/E
      {:X :Int
       :Y :Any
       :Z :Int}))
   (let [api-token "eyJhbGciOiJSUzI1NiIsInR5cCI6IkpXVCIsImtpZCI6Im00YTJzbG1IQkhxLTJVdDllREk1eiJ9.eyJpc3MiOiJodHRwczovL2ZyYWN0bC51cy5hdXRoMC5jb20vIiwic3ViIjoiQnRaT05YTVRRRWJzU0hpZkRTWW90WGZPdFk1QmVIdThAY2xpZW50cyIsImF1ZCI6Imh0dHBzOi8vZnJhY3RsLnVzLmF1dGgwLmNvbS9hcGkvdjIvIiwiaWF0IjoxNjQyNTI4MTQzLCJleHAiOjE2NDI2MTQ1NDMsImF6cCI6IkJ0Wk9OWE1UUUVic1NIaWZEU1lvdFhmT3RZNUJlSHU4Iiwic2NvcGUiOiJyZWFkOmNsaWVudF9ncmFudHMgY3JlYXRlOmNsaWVudF9ncmFudHMgZGVsZXRlOmNsaWVudF9ncmFudHMgdXBkYXRlOmNsaWVudF9ncmFudHMgcmVhZDp1c2VycyB1cGRhdGU6dXNlcnMgZGVsZXRlOnVzZXJzIGNyZWF0ZTp1c2VycyByZWFkOnVzZXJzX2FwcF9tZXRhZGF0YSB1cGRhdGU6dXNlcnNfYXBwX21ldGFkYXRhIGRlbGV0ZTp1c2Vyc19hcHBfbWV0YWRhdGEgY3JlYXRlOnVzZXJzX2FwcF9tZXRhZGF0YSByZWFkOnVzZXJfY3VzdG9tX2Jsb2NrcyBjcmVhdGU6dXNlcl9jdXN0b21fYmxvY2tzIGRlbGV0ZTp1c2VyX2N1c3RvbV9ibG9ja3MgY3JlYXRlOnVzZXJfdGlja2V0cyByZWFkOmNsaWVudHMgdXBkYXRlOmNsaWVudHMgZGVsZXRlOmNsaWVudHMgY3JlYXRlOmNsaWVudHMgcmVhZDpjbGllbnRfa2V5cyB1cGRhdGU6Y2xpZW50X2tleXMgZGVsZXRlOmNsaWVudF9rZXlzIGNyZWF0ZTpjbGllbnRfa2V5cyByZWFkOmNvbm5lY3Rpb25zIHVwZGF0ZTpjb25uZWN0aW9ucyBkZWxldGU6Y29ubmVjdGlvbnMgY3}lYXRlOmNvbm5lY3Rpb25zIHJlYWQ6cmVzb3VyY2Vfc2VydmVycyB1cGRhdGU6cmVzb3VyY2Vfc2VydmVycyBkZWxldGU6cmVzb3VyY2Vfc2VydmVycyBjcmVhdGU6cmVzb3VyY2Vfc2VydmVycyByZWFkOmRldmljZV9jcmVkZW50aWFscyB1cGRhdGU6ZGV2aWNlX2NyZWRlbnRpYWxzIGRlbGV0ZTpkZXZpY2VfY3JlZGVudGlhbHMgY3JlYXRlOmRldmljZV9jcmVkZW50aWFscyByZWFkOnJ1bGVzIHVwZGF0ZTpydWxlcyBkZWxldGU6cnVsZXMgY3JlYXRlOnJ1bGVzIHJlYWQ6cnVsZXNfY29uZmlncyB1cGRhdGU6cnVsZXNfY29uZmlncyBkZWxldGU6cnVsZXNfY29uZmlncyByZWFkOmhvb2tzIHVwZGF0ZTpob29rcyBkZWxldGU6aG9va3MgY3JlYXRlOmhvb2tzIHJlYWQ6YWN0aW9ucyB1cGRhdGU6YWN0aW9ucyBkZWxldGU6YWN0aW9ucyBjcmVhdGU6YWN0aW9ucyByZWFkOmVtYWlsX3Byb3ZpZGVyIHVwZGF0ZTplbWFpbF9wcm92aWRlciBkZWxldGU6ZW1haWxfcHJvdmlkZXIgY3JlYXRlOmVtYWlsX3Byb3ZpZGVyIGJsYWNrbGlzdDp0b2tlbnMgcmVhZDpzdGF0cyByZWFkOmluc2lnaHRzIHJlYWQ6dGVuYW50X3NldHRpbmdzIHVwZGF0ZTp0ZW5hbnRfc2V0dGluZ3MgcmVhZDpsb2dzIHJlYWQ6bG9nc191c2VycyByZWFkOnNoaWVsZHMgY3JlYXRlOnNoaWVsZHMgdXBkYXRlOnNoaWVsZHMgZGVsZXRlOnNoaWVsZHMgcmVhZDphbm9tYWx5X2Jsb2NrcyBkZWxldGU6YW5vbWFseV9ibG9ja3MgdXBkYXRlOnRyaWdnZXJzIHJlYWQ6dHJpZ2dlcnMgcmVhZDpncmFudHMgZGVsZXRlOmdyYW50cyByZWFkOmd1YXJkaWFuX2ZhY3RvcnMgdXBkYXRlOmd1YXJkaWFuX2ZhY3RvcnMgcmVhZDpndWFyZGlhbl9lbnJvbGxtZW50cyBkZWxldGU6Z3VhcmRpYW5fZW5yb2xsbWVudHMgY3JlYXRlOmd1YXJkaWFuX2Vucm9sbG1lbnRfdGlja2V0cyByZWFkOnVzZXJfaWRwX3Rva2VucyBjcmVhdGU6cGFzc3dvcmRzX2NoZWNraW5nX2pvYiBkZWxldGU6cGFzc3dvcmRzX2NoZWNraW5nX2pvYiByZWFkOmN1c3RvbV9kb21haW5zIGRlbGV0ZTpjdXN0b21fZG9tYWlucyBjcmVhdGU6Y3VzdG9tX2RvbWFpbnMgdXBkYXRlOmN1c3RvbV9kb21haW5zIHJlYWQ6ZW1haWxfdGVtcGxhdGVzIGNyZWF0ZTplbWFpbF90ZW1wbGF0ZXMgdXBkYXRlOmVtYWlsX3RlbXBsYXRlcyByZWFkOm1mYV9wb2xpY2llcyB1cGRhdGU6bWZhX3BvbGljaWVzIHJlYWQ6cm9sZXMgY3JlYXRlOnJvbGVzIGRlbGV0ZTpyb2xlcyB1cGRhdGU6cm9sZXMgcmVhZDpwcm9tcHRzIHVwZGF0ZTpwcm9tcHRzIHJlYWQ6YnJhbmRpbmcgdXBkYXRlOmJyYW5kaW5nIGRlbGV0ZTpicmFuZGluZyByZWFkOmxvZ19zdHJlYW1zIGNyZWF0ZTpsb2dfc3RyZWFtcyBkZWxldGU6bG9nX3N0cmVhbXMgdXBkYXRlOmxvZ19zdHJlYW1zIGNyZWF0ZTpzaWduaW5nX2tleXMgcmVhZDpzaWduaW5nX2tleXMgdXBkYXRlOnNpZ25pbmdfa2V5cyByZWFkOmxpbWl0cyB1cGRhdGU6bGltaXRzIGNyZWF0ZTpyb2xlX21lbWJlcnMgcmVhZDpyb2xlX21lbWJlcnMgZGVsZXRlOnJvbGVfbWVtYmVycyByZWFkOmVudGl0bGVtZW50cyByZWFkOmF0dGFja19wcm90ZWN0aW9uIHVwZGF0ZTphdHRhY2tfcHJvdGVjdGlvbiByZWFkOm9yZ2FuaXphdGlvbnMgdXBkYXRlOm9yZ2FuaXphdGlvbnMgY3JlYXRlOm9yZ2FuaXphdGlvbnMgZGVsZXRlOm9yZ2FuaXphdGlvbnMgY3JlYXRlOm9yZ2FuaXphdGlvbl9tZW1iZXJzIHJlYWQ6b3JnYW5pemF0aW9uX21lbWJlcnMgZGVsZXRlOm9yZ2FuaXphdGlvbl9tZW1iZXJzIGNyZWF0ZTpvcmdhbml6YXRpb25fY29ubmVjdGlvbnMgcmVhZDpvcmdhbml6YXRpb25fY29ubmVjdGlvbnMgdXBkYXRlOm9yZ2FuaXphdGlvbl9jb25uZWN0aW9ucyBkZWxldGU6b3JnYW5pemF0aW9uX2Nvbm5lY3Rpb25zIGNyZWF0ZTpvcmdhbml6YXRpb25fbWVtYmVyX3JvbGVzIHJlYWQ6b3JnYW5pemF0aW9uX21lbWJlcl9yb2xlcyBkZWxldGU6b3JnYW5pemF0aW9uX21lbWJlcl9yb2xlcyBjcmVhdGU6b3JnYW5pemF0aW9uX2ludml0YXRpb25zIHJlYWQ6b3JnYW5pemF0aW9uX2ludml0YXRpb25zIGRlbGV0ZTpvcmdhbml6YXRpb25faW52aXRhdGlvbnMiLCJndHkiOiJjbGllbnQtY3JlZGVudGlhbHMifQ.lSEMXLAuoJAZ9tLwtYudizukW0MJwwP03G9fPDUsA8UIi38nCNIDakklnWNxf6J8uO-13O4UTt5XQ1uwdwitdNgPpzoAuGIClvQ_eUHMdMiWIoQdc--UZ11TdNVzeFWzuOja8k4dKjsFZ_ZdwNnZXEswajz1sR1Z2WwPqFB9ztz6vfi5CZqT49iFPlp_leKMhDWYXNCjgWfV0FlFOWIOgnJ5HmYDGKfWp5Hb1CbPB9tzZRZ1dUBQgfawxGxz_Ihx45ewJ4JeEz_NisCDia_gQ1BRR8CUW73eVuKqGxnv1THbJXDZE5PnCET46krmpBzzXdXTWomZaMz6DVhYIFtNJg"
         e1 (tu/first-result
             {:I442/Create_E
              {:Instance
               {:I442/E
                {:X 10
                 :Y (make-object api-token)
                 :Z 20}}}})
         e2 (tu/first-result
             {:I442/Lookup_E
              {cn/id-attr (cn/id-attr e1)}})]
     (is (= 10 (:X e1) (:X e2)))
     (let [y1 (:Y e1) y2 (:Y e2)]
       (is (= api-token (:ApiToken y1) (:ApiToken y2)))
       (is (every?
            true?
            (mapv
             #(and (% y1) (% y2) true)
             [:Email cn/id-attr :AuthDomain :UserName
              :Password :ClientID :ClientSecret]))))
     (is (= 20 (:Z e1) (:Z e2))))))

(defn add-to-x [r n]
  (+ (:X (cn/maybe-deref (first r))) n))

(deftest issue-450-event-alias
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I450
     (entity
      :I450/E
      {:X :Int})
     (event
      :I450/Evt
      {:Y :Int})
     (dataflow
      :I450/Evt
      {:I450/E {:X :I450/Evt.Y}})
     (dataflow
      :I450/Main
      {:I450/Evt {:Y 100} :as :R}
      {:I450/E {:X '(agentlang.test.fixes02/add-to-x :R 5)}}))
   (let [r (tu/first-result
            {:I450/Main {}})]
     (is (cn/instance-of? :I450/E r))
     (is (= 105 (:X r))))))

(deftest issue-479-idempotent-update
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I479
     (entity
      :I479/Bid
      {:meta {:unique [:JobId :UserId]}
       :JobId :Int
       :UserId :Int
       :StatusDate {:type :DateTime
                    :default dt/now}
       :Status {:oneof ["default" "decline" "bid"]
                :default "default"}})
     (dataflow
      :I479/BidForJob
      {:I479/Bid {:JobId? :I479/BidForJob.JobId
                  :UserId? :I479/BidForJob.UserId
                  :Status :I479/BidForJob.Status}}))
   (let [bid1 (tu/first-result
               {:I479/Create_Bid
                {:Instance
                 {:I479/Bid
                  {:JobId 1
                   :UserId 101}}}})
         bid2 (tu/first-result
               {:I479/Create_Bid
                {:Instance
                 {:I479/Bid
                  {:JobId 2
                   :UserId 102}}}})
         j1 (tu/first-result
             {:I479/BidForJob
              {:JobId 1
               :UserId 101
               :Status "bid"}})
         j2 (tu/first-result
             {:I479/BidForJob
              {:JobId 2
               :UserId 102
               :Status "decline"}})
         j3 (tu/first-result
             {:I479/Create_Bid
              {:Instance
               {:I479/Bid
                {:JobId 3
                 :UserId 103}}}})
         j4 (tu/first-result
             {:I479/Create_Bid
              {:Instance
               {:I479/Bid
                {:JobId 1
                 :UserId 102}}}})
         bid1b j1
         bid2b j2]
     (defn inplace-update? [b1 b2 b2-status]
       (is (and (= (cn/id-attr b1) (cn/id-attr b2))
                (= (:JobId b1) (:JobId b2))
                (= (:UserId b1) (:UserId b2))
                (= b2-status (:Status b2))
                (= "default" (:Status b1)))))
     (inplace-update? bid1 bid1b "bid")
     (inplace-update? bid2 bid2b "decline")
     (is (and (= (:JobId j3) 3) (= (:UserId j3) 103)
              (= "default" (:Status j3))))
     (is (and (= (:JobId j4) 1) (= (:UserId j4) 102)
              (= "default" (:Status j4)))))))

(deftest
  issue-485-meta-str
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I485
     (entity
      :I485/Account
      {:Title :String
       :meta {:str :Title}})
     (entity
      :I485/User
      {:FirstName :String
       :LastName :String
       :Email :Email
       :Age :Int
       :meta {:str [:FirstName " " :LastName " <" :Email ">"]}}))
   (let [a1 (cn/make-instance
             {:I485/Account
              {:Title "ABC"}})
         u1 (cn/make-instance
             :I485/User
             {:FirstName "K"
              :LastName "J"
              :Email "kj@gmail.com"
              :Age 34})]
     (is (= "ABC" (cn/instance-str a1)))
     (is (= "K J <kj@gmail.com>" (cn/instance-str u1))))))

(deftest issue-565-list-bug
  (defcomponent :I565
    (record {:I565/R1
             {:X :Int}})
    (record {:I565/R2
             {:R1 {:listof :I565/R1}}})
    (entity {:I565/E
             {:R2 :I565/R2}})
    (dataflow
     :I565/D
     {:I565/E
      {:R2
       {:I565/R2
        {:R1 [{:I565/R1 {:X 100}}
              {:I565/R1 {:X 200}}]}}}}))
  (let [evt (cn/make-instance :I565/D {})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :I565/E result))
    (is (cn/instance-of? :I565/R2 (:R2 result)))
    (is (every? #(cn/instance-of? :I565/R1 %) (get-in [:R2 :R1] result)))))

(deftest issue-568-arg-compilation
  (defcomponent :I568
    (record {:I568/R
             {:K :Any}})
    (dataflow
     :I568/D
     {:I568/R {:K '(identity :I568/D)}}))
  (let [evt (cn/make-instance :I568/D {:I 100})
        result (first (tu/fresult (e/eval-all-dataflows evt)))]
    (is (cn/instance-of? :I568/R result))
    (is (cn/instance-of? :I568/D (:K result)))
    (is (= 100 (get-in result [:K :I])))))

(deftest issue-584-no-results
  (defcomponent :I584
    (entity
     {:I584/E1
      {:X :Int}})
    (entity
     {:I584/E2
      {:E1s {:listof :I584/E1}}})
    (entity
     {:I584/E3
      {:E1s
       {:check
        #(and
          (pos? (count %))
          (every? (fn [x] (cn/instance-of? :I584/E1 x)) %))}}})
    (dataflow
     :I584/Evt1
     {:I584/E1? {} :as :Result}
     {:I584/E2 {:E1s :Result}})
    (dataflow
     :I584/Evt2
     {:I584/E1? {} :as :Result}
     {:I584/E3 {:E1s :Result}}))
  (let [r1 (tu/first-result
            {:I584/Evt1 {}})
        r2 (e/eval-all-dataflows
            {:I584/Evt2 {}})
        xs [1 2 3]
        es (mapv
            #(tu/first-result
              {:I584/Create_E1
               {:Instance
                {:I584/E1
                 {:X %}}}})
            xs)
        r3 (tu/first-result
            {:I584/Evt1 {}})
        r4 (tu/first-result
            {:I584/Evt2 {}})]
    (tu/is-error (constantly r2))
    (is (not (seq (:E1s r1))))
    (let [es (:E1s r3)]
      (is (= (count es) 3))
      (is (= (apply + xs)
             (apply + (mapv :X es)))))
    (is (= (:E1s r3) (:E1s r4)))))

(deftest issue-1062-future-eval
  (defcomponent :I1062
    (entity
     :I1062/E
     {:Id :Identity
      :X :Int})
    (defn i1062f1 [x]
      (future (tu/eval-all-dataflows
               {:I1062/Create_E
                {:Instance
                 {:I1062/E {:X x}}}})))
    (defn i1062f2 [id x]
      (future (tu/eval-all-dataflows
               {:I1062/Update_E
                {:Id id
                 :Data {:X x}}})))
    (dataflow
     :I1062/CreateE
     [:eval '(agentlang.test.fixes02/i1062f1 :I1062/CreateE.X)])
    (dataflow
     :I1062/UpdateE
     [:eval '(agentlang.test.fixes02/i1062f2 :I1062/UpdateE.Id :I1062/UpdateE.X)]))
  (let [r (tu/result {:I1062/CreateE {:X 100}})
        res (fn [r] (first (:result (first @r))))
        e (res r)
        e? (partial cn/instance-of? :I1062/E)]
    (is (e? e))
    (is (cn/same-instance? e (tu/first-result
                              {:I1062/Lookup_E
                               {:Id (:Id e)}})))
    (let [r (tu/result {:I1062/UpdateE {:Id (:Id e) :X 200}})
          e2 (res r)]
      (is (e? e2))
      (is (and (= (:Id e) (:Id e2))
               (= 200 (:X e2))))
      (is (= 200 (:X (tu/first-result
                      {:I1062/Lookup_E
                       {:Id (:Id e)}}))))
      (let [es (tu/result {:I1062/LookupAll_E {}})]
        (is (and (= 1 (count es)) (cn/same-instance? e2 (first es))))))))

(deftest alias-bug-in-where-query
  (defcomponent :Abwq
    (entity :Abwq/A {:Id :Identity :X :Int})
    (dataflow
     :Abwq/FindX
     {:Abwq/A? {:where [:= :Id :Abwq/FindX.A]} :as [:A]}
     :A.X))
  (let [a1 (tu/first-result {:Abwq/Create_A {:Instance {:Abwq/A {:X 100}}}})
        a2 (tu/first-result {:Abwq/Create_A {:Instance {:Abwq/A {:X 200}}}})]
    (is (= 300 (+ (tu/result {:Abwq/FindX {:A (:Id a1)}})
                  (tu/result {:Abwq/FindX {:A (:Id a2)}}))))))

(deftest issue-1234-edn-bug
  (defcomponent :I1234
    (entity :I1234/E {:Id {:type :Int :guid true} :Data :Edn}))
  (let [make-e (fn [id data]
                 (let [e (first
                          (:result
                           (e/evaluate-pattern
                            nil nil
                            {:I1234/Create_E
                             {:Instance
                              {:I1234/E {:Id id :Data data}}}})))]
                   (is (cn/instance-of? :I1234/E e))
                   (is (= (:Data e) data))
                   (is (= data (:Data (tu/first-result {:I1234/Lookup_E {:Id id}}))))))]
    (make-e 1 {:Seed [] :Id "1234"}) ; not a good practice, always quote edn.
    (make-e 2 [:q# {:Seed [] :Id "1234"}])
    (make-e 3 [:q# {:Seed [{:name "aaaa" :age 12} {:name "bbbb" :age 4}] :Id "10028"}])
    (make-e 4 [])
    (make-e 5 [:q# [1 2 3 "abc" :d]])))

(deftest remove-event-with-inference
  (defcomponent :Rewi
    (event :Rewi/Evt {:X :Int})
    (inference :Rewi/Evt {:instructions '(str "event raised with x as: " :Rewi/Evt.X)}))
  (is (cn/event? :Rewi/Evt))
  (is (= '(do
            (component :Rewi)
            (event :Rewi/Evt {:X :Int})
            (inference :Rewi/Evt {:instructions (quote (str "event raised with x as: " :Rewi/Evt.X))}))
         (lr/as-edn :Rewi)))
  (is (cn/remove-event :Rewi/Evt))
  (is (= '(do (component :Rewi)) (lr/as-edn :Rewi)))
  (is (not (cn/event? :Rewi/Evt))))
