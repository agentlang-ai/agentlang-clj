(ns agentlang.test.fixes01
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [clojure.string :as s]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow relationship]]
            [agentlang.evaluator :as e]
            [agentlang.lang.datetime :as dt]
            [agentlang.lang.raw :as raw]
            [agentlang.lang.internal :as li]
            [agentlang.compiler.rule :as rule]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest issue-195
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I195
     (entity {:I195/E1 {:A :Int
                        :B :Int
                        :C :Int
                        :Y :DateTime}})
     (dataflow :I195/K
               {:I195/E1 {:A '(+ 5 :B)
                          :B 10
                          :C '(+ 10 :A)
                          :Y '(agentlang.lang.datetime/now)}})
     (entity {:I195/E2 {:Y :DateTime}})
     (dataflow :I195/KK {:I195/E2 {:Y '(agentlang.lang.datetime/now)}}))
   (let [evt (cn/make-instance :I195/K {})
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I195/E1 r))
     (is (dt/parse-default-date-time (:Y r)))
     (is (= 10 (:B r)))
     (is (= 15 (:A r)))
     (is (= 25 (:C r))))
   (let [evt (cn/make-instance :I195/KK {})
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I195/E2 r))
     (is (dt/parse-default-date-time (:Y r))))))

(defn- assert-transition [attr-names to-attr-vals r]
  (is (= to-attr-vals (mapv #(% r) attr-names))))

(deftest issue-196
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I196
     (entity
      :I196/E1
      {:Id {:type :Int tu/guid true}
       :A :Int
       :B :Int
       :C :Int
       :meta {:unique [:A :C]}}))
   (let [e01 (cn/make-instance :I196/E1 {:Id 1 :A 10 :B 20 :C 30})
         evt1 (cn/make-instance {:I196/Create_E1 {:Instance e01}})
         e02 {:A 10 :B 40 :C 30}
         evt2 (cn/make-instance {:I196/Update_E1 {:Id 1 :Data e02}})
         e03 (cn/make-instance :I196/E1 {:Id 2 :A 20 :B 60 :C 40})
         evt3 (cn/make-instance {:I196/Create_E1 {:Instance e03}})
         e04 {:A 20 :B 40 :C 40}
         evt4 (cn/make-instance {:I196/Update_E1 {:Id 2 :Data e04}})
         results (mapv #(first (tu/fresult (e/eval-all-dataflows %)))
                       [evt1 evt2 evt3 evt4])]
     (is (cn/instance-of? :I196/E1 (first results)))
     (is (cn/instance-of? :I196/E1 (nth results 2)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 40 30] (second results))
       (a [20 40 40] (nth results 3))))))

(deftest issue-206
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I206
     (entity
      :I206/E1
      {:Id {:type :Int tu/guid true}
       :A :Int
       :B :Int
       :C :Int
       :meta {:unique [:A :C]}}))
   (let [e01 (cn/make-instance :I206/E1 {:A 10 :B 20 :C 30 :Id 1})
         evt1 (cn/make-instance {:I206/Create_E1 {:Instance e01}})
         e02 {:A 10 :B 0 :C 30}
         evt2 (cn/make-instance {:I206/Update_E1 {:Id 1 :Data e02}})
         e03 {:A 10 :B 60 :C 30}
         evt3 (cn/make-instance {:I206/Update_E1 {:Id 1 :Data e03}})
         results (mapv #(first (tu/fresult (e/eval-all-dataflows %)))
                       [evt1 evt2 evt3])]
     (is (cn/instance-of? :I206/E1 (first results)))
     (let [a (partial assert-transition [:A :B :C])]
       (a [10 0 30] (second results))
       (a [10 60 30] (nth results 2))))))

(deftest issue-185
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I185
     (entity {:I185/E {:X :Int :Y :Int}})
     (record {:I185/R {:Y :Int}})
     (dataflow :I185/UpdateE
               {:I185/E {tu/q-id-attr (tu/append-id :I185/UpdateE)
                         :X :I185/UpdateE.X
                         :Y :I185/UpdateE.Y}})
     (dataflow [:I185/OnXGt10 :when [:and
                                     [:> :I185/E.X 10]
                                     [:= :I185/E.Y 200]]]
               {:I185/R {:Y '(* 2 :I185/E.Y)}}))
   (let [e (cn/make-instance {:I185/E {:X 10 :Y 1}})
         evt (cn/make-instance {:I185/Create_E {:Instance e}})
         r (tu/fresult (e/eval-all-dataflows evt))
         r1 (first r)
         id (cn/id-attr r1)
         evt (cn/make-instance {:I185/UpdateE {cn/id-attr id :X 20 :Y 100}})
         r2 (tu/fresult (e/eval-all-dataflows evt))
         r3 (first (tu/embedded-results r2))
         evt (cn/make-instance {:I185/UpdateE {cn/id-attr id :X 11 :Y 200}})
         r4 (tu/fresult (e/eval-all-dataflows evt))
         r5 (first (tu/embedded-results r4))
         evt (cn/make-instance {:I185/Lookup_E {cn/id-attr id}})
         r6 (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (nil? (tu/embedded-results r)))
     (is (cn/instance-of? :I185/E r1))
     (is (= 10 (:X r1)))
     (is (= 1 (:Y r1)))
     (let [inst (first r2)]
       (is (cn/instance-of? :I185/E inst))
       (is (= 20 (:X inst))))
     (is (nil? r3))
     (let [inst (first r4)]
       (is (cn/instance-of? :I185/E inst))
       (is (= 11 (:X inst)))
       (is (= 200 (:Y inst))))
     (is (cn/instance-of? :I185/R r5))
     (is (= 400 (:Y r5)))
     (is (cn/instance-of? :I185/E r6))
     (is (= 11 (:X r6)))
     (is (= 200 (:Y r6))))))

(deftest issue-213
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I213
     (entity {:I213/E1 {:X :Int}})
     (entity {:I213/E2 {:E1 {:ref (tu/append-id :I213/E1)}
                        :Y :Int}})
     (record {:I213/R {:Y :Int :Z :Int}})
     (dataflow :I213/UpdateE1
               {:I213/E1 {tu/q-id-attr (tu/append-id :I213/UpdateE1)
                          :X :I213/UpdateE1.X}})
     (dataflow :I213/UpdateE2
               {:I213/E2 {tu/q-id-attr (tu/append-id :I213/UpdateE2)
                          :Y :I213/UpdateE2.Y}})
     (dataflow [:I213/CrossCond
                :when [:and
                       [:> :I213/E1.X 10]
                       [:= :I213/E2.Y 200]]
                :on :I213/E2
                :where [:= :I213/E2.E1 (tu/append-id :I213/E1)]]
               {:I213/R {:Y '(* :I213/E2.Y :I213/E1.X) :Z 1}}))
   (let [e1 (cn/make-instance {:I213/E1 {:X 10}})
         evt (cn/make-instance {:I213/Create_E1 {:Instance e1}})
         r1 (tu/fresult (e/eval-all-dataflows evt))
         e1 (first r1)
         e2 (cn/make-instance {:I213/E2 {:E1 (cn/id-attr e1)
                                         :Y 20}})
         evt (cn/make-instance {:I213/Create_E2 {:Instance e2}})
         r2 (tu/fresult (e/eval-all-dataflows evt))
         e2 (first r2)
         evt (cn/make-instance {:I213/UpdateE1
                                {cn/id-attr (cn/id-attr e1)
                                 :X 20}})
         r3 (tu/fresult (e/eval-all-dataflows evt))
         e3 (first r3)
         evt (cn/make-instance {:I213/UpdateE2
                                {cn/id-attr (cn/id-attr e2)
                                 :Y 200}})
         r4 (tu/fresult (e/eval-all-dataflows evt))
         e4 (first r4)
         r5 (first (tu/embedded-results r4))]
     (is (cn/instance-of? :I213/E2 e2))
     (is (nil? (tu/embedded-results r1)))
     (is (nil? (tu/embedded-results r2)))
     (is (nil? (tu/embedded-results r3)))
     (is (= 20 (:X e3)))
     (is (= 200 (:Y e4)))
     (is (cn/instance-of? :I213/R r5))
     (is (= 1 (:Z r5)))
     (is (= 4000 (:Y r5))))))

(deftest issue-213-no-refs
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I213NR
     (entity {:I213NR/E1 {:X :Int
                          :Z {:type :Int
                              :indexed true}}})
     (entity {:I213NR/E2 {:Y :Int}})
     (record {:I213NR/R {:Y :Int}})
     (dataflow [:I213NR/CrossCond
                :when [:and
                       [:> :I213NR/E1.X 10]
                       [:= :I213NR/E2.Y 200]]
                :on :I213NR/E2
                :where [:= :I213NR/E1.Z 1]]
               {:I213NR/R {:Y '(+ :I213NR/E1.X :I213NR/E2.Y)}}))
   (let [e1 (cn/make-instance {:I213NR/E1 {:X 9 :Z 2}})
         evt (cn/make-instance {:I213NR/Create_E1 {:Instance e1}})
         r1 (tu/fresult (e/eval-all-dataflows evt))
         e1 (first r1)
         e2 (cn/make-instance {:I213NR/E2 {:Y 20}})
         evt (cn/make-instance {:I213NR/Create_E2 {:Instance e2}})
         r2 (tu/fresult (e/eval-all-dataflows evt))
         e2 (first r2)
         e11 (cn/make-instance {:I213NR/E1 {:X 11 :Z 1}})
         evt (cn/make-instance {:I213NR/Create_E1 {:Instance e11}})
         r11 (tu/fresult (e/eval-all-dataflows evt))
         e11 (first r11)
         e22 (cn/make-instance {:I213NR/E2 {:Y 200}})
         evt (cn/make-instance {:I213NR/Create_E2 {:Instance e22}})
         r22 (tu/fresult (e/eval-all-dataflows evt))
         e22 (first r22)
         r (first (tu/embedded-results r22))]
     (is (cn/instance-of? :I213NR/E1 e1))
     (is (nil? (tu/embedded-results r1)))
     (is (cn/instance-of? :I213NR/E2 e2))
     (is (nil? (tu/embedded-results r2)))
     (is (cn/instance-of? :I213NR/R r))
     (is (= 211 (:Y r))))))

(deftest issue-219-event-context
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I219
     (event :I219/Evt {:Y :Int})
     (record :I219/R {:Y :Int
                      :Z :Map})
     (dataflow :I219/Evt
               {:I219/R {:Y '(+ 10 :I219/Evt.Y)
                         :Z :I219/Evt.EventContext}}))
   (let [ctx {:a 1 :b 2}
         evt (cn/make-instance
              {:I219/Evt
               {:Y 100
                :EventContext ctx}})
         r (first (tu/fresult (e/eval-all-dataflows evt)))]
     (is (cn/instance-of? :I219/R r))
     (is (= 110 (:Y r)))
     (is (= ctx (:Z r))))))

(deftest issue-231-rules-operators
  (#?(:clj do
      :cljs cljs.core.async/go)
   (let [r1 (rule/compile-rule-pattern [:= 1 :A.B])
         r2 (rule/compile-rule-pattern
             [:and
              [:= "abc" :A.Name]
              [:> :A.Date "2020-01-20"]])
         r3 (rule/compile-rule-pattern
             [:between "2020-01-20" "2021-01-20" :A.Date])
         r4 (rule/compile-rule-pattern
             [:in [1 2 3] :A.B])
         r5 (rule/compile-rule-pattern
             [:> :X 100])]
     (is (r1 {:A {:B 1}}))
     (is (not (r1 {:A {:B 2}})))
     (is (r2 {:A {:Name "abc"
                  :Date "2021-01-20"}}))
     (is (r3 {:A {:Date "2021-01-10"}}))
     (is (not (r3 {:A {:Date "2021-02-10"}})))
     (is (r4 {:A {:B 2}}))
     (is (not (r4 {:A {:B 4}})))
     (is (r5 {:X 200})))
   (defn i231-extract-x [r]
     (:X r))
   (component :I231)
   (record :I231/R {:X :Int})
   (dataflow
    :I231/E
    {:I231/R {:X :I231/E.X} :as :r}
    [:eval '(agentlang.test.fixes01/i231-extract-x :r) :as :x]
    [:match
     [:> :x 10] 1
     [:= :x 1] 2
     3])
   (is (= 1 (tu/result {:I231/E {:X 101}})))
   (is (= 2 (tu/result {:I231/E {:X 1}})))
   (is (= 3 (tu/result {:I231/E {:X 9}})))))

(deftest issue-241-lowercase-names
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :lcase
     (entity :lcase/e {:x :Int})
     (let [e (cn/make-instance
              {:lcase/e {:x 100}})]
       (is (cn/instance-of? :lcase/e e))
       (is (= 100 (:x e)))))))

(deftest issue-314-compound-exprs-in-records
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :I314
     (record :I314/R {:X :Int
                      :Y '(+ 10 :X)})
     (dataflow :I314/Evt {:I314/R {:X :I314/Evt.X}}))
   (let [r (first
            (tu/fresult
             (e/eval-all-dataflows
              (cn/make-instance
               {:I314/Evt {:X 20}}))))]
     (is (cn/instance-of? :I314/R r))
     (is (= 20 (:X r)))
     (is (= 30 (:Y r))))))

(deftest issue-350-listof
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :LEnt
     (record {:LEnt/E {:X :Int}})
     (entity {:LEnt/F {:Es {:listof :LEnt/E}}})
     (event {:LEnt/MakeF1 {:Xs {:listof :Int}}})
     (event {:LEnt/MakeF2 {:Xs {:listof :Int}}})
     (dataflow
      :LEnt/MakeF1
      [:for-each :LEnt/MakeF1.Xs
       {:LEnt/E {:X :%}}
       :as :ListofEs]
      {:LEnt/F {:Es :ListofEs}})
     (dataflow
      :LEnt/MakeF2
      [:for-each [:LEnt/MakeF2.Xs :as :I]
       {:LEnt/E {:X '(* 10 :I)}}
       :as :ListofEs]
      {:LEnt/F {:Es :ListofEs}}))
   (let [xs [10 20 30 40]
         xs*10 (mapv #(* 10 %) xs)
         evt1 {:LEnt/MakeF1 {:Xs xs}}
         evt2 {:LEnt/MakeF2 {:Xs xs}}
         result1 (e/eval-all-dataflows evt1)
         result2 (e/eval-all-dataflows evt2)
         rs1 (first (tu/fresult result1))
         rs2 (first (tu/fresult result2))]
     (doseq [e (:Es rs1)]
       (is (cn/instance-of? :LEnt/E e))
       (is (some #{(:X e)} xs)))
     (doseq [e (:Es rs2)]
       (is (cn/instance-of? :LEnt/E e))
       (is (some #{(:X e)} xs*10))))))

(deftest for-each-alias
  (defcomponent :Fea
                (entity
                  :Fea/E
                  {:X :Int :Y :Int})
                (dataflow
                  :Fea/Evt1
                  [:for-each
                   :Fea/Evt1.Ys
                   {:Fea/E
                    {:X 10 :Y :%}}])
                (dataflow
                  :Fea/Evt2
                  [:for-each
                   :Fea/Evt2.Es
                   {:Fea/E
                    {:X :%.X :Y :%.Y}}]))
  (let [result1
        (tu/fresult
          (e/eval-all-dataflows
            {:Fea/Evt1
             {:Ys [20 3]}}))
        result2
        (tu/fresult
          (e/eval-all-dataflows
            {:Fea/Evt2
             {:Es [{:X 1 :Y 3}
                   {:X 2 :Y 4}]}}))]
    (doseq [e result1]
      (is (cn/instance-of? :Fea/E e))
      (is (= 10 (:X e)))
      (is (some #{(:Y e)} [20 3])))
    (doseq [e result2]
      (is (cn/instance-of? :Fea/E e))
      (is (some #{(:X e)} [1 2]))
      (is (some #{(:Y e)} [3 4])))))

(deftest issue-959
  (defcomponent :I959
    (entity
     :I959/A
     {:Name :String})

    (entity
     :I959/B
     {:Id {:type :UUID
           tu/guid true
           :default u/uuid-string}
      :Name :String})

    (relationship
     :I959/R
     {:meta {:contains [:I959/A :I959/B]}})

    (dataflow
     :I959/CreateB
     {:I959/A {:Name? "ABC"} :as [:A]}
     {:I959/B {:Name "A B"} :-> [[:I959/R :A]]}))
  (let [a1 (tu/first-result {:I959/Create_A {:Instance {:I959/A {:Name "ABC"}}}})
        b1 (tu/first-result {:I959/CreateB {}})]
    (is (cn/instance-of? :I959/A a1))
    (is (cn/instance-of? :I959/B b1))))

(deftest issue-968-raw-delete
  (defcomponent :I968
    (attribute
     :I968/K
     {:type :Int :indexed true})
    (record
     :I968/A
     {:X :Int :Y :I968/K})
    (entity
     :I968/B
     {:Name :String})
    (event
     :I968/C
     {:X :Int :Y :Int})
    (dataflow
     :I968/C
     {:I968/A
      {:X :I968/C.X :Y :I968/C.Y}}))
  (defn a-def? [df x]
    (= (take 2 x) df))
  (defn any-def? [r df]
    (some (partial a-def? df) r))
  (let [dfs ['(attribute :I968/K) '(record :I968/A)
             '(entity :I968/B) '(event :I968/C) '(dataflow :I968/C)]
        p #(partial any-def? (rest (rest (raw/as-edn :I968))))]
    (is (every? (p) dfs))
    (mapv cn/remove-record [:I968/A :I968/B])
    (defn rem-dfs [names dfs]
      (loop [names names, result dfs]
        (if-let [n (first names)]
          (recur (rest names) (remove #(= n (second %)) result))
          result)))
    (is (every? (p) (rem-dfs [:I968/A :I968/B] dfs)))
    (is (seq (cn/dataflows-for-event :I968/C)))
    (mapv cn/remove-record [:I968/C :I968/K])
    (is (not (seq (cn/dataflows-for-event :I968/C))))
    (let [r (rest (raw/as-edn :I968))]
      (is (= '(component :I968) (first r)))
      (is (nil? (seq (rest r)))))
    (cn/remove-component :I968)
    (is (nil? (raw/as-edn :I968)))))

(deftest issue-967-embedded-quotes-bug
  (defcomponent :I967
    (entity
     :I967/Employee
     {:Name {:type :String tu/guid true}
      :S :String
      :Roles {:listof :Keyword}})
    (event
     :I967/CreateEmployee
     {:Name :String :Roles :Any})
    (dataflow
     :I967/CreateEmployee
     {:I967/Employee
      {:Name :I967/CreateEmployee.Name
       :S :Name
       :Roles :I967/CreateEmployee.Roles}})
    (dataflow
     :I967/CallCreateEmployee
     {:I967/CreateEmployee
      {:Name :I967/CallCreateEmployee.Name
       :Roles [:q# [:admin [:uq# '(keyword :Name)]]]}}))
  (let [e1 (tu/first-result
            {:I967/CallCreateEmployee {:Name "abc"}})]
    (is (cn/instance-of? :I967/Employee e1))
    (is (= [:admin :abc] (:Roles e1)))))

(deftest issue-1009-raw-bugs
  (defcomponent :I1009
    (entity
     :I1009/E
     {:Id {:type :Int tu/guid true}
      :X :Int
      :meta {:unique :X}})
    (entity
     :I1009/F
     {:Id {:type :Int tu/guid true}
      :Y :Int})
    (entity
     :I1009/G
     {:Id {:type :Int tu/guid true}
      :Z :Int})    
    (record :I1009/A {:Z :Int})
    (relationship
     :I1009/R0
     {:meta {:between [:I1009/F :I1009/G]}
      :B :Int})
    (relationship
     :I1009/R1
     {:meta {:contains [:I1009/E :I1009/F]}}))
  (is (= #{:I1009/A} (cn/record-names :I1009)))
  (is (= {:meta {:contains [:I1009/E :I1009/F], :cascade-on-delete true}}
         (cn/fetch-user-schema :I1009/R1)))
  (is (= {:meta {:between [:I1009/F :I1009/G], :cascade-on-delete true}, :B :Int}
         (cn/fetch-user-schema :I1009/R0)))
  (is (= {:unique :X} (cn/fetch-user-meta :I1009/E))))

(deftest issue-i1063-preproc-match-foreach
  (def i1063-ids (atom 10))
  (defn i1063-next-id []
    (swap! i1063-ids inc)
    @i1063-ids)
  (defcomponent :I1063
    (entity
     :I1063/A
     {:Id :Identity})
    (entity
     :I1063/B
     {:Id {:type :Int tu/guid true
           :default i1063-next-id}})
    (entity
     :I1063/Log
     {:Id {:type :Int tu/guid true}
      :Msg :String})
    (relationship
     :I1063/R
     {:meta {:contains [:I1063/A :I1063/B]}})
    (event
     :I1063/Cr1
     {:A :UUID
      :Odd :Boolean})
    (dataflow
     :I1063/Cr1
     {:I1063/A {:Id? :I1063/Cr1.A} :as [:A]}
     [:match :I1063/Cr1.Odd
      true [{:I1063/Log {:Id 1 :Msg "odd"}}
            {:I1063/B {:Id 1}
            :-> [[:I1063/R :A]]}]
      [{:I1063/Log {:Id 2 :Msg "even"}}
       {:I1063/B {:Id 2}
        :-> [[:I1063/R :A]]}]])
    (dataflow
     :I1063/Cr2
     [:for-each {:I1063/A? {}}
      {:I1063/B {}
       :-> [[:I1063/R :%]]}]))
  (let [lookup-log (fn [id]
                     (tu/first-result
                      {:I1063/Lookup_Log
                       {:Id id}}))
        log? (partial cn/instance-of? :I1063/Log)
        log-of-type? #(let [r (lookup-log %1)]
                        (and (log? r) (= %2 (:Msg r))))
        even-log? #(log-of-type? 2 "even")
        odd-log? #(log-of-type? 1 "odd")
        create-a #(tu/first-result
                   {:I1063/Create_A
                    {:Instance
                     {:I1063/A {}}}})
        a (create-a)
        b1 (tu/first-result
            {:I1063/Cr1
             {:A (:Id a) :Odd false}})
        b? (partial cn/instance-of? :I1063/B)
        check-b-rel #(let [bs (filter (fn [b] (s/index-of (li/path-attr b) (:Id %2))) %1)]
                       (is (= 1 (count bs)))
                       (is (b? (first bs))))]
    (is (b? b1))
    (check-b-rel [b1] a)
    (is (even-log?))
    (is (not (odd-log?)))
    (let [b2 (tu/first-result
              {:I1063/Cr1
               {:A (:Id a) :Odd true}})]
      (check-b-rel [b2] a)
      (is (b? b2))
      (is (even-log?))
      (is (odd-log?)))
    (let [a2 (create-a)
          bs (tu/result {:I1063/Cr2 {}})
          chk-id (partial check-b-rel bs)]
      (is (= 2 (count bs)))
      (is (every? b? bs))
      (chk-id a) (chk-id a2))))

(deftest issue-i1070-nested-queries
  (defcomponent :I1070
    (entity
     :I1070/A
     {:Id {:type :Int tu/guid true}
      :X :Int})
    (entity
     :I1070/B
     {:Id {:type :Int tu/guid true}
      :Y :Int})
    (entity
     :I1070/C
     {:Id {:type :Int tu/guid true}
      :Z :Int})
    (relationship
     :I1070/R1
     {:meta {:contains [:I1070/A :I1070/B]}})
    (relationship
     :I1070/R2
     {:meta {:contains [:I1070/B :I1070/C]}})
    (dataflow
     :I1070/CreateB
     {:I1070/B
      {:Id :I1070/CreateB.Id
       :Y '(* :I1070/CreateB.Id 20)}
      :-> [[:I1070/R1 {:I1070/A {:Id? :I1070/CreateB.A}}]]})
    (dataflow
     :I1070/CreateC
     {:I1070/C
      {:Id :I1070/CreateC.Id
       :Z '(* :I1070/CreateC.Id 30)}
      :-> [[:I1070/R2 {:I1070/B? {}
                       :-> [[:I1070/R1? {:I1070/A {:Id? :I1070/CreateC.A}}
                             :I1070/CreateC.B]]}]]})
    (dataflow
     :I1070/FindC
     {:I1070/C? {}
      :-> [[:I1070/R2? {:I1070/B? {}
                        :-> [[:I1070/R1? {:I1070/A {:Id? :I1070/FindC.A}}
                              :I1070/FindC.B]]}
            :I1070/FindC.C]]})
    (dataflow
     :I1070/FindAllC
     {:I1070/C? {}
      :-> [[:I1070/R2? {:I1070/B? {}
                        :-> [[:I1070/R1? {:I1070/A {:Id? :I1070/FindAllC.A}}
                              :I1070/FindAllC.B]]}]]})
    (dataflow
     :I1070/FindAllCWithZ
     {:I1070/B? {}
      :-> [[:I1070/R1? {:I1070/A {:Id? :I1070/FindAllCWithZ.A}}
            :I1070/FindAllCWithZ.B]]
      :as [:B]}
     {:I1070/C? {:Z? :I1070/FindAllCWithZ.Z}
      :-> [[:I1070/R2? :B]]}))
  (let [create-a (fn [id]
                   (tu/first-result
                    {:I1070/Create_A
                     {:Instance
                      {:I1070/A {:Id id :X (* id 10)}}}}))
        a? (partial cn/instance-of? :I1070/A)
        create-b (fn [id a]
                   (tu/result
                    {:I1070/CreateB
                     {:Id id :A a}}))
        b? (partial cn/instance-of? :I1070/B)
        create-c (fn [id a b]
                   (tu/result
                    {:I1070/CreateC
                     {:Id id :A a :B b}}))
        find-c (fn [a b c]
                 (tu/first-result
                  {:I1070/FindC
                   {:A a :B b :C c}}))
        find-all-c (fn [a b]
                     (tu/result
                      {:I1070/FindAllC
                       {:A a :B b}}))
        find-all-c-with-z (fn [a b z]
                            (tu/result
                             {:I1070/FindAllCWithZ
                              {:A a :B b :Z z}}))
        c? (partial cn/instance-of? :I1070/C)
        as (mapv create-a [1 2])]
    (is (and (= 2 (count as)) (every? a? as)))
    (let [b1 (create-b 10 1)
          b2 (create-b 100 1)
          c1 (create-c 20 1 10)
          c2 (create-c 30 1 10)
          c3 (create-c 40 1 100)]
      (is (b? b1))
      (is (c? c1))
      (is (c? c2))
      (is (c? c3))
      (is (cn/same-instance? c1 (find-c 1 10 20)))
      (is (cn/same-instance? c2 (find-c 1 10 30)))
      (is (cn/same-instance? c3 (find-c 1 100 40)))
      (let [cs (mapv #(dissoc % :__instmeta__) (find-all-c 1 10))]
        (is (and (= 2 (count cs)) (every? c? cs)))
        (is (= [c1 c2] (sort #(< (:Id %1) (:Id %2)) cs))))
      (let [cs (find-all-c 1 100)]
        (is (and (= 1 (count cs)) (every? c? cs)))
        (is (cn/same-instance? (first cs) c3)))
      (let [cs (find-all-c-with-z 1 10 (* 30 30))]
        (is (and (= 1 (count cs)) (every? c? cs)))
        (is (cn/same-instance? (first cs) c2))))))

(deftest alias-support-for-between
  (defcomponent :Asfb
    (entity
     :Asfb/A
     {:Id {:type :Int tu/guid true}
      :X :Int})
    (entity
     :Asfb/B
     {:Id {:type :Int tu/guid true}
      :Y :Int})
    (relationship
     :Asfb/R
     {:meta {:between [:Asfb/A :Asfb/B]}})
    (dataflow
     :Asfb/MakeB
     {:Asfb/A {:Id? :Asfb/MakeB.A} :as [:AliasForA]}
     {:Asfb/B {:Id :Asfb/MakeB.B, :Y '(* :Asfb/MakeB.B 10)}
      :-> [[{:Asfb/R {}} :AliasForA]]})
    (dataflow
     :Asfb/FindR
     {:Asfb/R {:A? :Asfb/FindR.A :B? :Asfb/FindR.B}}))
  (let [create-a (fn [id]
                   (tu/first-result
                    {:Asfb/Create_A
                     {:Instance
                      {:Asfb/A {:Id id :X (* id 2)}}}}))
        make-b (fn [id a]
                 (tu/result
                  {:Asfb/MakeB {:A a :B id}}))
        find-r (fn [a b]
                 (tu/first-result
                  {:Asfb/FindR {:A a :B b}}))
        a (create-a 1)]
    (is (cn/instance-of? :Asfb/A a))
    (is (tu/is-error #(find-r 1 2)))
    (let [b (make-b 2 1)
          r (find-r 1 2)]
      (is (cn/instance-of? :Asfb/B b))
      (is (cn/instance-of? :Asfb/R r))
      (is (and (= 1 (:A r)) (= 2 (:B r)))))))
