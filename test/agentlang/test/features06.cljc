(ns agentlang.test.features06
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.evaluator :as e]
            [agentlang.evaluator.exec-graph :as exg]
            [agentlang.lang
             :refer [component entity event relationship dataflow
                     attribute pattern resolver]]
            [agentlang.lang.raw :as lr]
            [agentlang.lang.syntax :as ls]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest attribute-extension
  (defcomponent :AttrEx
    (entity
     :AttrEx/A
     {:Id {:type :Int :guid true}
      :X :Int})
    (entity
     :AttrEx/B
     {:Id {:type :Int :guid true}
      :Y :Int})
    (relationship
     :AttrEx/R
     {:meta {:between [:AttrEx/A :AttrEx/B]}})
    (attribute
     :AttrEx/AB
     {:extend :AttrEx/A
      :type :AttrEx/B
      :relationship :AttrEx/R})
     (dataflow
      :AttrEx/LookupRByB
      {:AttrEx/R {:B? :AttrEx/LookupRByB.B}}))
  (let [a1 (tu/first-result
            {:AttrEx/Create_A
             {:Instance
              {:AttrEx/A {:Id 1 :X 100 :AB [:q# {:Id 2 :Y 200}]}}}})
        a? (partial cn/instance-type :AttrEx/A)
        b? (partial cn/instance-type :AttrEx/B)
        r? (partial cn/instance-type :AttrEx/R)
        lookup-b #(tu/first-result {:AttrEx/Lookup_B {:Id %}})
        lookup-r-by-b #(tu/first-result {:AttrEx/LookupRByB {:B %}})
        b1 (lookup-b 2)
        r1 (lookup-r-by-b 2)]
    (is (a? a1))
    (is (b? b1))
    (is (r? r1))
    (is (= 1 (:A r1)))))

(deftest pattern-raw-test
  (defcomponent :Prt
    (entity :Prt/E {:Id {:type :Int :guid true} :X :Int})
    (pattern {:Prt/E {:Id 1 :X 100}})
    (entity :Prt/F {:Id {:type :Int :guid true} :Y :Int})
    (pattern {:Prt/E {:Id 2 :X 200}})
    (pattern {:Prt/F {:Id 3 :X 300}}))
  (is (= (lr/as-edn :Prt)
         '(do
            (component :Prt)
            (entity :Prt/E {:Id {:type :Int, :guid true}, :X :Int})
            #:Prt{:E {:Id 1, :X 100}}
            (entity :Prt/F {:Id {:type :Int, :guid true}, :Y :Int})
            #:Prt{:E {:Id 2, :X 200}}
            #:Prt{:F {:Id 3, :X 300}})))
  (let [check-pts (fn [ids]
                    (let [pts (lr/fetch-all-patterns :Prt)]
                      (is (= (count ids) (count pts)))
                      (mapv (fn [id inst]
                              (is (= id (:Id (first (vals inst))))))
                            ids pts)))]
    (check-pts [1 2 3])
    (lr/remove-pattern :Prt 1)
    (check-pts [1 3]))
  (is (= (lr/as-edn :Prt)
         '(do
            (component :Prt)
            (entity :Prt/E {:Id {:type :Int, :guid true}, :X :Int})
            #:Prt{:E {:Id 1, :X 100}}
            (entity :Prt/F {:Id {:type :Int, :guid true}, :Y :Int})
            #:Prt{:F {:Id 3, :X 300}})))
  (lr/replace-pattern :Prt 0 {:Prt/E {:Id 10 :X 1000}})
  (lr/replace-pattern :Prt 1 {:Prt/F {:Id 30 :X 3000}})
  (is (= (lr/as-edn :Prt)
         '(do
            (component :Prt)
            (entity :Prt/E {:Id {:type :Int, :guid true}, :X :Int})
            #:Prt{:E {:Id 10, :X 1000}}
            (entity :Prt/F {:Id {:type :Int, :guid true}, :Y :Int})
            #:Prt{:F {:Id 30, :X 3000}}))))

(deftest map-syntax-bug
  (let [pats [{:Agentlang.Core/Agent {:Name "AgeantOne"}}
              {:Agentlang.Core/Agent {:Name "AgentTwo"}}
              {:Agentlang.Core/Agent
               {:Name "technical-support-agent"
                :Type "chat"
                :LLM "llm01"
                :Chat {:Messages
                       [{:role :system
                         :content (str "You are a support agent for a Camera store. "
                                       "You are supposed to handle technical queries on camera gear "
                                       "that customer may have. "
                                       "Please use the documentation from the appropriate "
                                       "camera manufacturer to answer these queries. "
                                       "If you get a query on the pricing of camera gear, respond with the text: NA")}]}}}]]
    (defcomponent :Msb (doseq [p pats] (pattern p)))
    (let [rs (mapv ls/introspect (lr/fetch-all-patterns :Msb))]
      (is (= pats (mapv ls/raw rs))))))

(deftest await-construct
  (let [a (atom nil), done (atom nil)]
    (defn reset-done! [] (reset! done true))
    (defn reset-a! [inst]
      (if-let [old-inst @a]
        (reset! a (assoc old-inst :X (+ (:X old-inst) (:X inst))))
        (reset! a inst)))
    (defcomponent :Await
      (entity :Await/A {:Id {:type :Int :guid true} :X :Int})
      (dataflow
       [:after :create :Await/A]
       [:eval '(agentlang.test.features06/reset-a! :Instance)]
       :Instance)
      (dataflow
       :Await/Exec
       [:await {:Await/A {:Id :Await/Exec.Id, :X :Await/Exec.X} :as :A}]
       [:eval '(agentlang.test.features06/reset-done!)])
      (resolver
       :Await/R
       {:with-methods
        {:create (fn [inst] (Thread/sleep 2000) inst)}
        :paths [:Await/A]}))
    (u/run-init-fns)
    (is (not @done))
    (is (tu/result {:Await/Exec {:Id 1 :X 100}}))
    (is @done)
    (is (not @a))
    (Thread/sleep 4000)
    (let [a @a]
      (is (cn/instance-of? :Await/A a))
      (is (= 1 (:Id a)))
      (is (= 100 (:X a))))))

(deftest dataflow-suspension
  (defcomponent :DfSusp
    (entity :DfSusp/A {:Id {:type :Int :guid true} :X :Int :Flag {:type :Boolean :default false}})
    (entity :DfSusp/B {:Id {:type :Int :guid true} :Y :Int})
    (dataflow
     :DfSusp/MakeB
     {:DfSusp/A {:Id 1 :X :DfSusp/MakeB.X} :as :A}
     [:match :A.Flag
      true {:DfSusp/B {:Id 1 :Y 100}}
      false {:DfSusp/B {:Id 2 :Y 200}}]))
  (resolver
   :DfSusp/R
   {:with-methods
    {:create #(e/as-suspended (assoc % :Flag (odd? (:X %))))}
    :paths [:DfSusp/A]})
  (u/run-init-fns)
  (let [chkb (fn [id inst]
               (is (cn/instance-of? :DfSusp/B inst))
               (is (= (:Id inst) id))
               (is (= (:Y inst) (* 100 id))))
        susp? (partial cn/instance-of? :Agentlang.Kernel.Eval/Suspension)
        parse-susp-result (fn [r]
                            (is (= :ok (:status r)))
                            (is (cn/instance-of? :DfSusp/A (first (:result r))))
                            (is (seq (:suspension-id r)))
                            [(first (:result r)) (:suspension-id r)])
        [a1 s1] (parse-susp-result (first (tu/eval-all-dataflows {:DfSusp/MakeB {:X 1}})))
        [a2 s2] (parse-susp-result (first (tu/eval-all-dataflows {:DfSusp/MakeB {:X 2}})))]
    (is susp? (tu/first-result {:Agentlang.Kernel.Eval/LoadSuspension {:Id s1}}))
    (is susp? (tu/first-result {:Agentlang.Kernel.Eval/LoadSuspension {:Id s2}}))
    (chkb 1 (tu/first-result {:Agentlang.Kernel.Eval/RestartSuspension
                              {:Id s1 :Value (assoc a1 :Flag true)}}))
    (chkb 2 (tu/first-result {:Agentlang.Kernel.Eval/RestartSuspension
                              {:Id s2 :Value (assoc a2 :Flag false)}}))
    (is (nil? (tu/result {:Agentlang.Kernel.Eval/RestartSuspension
                          {:Id s1 :Value (assoc a1 :Flag false)}})))))

(deftest exec-graph-01
  (defcomponent :EG01
    (entity :EG01/A {:Id :Identity :X :Int})
    (entity :EG01/B {:Id :Identity :Y :Int})
    (event :EG01/E {:Y :Int})
    (dataflow
     :EG01/F
     {:EG01/A {:X :EG01/F.X} :as :A}
     {:EG01/E {:Y :A.X}})
    (dataflow :EG01/E {:EG01/B {:Y :EG01/E.Y}}))
  (let [br (tu/first-result {:EG01/F {:X 100 :EventContext {:ExecId "0001"}}})
        _   (is (cn/instance-of? :EG01/B br))
        g (exg/get-exec-graph "0001")
        _ (is (exg/graph? g))
        nodes (exg/nodes g)
        g2 (second nodes)]
    (is (cn/instance-of? :EG01/F (exg/root-event g)))
    (is (= 2 (count nodes)))
    (is (exg/graph? g2))
    (is (cn/instance-of? :EG01/E (exg/root-event g2)))
    (is (= 1 (count (exg/nodes g2))))
    (is (cn/same-instance? br (first (exg/result (last (exg/nodes g2))))))))
