(ns agentlang.test.syntax
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow relationship]]
            [agentlang.lang.syntax :as ls]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))
      
(deftest syntax-exp
  (is (= {} (ls/introspect {})))
  (let [es01 (ls/exp {ls/exp-fn-tag 'abc ls/exp-args-tag [:X 10 "hello"]})]
    (is (ls/syntax-object? es01))
    (is (ls/exp? es01))
    (is (= 'abc (ls/exp-fn-tag es01)))
    (is (= [:X 10 "hello"] (ls/exp-args-tag es01)))
    (is (= (ls/raw es01) `'(~'abc :X 10 "hello"))))
  (let [es02 (ls/introspect '(+ :X :Y 100))]
    (is (ls/exp? es02))
    (is (= '+ (ls/exp-fn-tag es02)))
    (is (= [:X :Y 100] (ls/exp-args-tag es02)))))

(deftest syntax-upsert
  (let [attrs {:FirstName "Mat"
               :LastName "K"
               :Age 23}
        exp (ls/exp {:fn 'abc :args [:Age 10]})
        es01 (ls/upsert {ls/record-tag :Acme/Person
                         ls/attrs-tag (assoc attrs :X exp)
                         ls/alias-tag :P})
        pat01 (ls/raw es01)
        p (dissoc pat01 ls/alias-tag)
        ir01 (ls/introspect pat01)]
    (is (ls/syntax-object? es01))
    (is (ls/upsert? es01))
    (is (= (ls/record-tag es01) :Acme/Person))
    (is (= (dissoc (ls/attributes es01) :X) attrs))
    (is (= (ls/alias-tag es01) :P))
    (is (= :P (ls/alias-tag pat01)))
    (is (= :Acme/Person (first (keys p))))
    (is (= attrs (dissoc (:Acme/Person p) :X)))
    (is (= ir01 es01))))

(deftest syntax-query
  (let [attrs {:id? "abc123"
               :Age 23}
        es01 (ls/query-upsert {ls/record-tag :Acme/Person
                               ls/attrs-tag attrs
                               ls/alias-tag :P})
        pat01 (ls/raw es01)
        p (dissoc pat01 ls/alias-tag)]
    (is (ls/syntax-object? es01))
    (is (ls/query-upsert? es01))
    (is (= (ls/record-tag es01) :Acme/Person))
    (is (= (ls/attributes es01) attrs))
    (is (= (ls/alias-tag es01) :P))
    (is (= :P (ls/alias-tag pat01)))
    (is (= :Acme/Person (first (keys p))))
    (is (= attrs (:Acme/Person p)))
    (is (= (ls/introspect pat01) es01)))
  (let [where {:where [:or [:>= :Age 20] [:= :Salary 1000]]}
        es02 (ls/query-object {ls/record-tag :Acme/Employee?
                               ls/query-tag where
                               ls/alias-tag :R})
        pat02 (ls/raw es02)
        p (dissoc pat02 ls/alias-tag)]
    (is (ls/syntax-object? es02))
    (is (ls/query-object? es02))
    (is (= (ls/record-tag es02) :Acme/Employee?))
    (is (= (ls/query-pattern es02) where))
    (is (= (ls/alias-tag es02) :R))
    (is (= :Acme/Employee? (first (keys p))))
    (is (= where (:Acme/Employee? p)))
    (is (= :R (ls/alias-tag pat02)))
    (is (= es02 (ls/introspect pat02)))))

(deftest match-for-each
  (let [m (ls/match {ls/value-tag :A.X
                     ls/cases-tag [[1 :B] [2 {:C {:X 100}}] [{:D {:Y 20}}]]
                     ls/alias-tag :R})]
    (is (ls/match? m))
    (is (ls/upsert? (second (second (ls/cases-tag m)))))
    (is (ls/upsert? (first (nth (ls/cases-tag m) 2))))
    (is (= :R (ls/alias-tag m)))
    (let [r (ls/raw m)]
      (is (= r [:match :A.X
                1 :B
                2 {:C {:X 100}}
                {:D {:Y 20}}
                :as :R]))
      (is (= m (ls/introspect r)))))
  (let [fe (ls/for-each {ls/value-tag {:Acme/E {:X? 10}}
                         ls/body-tag [{:Acme/R {:A :Acme/E.X}}]
                         ls/alias-tag :R})]
    (is (ls/for-each? fe))
    (is (ls/query? (ls/value-tag fe)))
    (is (ls/upsert? (first (ls/body-tag fe))))
    (is (= :R (ls/alias-tag fe)))
    (let [r (ls/raw fe)]
      (is (= r [:for-each {:Acme/E {:X? 10}}
                {:Acme/R {:A :Acme/E.X}}
                :as :R]))
      (is (= fe (ls/introspect r))))))

(deftest syntax-try
  (let [t (ls/_try {ls/body-tag {:E {:X? :Find.X}}
                    ls/cases-tag [[:ok {:R {:Y true}}]
                                  [[:error :not-found]
                                   {:R {:Y false}}]]
                    ls/alias-tag :K})
        cases (ls/cases-tag t)]
    (is (ls/try? t))
    (is (ls/query? (ls/body-tag t)))
    (is (= :ok (ffirst cases)))
    (is (ls/upsert? (second (first cases))))
    (is (= [:error :not-found] (first (second cases))))
    (is (ls/upsert? (second (second cases))))
    (is (= :K (ls/alias-tag t)))
    (let [r (ls/raw t)]
      (is (= r [:try {:E {:X? :Find.X}}
                :ok {:R {:Y true}}
                [:error :not-found] {:R {:Y false}}
                :as :K]))
      (is (= t (ls/introspect r))))))

(deftest syntax-query-2
  (let [p (ls/query-object {ls/record-tag :E?
                            ls/query-tag {:where [:>= :X 20]
                                          :order-by [:Y]}})
        q (ls/query {ls/query-tag p ls/alias-tag :R})]
    (is (ls/query? q))
    (is (= p (ls/query-tag q)))
    (is (= :R (ls/alias-tag q)))
    (let [r (ls/raw q)]
      (is (= r [:query
                {:E? {:where [:>= :X 20] :order-by [:Y]}}
                :as :R]))
      (is (= q (ls/introspect r))))))

(deftest syntax-delete
  (let [q (ls/introspect {:E {:id? :Find:E}})
        d (ls/delete {ls/query-upsert-tag q
                      ls/alias-tag :R})
        r (ls/raw d)]
    (is (ls/delete? d))
    (is (= :R (ls/alias-tag d)))
    (is (= r [:delete {:E {:id? :Find:E}} :as :R]))
    (is (= d (ls/introspect r)))))

(deftest syntax-eval
  #_[:call '(agentlang.test.fixes03/i585-f1 :I585/E)
     :check :I585/R :as :Result]
  (let [f (ls/introspect '(f :E))
        e (ls/_eval {ls/exp-tag f
                     ls/check-tag :K
                     ls/alias-tag :R})]
    (is (ls/eval? e))
    (is (ls/exp? (ls/exp-tag e)))
    (is (= f (ls/exp-tag e)))
    (is (= :K (ls/check-tag e)))
    (is (= :R (ls/alias-tag e)))
    (let [r (ls/raw e)]
      (is (= r [:call '(quote (f :E)) :check :K :as :R]))
      (is (= e (ls/introspect r))))))

(deftest query-introspect
  (let [p1 {:Family {:Id? :GetAssessmentAggregate.Family}
            :as [:F]}
        p2 {:Family {:Id? :GetAssessmentAggregate.Family
                     :Name "abc"}
            :as [:F]}
        obj1 (ls/introspect p1)
        obj2 (ls/introspect p2)]
    (is (ls/query? obj1))
    (is (= p1 (ls/raw obj1)))
    (is (ls/query-upsert? obj2))
    (is (= p2 (ls/raw obj2)))))

(deftest relationship-syntax
  (defcomponent :RelSyn
    (entity
     :RelSyn/Member
     {:Email {:type :Email :id true}})
    (entity
     :RelSyn/Family
     {:Name {:type :String :id true}})
    (relationship
     :RelSyn/FamilyMember
     {:meta {:between [:RelSyn/Family :RelSyn/Member]}}))
  (let [pat {:RelSyn/Member
             {:Email :RelSyn/CreateMember.Email}
             :RelSyn/FamilyMember :F
             :as :FM}
        ir (ls/introspect pat)]
    (is (= pat (ls/raw ir)))))

(deftest purge-in-syntax
  (let [ps [[:delete {:Acme.Core/Employee {:Id? 101}}]
            [:delete {:Acme.Core/Employee {:Id? 101}} :as [:E]]
            [:delete :Acme.Core/Employee :purge]
            [:delete :Acme.Core/Employee :purge :as :Es]]
        irs (mapv ls/introspect ps)]
    (is (= ps (mapv ls/raw irs)))))
