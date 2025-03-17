(ns agentlang.test.syntax
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow relationship]]
            [agentlang.lang.internal :as li]
            [agentlang.lang.syntax :as ls]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(defn- is-pat
  ([predic pat]
   (let [r (ls/introspect pat)]
     (is (predic r))
     (is (= pat (ls/raw r)))
     r))
  ([pat] (is-pat (constantly true) pat)))

(deftest instances
  (is (= (ls/empty-pattern? (ls/introspect (ls/empty-pattern)))))
  (defcomponent :SynInst
    (entity :SynInst/E {:Id {:type :Int :id true} :X :Int}))
  (let [r (is-pat ls/upsert? {:SynInst/E {:Id 1 :X 100}})]
    (is (= (ls/record-name r) :SynInst/E))
    (is (= (ls/attributes r) {:Id 1 :X 100})))
  (is-pat ls/upsert? {:SynInst/E {:Id 1 :X 100} :as :A})
  (is-pat ls/query-upsert? {:SynInst/E {:Id? 1 :X 200} :as :A})
  (is-pat ls/query? {:SynInst/E {:Id? 1} :as [:A]})
  (is-pat ls/upsert? {:SynInst/E {:Id 2 :X '(* 10 :SynInst/CreateE.X)} :as :A})
  (is-pat ls/query-object? {:SynInst/E {:? {:where [:>= :X 100]}} :as :Result})
  (let [qobj (ls/with-alias [:R] (ls/query-object :SynInst/E {:where [:= :X 1]}))]
    (is (ls/query-object? qobj))
    (is (= {:SynInst/E {:? {:where [:= :X 1]}}, :as [:R]} (ls/raw qobj))))
  (is (= {:SynInst/E {:Id 3, :X 30}, :as :A}
         (ls/raw (ls/with-alias :A (ls/upsert :SynInst/E {:Id 3 :X 30}))))))

(deftest relationships
  (defcomponent :SynRel
    (entity :SynRel/A {:Id {:type :Int :id true} :X :Int})
    (entity :SynRel/B {:Id {:type :Int :id true} :Y :Int})
    (relationship
     :SynRel/AB
     {:meta {:contains [:SynRel/A :SynRel/B]}}))
  (is-pat ls/upsert? {:SynRel/B {:Id 11 :Y 110}
                      :SynRel/AB {:SynRel/A {:Id? :SynRel/CreateB.A}}})
  (is-pat ls/upsert? {:SynRel/B {:Id 11 :Y 110}
                      :SynRel/AB {:SynRel/A {:Id? :SynRel/CreateB.A}}})
  (is-pat ls/query? {:SynRel/B? {}
                     :SynRel/AB? {:SynRel/A {:Id :SynRel/FindB.A}}
                     :as :Bs
                     :case {:not-found {:SynRel/B {:Id 1 :Y 1}}}})
  (let [r (is-pat ls/query? {:SynRel/B? {}
                             :SynRel/AB? {:SynRel/A {:Id :SynRel/FindB.A}}
                             :as :Bs
                             :into {:Y :SynRel/B.Y
                                    :X :SynRel/A.X}
                             :case {:not-found {:SynRel/B {:Id 1 :Y 1}}}})]
    (is (= (ls/record-name r) :SynRel/B))
    (is (= (ls/record-name-tag r) :SynRel/B?))
    (is (= (ls/attributes r) {}))
    (is (= (ls/raw-relationships (ls/relationships r))
           #:SynRel{:AB? #:SynRel{:A {:Id :SynRel/FindB.A}}}))
    (is (= (ls/raw (ls/alias-tag r)) :Bs))
    (is (= (ls/raw-into (ls/into-tag r))
           {:Y :SynRel/B.Y, :X :SynRel/A.X}))
    (is (= (ls/raw-case (ls/case-tag r))
           {:not-found {:SynRel/B {:Id 1 :Y 1}}})))
  (is (= {:SynRel/B? {}
          :SynRel/AB? {:SynRel/A {:Id :SynRel/FindB.A}}
          :as :Rs
          :into {:Y :SynRel/B.Y
                 :X :SynRel/A.X}}
         (ls/raw
          (ls/with-into
            {:Y :SynRel/B.Y
             :X :SynRel/A.X}
            (ls/with-alias
              :Rs
              (ls/query
               :SynRel/B? {}
               [[:SynRel/AB? (ls/upsert :SynRel/A {:Id :SynRel/FindB.A})]])))))))

(deftest for-each
  (defcomponent :SynFe
    (record :SynFe/R {:A :Int})
    (entity :SynFe/E {:Id {:type :Int :id true} :X :Int}))
  (let [r (is-pat ls/for-each? [:for-each {:SynFe/E? {}}
                                {:SynFe/R {:A :%.X}}
                                {:SynFe/R {:A '(* 10 :%.X)}}
                                :as :Rs])]
    (is (= (ls/raw (ls/for-each-value r))
           {:SynFe/E? {}}))
    (is (= (mapv ls/raw (ls/for-each-body r))
           [{:SynFe/R {:A :%.X}}
            {:SynFe/R {:A '(* 10 :%.X)}}]))
    (is (= (ls/raw (ls/alias-tag r)) :Rs)))
  (= [:for-each :Result
      {:SynFe/R {:A :%.A}}
      :as :Rs
      :case {:error #:SynFe{:R {:A 100}}}]
     (ls/raw (ls/with-case
               {:error {:SynFe/R {:A 100}}}
               (ls/with-alias :Rs
                 (ls/for-each :Result (mapv ls/introspect [{:SynFe/R {:A :%.A}}])))))))

(deftest delete
  (defcomponent :SynDel
    (entity :SynDel/E {:Id {:type :Int :id true} :X :Int}))
  (is-pat ls/delete? [:delete {:SynDel/E {:Id? 10}}])
  (let [r (is-pat ls/delete? [:delete {:SynDel/E {:Id? 10}} :as :R])]
    (is (= (ls/raw (ls/query-pattern r))
           {:SynDel/E {:Id? 10}}))
    (is (= (ls/raw (ls/alias-tag r)) :R)))
  (is-pat ls/delete? [:delete :SynDel/E :purge])
  (is (= [:delete {:SynDel/E {:Id? 10}} :as :R]
         (ls/raw
          (ls/with-alias
            :R
            (ls/delete (ls/query :SynDel/E {:Id? 10})))))))

(deftest match
  (defcomponent :SynMatch
    (entity {:SynMatch/E {:Id {:type :Int :id true} :X :Int}}))
  (let [testpat [:match :SynMatch/Event.Flag
                 true {:SynMatch/E {:Id 1 :X 2}}
                 false {:SynMatch/E {:Id 3 :X 4}}
                 {:SynMatch/E {:Id 5 :X 6}}
                 :as :R]
        r (is-pat ls/match? testpat)]
    (is (= (ls/raw (ls/match-value r))
           :SynMatch/Event.Flag))
    (is (= (ls/raw-match-body (ls/match-body r))
           [true  #:SynMatch{:E {:Id 1, :X 2}}
            false #:SynMatch{:E {:Id 3, :X 4}}
            #:SynMatch{:E {:Id 5, :X 6}}]))
    (is (= :R (ls/raw (ls/alias-tag r))))
    (let [r (is-pat ls/match? [:match
                               [:= :SynMatch/E.X 1] {:SynMatch/E {:Id 1 :X 2}}
                               [:> :SynMatch/E.X 1] {:SynMatch/E {:Id 3 :X 4}}
                               {:SynMatch/E {:Id 5 :X 6}}
                               :as :R])]
      (is (= (ls/raw-match-body (ls/match-body r))
             [[:= :SynMatch/E.X 1] #:SynMatch{:E {:Id 1, :X 2}}
              [:> :SynMatch/E.X 1] #:SynMatch{:E {:Id 3, :X 4}}
              #:SynMatch{:E {:Id 5, :X 6}}])))
    (is (= testpat
           (ls/raw
            (ls/with-alias
              :R
              (ls/match
               :SynMatch/Event.Flag
               [[true (ls/upsert :SynMatch/E {:Id 1 :X 2})]
                [false (ls/upsert :SynMatch/E {:Id 3 :X 4})]
                (ls/upsert :SynMatch/E {:Id 5 :X 6})])))))))

(deftest _try
  (defcomponent :SynTry
    (record :SynTry/R {:K :Int :Err :Boolean})
    (event :SynTry/Evt {:X :Int :Y :String}))
  (let [testpat [:try
                 {:SynTry/Evt {:X 100 :Y "hello"}}
                 :not-found {:SynTry/R {:K 0 :Err false}}
                 :error {:SynTry/R {:K 1 :Err true}}
                 :as :R]
        r (is-pat ls/try? testpat)]
    (is (= (mapv ls/raw (ls/try-body r))
           [#:SynTry{:Evt {:X 100, :Y "hello"}}]))
    (is (= (ls/raw-case (ls/case-tag r))
           {:not-found #:SynTry{:R {:K 0, :Err false}},
            :error #:SynTry{:R {:K 1, :Err true}}}))
    (is (= (ls/raw (ls/alias-tag r)) :R))
    (is (= testpat
           (ls/raw
            (ls/with-alias
              :R
              (ls/_try
               [(ls/upsert :SynTry/Evt {:X 100 :Y "hello"})]
               {:not-found (ls/upsert :SynTry/R {:K 0 :Err false})
                :error (ls/upsert :SynTry/R {:K 1 :Err true})})))))))

(deftest call
  (let [testpat [:call '(acme.com/f :A :B.X 100) :as :R]
        r (ls/introspect testpat)]
    (ls/call? r)
    (is (= (second testpat) (ls/function-expression r)))
    (is (= :R (ls/alias-tag r)))
    (is (= testpat (ls/raw r)))
    (is (= testpat (ls/raw (ls/with-alias :R (ls/call '(acme.com/f :A :B.X 100))))))))

(deftest empty-pats
  (let [p? (fn [predic pat]
             (is (ls/empty-pattern? pat))
             (is (predic pat)))]
    (p? ls/for-each? (ls/for-each))
    (p? ls/match? (ls/match))
    (p? ls/try? (ls/_try))
    (p? ls/upsert? (ls/upsert))
    (p? ls/query? (ls/query))
    (p? ls/query-object? (ls/query-object))
    (p? ls/call? (ls/call))
    (p? ls/delete? (ls/delete))))
