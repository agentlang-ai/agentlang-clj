(ns agentlang.test.basic
  (:require #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [agentlang.util :as u]
            [agentlang.api :as api]
            [agentlang.util.hash :as sh]
            [agentlang.store :as store]
            [agentlang.component :as cn]
            [agentlang.lang
             :as ln
             :refer [component attribute event relationship
                     entity record dataflow inference resolver]]
            [agentlang.lang.internal :as li]
            [agentlang.suspension :as sp]
            [agentlang.lang.datetime :as dt]
            [agentlang.lang.relgraph :as rg]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

(deftest basic-eval
  (defcomponent :BasicEval
    (entity
     :BasicEval/E
     {:Id :Identity
      :X :Int
      :Y :Int
      :Z {:type :Int :expr '(* :X :Y)}})
    (dataflow
     :BasicEval/MakeE
     {:BasicEval/E {:X :BasicEval/MakeE.X :Y (quote (+ :X :BasicEval/MakeE.X 1))} :as :E}
     :E)
    (dataflow
     :BasicEval/LookupE
     {:BasicEval/E {:Id? :BasicEval/LookupE.Id} :as [:E]}
     :E)
    (dataflow
     :BasicEval/LookupAllE
     {:BasicEval/E? {}})
    (dataflow
     :BasicEval/LookupEByX1
     {:BasicEval/E {:X? :BasicEval/LookupEByX1.X} :as [:E]}
     :E)
    (dataflow
     :BasicEval/LookupEByX2
     {:BasicEval/E {:? {:where [:= :X :BasicEval/LookupEByX2.X]}}})
    (dataflow
     :BasicEval/UpdateE
     {:BasicEval/E {:Id? :BasicEval/UpdateE.Id
                    :Y (quote (+ :X :X 1))
                    :X :BasicEval/UpdateE.X}})
    (dataflow
     :BasicEval/DeleteById
     [:delete {:BasicEval/E {:Id? :BasicEval/DeleteById.Id}} :as :R]
     :R)
    (dataflow
     :BasicEval/DeleteAll
     [:delete :BasicEval/E :* :as :R]
     :R))
  (let [ev tu/invoke
        cre #(ev {:BasicEval/MakeE {:X %}})
        result (cre 100)
        e? (partial cn/instance-of? :BasicEval/E)
        verify-e (fn [e x]
                   (is (e? e))
                   (is (= x (:X e)))
                   (let [y (+ x x 1)]
                     (is (= y (:Y e)))
                     (is (* x y) (:Z e))))
        lookup-all-e #(let [rs (ev {:BasicEval/LookupAllE {}})]
                        (is (= 2 (count rs)))
                        (is (every? e? rs)))]
    (verify-e result 100)
    (is (cn/same-instance? result (ev {:BasicEval/LookupE {:Id (:Id result)}})))
    (is (cn/same-instance? result (ev {:BasicEval/LookupEByX1 {:X 100}})))
    (let [e2 (cre 200)]
      (verify-e e2 200)
      (is (cn/same-instance? e2 (ev {:BasicEval/LookupEByX1 {:X 200}})))
      (is (cn/same-instance? e2 (first (ev {:BasicEval/LookupEByX2 {:X 200}}))))
      (lookup-all-e)
      (let [r0 (first (ev {:BasicEval/UpdateE {:Id (:Id result) :X 500}}))
            r1 (ev {:BasicEval/LookupE {:Id (:Id result)}})]
        (verify-e r0 500)
        (is (= (:Id result) (:Id r0)))
        (is (cn/same-instance? r0 r1))
        (lookup-all-e)
        (is (= (:Id r0) (:Id (first (ev {:BasicEval/DeleteById {:Id (:Id r0)}})))))
        (is (nil? (ev {:BasicEval/LookupE {:Id (:Id result)}})))
        (is (ev {:BasicEval/DeleteAll {}}))
        (is (nil? (ev {:BasicEval/LookupE {:Id (:Id e2)}})))))))

(deftest dependency
  (defcomponent :Df03
    (record {:Df03/R {:A :Int}})
    (entity {:Df03/E {:X :Int
                      :Y :Int
                      :Z :Int}})
    (event {:Df03/PostE {:R :Df03/R}}))
  (dataflow :Df03/PostE
            {:Df03/E {:X :Df03/PostE.R.A
                      :Z '(+ :X :Y)
                      :Y '(* :X 10)}})
  (let [r (cn/make-instance :Df03/R {:A 100})
        evt {:Df03/PostE {:R r}}
        result (tu/invoke evt)]
    (is (cn/instance-of? :Df03/E result))
    (is (u/uuid-from-string (cn/id-attr result)))
    (is (= 100 (:X result)))
    (is (= 1000 (:Y result)))
    (is (= 1100 (:Z result)))))

(deftest boolean-type
  (defcomponent :Bool
    (entity {:Bool/E {:X :Boolean
                      :Y :Boolean}})
    (event {:Bool/PostE1 {:B :Boolean}})
    (event {:Bool/PostE2 {:B :Boolean}}))
  (dataflow :Bool/PostE1
            {:Bool/E {:X :Bool/PostE1.B
                      :Y true}})
  (dataflow :Bool/PostE2
            {:Bool/E {:X :Bool/PostE2.B
                      :Y false}})
  (let [evt (cn/make-instance :Bool/PostE1 {:B true})
        result (tu/invoke evt)]
    (is (cn/instance-of? :Bool/E result))
    (is (u/uuid-from-string (cn/id-attr result)))
    (is (= true (:X result)))
    (is (= true (:Y result))))
  (let [evt (cn/make-instance :Bool/PostE2 {:B false})
        result (tu/invoke evt)]
    (is (cn/instance-of? :Bool/E result))
    (is (u/uuid-from-string (cn/id-attr result)))
    (is (= false (:X result)))
    (is (= false (:Y result)))))

(deftest self-reference
  (defcomponent :SelfRef
    (entity {:SelfRef/E
             {:Id :Identity
              :X :Int
              :Y :Int
              :Z :Int}})
    (event {:SelfRef/AddToX {:EId :UUID
                             :Y :Int}}))
  (dataflow
   :SelfRef/AddToX
   {:SelfRef/E {:Id? :SelfRef/AddToX.EId
                :X '(+ :SelfRef/E.X :SelfRef/AddToX.Y)
                :Y :SelfRef/AddToX.Y
                :Z 1}})
  (let [e (cn/make-instance :SelfRef/E {:X 100 :Y 200 :Z 300})
        evt (cn/make-instance :SelfRef/Create_E {:Instance e})
        result (tu/invoke evt)]
    (is (cn/instance-of? :SelfRef/E result))
    (is (u/uuid-from-string (:Id result)))
    (is (= 100 (:X result)))
    (is (= 200 (:Y result)))
    (is (= 300 (:Z result)))
    (let [id (:Id result)
          addevt (cn/make-instance :SelfRef/AddToX {:EId id :Y 10})
          inst (first (tu/invoke addevt))]
      (is (cn/instance-of? :SelfRef/E inst))
      (is (u/uuid-from-string (:Id inst)))
      (is (= 110 (:X inst)))
      (is (= 10 (:Y inst)))
      (is (= 1 (:Z inst))))))

(deftest basic-contains
  (defcomponent :BC01
    (entity :BC01/A {:Id {:type :Int :id true} :X :Int})
    (entity :BC01/B {:Id {:type :Int :id true} :Y :Int})
    (entity :BC01/C {:Id {:type :Int :id true} :Z :Int})
    (relationship :BC01/AB {:meta {:contains [:BC01/A :BC01/B]}})
    (relationship :BC01/BC {:meta {:contains [:BC01/B :BC01/C]}})
    (is (= #{:BC01/A} (rg/get-roots (rg/build-graph :BC01))))
    (dataflow
     :BC01/DeleteA
     [:delete {:BC01/A {:Id? :BC01/DeleteA.Id}}])
    (dataflow
     :BC01/LookupAllA
     {:BC01/A? {}})
    (dataflow
     :BC01/CreateB
     {:BC01/B {:Id :BC01/CreateB.Id
               :Y :BC01/CreateB.Y}
      :BC01/AB {:BC01/A {:Id? :BC01/CreateB.A}}})
    (dataflow
     :BC01/LookupEveryB
     {:BC01/B? {}})
    (dataflow
     :BC01/LookupEveryC
     {:BC01/C? {}})
    (dataflow
     :BC01/LookupAllB
     {:BC01/B? {}
      :BC01/AB? {:BC01/A {:Id :BC01/LookupAllB.A}}})
    (dataflow
     :BC01/LookupAllBByX
     {:BC01/B? {}
      :BC01/AB? {:BC01/A {:X :BC01/LookupAllBByX.X}}})
    (dataflow
     :BC01/CreateC
     {:BC01/C {:Id :BC01/CreateC.Id
               :Z :BC01/CreateC.Z}
      :BC01/BC {:BC01/B {:Id :BC01/CreateC.B}
                :BC01/AB? {:BC01/A {:Id :BC01/CreateC.A}}}})
    (dataflow
     :BC01/LookupAllC
     {:BC01/C? {}
      :BC01/BC? {:BC01/B {:Id :BC01/LookupAllC.B}
                 :BC01/AB {:BC01/A {:Id :BC01/LookupAllC.A}}}})
    (dataflow
     :BC01/UpdateC
     {:BC01/C {:Id? :BC01/UpdateC.C
               :Z '(* 10 :BC01/C.Z)}
      :BC01/BC? {:BC01/B {:Id :BC01/UpdateC.B}
                 :BC01/AB {:BC01/A {:Id :BC01/UpdateC.A}}}})
    (dataflow
     :BC01/LookupAllCByZ
     {:BC01/C {:Z? :BC01/LookupAllCByZ.Z}
      :BC01/BC? {:BC01/B {:Id :BC01/LookupAllCByZ.B}
                 :BC01/AB {:BC01/A {:Id :BC01/LookupAllCByZ.A}}}})
    (dataflow
     :BC01/LookupAFromB
     {:BC01/A? {}
      :BC01/AB? {:BC01/B {:Id :BC01/LookupAFromB.B}}})
    (dataflow
     :BC01/LookupAFromC
     {:BC01/A? {}
      :BC01/AB? {:BC01/B {}
                 :BC01/BC {:BC01/C {:Id :BC01/LookupAFromC.C}}}}))
  (let [a? (partial cn/instance-of? :BC01/A)
        b? (partial cn/instance-of? :BC01/B)
        c? (partial cn/instance-of? :BC01/C)
        check-paths (fn [aid b]
                      (is (b? b))
                      (is (= (cn/instance-path b)
                             (li/vec-to-path (vec (concat [:BC01/A aid :BC01/AB :BC01/B] [(:Id b)]))))))
        lookup-bs #(tu/invoke {:BC01/LookupAllB {:A %}})
        lookup-bs-by-x #(tu/invoke {:BC01/LookupAllBByX {:X %}})
        check-bs (fn [aid bs]
                   (is (seq bs))
                   (is (every? b? bs))
                   (doseq [b bs] (check-paths aid b)))
        create-c (fn [id z b a]
                   (tu/invoke
                    {:BC01/CreateC {:Id id :Z z :B b :A a}}))
        lookup-cs #(tu/invoke {:BC01/LookupAllC {:B %1 :A %2}})
        check-cs (fn [n s cs]
                   (is (count cs) n)
                   (is (every? c? cs))
                   (is (= s (apply + (mapv :Z cs)))))
        lookup-all-cs #(tu/invoke {:BC01/LookupAllCByZ {:Z %1 :B %2 :A %3}})
        check-a (fn [id inst]
                  (is (a? inst))
                  (is (= id (:Id inst))))
        update-c #(tu/invoke {:BC01/UpdateC {:C %1 :B %2 :A %3}})]
    (is (a? (tu/invoke {:BC01/Create_A {:Instance {:BC01/A {:Id 1 :X 100}}}})))
    (is (a? (tu/invoke {:BC01/Create_A {:Instance {:BC01/A {:Id 2 :X 300}}}})))
    (is (b? (tu/invoke {:BC01/CreateB {:Id 101 :Y 10 :A 1}})))
    (is (b? (tu/invoke {:BC01/CreateB {:Id 102 :Y 11 :A 1}})))
    (is (b? (tu/invoke {:BC01/CreateB {:Id 103 :Y 12 :A 2}})))
    (check-bs 1 (lookup-bs 1))
    (check-bs 2 (lookup-bs 2))
    (check-bs 1 (lookup-bs-by-x 100))
    (check-bs 2 (lookup-bs-by-x 300))
    (is (c? (create-c 201 30 101 1)))
    (is (c? (create-c 202 40 101 1)))
    (is (c? (create-c 203 40 101 1)))
    (is (c? (create-c 204 50 102 1)))
    (is (c? (create-c 205 60 102 1)))
    (is (c? (create-c 206 70 103 2)))
    (check-cs 3 (+ 40 40 30) (lookup-cs 101 1))
    (check-cs 2 (+ 50 60) (lookup-cs 102 1))
    (check-cs 2 70 (lookup-cs 103 2))
    (check-cs 2 80 (lookup-all-cs 40 101 1))
    (check-cs 1 30 (lookup-all-cs 30 101 1))
    (check-cs 1 60 (lookup-all-cs 60 102 1))
    (check-cs 1 70 (lookup-all-cs 70 103 2))
    (check-cs 1 300 (update-c 201 101 1))
    (check-cs 3 (+ 40 40 300) (lookup-cs 101 1))
    (let [res (tu/invoke {:BC01/LookupAFromB {:B 101}})]
      (is (= 1 (count res)))
      (check-a 1 (first res)))
    (let [res (tu/invoke {:BC01/LookupAFromB {:B 102}})]
      (is (= 1 (count res)))
      (check-a 1 (first res)))
    (let [res (tu/invoke {:BC01/LookupAFromC {:C 206}})]
      (is (= 1 (count res)))
      (check-a 2 (first res)))
    (let [as (tu/invoke {:BC01/LookupAllA {}})]
      (is (= 2 (count as)))
      (is (every? a? as)))
    (let [as (tu/invoke {:BC01/DeleteA {:Id 1}})]
      (is (= 1 (count as)))
      (is (= 1 (:Id (first as)))))
    (let [as (tu/invoke {:BC01/LookupAllA {}})]
      (is (= 1 (count as)))
      (is (every? a? as)))
    (let [bs (tu/invoke {:BC01/LookupEveryB {}})]
      (is (= 1 (count bs)))
      (is (every? b? bs)))
    (let [cs (tu/invoke {:BC01/LookupEveryC {}})]
      (is (= 1 (count cs)))
      (is (every? c? cs)))
    (is (nil? (seq (lookup-bs 1))))
    (check-bs 2 (lookup-bs 2))))

(deftest basic-between
  (defcomponent :BB01
    (entity :BB01/A {:Id {:type :Int :id true} :X :Int})
    (entity :BB01/B {:Id {:type :Int :id true} :Y :Int})
    (entity :BB01/C {:Id {:type :Int :id true} :Z :Int})

    (relationship :BB01/AB {:meta {:between [:BB01/A :BB01/B]}})
    (relationship :BB01/BC {:meta {:contains [:BB01/B :BB01/C]}})

    (dataflow
     :BB01/CreateB
     {:BB01/B {:Id :BB01/CreateB.Id :Y :BB01/CreateB.Y}
      :BB01/AB {:BB01/A {:Id? :BB01/CreateB.A}}})

    (dataflow
     :BB01/CreateC
     {:BB01/C {:Id :BB01/CreateC.Id :Z :BB01/CreateC.Z}
      :BB01/BC {:BB01/B {:Id? :BB01/CreateC.B}}})

    (dataflow
     :BB01/LookupB
     {:BB01/B? {}
      :BB01/AB? {:BB01/A {:Id :BB01/LookupB.A}}})

    (dataflow
     :BB01/LookupC
     {:BB01/C? {}
      :BB01/BC? {:BB01/B {:Id :BB01/LookupC.B}}})

    (dataflow
     :BB01/LookupAllCforA
     {:BB01/C? {}
      :BB01/BC?
      {:BB01/B {}
       :BB01/AB?
       {:BB01/A {:Id :BB01/LookupAllCforA.A}}}})

    (dataflow
     :BB01/LookupAllForA
     {:BB01/C? {}
      :BB01/BC?
      {:BB01/B {}
       :BB01/AB?
       {:BB01/A {:Id :BB01/LookupAllForA.A} :as :A}}
      :into
      {:AX :A.X
       :BY :BB01/B.Y
       :CZ :BB01/C.Z}}))

  (let [create-a #(tu/invoke {:BB01/Create_A
                              {:Instance
                               {:BB01/A {:Id %1 :X %2}}}})
        create-b #(tu/invoke {:BB01/CreateB {:Id %1 :Y %2 :A %3}})
        create-c #(tu/invoke {:BB01/CreateC {:Id %1 :Z %2 :B %3}})

        lookup-b #(tu/invoke {:BB01/LookupB {:A %}})

        lookup-c-for-a #(tu/invoke {:BB01/LookupAllCforA {:A %}})
        lookup-all-for-a #(tu/invoke {:BB01/LookupAllForA {:A %}})

        a? (partial cn/instance-of? :BB01/A)
        b? (partial cn/instance-of? :BB01/B)
        c? (partial cn/instance-of? :BB01/C)
        check-bs (fn [ids bs]
                   (is (= (count bs) (count ids)))
                   (is (every? b? bs))
                   (doseq [b bs]
                     (is (some (fn [id] (= id (:Id b))) ids))))]
    (is (a? (create-a 1 10)))
    (is (a? (create-a 2 20)))
    (is (b? (create-b 11 110 1)))
    (is (b? (create-b 12 120 1)))
    (is (b? (create-b 13 130 2)))

    (is (c? (create-c 20 1010 11)))
    (is (c? (create-c 21 1030 12)))
    (is (c? (create-c 21 1030 13)))

    (is (= 2 (count (lookup-c-for-a 1))))
    (let [rs (lookup-all-for-a 1)]
      (is (= 2 (count rs)))
      (is (= 20 (apply + (mapv :AX rs))))
      (is (= (+ 110 120) (apply + (mapv :BY rs))))
      (is (= (+ 1010 1030) (apply + (mapv :CZ rs)))))
    (check-bs [11 12] (lookup-b 1))
    (check-bs [13] (lookup-b 2))))

(deftest query-operators
  (defcomponent :QO
    (entity
     :QO/E
     {:Id :Identity
      :S :String
      :I :Int})
    (dataflow
     :QO/FindEByS
     {:QO/E
      {:S? [:in :QO/FindEByS.Values]}})
    (dataflow
     :QO/FindEByI
     {:QO/E
      {:I? [:between :QO/FindEByI.Start :QO/FindEByI.End]}}))
  (let [cre #(tu/invoke
              {:QO/Create_E
               {:Instance
                {:QO/E {:S %1 :I %2}}}})
        e? (partial cn/instance-of? :QO/E)
        es (mapv cre ["a" "b" "c" "d" "e"] [1 2 3 4 5])
        chkes (fn [n predic? es]
                (is (= n (count es)))
                (is (every? e? es))
                (when predic?
                  (is (every? identity (mapv predic? es)))))]
    (chkes 5 nil es)
    (chkes 2 (fn [e] (some #{(:S e)} #{"a" "c"})) (tu/invoke {:QO/FindEByS {:Values ["a" "c"]}}))
    (chkes 3 (fn [e] (some #{(:I e)} #{2 3 4})) (tu/invoke {:QO/FindEByI {:Start 2 :End 4}}))))

(deftest handle-cases
  (defcomponent :HC
    (entity
     :HC/E
     {:Id {:type :Int :id true}})
    (record :HC/R {:Id :Int})
    (dataflow
     :HC/FindE
     {:HC/E {:Id? :HC/FindE.Id}
      li/except-tag
      {:not-found {:HC/R {:Id :HC/FindE.Id}}}}))
  (let [r1 (tu/invoke
            {:HC/Create_E {:Instance {:HC/E {:Id 10}}}})
        e? (partial cn/instance-of? :HC/E)
        find-e #(tu/invoke {:HC/FindE {:Id %}})]
    (is (e? r1))
    (is (cn/same-instance? r1 (first (find-e 10))))
    (let [r (find-e 20)]
      (is (cn/instance-of? :HC/R r))
      (is (= 20 (:Id r))))))

(defn ppe-update-y [inst]
  (assoc inst :Y (* (:X inst) 10)))

(deftest pre-post-events
  (defcomponent :PPE
    (entity
     :PPE/E
     {:Id {:type :Int :id true}
      :X :Int
      :Y {:type :Int :optional true}})
    (entity
     :PPE/F
     {:Id {:type :Int :id true}
      :Z :Int})
    (dataflow
     :PPE/LookupF
     {:PPE/F {:Id? :PPE/LookupF.Id} :as [:F]}
     :F)
    (dataflow
     [:before :create :PPE/E]
     {:PPE/F {:Id :Instance.Id
              :Z :Instance.X}}
     [li/call-fn '(agentlang.test.basic/ppe-update-y :Instance)])
    (dataflow
     [:before :update :PPE/E]
     [li/call-fn '(agentlang.test.basic/ppe-update-y :Instance)])
    (dataflow
     [:after :update :PPE/E]
     {:PPE/F {:Id? :Instance.Id
              :Z :Instance.X}})
    (dataflow
     [:after :delete :PPE/E]
     [:delete {:PPE/F {:Id? :Instance.Id}}]))
  (let [cre (fn [id x]
              (tu/invoke
               {:PPE/Create_E
                {:Instance
                 {:PPE/E
                  {:Id id :X x}}}}))
        upe (fn [path new-x]
              (first
               (tu/invoke
                {:PPE/Update_E
                 {:Data {:X new-x}
                  :path path}})))
        allf (fn [] (tu/invoke {:PPE/LookupAll_F {}}))
        findf (fn [id] (tu/invoke {:PPE/LookupF {:Id id}}))
        e? (partial cn/instance-of? :PPE/E)
        f? (partial cn/instance-of? :PPE/F)
        e1 (cre 1 10)
        chkfs (fn [n]
                (let [fs (allf)]
                  (is (= n (count fs)))
                  (is (every? f? fs))))
        chkf (fn [id x]
                (let [f (findf id)]
                  (= (:Z f) x)))]
    (is (e? e1))
    (is (= 100 (:Y e1)))
    (chkfs 1)
    (tu/is-error "duplicate PPE/E.Id" #(cre 1 10))
    (chkfs 1)
    (is (e? (cre 2 20)))
    (chkfs 2)
    (chkf 1 10)
    (chkf 2 20)
    (let [e (upe (li/path-attr e1) 30)]
      (is (e? e))
      (is (= 30 (:X e)))
      (is (= 300 (:Y e))))
    (chkf 1 30)
    (chkf 2 20)
    (is (e? (first (tu/invoke {:PPE/Delete_E {:path (li/path-attr e1)}}))))
    (chkfs 1)
    (is (nil? (findf 1)))
    (chkf 2 20)))

(defn f1 [x y] (+ x y))
(defn f2 [x] (cn/make-instance :Fnc/R {:X x}))

(deftest fncall
  (let [r? (partial cn/instance-of? :Fnc/R)]
    (defcomponent :Fnc
      (record :Fnc/R {:X :Int})
      (dataflow
       :Fnc/CallF1
       [li/call-fn '(agentlang.test.basic/f1 :Fnc/CallF1.X 100) :as :R]
       :R)
      (dataflow
       :Fnc/CallF2
       [li/call-fn '(agentlang.test.basic/f2 :Fnc/CallF2.X) :check :Fnc/R :as :R]
       :R)
      (dataflow
       :Fnc/CallF3
       [li/call-fn '(agentlang.test.basic/f2 :Fnc/CallF3.X) :check (fn [r] (and (r? r) (= 10 (:X r)))) :as :R]
       :R))
    (is (= 105 (tu/invoke {:Fnc/CallF1 {:X 5}})))
    (is (r? (tu/invoke {:Fnc/CallF2 {:X 10}})))
    (is (r? (tu/invoke {:Fnc/CallF3 {:X 10}})))
    (tu/is-error "check for :X failed" #(tu/invoke {:Fnc/CallF3 {:X 11}}))))

(deftest queries
  (defcomponent :Qs
    (entity :Qs/E {:Name {:type :String :id true} :X :Int})
    (dataflow
     :Qs/FindE
     {:Qs/E {:Name? [:in :Qs/FindE.Names]}}))
  (let [es (mapv (fn [n x]
                   (tu/invoke
                    {:Qs/Create_E
                     {:Instance
                      {:Qs/E {:Name n :X x}}}}))
                 ["a" "b" "c" "d" "e"]
                 [1 2 3 4 5])
        e? (partial cn/instance-of? :Qs/E)
        chkes (fn [n sum-x es]
                (is (= n (count es)))
                (is (every? e? es))
                (is (= sum-x (apply + (mapv :X es)))))]
    (chkes 5 15 es)
    (chkes 3 8 (tu/invoke {:Qs/FindE {:Names ["a" "c" "d"]}})))
  ;; TODO: more query tests here
  )

(deftest filter-test
  (defcomponent :Ft
    (entity :Ft/E {:Id {:type :Int :id true} :X :Int})
    (dataflow
     :Ft/FindOddEs
     {:Ft/E? {} :as :Es}
     [:filter [:match :%.X
               odd? true
               false]
      :Es]))
  (let [mke (fn [id x]
              (tu/invoke
               {:Ft/Create_E
                {:Instance
                 {:Ft/E {:Id id :X x}}}}))
        es (mapv mke [1 2 3 4 5] [11 12 13 14 15])
        e? (partial cn/instance-of? :Ft/E)
        chkes (fn [es n]
                (is (= n (count es)))
                (is (every? e? es)))]
    (chkes es 5)
    (let [es (tu/invoke {:Ft/FindOddEs {}})]
      (chkes es 3)
      (is (every? #(odd? (:X %)) es)))))

(deftest dataflow-suspension
  (defcomponent :DfSusp
    (entity :DfSusp/A {:Id {:type :Int :id true} :X :Int :Flag {:type :Boolean :default false}})
    (entity :DfSusp/B {:Id {:type :Int :id true} :Y :Int})
    (dataflow
     :DfSusp/MakeB
     {:DfSusp/A {:Id :DfSusp/MakeB.Id :X :DfSusp/MakeB.X} :as :A}
     [:match :A.Flag
      true {:DfSusp/B {:Id 1 :Y 100}}
      false {:DfSusp/B {:Id 2 :Y 200}}]))
  (resolver
   :DfSusp/R
   {:with-methods
    {:create #(sp/as-suspended (assoc % :Flag (odd? (:X %))))}
    :paths [:DfSusp/A]})
  (u/run-init-fns)
  (let [chkb (fn [id inst]
               (is (cn/instance-of? :DfSusp/B inst))
               (is (= (:Id inst) id))
               (is (= (:Y inst) (* 100 id))))
        susp? (partial cn/instance-of? :Agentlang.Kernel.Eval/Suspension)
        parse-susp-result (fn [r]
                            (is (cn/instance-of? :DfSusp/A (:suspended-with r)))
                            (is (string? (:suspension-id r)))
                            [(:suspended-with r) (:suspension-id r)])
        [a1 s1] (parse-susp-result (tu/invoke {:DfSusp/MakeB {:Id 101 :X 1}}))
        [a2 s2] (parse-susp-result (tu/invoke {:DfSusp/MakeB {:Id 102 :X 2}}))]
    (is susp? (tu/invoke {:Agentlang.Kernel.Eval/LoadSuspension {:Id s1}}))
    (is susp? (tu/invoke {:Agentlang.Kernel.Eval/LoadSuspension {:Id s2}}))
    (chkb 1 (tu/invoke {:Agentlang.Kernel.Eval/RestartSuspension
                        {:Id s1 :Value (assoc a1 :Flag true)}}))
    (chkb 2 (tu/invoke {:Agentlang.Kernel.Eval/RestartSuspension
                        {:Id s2 :Value (assoc a2 :Flag false)}}))
    (is (nil? (tu/invoke {:Agentlang.Kernel.Eval/RestartSuspension
                          {:Id s1 :Value (assoc a1 :Flag false)}})))))

(deftest s3-test
  (defcomponent :AWS
    (record {:AWS/CreateBucketConfig
             {:LocationConstraint :String}})
    (entity {:AWS/S3Bucket
             {:Bucket :String
              :CreateBucketConfiguration :AWS/CreateBucketConfig}})
    (event {:AWS/CreateBucket
            {:Bucket :String
             :Region :String}})
    (dataflow :AWS/CreateBucket
              {:AWS/CreateBucketConfig {:LocationConstraint :AWS/CreateBucket.Region}}
              {:AWS/S3Bucket {:Bucket :AWS/CreateBucket.Bucket
                              :CreateBucketConfiguration :AWS/CreateBucketConfig}}))
  ;(override-test-resolver :AWSS3Resolver :AWS/S3Bucket)
  (let [bucket "ftltestbucket11"
        region "us-east-1"
        evt {:AWS/CreateBucket {:Bucket bucket :Region region}}
        e1 (tu/invoke evt)]
    (is (cn/instance-of? :AWS/S3Bucket e1))
    (is (= bucket (:Bucket e1)))
    (is (cn/instance-of? :AWS/CreateBucketConfig (:CreateBucketConfiguration e1)))
    (is (= region (get-in e1 [:CreateBucketConfiguration :LocationConstraint])))))

(deftest record-in-entity
  (defcomponent :RecordEnt
    (record {:RecordEnt/R {:A :Int}})
    (entity {:RecordEnt/E {:Q :Int
                           :R :RecordEnt/R}})
    (event {:RecordEnt/PostE {:RA :Int}}))
  (dataflow :RecordEnt/PostE
            {:RecordEnt/R {:A :RecordEnt/PostE.RA} :as :R}
            {:RecordEnt/E {:Q 100
                           :R :R}})
  (let [evt {:RecordEnt/PostE {:RA 10}}
        result (tu/invoke evt)]
    (is (cn/instance-of? :RecordEnt/E result))
    (is (u/uuid-from-string (cn/id-attr result)))
    (is (= 100 (:Q result)))
    (is (= 10 (:A (:R result))))))

(deftest hidden-attributes
  (defcomponent :H
    (entity {:H/E {:A :Int
                   :X {:type :String
                       :secure-hash true
                       :write-only true}}}))
  (let [x "this is a secret"
        e (cn/make-instance :H/E {:A 10 :X x})
        evt {:H/Create_E {:Instance e}}
        result (tu/invoke evt)
        r2 (cn/dissoc-write-only result)]
    (is (cn/instance-of? :H/E result))
    (is (sh/crypto-hash-eq? (:X result) x))
    (is (= 10 (:A result)))
    (is (cn/instance-of? :H/E r2))
    (is (not (:X r2)))
    (is (= 10 (:A r2)))))

(deftest alias-for-instances
  (defcomponent :Alias
    (entity {:Alias/E {:X :Int}})
    (entity {:Alias/F {:Y :Int}})
    (record {:Alias/R {:F :Alias/F}})
    (event {:Alias/Evt {:Instance :Alias/E}})
    (dataflow :Alias/Evt
              {:Alias/F {:Y :Alias/Evt.Instance.X} :as :G}
              {:Alias/R {:F :G}}))
  (let [e (cn/make-instance :Alias/E {:X 100})
        evt (cn/make-instance :Alias/Evt {:Instance e})
        result (tu/invoke evt)]
    (is (cn/instance-of? :Alias/R result))
    (is (cn/instance-of? :Alias/F (:F result)))
    (is (= 100 (get-in result [:F :Y])))))

(deftest multi-alias
  (defcomponent :MultiAlias
    (entity {:MultiAlias/E {:X :Int}})
    (entity {:MultiAlias/F {:A :Int :B :Int}})
    (event {:MultiAlias/Evt {:EX1 :Int :EX2 :Int}})
    (dataflow
     :MultiAlias/Evt
     {:MultiAlias/E {:X :MultiAlias/Evt.EX1} :as :E1}
     {:MultiAlias/E {:X :MultiAlias/Evt.EX2} :as :E2}
     {:MultiAlias/F {:A :E1.X :B :E2.X}}))
  (let [evt {:MultiAlias/Evt {:EX1 100 :EX2 10}}
        result (tu/invoke evt)]
    (is (cn/instance-of? :MultiAlias/F result))
    (is (= 100 (:A result)))
    (is (= 10 (:B result)))))

(deftest conditional
  (defcomponent :Cond
    (record {:Cond/R {:X :Int}})
    (event {:Cond/Evt {:I :Int}})
    (dataflow
     :Cond/Evt
     [:match :Cond/Evt.I
      0 {:Cond/R {:X 100}}
      1 {:Cond/R {:X 200}}
      {:Cond/R {:X 300}}
      :as :Result]
     :Result)
    (event {:Cond/EvtWithInst {:R :Cond/R}})
    (dataflow
     :Cond/EvtWithInst
     {:Cond/R {:X 100} :as :R1}
     [:match :Cond/EvtWithInst.R.X
      :R1.X true])
    (dataflow
     :Cond/Evt2
     [:match
      [:= :Cond/Evt2.X 10] {:Cond/R {:X 100}}
      [:>= :Cond/Evt2.X 5] {:Cond/R {:X 50}}
      {:Cond/R {:X 0}}]))
  (let [conditional-event-01
        (fn [i x]
          (let [evt {:Cond/Evt {:I i}}
                result (tu/invoke evt)]
            (is (cn/instance-of? :Cond/R result))
            (is (= x (:X result)))))
        conditional-event-02
        (fn [r predic]
          (let [evt {:Cond/EvtWithInst {:R r}}
                result (tu/invoke evt)]
            (is (predic result))))
        conditional-event-03
        (fn [x]
          (let [r (tu/invoke {:Cond/Evt2 {:X x}})]
            (is (cn/instance-of? :Cond/R r))
            (is (= (* x 10) (:X r)))))]
    (conditional-event-01 0 100)
    (conditional-event-01 1 200)
    (conditional-event-01 3 300)
    (conditional-event-02 (cn/make-instance :Cond/R {:X 200}) nil?)
    (conditional-event-02 (cn/make-instance :Cond/R {:X 100}) true?)
    (conditional-event-03 10)
    (conditional-event-03 5)
    (conditional-event-03 0)))

(deftest conditional-boolean
  (defcomponent :CondBool
    (record {:CondBool/R {:X :Int}})
    (event {:CondBool/Evt {:I :Any}})
    (dataflow :CondBool/Evt
              [:match :CondBool/Evt.I
               true {:CondBool/R {:X 100}}
               false {:CondBool/R {:X 200}}
               {:CondBool/R {:X 300}} :as :R]
              :R))
  (let [chk (fn [i x]
              (let [evt {:CondBool/Evt {:I i}}
                    result (tu/invoke evt)]
                (is (cn/instance-of? :CondBool/R result))
                (is (= x (:X result)))))]
    (chk true 100)
    (chk false 200)
    (chk 1 300)))

(deftest conditional-pattern-list
  (defcomponent :CondPatList
    (entity {:CondPatList/E {:Id {:type :Int :id true} :Y :Int}})
    (record {:CondPatList/R {:X :Int}})
    (event {:CondPatList/Evt {:I :Int}})
    (dataflow :CondPatList/Evt
              [:match :CondPatList/Evt.I
               0 [{:CondPatList/E {:Id 1 :Y 100}}
                  {:CondPatList/R {:X 101}}]
               1 {:CondPatList/R {:X 200}}
               {:CondPatList/R {:X 300}}]))
  (let [evt {:CondPatList/Evt {:I 0}}
        result (tu/invoke evt)
        chkes (fn []
                (let [es (tu/invoke {:CondPatList/LookupAll_E {}})]
                  (is (= 1 (count es)))
                  (is (cn/instance-of? :CondPatList/E (first es)))
                  (is (= 100 (:Y (first es))))))]
    (is (cn/instance-of? :CondPatList/R result))
    (is (= 101 (:X result)))
    (chkes)
    (let [evt {:CondPatList/Evt {:I 1}}
          result (tu/invoke evt)]
      (is (cn/instance-of? :CondPatList/R result))
      (is (= 200 (:X result))))
    (chkes)))

(deftest for-each
  (defcomponent :ForEach
    (entity {:ForEach/E {:X :Int}})
    (record {:ForEach/R {:A :Int}})
    (event {:ForEach/Evt {:I :Int}})
    (dataflow :ForEach/Evt
              {:ForEach/E {:X :ForEach/Evt.I} :as :E1}
              {:ForEach/E {:X '(+ :E1.X 1)}}
              [:for-each {:ForEach/E? {}}
               {:ForEach/R {:A :%.X}}]))
  (let [result (tu/invoke {:ForEach/Evt {:I 10}})
        firstE (first result)
        secondE (second result)]
    (is (= 2 (count result)))
    (is (cn/instance-of? :ForEach/R firstE))
    (is (= 10 (:A firstE)))
    (is (cn/instance-of? :ForEach/R secondE))
    (is (= 11 (:A secondE)))))

(deftest for-each-literal-result
  (defcomponent :ForEachLR
    (entity {:ForEachLR/E {:X :Int}})
    (record {:ForEachLR/R {:A :Int}})
    (event {:ForEachLR/Evt {:I :Int}})
    (dataflow :ForEachLR/Evt
              {:ForEachLR/E {:X :ForEachLR/Evt.I} :as :E1}
              {:ForEachLR/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each :ForEachLR/E?
               {:ForEachLR/R {:A :ForEachLR/E.X}}
               :ForEachLR/R.A]))
  (let [result (tu/invoke {:ForEachLR/Evt {:I 10}})]
    (is (= 2 (count result)))
    (is (= 10 (first result)))
    (is (= 11 (second result)))))

(deftest for-each-basic-query
  (defcomponent :ForEachBQ
    (entity {:ForEachBQ/E {:X {:type :Int
                               :indexed true}}})
    (record {:ForEachBQ/R {:A :Int}})
    (event {:ForEachBQ/Evt {:I :Int}})
    (dataflow :ForEachBQ/Evt
              {:ForEachBQ/E {:X :ForEachBQ/Evt.I} :as :E1}
              {:ForEachBQ/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each {:ForEachBQ/E {:X? 10}}
               {:ForEachBQ/R {:A :ForEachBQ/E.X}}]))
  (let [result (tu/invoke {:ForEachBQ/Evt {:I 10}})
        firstE (first result)]
    (is (= 1 (count result)))
    (is (cn/instance-of? :ForEachBQ/R firstE))
    (is (= 10 (:A firstE)))))

(deftest for-each-with-alias
  (defcomponent :ForEachAlias
    (entity {:ForEachAlias/E {:X :Int}})
    (record {:ForEachAlias/R {:A :Int}})
    (event {:ForEachAlias/Evt {:I :Int}})
    (dataflow :ForEachAlias/Evt
              {:ForEachAlias/E {:X :ForEachAlias/Evt.I} :as :E1}
              {:ForEachAlias/E {:X '(+ :E1.X 1)} :as :E2}
              [:for-each :ForEachAlias/E?
               {:ForEachAlias/R {:A :ForEachAlias/E.X}} :as :L]
              :L))
  (let [result (tu/invoke {:ForEachAlias/Evt {:I 10}})
        firstE (first result)
        secondE (second result)]
    (is (= 2 (count result)))
    (is (cn/instance-of? :ForEachAlias/R firstE))
    (is (= 10 (:A firstE)))
    (is (cn/instance-of? :ForEachAlias/R secondE))
    (is (= 11 (:A secondE)))))

(deftest destructuring-alias
  (defcomponent :DestructuringAlias
    (entity {:DestructuringAlias/E {:X {:type :Int
                                        :indexed true}
                                    :N :String}})
    (record {:Result {:Values :Any}})
    (event {:DestructuringAlias/Evt {:X :Int}})

    (dataflow :DestructuringAlias/Evt
              {:DestructuringAlias/E {:X? :DestructuringAlias/Evt.X} :as [:R1 :R2 :_ :R4 :& :RS]}
              {:DestructuringAlias/Result {:Values [:R1 :R2 :R4 :RS]}}))
  (let [cres [(cn/make-instance :DestructuringAlias/E {:X 1 :N "e01"})
              (cn/make-instance :DestructuringAlias/E {:X 2 :N "e02"})
              (cn/make-instance :DestructuringAlias/E {:X 1 :N "e03"})
              (cn/make-instance :DestructuringAlias/E {:X 1 :N "e04"})
              (cn/make-instance :DestructuringAlias/E {:X 1 :N "e05"})
              (cn/make-instance :DestructuringAlias/E {:X 1 :N "e06"})
              (cn/make-instance :DestructuringAlias/E {:X 1 :N "e07"})]
        es (mapv #(tu/invoke {:DestructuringAlias/Create_E {:Instance %}}) cres)
        _ (is (every? (partial cn/instance-of? :DestructuringAlias/E) es))
        result (:Values (tu/invoke {:DestructuringAlias/Evt {:X 1}}))]
    (is (and (= "e01" (:N (first result)))
             (= 1 (:X (first result)))))
    (is (= "e03" (:N (nth result 1))))
    (is (= 1 (:X (nth result 1))))
    (is (= "e05" (:N (nth result 2))))
    (is (= 1 (:X (nth result 2))))
    (is (= ["e06" "e07"] (map :N (last result))))
    (is (= [1 1] (map :X (last result))))))

(defn- assert-le
  ([n obj xs y]
   (is (cn/instance-of? n obj))
   (is (cn/id-attr obj))
   (is (= xs (:Xs obj)))
   (is (= y (:Y obj))))
  ([obj xs y] (assert-le :L/E obj xs y)))

(deftest listof
  (defcomponent :L
    (entity {:L/E {:Xs {:listof :Int}
                   :Y :Int}})
    (event {:L/MakeE0 {:Xs {:listof :Int} :Y :Int}})
    (dataflow :L/MakeE0
              {:L/E {:Xs :L/MakeE0.Xs :Y :L/MakeE0.Y}})
    (event {:L/MakeE1 {:X1 :Int :X2 :Int :Y :Int}})
    (dataflow :L/MakeE1
              {:L/E {:Xs [:L/MakeE1.X1 :L/MakeE1.X2] :Y :L/MakeE1.Y}})
    (event {:L/MakeE2 {:X :Int :Y :Int}})
    (dataflow :L/MakeE2
              {:L/E {:Xs [(* 100 2) 780 :L/MakeE2.X] :Y :L/MakeE2.Y}})
    (entity {:L/F {:Xs {:listof :Any}
                   :Y :Int}}))
  (let [e (cn/make-instance :L/E {:Xs [1 2 3] :Y 100})
        evt {:L/Create_E {:Instance e}}
        result (tu/invoke evt)]
    (assert-le result [1 2 3] 100))
  (let [evt {:L/MakeE0 {:Xs [10 20 30 40] :Y 1}}
        result (tu/invoke evt)]
    (assert-le result [10 20 30 40] 1))
  (try
    (let [evt {:L/MakeE0 {:Xs [10 "hi"] :Y 1}}
          result (tu/invoke evt)]
      (is (nil? result)))
    (catch #?(:clj Exception :cljs :default) ex
      (is ex)))
  (let [evt {:L/MakeE1 {:X1 10 :X2 20 :Y 1}}
        result (tu/invoke evt)]
    (assert-le result [10 20] 1))
  (let [evt {:L/MakeE2 {:X 10 :Y 90}}
        result (tu/invoke evt)]
    (assert-le result [200 780 10] 90))
  (let [e (cn/make-instance :L/F {:Xs [10 "hi"] :Y 1})
        evt {:L/Create_F {:Instance e}}
        result (tu/invoke evt)]
    (assert-le :L/F result [10 "hi"] 1)))

(deftest embedded-record
  (defcomponent :ER
    (record :ER/R {:X :Int})
    (entity
     :ER/E
     {:Id {:type :Int :id true}
      :R :ER/R})
    (dataflow
     :ER/MakeE
     {:ER/E
      {:Id :ER/MakeE.Id
       :R {:ER/R {:X :ER/MakeE.X}}}}))
  (let [e (tu/invoke {:ER/MakeE {:Id 1 :X 100}})]
    (is (cn/instance-of? :ER/E e))
    (is (cn/instance-of? :ER/R (:R e)))))

(deftest try_
  (defcomponent :Try
    (entity
     :Try/E
     {:Id {:type :Int :id true}
      :X {:type :Int :indexed true}})
    (record
     :Try/R
     {:Y :Int})
    (dataflow
     :Try/Find
     [:try
      {:Try/E {:X? :Try/Find.X}}
      :error {:Try/R {:Y 1}}
      :not-found {:Try/R {:Y 0}}
      :as :Result]
     :Result))
  (let [r1 (tu/invoke {:Try/Find {:X 100}})
        e0 (cn/make-instance {:Try/E {:Id 1 :X 100}})
        e1 (tu/invoke {:Try/Create_E {:Instance e0}})
        e? (partial cn/instance-of? :Try/E)
        _ (is e? e1)
        e2 (first (tu/invoke {:Try/Find {:X 100}}))
        r? (partial cn/instance-of? :Try/R)]
    (is (and (r? r1) (= 0 (:Y r1))))
    (is (e? e2))))

(deftest compound-path-attributes
  (defcomponent :Cpa
    (entity
     :Cpa/A
     {:Id {:type :Int :id true}
      :B :Path
      :C :Path
      :BZ {:type :Int :expr :B.Z}
      :X {:type :Int :expr '(+ :Y 5 :B.Z)}
      :Y :Int
      :Z {:type :Int :expr '(* :X :C.B.Z)}})
    (entity
     :Cpa/B
     {:Id {:type :Int :id true}
      :Z :Int})
     (entity
      :Cpa/C
      {:Id {:type :Int :id true}
       :B :Path})
    (dataflow
     :Cpa/UpdateY
     {:Cpa/A
      {:Id? :Cpa/UpdateY.A
       :Y :Cpa/UpdateY.Y}})
    (dataflow
     :Cpa/UpdateZ
     {:Cpa/B
      {:Id? :Cpa/UpdateZ.B
       :Z :Cpa/UpdateZ.Z}}))
  (let [a? (partial cn/instance-of? :Cpa/A)
        b? (partial cn/instance-of? :Cpa/B)
        c? (partial cn/instance-of? :Cpa/C)
        crb (fn [id z]
              (let [b (tu/invoke
                       {:Cpa/Create_B
                        {:Instance
                         {:Cpa/B {:Id id :Z z}}}})]
                (is (b? b))
                b))
        cra (fn [id bpath cpath y]
              (let [a
                    (tu/invoke
                     {:Cpa/Create_A
                      {:Instance
                       {:Cpa/A {:Id id :B bpath :C cpath :Y y}}}})]
                (is (a? a))
                a))
        crc (fn [id bpath]
              (let [c
                    (tu/invoke
                     {:Cpa/Create_C
                      {:Instance
                       {:Cpa/C {:Id id :B bpath}}}})]
                (is (c? c))
                c))
        [b1 b2] (mapv crb [10 20] [90 91])
        c (crc 100 (li/path-attr b2))
        a (cra 1 (li/path-attr b1) (li/path-attr c) 10)
        chka (fn [a bz cbz]
               (is (= bz (:BZ a)))
               (is (= (+ (:Y a) bz 5) (:X a)))
               (is (= (* (:X a) cbz))))]
    (let [a (first (tu/invoke {:Cpa/UpdateY {:A (:Id a) :Y 20}}))
          _ (chka a 90 91)
          a1 (first (tu/invoke {:Cpa/Lookup_A {:path (li/path-attr a)}}))
          _ (is (= a a1))
          _ (is (b? (first (tu/invoke {:Cpa/UpdateZ {:B (:Id b1) :Z 100}}))))
          a2 (first (tu/invoke {:Cpa/Lookup_A {:path (li/path-attr a)}}))]
      (chka a2 100 91))))

(deftest optional-attributes
  (defcomponent :OptAttr
    (entity {:OptAttr/E {:X :Int
                         :Y {:type :Int
                             :optional true}
                         :S :String}})
    (entity {:OptAttr/F {:X :Int
                         :Y :Int
                         :S :String
                         :meta {:required-attributes [:X :S]}}}))
  (let [e1 (cn/make-instance :OptAttr/E {:X 10 :S "hello"})
        e2 (cn/make-instance :OptAttr/E {:X 1 :Y 2 :S "hi"})]
    (is (cn/instance-of? :OptAttr/E e1))
    (is (= [10 nil "hello"] [(:X e1) (:Y e1) (:S e1)]))
    (is (cn/instance-of? :OptAttr/E e2))
    (is (= [1 2 "hi"] [(:X e2) (:Y e2) (:S e2)])))
  (let [f1 (cn/make-instance :OptAttr/F {:X 10 :S "hello"})
        f2 (cn/make-instance :OptAttr/F {:X 1 :Y 2 :S "hi"})]
    (is (cn/instance-of? :OptAttr/F f1))
    (is (= [10 nil "hello"] [(:X f1) (:Y f1) (:S f1)]))
    (is (cn/instance-of? :OptAttr/F f2))
    (is (= [1 2 "hi"] [(:X f2) (:Y f2) (:S f2)]))))

(deftest optional-record-attribute
  (defcomponent :OptRecAttr
    (record {:OptRecAttr/R {:A :Int}})
    (entity {:OptRecAttr/E {:Q :Int
                            :R {:type :OptRecAttr/R
                                :optional true}}})
    (event {:OptRecAttr/PostE {:Q :Int}}))
  (dataflow :OptRecAttr/PostE
            {:OptRecAttr/E {:Q :OptRecAttr/PostE.Q}})
  (let [evt {:OptRecAttr/PostE {:Q 10}}
        result (tu/invoke evt)]
    (is (cn/instance-of? :OptRecAttr/E result))
    (is (u/uuid-from-string (cn/id-attr result)))
    (is (= 10 (:Q result)))))

(deftest inherits
  (defcomponent :Inherits
    (record {:Inherits/Base {:A {:type :Int
                                 :optional true}
                             :B :Int}})
    (entity {:Inherits/E {:meta {:inherits :Inherits/Base}
                          :X :Int
                          :Y {:type :Int
                              :optional true}
                          :S :String}})
    (record {:Inherits/BaseMeta {:A :Int
                                 :B :Int
                                 :meta {:required-attributes [:B]}}})
    (entity {:Inherits/F {:meta {:inherits :Inherits/BaseMeta}
                          :X :Int
                          :Y {:type :Int
                              :optional true}
                          :S :String}}))
  (let [e1 (cn/make-instance :Inherits/E {:X 10 :S "hello" :A 100 :B 200})
        e2 (cn/make-instance :Inherits/E {:X 1 :Y 2 :S "hi" :B 200})]
    (is (cn/instance-of? :Inherits/E e1))
    (is (= [10 nil "hello" 100 200] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1)]))
    (is (cn/instance-of? :Inherits/E e2))
    (is (= [1 2 "hi" nil 200] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2)])))
  (let [f1 (cn/make-instance :Inherits/F {:X 10 :S "hello" :A 100 :B 200})
        f2 (cn/make-instance :Inherits/F {:X 1 :Y 2 :S "hi" :B 200})]
    (is (cn/instance-of? :Inherits/F f1))
    (is (= [10 nil "hello" 100 200] [(:X f1) (:Y f1) (:S f1) (:A f1) (:B f1)]))
    (is (cn/instance-of? :Inherits/F f2))
    (is (= [1 2 "hi" nil 200] [(:X f2) (:Y f2) (:S f2) (:A f2) (:B f2)]))))

(deftest multi-level-inherits
  (defcomponent :MultiInherits
    (record {:MultiInherits/Base1 {:A {:type :Int
                                       :optional true}
                                   :B :Int}})
    (record {:MultiInherits/Base2 {:C :Int
                                   :meta {:inherits :MultiInherits/Base1}}})
    (entity {:MultiInherits/E {:X :Int
                               :Y {:type :Int
                                   :optional true}
                               :S :String
                               :meta {:inherits :MultiInherits/Base2}}}))
  (let [e1 (cn/make-instance :MultiInherits/E {:X 10 :S "hello" :A 100 :B 200 :C 300})
        e2 (cn/make-instance :MultiInherits/E {:X 1 :Y 2 :S "hi" :B 200 :C 300})]
    (is (cn/instance-of? :MultiInherits/E e1))
    (is (= [10 nil "hello" 100 200 300] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1) (:C e1)]))
    (is (cn/instance-of? :MultiInherits/E e2))
    (is (= [1 2 "hi" nil 200 300] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2) (:C e1)]))))

(deftest multi-level-inherits-meta
  (defcomponent :MultiInheritsMeta
    (record {:MultiInheritsMeta/Base1 {:A :Int
                                       :B :Int
                                       :meta {:required-attributes [:B]}}})
    (record {:MultiInheritsMeta/Base2 {:C :Int
                                       :meta {:inherits :MultiInheritsMeta/Base1}}})
    (entity {:MultiInheritsMeta/E {:X :Int
                                   :Y {:type :Int
                                       :optional true}
                                   :S :String
                                   :meta {:inherits :MultiInheritsMeta/Base2}}}))
  (let [e1 (cn/make-instance :MultiInheritsMeta/E {:X 10 :S "hello" :A 100 :B 200 :C 300})
        e2 (cn/make-instance :MultiInheritsMeta/E {:X 1 :Y 2 :S "hi" :B 200 :C 300})]
    (is (cn/instance-of? :MultiInheritsMeta/E e1))
    (is (= [10 nil "hello" 100 200 300] [(:X e1) (:Y e1) (:S e1) (:A e1) (:B e1) (:C e1)]))
    (is (cn/instance-of? :MultiInheritsMeta/E e2))
    (is (= [1 2 "hi" nil 200 300] [(:X e2) (:Y e2) (:S e2) (:A e2) (:B e2) (:C e1)]))))

(deftest edn-attribute
  (defcomponent :EdnAttr
    (entity :EdnAttr/Form {:Title :String
                           :X :Int
                           :Y {:type :Int
                               :expr '(+ :X 10)}
                           :View :Edn})
    (event :EdnAttr/RenderLoginForm {:Title :String
                                     :X :Int
                                     :Y :Int})
    (dataflow :EdnAttr/RenderLoginForm
              {:EdnAttr/Form
               {:Title :EdnAttr/RenderLoginForm.Title
                :X :EdnAttr/RenderLoginForm.X
                :View [:q#
                       [:div [:p [:b [:uq# :EdnAttr/RenderLoginForm.Title]]]
                        [:div
                         [:label "Username: "]
                         [:textinput [:size [:uq# '(* :EdnAttr/RenderLoginForm.Y 10)]]]]
                        [:div
                         [:label "Password: "]
                         [:password [:size 40]]]
                        [:div
                         [:submit [:text "Login"]]]]]}}))
  (let [result (tu/invoke
                {:EdnAttr/RenderLoginForm
                 {:Title "Login" :X 150 :Y 12}})]
    (is (cn/instance-of? :EdnAttr/Form result))
    (is (= 160 (:Y result)))
    (is (= [:div [:p [:b "Login"]]]
           [(first (:View result))
            (second (:View result))]))
    (is (= 120 (-> (nthrest (:View result) 2)
                   first
                   (nth 2) second second)))))

(deftest patterns-in-attributes
  (defcomponent :PA
    (record {:PA/OnClickEvent {:Source {:type :Int :optional true}}})
    (record {:PA/Position {:X :Int :Y :Int
                           :W :Int :H :Int}})
    (entity {:PA/Button {:Id {:type :Int :id true}
                         :Title :String
                         :Position :PA/Position
                         :OnClick :PA/OnClickEvent}})
    (event {:PA/AddButton {:Title :String :Position :PA/Position :Id :Int}})
    (dataflow :PA/AddButton
              {:PA/Button {:Id :PA/AddButton.Id
                           :Title :PA/AddButton.Title
                           :Position :PA/AddButton.Position
                           :OnClick
                           {:PA/OnClickEvent {:Source :PA/AddButton.Id}}}}))
  (let [pos (cn/make-instance {:PA/Position {:X 10 :Y 10 :W 100 :H 50}})
        add-btn (cn/make-instance {:PA/AddButton {:Title "OK" :Position pos :Id 1}})
        result (tu/invoke add-btn)]
    (is (cn/instance-of? :PA/Button result))
    (is (cn/instance-of? :PA/OnClickEvent (:OnClick result)))))

(deftest format-test
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :FormatTest
     (entity {:FormatTest/E
              {:Phone {:type :String
                       :format "^(1\\s?)?(\\d{3}|\\(\\d{3}\\))[\\s\\-]?\\d{3}[\\s\\-]?\\d{4}$"}
               :DOB {:type :String
                     :format "^((19|2[0-9])[0-9]{2})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$"}}})
     (is (cn/instance-of?
          :FormatTest/E
          (cn/make-instance
           {:FormatTest/E
            {:Phone "(555)555-5555"
             :DOB "1999-03-20"}})))
     (tu/is-error #(cn/make-instance
                    {:FormatTest/E
                     {:Phone "(555)555-5555"
                      :DOB "1877-09-01"}})))))

(deftest numeric-types
  (#?(:clj do
      :cljs cljs.core.async/go)
   (defcomponent :NT
     (entity
      :NT/E
      {:X :Int
       :Y :Int64
       :Z :BigInteger
       :A :Float
       :B :Double
       :C :Decimal
       :D :DateTime})
     (let [bi 8993993938884848858996996
           f 1.2
           d "90.8"
           dt "2014-03-14T12:34:20.000000"
           e (cn/make-instance
              {:NT/E
               {:X 10
                :Y #?(:clj Long/MAX_VALUE
                      :cljs Number.MAX_VALUE)
                :Z bi
                :A f
                :B f
                :C d
                :D dt}})]
       (is (cn/instance-of? :NT/E e))
       (is (= 90.8M (:C e)))
       (is (= (float f) (:A e)))
       (is (= (double f) (:B e)))
       (is (= bi (:Z e)))
       (is (= dt (:D e)))))))

(deftest inheritance-type-check
  (defcomponent :Itc
    (record
     :Itc/Base
     {:X :Int})
    (record
     :Itc/Child1
     {:meta {:inherits :Itc/Base}
      :Y :Int})
    (record
     :Itc/Child2
     {:meta {:inherits :Itc/Base}
      :Z :Int})
    (record
     :Itc/Child3
     {:meta {:inherits :Itc/Child1}
      :A :Int})
    (entity
     :Itc/E
     {:Vals {:listof :Itc/Base}}))
  (let [v1 (cn/make-instance {:Itc/Child1 {:X 1 :Y 2}})
        v2 (cn/make-instance {:Itc/Child2 {:X 10 :Z 20}})
        v3 (cn/make-instance {:Itc/Child3 {:X 9 :Y 8 :A 7}})
        e (cn/make-instance {:Itc/E
                             {:Vals [v1 v2 v3]}})
        result (tu/invoke
                (cn/make-instance
                 {:Itc/Create_E
                  {:Instance e}}))]
    (is (cn/instance-of? :Itc/E result))
    (every? #(or (cn/instance-of? :Itc/Child1 %)
                 (cn/instance-of? :Itc/Child2 %)
                 (cn/instance-of? :Itc/Child3 %))
            (:Vals result))
    (every? (partial cn/instance-of? :Itc/Base) (:Vals result))))

#?(:clj
   (deftest data-generation-and-validation-for-listof []
     (component :RefCheck2)
     (testing "list-of"
       (testing "string"
         (entity {:RefCheck2/E1 {:AIdId {:listof :String}}})

         (let [data (tu/generate-data :RefCheck2/E1)]
           (is (seq? data))
           (is (every? coll? (mapv :RefCheck2/E1.AIdId data)))
           (is (every? #(every? string? %) (map :RefCheck2/E1.AIdId data)))
           (is (every? uuid? (mapv (tu/append-id :RefCheck2/E1) data)))))

       (testing "unique string"
         (entity {:RefCheck2/E2 {:AIdId {:listof :String}}})

         (let [data (tu/generate-data :RefCheck2/E2)]
           (is (seq? data))
           (is (every? coll? (mapv :RefCheck2/E2.AIdId data)))
           (is (every? #(every? string? %) (map :RefCheck2/E2.AIdId data)))
           (is (every? uuid? (mapv (tu/append-id :RefCheck2/E2) data)))))

       (testing "int"
         (entity {:RefCheck2/E3 {:AIdId {:listof :Int}}})

         (let [data (tu/generate-data :RefCheck2/E3)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E3.AIdId data)))
           (is (every? #(every? int? %) (map :RefCheck2/E3.AIdId data)))))

       (testing "keyword"
         (entity {:RefCheck2/E4 {:AIdId {:listof :Keyword}}})

         (let [data (tu/generate-data :RefCheck2/E4)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E4.AIdId data)))
           (is (every? #(every? keyword? %) (map :RefCheck2/E4.AIdId data)))))

       (testing "path"
         (entity {:RefCheck2/E5 {:AIdId {:listof :Path}}})

         (let [data (tu/generate-data :RefCheck2/E5)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E5.AIdId data)))
           (is (every? (fn [val]
                         (every? #(or (keyword? %) (string? %)) val))
                       (map :RefCheck2/E5.AIdId data)))))

       (testing "float"
         (entity {:RefCheck2/E6 {:AIdId {:listof :Float}}})

         (let [data (tu/generate-data :RefCheck2/E6)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E6.AIdId data)))
           (is (every? #(every? float? %) (map :RefCheck2/E6.AIdId data)))))

       (testing "double"
         (entity {:RefCheck2/E7 {:AIdId {:listof :Double}}})

         (let [data (tu/generate-data :RefCheck2/E7)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E7.AIdId data)))
           (is (every? #(every? double? %) (map :RefCheck2/E7.AIdId data)))))

       (testing "decimal"
         (entity {:RefCheck2/E8 {:AIdId {:listof :Decimal}}})

         (let [data (tu/generate-data :RefCheck2/E8)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E8.AIdId data)))
           (is (every? #(every? decimal? %) (map :RefCheck2/E8.AIdId data)))))

       (testing "boolean"
         (entity {:RefCheck2/E9 {:AIdId {:listof :Boolean}}})

         (let [data (tu/generate-data :RefCheck2/E9)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E9.AIdId data)))
           (is (every? #(every? boolean? %) (map :RefCheck2/E9.AIdId data)))))

       (testing "any"
         (entity {:RefCheck2/E10 {:AIdId {:listof :Any}}})

         (let [data (tu/generate-data :RefCheck2/E10)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E10.AIdId data)))
           (is (every? #(every? any? %) (map :RefCheck2/E10.AIdId data)))))

       (testing "map"
         (entity {:RefCheck2/E11 {:AIdId {:listof :Map}}})

         (let [data (tu/generate-data :RefCheck2/E11)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E11.AIdId data)))
           (is (every? #(every? map? %) (map :RefCheck2/E11.AIdId data)))))

       (testing "date"
         (entity {:RefCheck2/E12 {:AIdId {:listof :Date}}})

         (let [data (tu/generate-data :RefCheck2/E12)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E12.AIdId data)))
           (is (every? (fn [val]
                         (every? #(instance? java.time.LocalDate %) val))
                       (map :RefCheck2/E12.AIdId data)))))

       (testing "date-time"
         (entity {:RefCheck2/E13 {:AIdId {:listof :DateTime}}})

         (let [data (tu/generate-data :RefCheck2/E13)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E13.AIdId data)))
           (is (every? (fn [val]
                         (every? #(instance? java.time.LocalDateTime %) val))
                       (map :RefCheck2/E13.AIdId data)))))

       (testing "time"
         (entity {:RefCheck2/E14 {:AIdId {:listof :Time}}})

         (let [data (tu/generate-data :RefCheck2/E14)]
           (is (seq? data))
           (is (every? coll? (map :RefCheck2/E14.AIdId data)))
           (is (every? (fn [val]
                         (every? #(instance? java.time.LocalTime %) val))
                       (map :RefCheck2/E14.AIdId data))))))))

#?(:clj
   (deftest data-generation-and-validation-for-agentlang-types []
     (component :RefCheck3)
     (testing "agentlang type"
       (testing "entity"
         (entity {:RefCheck3/E
                  {:X :Int
                   :Y :Int}})

         (let [data (tu/generate-data :RefCheck3/E)]
           (is (seq? data))
           (is (every? #(int? (:RefCheck3/E.X %)) data))
           (is (every? #(int? (:RefCheck3/E.Y %)) data))
           (is (every? #(uuid? ((tu/append-id :RefCheck3/E) %)) data))))

       (testing "record"
         (record {:RefCheck3/E
                  {:X :Int
                   :Y :Int}}))

       (let [data (tu/generate-data :RefCheck3/E)]
         (is (seq? data))
         (is (every? #(int? (:RefCheck3/E.X %)) data))
         (is (every? #(int? (:RefCheck3/E.Y %)) data))
         (is (every? #(nil? ((tu/append-id :RefCheck3/E) %)) data))))

     (testing "Reference to an entity's attribute"
       (entity {:RefCheck3/E1 {:A :Int}})
       (entity {:RefCheck3/E2 {:AId {:ref (tu/append-id :RefCheck3/E1)}
                               :X :Int}})
       (entity {:RefCheck3/E3 {:AIdId {:ref :RefCheck3/E2.AId}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(uuid? (:RefCheck3/E3.AIdId %)) data))))

     (testing "Reference to an entity"
       (entity {:RefCheck3/E1 {:A :Int}})
       (entity {:RefCheck3/E2 {:AId {:ref (tu/append-id :RefCheck3/E1)}
                               :X :Int}})
       (entity {:RefCheck3/E3 {:AIdId {:type :RefCheck3/E1}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(instance? clojure.lang.PersistentArrayMap (:RefCheck3/E3.AIdId %)) data))))

     (testing "Reference to a record"
       (record {:RefCheck3/E1 {:A :Int}})
       (entity {:RefCheck3/E2 {:AId {:ref :RefCheck3/E1.A}
                               :X :Int}})
       (entity {:RefCheck3/E3 {:AIdId {:type :RefCheck3/E1}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(instance? clojure.lang.PersistentArrayMap (:RefCheck3/E3.AIdId %)) data))))

     (testing "oneof"
       (entity {:RefCheck3/E3 {:AIdId {:oneof ["diesel", "petrol", "cng"]}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(string? (:RefCheck3/E3.AIdId %)) data))))

     (testing "string without format"
       (entity {:RefCheck3/E3 {:AIdId {:type :String}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(string? (:RefCheck3/E3.AIdId %)) data))))

     (testing "keyword"
       (entity {:RefCheck3/E3 {:AIdId {:type :Keyword}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(keyword? (:RefCheck3/E3.AIdId %)) data))))

     (testing "path"
       (entity {:RefCheck3/E3 {:AIdId {:type :Path}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(or (keyword? %) (string? %)) (map :RefCheck3/E3.AIdId data)))))

     (testing "int64"
       (entity {:RefCheck3/E3 {:AIdId {:type :Int64}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(int? (:RefCheck3/E3.AIdId %)) data))))

     (testing "bigInteger"
       (entity {:RefCheck3/E3 {:AIdId {:type :BigInteger}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(integer? (:RefCheck3/E3.AIdId %)) data))))

     (testing "float"
       (entity {:RefCheck3/E3 {:AIdId {:type :Float}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(float? (:RefCheck3/E3.AIdId %)) data))))

     (testing "double"
       (entity {:RefCheck3/E3 {:AIdId {:type :Double}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(double? (:RefCheck3/E3.AIdId %)) data))))

     (testing "decimal"
       (entity {:RefCheck3/E3 {:AIdId {:type :Decimal}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(decimal? (:RefCheck3/E3.AIdId %)) data))))

     (testing "boolean"
       (entity {:RefCheck3/E3 {:AIdId {:type :Boolean}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(boolean? (:RefCheck3/E3.AIdId %)) data))))

     (testing "date"
       (entity {:RefCheck3/E3 {:AIdId {:type :Date}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(instance? java.time.LocalDate %) (map :RefCheck3/E3.AIdId data)))))

     (testing "dateTime"
       (entity {:RefCheck3/E3 {:AIdId {:type :DateTime}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(instance? java.time.LocalDateTime %) (map :RefCheck3/E3.AIdId data)))))

     (testing "time"
       (entity {:RefCheck3/E3 {:AIdId {:type :Time}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(instance? java.time.LocalTime %) (map :RefCheck3/E3.AIdId data)))))

     (testing "any"
       (entity {:RefCheck3/E3 {:AIdId {:type :Any}}})

       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(any? (:RefCheck3/E3.AIdId %)) data))))

     (testing "map"
       (entity {:RefCheck3/E3 {:AIdId {:type :Map}}})
       (let [data (tu/generate-data :RefCheck3/E3)]
         (is (seq? data))
         (is (every? #(map? (:RefCheck3/E3.AIdId %)) data))))))

(deftest components-in-model
  (component :Abc.X)
  (component :Abc.Y)
  (component :Bcd.Z)
  (is (= #{:Abc.X :Abc.Y} (set (cn/component-names :Abc))))
  (is (= #{:Bcd.Z} (set (cn/component-names :Bcd)))))

(deftest attribute-meta
  (defcomponent :AMeta
    (attribute
     :AMeta/Name
     {:type :String
      :meta {:instance {:ui {:Agentlang.Fx/Input {}}}}}))
  (is (= {:unique false, :immutable false, :type :Agentlang.Kernel.Lang/String}
         (dissoc (cn/find-attribute-schema :AMeta/Name) :check)))
  (is (= {:instance {:ui #:Agentlang.Fx{:Input {}}}}
         (cn/fetch-meta :AMeta/Name))))

(deftest model-spec
  (let [spec {:name :crm :version "1.0.2"
              :agentlang-version "current"
              :components [:Crm.Core :Crm.Sales]}]
    (ln/model spec)
    (is (= spec (cn/fetch-model :crm)))
    (is (= "1.0.2" (cn/model-version :crm)))))

#?(:clj
   (deftest multi-threaded-transactions
     (defcomponent :Mtt01
       (entity
        :Mtt01/E
        {:Id {:type :Int :id true}
         :X :Int
         :Y :String})
       (dataflow
        :Mtt01/DeleteE
        [:delete {:Mtt01/E {:Id? :Mtt01/DeleteE.Id}}]))
     (let [create-e (fn [id]
                      (let [x (* id 10)]
                        (loop [attempts 0]
                          (let [res (tu/invoke
                                     {:Mtt01/Create_E
                                      {:Instance
                                       {:Mtt01/E {:Id id :X x
                                                  :Y (str id ", " x)}}}})]
                            (if (seq res) res
                                (when (< attempts 4) (recur (inc attempts))))))))
           e? (partial cn/instance-of? :Mtt01/E)
           create-es #(mapv create-e %)
           lookup-all (fn [] (tu/invoke
                              {:Mtt01/LookupAll_E {}}))
           delete-e (fn [id]
                      (loop [attempts 0]
                        (let [res (tu/invoke {:Mtt01/DeleteE {:Id id}})]
                          (if (seq res)
                            (first res)
                            (when (< attempts 4) (recur (inc attempts)))))))
           delete-es #(mapv delete-e %)
           trtest (fn [opr ids]
                    (let [res (atom nil)
                          ^Thread t (Thread. #(reset! res (opr ids)))]
                      (.start t)
                      [t res]))
           chk (fn [[^Thread t res] ids]
                 (.join t)
                 (is (= (count ids) (count @res)))
                 (let [rs (filter e? @res)]
                   (is (> (count rs)
                          (int (* (count ids) 0.98))))))
           crtest (partial trtest create-es)
           deltest (partial trtest delete-es)]
       (let [ids1 (range 1 500)
             ids2 (range 500 1000)
             ids3 (range 20 100)
             ids4 (range 470 600)
             t1 (crtest ids1)
             t2 (crtest ids2)]
         (chk t1 ids1)
         (chk t2 ids2)
         (let [es (lookup-all)]
           (is (every? e? es)))
         (let [t3 (deltest ids3)
               t4 (deltest ids4)]
           (chk t3 ids3)
           (chk t4 ids4)
           (let [es (lookup-all)]
             (is (every? e? es))))))))

(deftest api-test
  (api/component :Api.Test)
  (api/record
   :Api.Test/R
   {:A :String})
  (api/entity
   :Api.Test/E
   {:Id :Identity :R :Api.Test/R})
  (api/entity
   :Api.Test/F
   {:Id :Identity
    :Name {:type :String :id true}
    :Y :Int})
  (api/relationship
   :Api.Test/Rel
   {:meta {:contains [:Api.Test/E :Api.Test/F]}})
  (api/event
   :Api.Test/MakeF
   {:Name :String :Y :Int})
  (api/dataflow
   :Api.Test/MakeF
   {:Api.Test/R
    {:A "hello, world"} :as :R}
   {:Api.Test/E {:R :R} :as :E}
   {:Api.Test/F
    {:Name :Api.Test/MakeF.Name
     :Y :Api.Test/MakeF.Y}
    :Api.Test/Rel :E})
  (tu/finalize-component :Api.Test)
  (let [f (tu/invoke
           {:Api.Test/MakeF
            {:Name "abc" :Y 100}})]
    (is (cn/instance-of? :Api.Test/F f))
    (is (cn/same-instance?
         f
         (first
          (tu/invoke
           {:Api.Test/Lookup_F
            {:path (li/path-attr f)}}))))))

(deftest password-encryption-bug
  (defcomponent :PswdEnc
    (entity
     :PswdEnc/E
     {:X :String
      :Y {:type :Password :optional true
          :check #(or (agentlang.util.hash/crypto-hash? %)
                      (let [c (count %)]
                        (< 2 c 10)))}}))
  (let [e (tu/invoke
           {:PswdEnc/Create_E
            {:Instance
             {:PswdEnc/E {:X "hello" :Y "jjj3223"}}}})]
    (is (cn/instance-of? :PswdEnc/E e))
    (is (agentlang.util.hash/crypto-hash? (:Y e)))))

(deftest default-component
  (component :Dc01)
  (component :Dc02)
  (cn/set-current-component :Dc02)
  (is (= :Dc02/A (entity :A {:Id :Identity :X :Int})))
  (is (= :Dc02/B (record :B {:Name :String})))
  (is (= :Dc02/C (event :C {:A :UUID :B :String})))
  (is (= :Dc02/C (dataflow :C {:Dc02/A {:X :C.X}})))
  (is (= :Dc02/OnA (inference :OnA {:instructions "handle new A"})))
  (is (= :Dc02/R (relationship :R {:meta {:between [:Dc02/A :Dc02/A]}})))
  (cn/set-current-component :Dc01)
  (is (= :Dc01/A (entity :A {:Id :Identity :X :Int})))
  (is (= :Dc01/B (record :B {:Name :String}))))

(deftest fetch-from-raw
  (defcomponent :Ffr
    (entity
     :Ffr/E
     {:Id :Identity
      :rbac [{:roles ["user"] :allow [:create]}]})
    (entity
     :Ffr/F
     {:X {:type :Int :id true}})
    (relationship
     :Ffr/R
     {:meta {:between [:Ffr/E :Ffr/F]}
      :Y :Int}))
  (is (= {:Id :Identity
          :rbac [{:roles ["user"] :allow [:create]}]}
         (cn/fetch-user-schema :Ffr/E)))
  (is (= {:X {:type :Int :id true}}
         (cn/fetch-user-schema :Ffr/F)))
  (is (= {:meta {:between [:Ffr/E :Ffr/F], :cascade-on-delete true}, :Y :Int}
         (cn/fetch-user-schema :Ffr/R))))

(defn one-to-one-helper [cn uq-tag]
  (let [mkn (partial li/make-path cn)
        A (mkn :A)
        B (mkn :B)
        AB (mkn :AB)]
    (defcomponent cn
      (entity A {:Id {:type :Int :id true} :X :Int})
      (entity B {:Id {:type :Int :id true} :Y :Int})
      (relationship AB {:meta (merge {:between [A B]} (when uq-tag {uq-tag true}))}))
    (let [cra (fn [id x] (tu/invoke {(mkn :Create_A) {:Instance {A {:Id id :X x}}}}))
          crb (fn [id y] (tu/invoke {(mkn :Create_B) {:Instance {B {:Id id :Y y}}}}))
          crab (fn [a b] (tu/invoke {(mkn :Create_AB) {:Instance {AB {:A a :B b}}}}))
          a? (partial cn/instance-of? A)
          b? (partial cn/instance-of? B)
          ab? (partial cn/instance-of? AB)
          as (mapv cra [1 2 3] [10 20 30])
          bs (mapv crb [90 91 92] [101 102 103])
          _ (is (and (= 3 (count as)) (every? a? as)))
          _ (is (and (= 3 (count bs)) (every? b? bs)))]
      (let [f #(crab (li/path-attr (first as)) (li/path-attr (first bs)))]
        (is (ab? (f)))
        (if (= uq-tag :one-one)
          (tu/is-error "duplicate between relationship" f)
          (is (ab? (f)))))
      (is (ab? (crab (li/path-attr (second as)) (li/path-attr (nth bs 2))))))))

(deftest one-to-one
  (one-to-one-helper :Oto1 nil)
  (one-to-one-helper :Oto2 :one-one))

;; TODO: add more HTTP-apis test, move to separate test-file.
(deftest basic-http-api
  (defcomponent :Bha
    (entity :Bha/A {:Id {:type :Int :id true} :X :Int}))
  #?(:clj
     (let [r (tu/api-post "Bha/A" {:Bha/A {:Id 1 :X 100}})]
       (is (= 200 (:status r)))
       (let [body (:body r)]
         (is (= :Bha/A (:type body)))
         (is (= 1 (:Id (:result body))))))))
