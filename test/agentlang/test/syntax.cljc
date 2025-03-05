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
     (is (= pat (ls/raw r)))))
  ([pat] (is-pat (constantly true) pat)))

(deftest instances
  (is (= {} (ls/introspect {})))
  (defcomponent :SynInst
    (entity :SynInst/E {:Id {:type :Int :id true} :X :Int}))
  (is-pat ls/upsert? {:SynInst/E {:Id 1 :X 100}})
  (is-pat ls/upsert? {:SynInst/E {:Id 1 :X 100} :as :A})
  (is-pat ls/upsert? {:SynInst/E {:Id? 1 :X 200} :as :A})
  (is-pat ls/query? {:SynInst/E {:Id? 1} :as [:A]})
  (is-pat ls/upsert? {:SynInst/E {:Id 2 :X '(* 10 :SynInst/CreateE.X)} :as :A})
  (is-pat ls/query-object? {:SynInst/E {:? {:where [:>= :X 100]}} :as :Result})
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
  (is-pat ls/query? {:SynRel/B? {}
                     :SynRel/AB? {:SynRel/A {:Id :SynRel/FindB.A}}
                     :as :Bs
                     :into {:Y :SynRel/B.Y
                            :X :SynRel/A.X}
                     :case {:not-found {:SynRel/B {:Id 1 :Y 1}}}})
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
  (is-pat ls/for-each? [:for-each {:SynFe/E? {}}
                        {:SynFe/R {:A :%.X}}
                        {:SynFe/R {:A '(* 10 :%.X)}}
                        :as :Rs])
  (= [:for-each :Result
      {:SynFe/R {:A :%.A}}
      :as :Rs
      :case {:error #:SynFe{:R {:A 100}}}]
     (ls/raw (ls/with-case
               {:error {:SynFe/R {:A 100}}}
               (ls/with-alias :Rs
                 (ls/for-each :Result (mapv ls/introspect [{:SynFe/R {:A :%.A}}])))))))
