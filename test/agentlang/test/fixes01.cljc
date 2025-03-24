(ns agentlang.test.fixes01
  (:require #?(:clj  [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
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
