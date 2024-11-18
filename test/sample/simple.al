(ns sample.simple
  (:use [agentlang.lang]))

(component :Sample.Simple)

(entity
 :E1
 {:A :Int
  :B :Int
  :C :Int
  :X {:type :String
      :write-only true}
  :Y :DateTime
  :rbac [{:roles ["order-users"] :allow [:create]}]})

(dataflow
 :K
 {:E1 {:A '(+ 5 :B)
       :B :K.Data.I
       :C '(+ 10 :A)
       :X "secret"
       :Y '(agentlang.lang.datetime/now)}})

(record
 {:Result
  {:Data :Any}})

(dataflow
 :RaiseError
 [:match :RaiseError.I
  0 [:eval '(agentlang.util/throw-ex "blah!")]
  1 {:Result {:Data "hello"}}])

(defn err []
  (agentlang.util/throw-ex "an error occured"))

(dataflow
 :JJ
 [:eval '(sample.simple/err)
  :throws
  [:error {:Agentlang.Kernel.Lang/Response {:HTTP {:status 422 :body "some error"}}}]])

(dataflow
 :Test2
 {:Result {:Data "hello, world"}})

(dataflow
 :Test3
 {:Result {:Data "bye, bye"}
  :as :A}
 {:Agentlang.Kernel.Lang/Response {:HTTP {:status 422 :body :A}}})

(dataflow
 :Q
 [:try
  {:E1? {}}
  :not-found {:Agentlang.Kernel.Lang/Response {:HTTP {:status 422 :body "no issues"}}}])

(entity :T {:X :Int})
(entity :U {:Y :Int})

(dataflow
 :TU
 {:T {:X :TU.X} :as :T1}
 {:U {:Y '(* :T1.X 100)}})

(dataflow
 :F
 {:E1? {} :as :Es}
 {:Result {:Data :Es}})

(entity {:E2 {:Y :DateTime}})

(record {:StringField
         {:Question {:type :String}
          :Value {:type :String
                  :optional true}}})

(defn valid-name? [s]
  (and (string? s)
       (<= 3 (count s) 50)))

(entity {:Survey
         {:Name {:check valid-name?}
          :Field {:type :StringField}}})

(dataflow :KK {:E2 {:Y '(agentlang.lang.datetime/now)}})

(event :OnTimer {:X :Int})

(dataflow
 :StartTimer
 {:Agentlang.Kernel.Lang/Timer
  {:Name "BasicTimer/Timer02"
   :Expiry 25
   :ExpiryEvent
   [:q# {:Sample.Simple/OnTimer
         {:X [:uq# :Sample.Simple/StartTimer.X]}}]}})

(dataflow :OnTimer [:eval '(println :OnTimer.X)])

(dataflow
 :SuspensionTest
 {:T {:X 100} :as :T1}
 {:U {:Y 200} :as :U1}
 [:suspend]
 {:T {:X '(+ :T1.X :U1.Y)}})
