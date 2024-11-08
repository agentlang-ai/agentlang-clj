(component :TimerTests)

(event :OnTimer {:Name :String})

(dataflow
 :StartTimer
 {:Agentlang.Kernel.Lang/Timer
  {:Name :StartTimer.Name
   :Expiry 10
   :ExpiryEvent
   [:q# {:TimerTests/OnTimer
         {:Name [:uq# :TimerTests/StartTimer.Name]}}]}})

(dataflow
 :OnTimer
 [:eval '(println (str "timer ":OnTimer.Name " fired"))])
