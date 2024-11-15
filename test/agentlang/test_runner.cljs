(ns agentlang.test-runner
  "This ns should be used to run tests with `figwheel-main` test runner."
  (:require #_[cljs-test-display.core]
            #_[figwheel.main.testing :refer-macros [run-tests run-tests-async]]
            [cljs.test :refer-macros [run-tests]]
            [agentlang.test.util :as test-util]
            [agentlang.test.basic]
            #_[agentlang.test.resolver]
            #_[agentlang.test.query]))

#_(defn -main [& args]
    (run-tests-async 100000)
    ;; return a message to the figwheel process that tells it to wait
    [:figwheel.main.async-result/wait 100000])

;; (enable-console-print!)

(defn ^:export main []
  (run-tests
   'agentlang.test.basic
   ;; 'agentlang.test.resolver
   ;; 'agentlang.test.query
   ))

(set! *main-cli-fn* main)
