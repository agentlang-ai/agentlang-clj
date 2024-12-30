(component :Email.Core)

(require '[agentlang.inference.service.model :as agents])
(require '[agentlang.subs :as subs])
(require '[agentlang.evaluator :as ev])

(entity
 :Email.Core/Email
 {:Body :String})

(resolver
 :Email.Core/Resolver
 {:paths [:Email.Core/Email]
  :with-methods
  {:on-change-notification
   {:handler
    (fn [obj]
      (when-let [input-evt (agents/get-subscription-event :Email)]
        (println (ev/safe-eval-pattern {input-evt {:UserInstruction (:Body (:instance obj))}}))))}}})

(defn do-subs []
  (let [conn (subs/open-connection
              {:type :mem
               :data
               [{:operation :create
                 :instance
                 {:Email.Core/Email
                  {:Body "This is a report for \"acme\":
  1. 2024-12-01 $405 income from rent.
  2. 2024-11-30 $5000 salaries paid."}}}
                {:operation :create
                 :instance
                 {:Email.Core/Email
                  {:Body "This is a report for \"abc ltd\":
  1. 2024-12-03 $100 some income."}}}]})]
    (Thread/sleep 2000)
    (subs/listen conn)))

 (.start (Thread. do-subs))
