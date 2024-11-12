(component
 :Agentlang.Kernel.Lang
 {:clj-import '[(:require [agentlang.util :as u]
                          [agentlang.lang.kernel :as k]
                          [agentlang.lang.internal :as li]
                          [agentlang.lang.datetime :as dt]
                          [agentlang.resolver.registry :as r]
                          [agentlang.component :as cn])]})

(attribute :String {:check k/kernel-string?})
(attribute :Keyword {:check #(or (keyword? %) (string? %))})
(attribute :Path {:check k/path?})
(attribute :DateTime {:check k/date-time?})
(attribute :Date {:check k/date?})
(attribute :Time {:check k/time?})
(attribute :UUID {:check k/UUID?})
(attribute :Int {:check int?})
(attribute :Int64 {:check int?})
(attribute :BigInteger {:check integer?})
(attribute :Float {:check k/kernel-float?})
(attribute :Double {:check k/kernel-double?})
(attribute :Decimal {:check cn/decimal-value?})
(attribute :Boolean {:check boolean?})
(attribute :Record {:check cn/record-instance?})
(attribute :Entity {:check cn/entity-instance?})
(attribute :Event {:check cn/event-instance?})
(attribute :Any {:check k/any-obj?})
(attribute :Email {:check k/email?})
(attribute :Map {:check map?})
(attribute :Edn {:check k/edn?})

(attribute :Identity {:type :UUID :default u/uuid-string li/guid true})
(attribute :Now {:type :DateTime :default dt/now})

(attribute (k/event-context-attribute-name)
           (k/event-context-attribute-schema))

(attribute
 :Password
 {:type :String
  :secure-hash true})

(record
 :Future
 {:Result :Any
  :TimeoutMillis {:type :Int
                  :default 2000}})

(entity
 :Policy
 {:Intercept {:type :Keyword
              :indexed true}
  :Resource {:type :Path
             :indexed true}
  :Spec :Edn
  :InterceptStage
  {:oneof [:PreEval :PostEval :Default]
   :default :Default}})

(entity
 :Timer
 {:Name {:type :String :guid true}
  :Expiry :Int
  :ExpiryUnit {:oneof ["Seconds" "Minutes" "Hours" "Days"]
               :default "Seconds"}
  :ExpiryEvent :Map
  :Status {:oneof ["ready" "running" "terminating" "term-cancel" "term-ok" "term-error" "term-abnormal"]
           :default "ready" :indexed true}
  :CreatedTimeSecs {:type :Int :default dt/unix-timestamp}
  :LastHeartbeatSecs {:type :Int :default dt/unix-timestamp}})

(event :SetTimerStatus {:TimerName :String :Status :String})
(event :SetTimerHeartbeat {:TimerName :String})
(dataflow :SetTimerStatus {:Timer {:Name? :SetTimerStatus.TimerName :Status :SetTimerStatus.Status}})
(dataflow :SetTimerHeartbeat {:Timer {:Name? :SetTimerHeartbeat.TimerName :LastHeartbeatSecs '(agentlang.lang.datetime/unix-timestamp)}})

(dataflow
 :FindRunnableTimers
 {:Timer? {:where [:or [:= :Status "ready"] [:= :Status "running"]]}})

(dataflow
 :LoadPolicies
 {:Policy
  {:Intercept? :LoadPolicies.Intercept
   :Resource? :LoadPolicies.Resource}})

(event
 :AppInit
 {:Data :Map})

(event
 :InitConfig
 {})

(record
 :InitConfigResult
 {:Data {:listof :Agentlang.Kernel.Lang/Map}})

(record
 :DataSource
 {:Uri {:type :String
        :optional true} ;; defaults to currently active store
  :Entity :String ;; name of an entity
  :AttributeMapping {:type :Map
                     :optional true}})

(event
 :DataSync
 {:Source :DataSource
  :DestinationUri {:type :String
                   :optional true}})

;; Base-type of model-configuration entities.
(record :Config {:Id {:type :Int :guid true :default 1 :read-only true}})

(defn- http-response? [x]
  (and (map? x)
       (int? (:status x))))

;; Various responses that a dataflow-pattern may return and processed
;; at the "edge" (like the http server endpoint).
(record :Response {:HTTP {:check http-response? :optional true}})

(r/register-resolvers
 [{:name :meta
   :type :meta
   :compose? false
   :config {:agentlang-api
            {:component component
             :entity entity
             :event event
             :record record
             :dataflow dataflow}}
   :paths [:Agentlang.Kernel.Lang/LoadModelFromMeta]}
  {:name :timer
   :type :timer
   :compose? true
   :paths [:Agentlang.Kernel.Lang/Timer]}
  (when (u/host-is-jvm?)
    {:name :data-sync
     :type :data-sync
     :compose? false
     :paths [:Agentlang.Kernel.Lang/DataSync]})])
