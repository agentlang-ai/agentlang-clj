(ns
 agentlang.kernel.lang
 (:require
  [agentlang.util :as u]
  [agentlang.lang.kernel :as k]
  [agentlang.lang.internal :as li]
  [agentlang.lang.datetime :as dt]
  [agentlang.resolver.registry :as r]
  [agentlang.component :as cn]
  [agentlang.lang
   :refer
   [dataflow
    entity
    view
    pattern
    attribute
    rule
    relationship
    component
    resolver
    event
    inference
    record]]))
(component
 :Agentlang.Kernel.Lang
 {:clj-import
  '[(:require
     [agentlang.util :as u]
     [agentlang.lang.kernel :as k]
     [agentlang.lang.internal :as li]
     [agentlang.lang.datetime :as dt]
     [agentlang.resolver.registry :as r]
     [agentlang.component :as cn])]})
(attribute :Agentlang.Kernel.Lang/String {:check k/kernel-string?})
(attribute
 :Agentlang.Kernel.Lang/Keyword
 {:check (fn* [p1__394#] (or (keyword? p1__394#) (string? p1__394#)))})
(attribute :Agentlang.Kernel.Lang/Path {:check k/path?})
(attribute :Agentlang.Kernel.Lang/DateTime {:check k/date-time?})
(attribute :Agentlang.Kernel.Lang/Date {:check k/date?})
(attribute :Agentlang.Kernel.Lang/Time {:check k/time?})
(attribute :Agentlang.Kernel.Lang/UUID {:check k/UUID?})
(attribute :Agentlang.Kernel.Lang/Int {:check int?})
(attribute :Agentlang.Kernel.Lang/Int64 {:check int?})
(attribute :Agentlang.Kernel.Lang/BigInteger {:check integer?})
(attribute :Agentlang.Kernel.Lang/Float {:check k/kernel-float?})
(attribute :Agentlang.Kernel.Lang/Double {:check k/kernel-double?})
(attribute :Agentlang.Kernel.Lang/Decimal {:check cn/decimal-value?})
(attribute :Agentlang.Kernel.Lang/Boolean {:check boolean?})
(attribute :Agentlang.Kernel.Lang/Record {:check cn/record-instance?})
(attribute :Agentlang.Kernel.Lang/Entity {:check cn/entity-instance?})
(attribute :Agentlang.Kernel.Lang/Event {:check cn/event-instance?})
(attribute :Agentlang.Kernel.Lang/Any {:check k/any-obj?})
(attribute :Agentlang.Kernel.Lang/Email {:check k/email?})
(attribute :Agentlang.Kernel.Lang/Map {:check map?})
(attribute :Agentlang.Kernel.Lang/Edn {:check k/edn?})
(attribute
 :Agentlang.Kernel.Lang/Identity
 {:type :Agentlang.Kernel.Lang/UUID,
  :default u/uuid-string,
  li/guid true})
(attribute
 :Agentlang.Kernel.Lang/Now
 {:type :Agentlang.Kernel.Lang/DateTime, :default dt/now})
(attribute
 (k/event-context-attribute-name)
 (k/event-context-attribute-schema))
(attribute
 :Agentlang.Kernel.Lang/Password
 {:type :Agentlang.Kernel.Lang/String, :secure-hash true})
(record
 :Agentlang.Kernel.Lang/Future
 {:Result :Agentlang.Kernel.Lang/Any,
  :TimeoutMillis {:type :Agentlang.Kernel.Lang/Int, :default 2000}})
(entity
 :Agentlang.Kernel.Lang/Policy
 {:Intercept {:type :Agentlang.Kernel.Lang/Keyword, :indexed true},
  :Resource {:type :Agentlang.Kernel.Lang/Path, :indexed true},
  :Spec :Agentlang.Kernel.Lang/Edn,
  :InterceptStage
  {:oneof [:PreEval :PostEval :Default], :default :Default}})
(entity
 :Agentlang.Kernel.Lang/Timer
 {:LastHeartbeatSecs
  {:type :Agentlang.Kernel.Lang/Int, :default dt/unix-timestamp},
  :ExpiryEvent :Agentlang.Kernel.Lang/Map,
  :Restart {:type :Agentlang.Kernel.Lang/Boolean, :default false},
  :ExpiryUnit
  {:oneof ["Seconds" "Minutes" "Hours" "Days"], :default "Seconds"},
  :CreatedTimeSecs
  {:type :Agentlang.Kernel.Lang/Int, :default dt/unix-timestamp},
  :Name {:type :Agentlang.Kernel.Lang/String, :guid true},
  :Retries {:type :Agentlang.Kernel.Lang/Int, :default 0},
  :Expiry :Agentlang.Kernel.Lang/Int,
  :Status
  {:oneof
   ["ready"
    "running"
    "terminating"
    "term-cancel"
    "term-ok"
    "term-error"
    "term-abnormal"],
   :default "ready",
   :indexed true}})
(event
 :Agentlang.Kernel.Lang/SetTimerStatus
 {:TimerName :Agentlang.Kernel.Lang/String,
  :Status :Agentlang.Kernel.Lang/String})
(event
 :Agentlang.Kernel.Lang/SetTimerHeartbeat
 {:TimerName :Agentlang.Kernel.Lang/String})
(dataflow
 :Agentlang.Kernel.Lang/SetTimerStatus
 #:Agentlang.Kernel.Lang{:Timer
                         {:Name?
                          :Agentlang.Kernel.Lang/SetTimerStatus.TimerName,
                          :Status
                          :Agentlang.Kernel.Lang/SetTimerStatus.Status}})
(dataflow
 :Agentlang.Kernel.Lang/SetTimerHeartbeat
 #:Agentlang.Kernel.Lang{:Timer
                         {:Name?
                          :Agentlang.Kernel.Lang/SetTimerHeartbeat.TimerName,
                          :LastHeartbeatSecs
                          '(agentlang.lang.datetime/unix-timestamp)}})
(dataflow
 :Agentlang.Kernel.Lang/CancelTimer
 #:Agentlang.Kernel.Lang{:SetTimerStatus
                         {:TimerName
                          :Agentlang.Kernel.Lang/CancelTimer.TimerName,
                          :Status "term-cancel"}})
(dataflow
 :Agentlang.Kernel.Lang/FindRunnableTimers
 #:Agentlang.Kernel.Lang{:Timer?
                         {:where
                          [:or
                           [:= :Status "ready"]
                           [:= :Status "running"]]}})
(dataflow
 :Agentlang.Kernel.Lang/LoadPolicies
 #:Agentlang.Kernel.Lang{:Policy
                         {:Intercept?
                          :Agentlang.Kernel.Lang/LoadPolicies.Intercept,
                          :Resource?
                          :Agentlang.Kernel.Lang/LoadPolicies.Resource}})
(event
 :Agentlang.Kernel.Lang/AppInit
 {:Data :Agentlang.Kernel.Lang/Map})
(event :Agentlang.Kernel.Lang/InitConfig {})
(record
 :Agentlang.Kernel.Lang/InitConfigResult
 {:Data {:listof :Agentlang.Kernel.Lang/Map}})
(record
 :Agentlang.Kernel.Lang/DataSource
 {:Uri {:type :Agentlang.Kernel.Lang/String, :optional true},
  :Entity :Agentlang.Kernel.Lang/String,
  :AttributeMapping {:type :Agentlang.Kernel.Lang/Map, :optional true}})
(event
 :Agentlang.Kernel.Lang/DataSync
 {:Source :Agentlang.Kernel.Lang/DataSource,
  :DestinationUri
  {:type :Agentlang.Kernel.Lang/String, :optional true}})
(record
 :Agentlang.Kernel.Lang/Config
 {:Id
  {:type :Agentlang.Kernel.Lang/Int,
   :guid true,
   :default 1,
   :read-only true}})
(entity
 :Agentlang.Kernel.Lang/AuthConfig
 {:meta {:inherits :Agentlang.Kernel.Lang/Config}
  :Service            {:type :Keyword :optional true}
  :Mode               {:type :Keyword :optional true}
  :SsoUrl             {:type :String :optional true}
  :AuthorizeRedirectUrl {:type :String :optional true}
  :SamlCertificate    {:type :String :optional true}
  :CookieDomain       {:type :String :optional true}
  :CookieTtlMs        {:type :Int :default 1209600000}
  :ClientUrl          {:type :String :optional true}
  :SuperuserEmail     {:type :String :optional true}
  :Domain             {:type :String :optional true}
  :AuthServer         {:type :String :default "default"}
  :ClientId           {:type :String :optional true}
  :ClientSecret       {:type :String :optional true}
  :ApiToken           {:type :String :optional true}
  :Scope              {:type :String :default "openid offline_access"}
  :Introspect         {:type :Boolean :default true}
  :RoleClaim          {:type :Keyword :default :roles}
  :DefaultRole        {:type :String :default "user"}
  :Whitelist          {:type :Boolean :default false}
  :DisableUserSessions {:type :Boolean :default false}
  :UserPoolId         {:type :String :optional true}
  :AccessKey          {:type :String :optional true}
  :Region             {:type :String :optional true}})
(defn- http-response? [x] (and (map? x) (int? (:status x))))
(record
 :Agentlang.Kernel.Lang/Response
 {:HTTP {:check agentlang.kernel.lang/http-response?, :optional true}})
(r/register-resolvers
 [{:name :meta,
   :type :meta,
   :compose? false,
   :config
   {:agentlang-api
    {:component component,
     :entity entity,
     :event event,
     :record record,
     :dataflow dataflow}},
   :paths [:Agentlang.Kernel.Lang/LoadModelFromMeta]}
  {:name :timer,
   :type :timer,
   :compose? true,
   :paths [:Agentlang.Kernel.Lang/Timer]}
  (when
   (u/host-is-jvm?)
   {:name :data-sync,
    :type :data-sync,
    :compose? false,
    :paths [:Agentlang.Kernel.Lang/DataSync]})])
(def
 Agentlang_Kernel_Lang___COMPONENT_ID__
 "fbf212fe-4d7a-4a82-b497-89c77854f5cb")
