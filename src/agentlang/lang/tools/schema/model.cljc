(ns agentlang.lang.tools.schema.model
  (:require [malli.core :as m]))

(def service-spec
  [:map
   [:port :int]])

(def connmgr-spec
  ;; TODO
  [])

(def telemetry-spec
  [:map
   [:enabled? :boolean]])

(def llms-spec
  ;; TODO
  [])

(def config-spec
  [:map
   [:service service-spec
    :store :map
    :connection-manager connmgr-spec
    :authentication :map
    :telemetry telemetry-spec
    :llms llms-spec]])

(defn url? [s]
  ;; TODO
  )

(def dependencies-spec
  ;; TODO
  [])

(def model-spec
  [:map
   [:description :string]
   [:tags [:set :keyword]]
   [:workspace :string]
   [:config config-spec]
   [:agentlang-version :string]
   [:name :keyword]
   [:git-hub-url url?]
   [:components [:set :keyword]]
   [:config-entity :keyword]
   [:github-org :string]
   [:version :string]
   [:branch :string]
   [:created-at :string]
   [:dependencies dependencies-spec]
   [:owner :string]])

(def validate (partial m/validate model-spec))
