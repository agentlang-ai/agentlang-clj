(ns agentlang.lang.tools.schema.model
  (:require [clojure.string :as s]
            [malli.core :as m]
            [agentlang.global-state :as gs]))

(def service-spec
  [:map
   [:port :int]])

(defn conn-inst? [obj]
  (and (vector? obj)
       (keyword? (first obj))
       (string? (second obj))))

(defn connections? [obj]
  (and (vector? obj)
       (every? conn-inst? obj)))

(def connmgr-spec
  [:map
   [:integrations {:optional true} [:set :string]]
   [:configurations {:optional true} :map]
   [:username :string]
   [:password :string]
   [:token {:optional true} :string]
   [:connections [:fn connections?]]])

(def telemetry-spec
  [:map
   [:enabled? :boolean]])

(def llms-spec :map)

(def store-spec :map)

(def config-spec
  [:map
   [:service {:optional true} service-spec]
   [:store {:optional true} store-spec]
   [:connection-manager {:optional true} connmgr-spec]
   [:authentication {:optional true} :map]
   [:telemetry {:optional true} telemetry-spec]
   [:llms {:optional true} llms-spec]])

(defn git-hub-url? [s]
  (if s
    (s/starts-with? s "https://github.com/")
    true))

(defn- dep-tag? [t]
  (and (keyword? t) (or (= t :git) (= t :fs))))

(defn- dep-spec? [d]
  (and (vector? d)
       (let [f (first d)]
         (if (or (dep-tag? f) (symbol? f))
           (string? (second d))
           false))))

(defn dependencies? [deps]
  (if deps
    (and (vector? deps)
         (every? dep-spec? deps))
    true))

(defn model-name? [n]
  (or (string? n) (keyword? n)))

(def model-spec
  {(gs/agentlang-version)
   [:map
    [:description {:optional true} :string]
    [:tags {:optional true} [:set :keyword]]
    [:workspace {:optional true} :string]
    [:config {:optional true} config-spec]
    [:agentlang-version :string]
    [:name [:fn model-name?]]
    [:git-hub-url {:optional true} [:fn git-hub-url?]]
    [:components [:vector :keyword]]
    [:config-entity {:optional true} :keyword]
    [:github-org {:optional true} :string]
    [:version {:optional true} :string]
    [:branch {:optional true} :string]
    [:created-at {:optional true} :string]
    [:dependencies {:optional true} [:fn dependencies?]]
    [:owner {:optional true} :string]]})

(defn get-current-model-spec []
  (get model-spec (gs/agentlang-version)))

(def validate (partial m/validate (get-current-model-spec)))
(def explain (partial m/explain (get-current-model-spec)))

(defn explain-errors [spec]
  (:errors (explain spec)))
