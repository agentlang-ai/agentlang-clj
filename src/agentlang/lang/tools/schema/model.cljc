(ns agentlang.lang.tools.schema.model
  (:require [clojure.string :as s]
            [malli.core :as m]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.lang.tools.schema.diff :as diff]))

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
    [:root-agentlang-version {:optional true} :string]
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

(def diffs {})

;;;; example diff entry
;; {"0.6.1-alpha13"
;;  [[:- :tags] [:- :workspace] [:workspace :string]]}

(defn- get-model-spec [runtime-version model]
  (let [spec (get model-spec runtime-version)]
    (or spec
        (if-let [diff (get diffs runtime-version)]
          (if-let [root-spec (or (get model-spec (:root-agentlang-version model))
                                 spec)]
            (diff/apply-diff root-spec diff)
            (u/throw-ex (str "Failed to load a root-specification for model schema diffm, version is " runtime-version)))
          (u/throw-ex (str "No model schema or diff for version: " runtime-version))))))

(defn- call-validation [vfn model]
  (let [rv (:agentlang-version model)
        runtime-version (if (or (not rv) (= rv "current"))
                          (gs/agentlang-version)
                          rv)]
    (vfn (get-model-spec runtime-version model) model)))

(def validate (partial call-validation m/validate))
(def explain (partial call-validation m/explain))

(defn explain-errors [spec] (:errors (explain spec)))
