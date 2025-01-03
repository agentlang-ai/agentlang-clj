(ns agentlang.lang.tools.schema.model
  (:require [clojure.string :as s]
            [clojure.set :as set]
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

(def channel-spec
  [:map
   [:subscriptions {:optional true} :boolean]
   [:tools {:optional true} [:vector :keyword]]])

(def ^:private conn-type-keys #{:name :type :title :description})

(defn- connection-type-entry? [v]
  (and (map? v)
       (= conn-type-keys (set/union conn-type-keys (set (keys v))))))

(defn connection-type-spec? [obj]
  (if (vector? obj)
    (every? connection-type-entry? obj)
    (and (map? obj)
         (every?
          (fn [k]
            (and (or (keyword? k) (string? k))
                 (connection-type-entry? (get obj k))))
          (keys obj)))))

(def model-spec
  {(gs/agentlang-version)
   [:map
    {:closed false}
    [:description {:optional true} :string]
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
    [:channel {:optional true} channel-spec]
    [:connection-types {:optional true} [:fn connection-type-spec?]]
    [:model-paths {:optional true} [:vector :string]]
    [:owner {:optional true} :string]]})

(def diffs {})
;;;; example diff entry
;; {"0.6.1"
;;  [[:- :tags] [:- :workspace] [:workspace :string]]}

(defn- find-preceding-diffs [vers]
  (let [ks (filter #(pos? (compare vers %)) (keys diffs))]
    (vals (into (sorted-map) (select-keys diffs ks)))))

(defn- get-model-spec [runtime-version model]
  (let [spec (get model-spec runtime-version)]
    (or spec
        (if-let [diff (get diffs runtime-version)]
          (if-let [root-spec (or (get model-spec (:root-agentlang-version model))
                                 spec)]
            (diff/apply-diffs root-spec (concat (find-preceding-diffs runtime-version) [diff]))
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
