(ns agentlang.evaluator
  "Helper functions for compiling and evaluating patterns."
  (:require [agentlang.util :as u]
            [agentlang.global-state :as gs]
            [agentlang.component :as cn]
            [agentlang.store :as store]))

(defn eval-internal [_] (u/raise-not-implemented 'eval-internal))
(defn safe-eval-internal [_] (u/raise-not-implemented 'safe-eval-internal))
(defn safe-eval [_] (u/raise-not-implemented 'safe-eval))
(defn fire-post-events [_ _] (u/raise-not-implemented 'fire-post-events))
(defn eval-all-dataflows-atomic [_] (u/raise-not-implemented 'eval-all-dataflows-atomic))
(defn eval-all-dataflows [_] (u/raise-not-implemented 'eval-all-dataflows))
(defn mark-internal [_] (u/raise-not-implemented 'mark-internal))
(defn eval-patterns [_] (u/raise-not-implemented 'eval-patterns))
(defn evaluate-pattern [_] (u/raise-not-implemented 'evaluate-pattern))
(defn debug-dataflow [_] (u/raise-not-implemented 'debug-dataflow))
(defn debug-step [_] (u/raise-not-implemented 'debug-step))
(defn debug-continue [_] (u/raise-not-implemented 'debug-continue))
(defn debug-cancel [_] (u/raise-not-implemented 'debug-cancel))
(defn async-evaluate-pattern [_ _ _] (u/raise-not-implemented 'async-evaluate-pattern))

(defn store-from-config
  [store-or-store-config]
  (cond
    (or (nil? store-or-store-config)
        (map? store-or-store-config))
    (store/open-default-store store-or-store-config)

    (and (keyword? store-or-store-config)
         (= store-or-store-config :none))
    nil

    :else
    store-or-store-config))

(defn- maybe-delete-model-config-instance [entity-name]
  (let [evt-name (cn/crud-event-name entity-name :Delete)]
    (safe-eval-internal {evt-name {:Id 1}})))

(defn save-model-config-instance [app-config model-name]
  (when-let [ent (cn/model-config-entity model-name)]
    (when-let [attrs (ent app-config)]
      (maybe-delete-model-config-instance ent)
      (let [evt-name (cn/crud-event-name ent :Create)]
        (safe-eval-internal {evt-name {:Instance {ent attrs}}})))))

(defn save-model-config-instances []
  (when-let [app-config (gs/get-app-config)]
    (mapv (partial save-model-config-instance app-config) (cn/model-names))))
