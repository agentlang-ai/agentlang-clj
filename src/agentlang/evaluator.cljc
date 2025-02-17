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
