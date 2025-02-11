(ns agentlang.compiler
  "Compile dataflow patterns to calls into the resolver protocol."
  (:require [agentlang.util :as u]))

(defn parse-relationship-tree [_ _]
  (u/raise-not-implemented 'parse-relationship-tree))
