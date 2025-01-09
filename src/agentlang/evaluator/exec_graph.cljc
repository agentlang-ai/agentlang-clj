(ns agentlang.evaluator.exec-graph
  (:require [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator.suspend :as sp]))

(def ^:private active-exec-graph
  #?(:clj (ThreadLocal.) :cljs (atom)))

(defn get-active-exec-graph []
  #?(:clj (.get active-exec-graph)
     :cljs @active-exec-graph))

(defn update-active-exec-graph! [new-graph]
  #?(:clj (.set active-exec-graph new-graph)
     :cljs (reset! active-exec-graph new-graph)))

(defn init [event-instance]
  (let [parent-graph (get-active-exec-graph)
        g {:parent parent-graph :steps [[event-instance (cn/inference? (cn/instance-type-kw event-instance))]]}]
    #?(:clj (.set active-exec-graph g)
       :cljs (reset! active-exec-graph g))
    true))

(defn add-step [pat]
  (let [g (get-active-exec-graph)
        can-add? (if (map? pat)
                   (let [recname (li/record-name pat)]
                     (not (cn/event? recname)))
                   true)]
    (when can-add?
      (update-active-exec-graph! (assoc g :steps (conj (:steps g) pat))))
    pat))
