(ns agentlang.evaluator.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator.suspend :as sp]))

(def ^:private active-exec-graph #?(:clj (ThreadLocal.) :cljs (atom)))
(def ^:private event-stack #?(:clj (ThreadLocal.) :cljs (atom)))

(def ^:private execution-cache (u/make-cell {}))

(defn- reset-exec-graph! []
  #?(:clj (.set active-exec-graph nil)
     :cljs (reset! active-exec-graph nil))
  #?(:clj (.set event-stack nil)
     :cljs (reset! event-stack nil)))

(defn get-active-exec-graph []
  #?(:clj (.get active-exec-graph)
     :cljs @active-exec-graph))

(defn update-active-exec-graph! [new-graph]
  #?(:clj (.set active-exec-graph new-graph)
     :cljs (reset! active-exec-graph new-graph)))

(defn- get-event-stack []
  #?(:clj (.get event-stack)
     :cljs @event-stack))

(defn- update-event-stack! [new-stack]
  #?(:clj (.set event-stack new-stack)
     :cljs (reset! event-stack new-stack)))

(defn- push-event [event-instance]
  (let [stk (conj (get-event-stack) event-instance)]
    (update-event-stack! stk)
    event-instance))

(defn- pop-event []
  (let [stk0 (get-event-stack)
        inst (peek stk0)
        stk (pop stk0)]
    (update-event-stack! stk)
    inst))

(defn- has-more-events? []
  (let [stk0 (get-event-stack)]
    (seq stk0)))

(defn init [event-instance]
  (let [parent-graph (get-active-exec-graph)
        g {:parent parent-graph :steps [[event-instance (cn/inference? (cn/instance-type-kw event-instance))]]}]
    #?(:clj (.set active-exec-graph g)
       :cljs (reset! active-exec-graph g))
    #?(:clj (.set event-stack [])
       :cljs (reset! event-stack []))
    (push-event event-instance)))

(defn add-step! [pat]
  (let [g (get-active-exec-graph)
        can-add? (if (map? pat)
                   (let [recname (li/record-name pat)]
                     (not (cn/event? recname)))
                   true)]
    (when can-add?
      (update-active-exec-graph! (assoc g :steps (conj (:steps g) pat))))
    pat))

(defn finalize! []
  (when-let [event-instance (pop-event)]
    (when-not (has-more-events?)
      (let [k (or (get-in event-instance [:EventContext :ExecId])
                  (u/keyword-as-string (cn/instance-type-kw event-instance)))]
        (u/safe-set execution-cache (assoc @execution-cache k (get-active-exec-graph)))
        (reset-exec-graph!)
        k))))

(defn get-exec-graph [k]
  (let [g (get @execution-cache k)]
    (u/pprint g)
    g))

(ln/event
 :Agentlang.Kernel.Eval/GetExecGraph
 {:Key :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/GetExecGraph
 [:eval '(agentlang.evaluator.exec-graph/get-exec-graph :Agentlang.Kernel.Eval/GetExecGraph.Key)])
