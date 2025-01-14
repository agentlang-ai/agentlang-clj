(ns agentlang.evaluator.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator.suspend :as sp]))

(def ^:private active-exec-graph #?(:clj (ThreadLocal.) :cljs (atom)))
(def ^:private exec-graph-stack #?(:clj (ThreadLocal.) :cljs (atom)))
(def ^:private event-stack #?(:clj (ThreadLocal.) :cljs (atom)))

(def ^:private execution-cache (u/make-cell {}))

(defn get-active-exec-graph []
  #?(:clj (.get active-exec-graph)
     :cljs @active-exec-graph))

(defn has-active-exec-graph? []
  (if (get-active-exec-graph)
    true
    false))

(defn update-active-exec-graph! [new-graph]
  #?(:clj (.set active-exec-graph new-graph)
     :cljs (reset! active-exec-graph new-graph)))

(defn- get-stack [obj]
  #?(:clj (.get obj)
     :cljs @obj))

(defn- update-stack! [obj new]
  #?(:clj (.set obj new)
     :cljs (reset! obj new)))

(defn- stack-push [get-fn update-fn x]
  (let [stk (conj (get-fn) x)]
    (update-fn stk)
    x))

(defn- stack-pop [get-fn update-fn]
  (let [stk0 (get-fn)]
    (when (seq stk0)
      (let [x (peek stk0)
            stk (pop stk0)]
        (update-fn stk)
        x))))

(defn- stack-has-more [get-fn]
  (let [stk0 (get-fn)]
    (seq stk0)))

(def ^:private get-event-stack (partial get-stack event-stack))
(def ^:private update-event-stack! (partial update-stack! event-stack))
(def ^:private push-event (partial stack-push get-event-stack update-event-stack!))
(def ^:private pop-event  (partial stack-pop get-event-stack update-event-stack!))
(def ^:private has-more-events? (partial stack-has-more get-event-stack))

(def ^:private get-exec-graph-stack (partial get-stack exec-graph-stack))
(def ^:private update-exec-graph-stack! (partial update-stack! exec-graph-stack))
(def ^:private push-exec-graph (partial stack-push get-exec-graph-stack update-exec-graph-stack!))
(def ^:private pop-exec-graph  (partial stack-pop get-exec-graph-stack update-exec-graph-stack!))
(def ^:private has-more-exec-graphs (partial stack-has-more get-exec-graph-stack))

(defn- reset-exec-graph! [k]
  (let [g (get-active-exec-graph)
        parent (pop-exec-graph)]
    (if parent
      (update-active-exec-graph! (assoc parent :nodes (conj (:nodes parent) g)))
      (do (u/safe-set execution-cache (assoc @execution-cache k g))
          (update-active-exec-graph! nil)))
    (or parent g)))

(defn init [event-instance]
  (let [parent-graph (get-active-exec-graph)
        g {:nodes [[event-instance (cn/inference? (cn/instance-type-kw event-instance))]]}]
    (when parent-graph
      (push-exec-graph parent-graph))
    #?(:clj (.set active-exec-graph g)
       :cljs (reset! active-exec-graph g))
    (push-event event-instance)))

(defn- cleanup [result]
  (if (map? result)
    (dissoc result :env :message)
    result))

(defn add-step! [pat result]
  (when-let [g (get-active-exec-graph)]
    (let [can-add? (if (map? pat)
                     (let [recname (li/record-name pat)]
                       (not (cn/event? recname)))
                     true)]
      (when can-add?
        (update-active-exec-graph! (assoc g :nodes (conj (:nodes g) [pat (cleanup result)]))))
      pat)))

(defn finalize! []
  (when-let [event-instance (pop-event)]
    (let [k (or (get-in event-instance [:EventContext :ExecId])
                (u/keyword-as-string (cn/instance-type-kw event-instance)))]
      (reset-exec-graph! k)
      k)))

(defn init-node [event-instance]
  (when (get-active-exec-graph)
    (init event-instance)))

(defn finalize-node! []
  (when (get-active-exec-graph)
    (finalize!)))

(defn get-exec-graph [k]
  (get @execution-cache k))

(ln/event
 :Agentlang.Kernel.Eval/GetExecGraph
 {:Key :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/GetExecGraph
 [:eval '(agentlang.evaluator.exec-graph/get-exec-graph :Agentlang.Kernel.Eval/GetExecGraph.Key)])

(defn graph? [x] (and (map? x) (:nodes x)))
(defn root-event [g] (ffirst (:nodes g)))
(defn nodes [g] (vec (rest (:nodes g))))
(def pattern first)
(defn result [n] (:result (second n)))
(defn status [n] (:status (second n)))

(defn suspended? [g]
  (when-let [g0 (last (:nodes g))]
    (when-let [n (last (and (graph? g0) (:nodes g0)))]
      (cn/instance-of? :Agentlang.Kernel.Eval/Suspension (first (result n))))))
