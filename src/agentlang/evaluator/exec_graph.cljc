(ns agentlang.evaluator.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]))

(def ^:private exec-graphs (u/make-cell {}))

(def ^:private active-exec-graph
  #?(:clj (ThreadLocal.)
     :cljs (atom nil)))

(defn- set-graph! [g]
  #?(:clj (.set active-exec-graph g)
     :cljs (reset! active-exec-graph g)))

(defn- get-graph []
  #?(:clj (.get active-exec-graph)
     :cljs @active-exec-graph))

(def ^:private graph-stack
  #?(:clj (ThreadLocal.)
     :cljs (atom [])))

(defn- set-stack! [s]
  #?(:clj (.set graph-stack s)
     :cljs (reset! graph-stack s)))

(defn- get-stack []
  #?(:clj (.get graph-stack)
     :cljs @graph-stack))

(defn- push-graph
  ([g]
   (let [s (or (get-stack) [])]
     (set-stack! (conj s g))
     g))
  ([] (push-graph (get-graph))))

(defn- pop-graph []
  (let [s (get-stack)
        g (peek s)]
    (when g (set-stack! (pop s)))
    g))

(defn- init-graph [pattern result]
  {:event [pattern result]
   :nodes []})

(defn- append-node
  ([g pattern result]
   (let [nodes (:nodes g)]
     (assoc g :nodes (conj nodes [pattern result]))))
  ([g sub-g]
   (let [nodes (:nodes g)]
     (assoc g :nodes (conj nodes sub-g)))))

(defn graph? [g]
  (and (map? g) (:nodes g) (:event g)))

(def graph-nodes :nodes)

(defn graph-event [g]
  (first (:event g)))

(defn graph-event-type [g]
  (li/record-name (graph-event)))

(def graph-node-pattern first)
(def graph-node-result second)

(defn graph-walk [g on-sub-graph on-node]
  (doseq [n (graph-nodes g)]
    (if (graph? n)
      (on-sub-graph n)
      (on-node n))))

(defn- finalize-graph! []
  (when-let [g (get-graph)]
    (u/safe-set exec-graphs (assoc @exec-graphs (u/uuid-string) g))))

(defn add-node [{pattern :Pattern df-start? :DfStart df-end? :DfEnd} result]
  (let [new-g
        (if df-start?
          (let [_ (push-graph)
                new-g (init-graph pattern result)]
            (set-graph! new-g)
            new-g)
          (let [g (get-graph)
                new-g (if g (append-node g pattern result) (init-graph pattern result))]
            (set-graph! new-g)
            new-g))]
    (when df-end?
      (if-let [g (pop-graph)]
        (set-graph! (append-node g new-g))
        (finalize-graph!)))
    new-g))

(defn events-with-exec-graphs []
  (mapv (fn [[k v]]
          {:Key k :Event (graph-event v)})
        @exec-graphs))

(defn get-exec-graph [k] (get @exec-graphs k))

(ln/event :Agentlang.Kernel.Eval/LookupEventsWithGraphs {})

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupEventsWithGraphs
 [:eval '(agentlang.evaluator.exec-graph/events-with-exec-graphs)])

(ln/event :Agentlang.Kernel.Eval/GetExecGraph {:Key :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/GetExecGraph
 [:eval '(agentlang.evaluator.exec-graph/get-exec-graph :Agentlang.Kernel.Eval/GetExecGraph.Key)])
