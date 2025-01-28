(ns agentlang.evaluator.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.evaluator.internal :as ei]
            [agentlang.evaluator.state :as es]))

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

(defn graph-walk! [g on-sub-graph! on-node!]
  (doseq [n (graph-nodes g)]
    (if (graph? n)
      (on-sub-graph! n)
      (on-node! n))))

(defn- dissoc-env [result]
  (cond
    (map? result)
    (dissoc result :env)

    (vector? result)
    (mapv dissoc-env result)

    :else result))

(defn trim-graph [g]
  (let [[evt r] (:event g)
        nodes (mapv (fn [n]
                      (if (graph? n)
                        (trim-graph n)
                        (let [[p r] n]
                          [p (dissoc-env r)])))
                    (graph-nodes g))]
    (assoc g :nodes nodes :event [evt (dissoc-env r)])))

(defn- finalize-graph! []
  (when-let [g (get-graph)]
    ((es/get-active-evaluator)
     (ei/mark-internal
      (cn/make-instance
       {:Agentlang.Kernel.Eval/Create_ExecGraph
        {:Instance
         {:Agentlang.Kernel.Eval/ExecGraph
          {:Graph g}}}})))))

(defn- cleanup-result [result]
  (cond
    (map? result)
    (assoc result :env (env/cleanup (:env result)))

    (vector? result)
    (mapv cleanup-result result)

    :else result))

(defn add-node [{pattern :Pattern df-start? :DfStart df-end? :DfEnd} result]
  (let [result (cleanup-result result)
        new-g
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
