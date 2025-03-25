(ns agentlang.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.global-state :as gs]
            [agentlang.lang.internal :as li]
            [agentlang.lang.datetime :as dt]
            [agentlang.component :as cn]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(def ^:private exec-graph-enabled-flag #?(:clj (ThreadLocal.) :cljs (atom nil)))

(defn- enabled? []
  (let [r #?(:clj (.get exec-graph-enabled-flag)
             :cljs @exec-graph-enabled-flag)]
    (if (nil? r)
      true
      r)))

(defn enable! []
  #?(:clj (.set exec-graph-enabled-flag true)
     :cljs (reset! exec-graph-enabled-flag true)))

(defn disable! []
  #?(:clj (.set exec-graph-enabled-flag false)
     :cljs (reset! exec-graph-enabled-flag false)))

(def ^:private global-enabled-flag (atom nil))

(defn exec-graph-enabled? []
  (and (or (:exec-graph-enabled? (gs/get-app-config)) @global-enabled-flag)
       (enabled?)))

(defn call-disabled [f]
  (disable!)
  (try
    (f)
    (finally
      (enable!))))

(defn call-with-exec-graph [f]
  (reset! global-enabled-flag true)
  (try
    (f)
    (finally
      (reset! global-enabled-flag false))))

(def ^:private current-graph #?(:clj (ThreadLocal.) :cljs (atom nil)))

(defn- set-current-graph! [g]
  #?(:clj (.set current-graph g)
     :cljs (reset! current-graph g))
  g)

(defn- reset-current-graph! [] (set-current-graph! nil))

(defn- get-current-graph []
  #?(:clj (.get current-graph)
     :cljs @current-graph))

(def ^:private graph-stack #?(:clj (ThreadLocal.) :cljs (atom nil)))

(defn- set-graph-stack! [s]
  #?(:clj (.set graph-stack s)
     :cljs (reset! graph-stack s))
  s)

(defn- get-graph-stack []
  #?(:clj (or (.get graph-stack) [])
     :cljs (or @graph-stack [])))

(defn- reset-graph-stack! [] (set-graph-stack! nil))

(defn- push-graph! [g]
  (set-graph-stack! (vec (conj (get-graph-stack) g))))

(defn- pop-graph! []
  (let [s (get-graph-stack)]
    (when-let [g (peek s)]
      (do (set-graph-stack! (pop s))
          g))))

(defn- all-nodes-popped? [] (not (peek (get-graph-stack))))

(defn- push-node [tag n event-instance]
  (let [oldg (get-current-graph)
        newg (merge {:graph tag :name n :patterns [] :push-ts (dt/unix-timestamp)}
                    (when event-instance {:trigger event-instance}))]
    (when oldg (push-graph! oldg))
    (set-current-graph! newg)))

(defn- update-node-result [result]
  (let [g0 (assoc (get-current-graph) :result result :pop-ts (dt/unix-timestamp))
        currg (if (map? result)
                (cond
                  (:suspension-id result) (assoc g0 :suspended? true)
                  (:error result) (assoc g0 :error? true)
                  :else g0)
                g0)]
    (if-let [oldg (pop-graph!)]
      (let [pats (:patterns oldg)]
        (set-current-graph! (assoc oldg :patterns (vec (conj (:patterns oldg) currg)))))
      (set-current-graph! currg))
    result))

(defn add-pattern [pat result]
  (when (exec-graph-enabled?)
    (let [g (get-current-graph)]
      (if-let [pats (:patterns g)]
        (set-current-graph! (assoc g :patterns (vec (conj pats {:pattern pat :result result}))))
        (u/throw-ex "Cannot add patterns - no active execution graph."))))
  true)

(defn- init-graph
  ([tag n event-instance]
   (if (exec-graph-enabled?)
     (push-node tag n event-instance)
     n))
  ([tag n] (init-graph tag n nil)))

(def init-event-graph (partial init-graph :event))
(def init-agent-graph (partial init-graph :agent))

(def add-node init-graph)

(def add-event-node init-event-graph)
(def add-agent-node init-agent-graph)

(declare save-current-graph)

(defn- exit-all-nodes []
  (loop [empty-stack? (all-nodes-popped?)]
    (if empty-stack?
      (save-current-graph)
      (do (update-node-result nil)
          (recur (all-nodes-popped?))))))

(defn- error-result? [result]
  (and (map? result) (:error result)))

(defn exit-node [result]
  (when (and (exec-graph-enabled?) (get-current-graph))
    (let [empty-stack? (all-nodes-popped?)]
      (update-node-result result)
      (cond
        empty-stack? (save-current-graph)
        (error-result? result) (exit-all-nodes))))
  result)

(ln/entity
 :Agentlang.Kernel.Eval/ExecutionGraph
 {:Id {:type :UUID :default u/uuid-string :id true}
  :Name {:type :String :indexed true}
  :Graph :Text
  :Created {:type :String :default dt/now}})

(ln/event :Agentlang.Kernel.Eval/CreateExecutionGraph {:Name :String :Graph :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/CreateExecutionGraph
 {:Agentlang.Kernel.Eval/ExecutionGraph
  {:Name :Agentlang.Kernel.Eval/CreateExecutionGraph.Name
   :Graph :Agentlang.Kernel.Eval/CreateExecutionGraph.Graph}})

(ln/event :Agentlang.Kernel.Eval/LoadExecutionGraph {:Id :UUID})

(defn parse-loaded-graph [g]
  (when g
    (assoc g :Graph (u/parse-string (:Graph g)))))

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadExecutionGraph
 {:Agentlang.Kernel.Eval/ExecutionGraph
  {:Id? :Agentlang.Kernel.Eval/LoadExecutionGraph.Id} :as [:Ex]}
 [:call '(agentlang.exec-graph/parse-loaded-graph :Ex)])

(defn user-graph? [g]
  (let [gn (:name g)]
    (if (keyword? gn)
      (let [[c n] (li/split-path gn)]
        (if (and c n)
          (not (cn/internal-component? c))
          true))
      true)))

(defn- make-empty-exec-graph [g]
  (cn/make-instance
   :Agentlang.Kernel.Eval/ExecutionGraph
   {:Name (u/keyword-as-string (:name g)) :Graph "--"}))

(def ^:private saved-graphs (u/make-cell []))

(defn graph-names [gs]
  (into {} (mapv (fn [g] [(:Id g) {:name (:Name g) :event (:trigger (u/parse-string (:Graph g)))}]) gs)))

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupEventsWithGraphs
 {:Agentlang.Kernel.Eval/ExecutionGraph? {} :as :Graphs}
 [:call '(agentlang.exec-graph/graph-names :Graphs)])

(defn graph? [x] (and (map? x) (:graph x) (:patterns x)))
(defn event-graph? [g] (and (graph? g) (= :event (:graph g))))
(defn agent-graph? [g] (and (graph? g) (= :agent (:graph g))))
(def graph-name :name)
(def graph-result :result)
(def graph-nodes :patterns)
(def graph-start-timestamp :push-ts)
(def graph-end-timestamp :pop-ts)
(def graph-suspended? :suspended?)
(def graph-error? :error?)
(def graph-error-message :error)
(def graph-event :trigger)

(defn pattern? [x] (and (map? x) (:pattern x)))
(def pattern :pattern)
(def pattern-result :result)

(defn graph-walk! [g on-sub-graph! on-pattern!]
  (doseq [n (graph-nodes g)]
    (if (graph? n)
      (on-sub-graph! n)
      (on-pattern! n))))

(defn- call-inference-pattern? [p]
  (let [pat (pattern p)]
    (and (vector? pat) (= :call (first pat))
         (= 'agentlang.inference/run-inference-for-event (first (second pat))))))

(defn- graph-name-as-kw [g]
  (let [n (graph-name g)]
    (if (keyword? n)
      n
      (keyword n))))

(defn- find-real-agent-graph [n nodes]
  (first (filter #(and (agent-graph? %) (= n (graph-name-as-kw %))) nodes)))

(defn- extract-core-agent-graph [g]
  (let [n (graph-name-as-kw g)
        nodes (graph-nodes g)
        ag (find-real-agent-graph n nodes)
        evt (graph-event g)]
    (merge (or ag g) (when evt {graph-event evt}))))

(defn- maybe-trim-agent-graph [g]
  (if (agent-graph? g)
    (let [nodes (graph-nodes g)
          final-nodes (if (call-inference-pattern? (last nodes))
                        (drop-last nodes)
                        nodes)]
      (assoc g graph-nodes (mapv #(if (agent-graph? %) (maybe-trim-agent-graph %) %) final-nodes)))
    g))

(defn save-current-graph []
  (when (exec-graph-enabled?)
    (let [g (maybe-trim-agent-graph
             (extract-core-agent-graph
              (get-current-graph)))
          save? (user-graph? g)
          r (if save?
              (call-disabled
               #(:result (gs/evaluate-dataflow-atomic
                          {:Agentlang.Kernel.Eval/CreateExecutionGraph
                           {:Name (u/keyword-as-string (:name g)) :Graph (pr-str g)}})))
              (make-empty-exec-graph g))]
      (when-not (cn/instance-of? :Agentlang.Kernel.Eval/ExecutionGraph r)
        (log/error (str "Failed to save graph for " (:name g))))
      (when save? (u/safe-set saved-graphs (conj @saved-graphs {(:Id r) (:Name r)})))
      (reset-current-graph!)
      (reset-graph-stack!)))
  true)

#?(:clj
   (defn load-graph
     ([graph-id]
      (when-let [g (call-disabled
                    #(:result
                      (gs/evaluate-dataflow
                       {:Agentlang.Kernel.Eval/LoadExecutionGraph
                        {:Id graph-id}})))]
        (:Graph g)))
     ([]
      (when-let [id (ffirst (peek @saved-graphs))]
        (load-graph id))))
   :cljs
   (defn load-graph
     ([host options graph-id]
      (:Graph
       (:result
        (uh/POST
         (str host "/api/Agentlang.Kernel.Eval/LoadExecutionGraph")
         options
         {:Agentlang.Kernel.Eval/LoadExecutionGraph
          {:Id graph-id}}))))
     ([host graph-id] (load-graph host nil graph-id))))

(defn saved-graph-names [] @saved-graphs)

(defn pop-saved-graph-name []
  (let [sgs @saved-graphs]
    (when-let [n (peek sgs)]
      (u/safe-set saved-graphs (pop sgs))
      n)))

(defn reset-saved-graph-names []
  (let [sgs @saved-graphs]
    (u/safe-set saved-graphs [])
    sgs))
