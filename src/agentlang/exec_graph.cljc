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

(defn- push-node [tag n]
  (let [oldg (get-current-graph)
        newg {:graph tag :name n :patterns []}]
    (when oldg (push-graph! oldg))
    (set-current-graph! newg)))

(defn- update-node-result [result]
  (let [currg (assoc (get-current-graph) :result result)]
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

(defn- init-graph [tag n]
  (if (exec-graph-enabled?)
    (push-node tag n)
    n))

(def init-event-graph (partial init-graph :event))
(def init-agent-graph (partial init-graph :agent))

(def add-node init-graph)

(def add-event-node init-event-graph)
(def add-agent-node init-agent-graph)

(declare save-current-graph)

(defn exit-node [result]
  (when (exec-graph-enabled?)
    (let [empty-stack? (all-nodes-popped?)]
      (update-node-result result)
      (when empty-stack?
        (save-current-graph))))
  result)

(ln/entity
 :Agentlang.Kernel.Eval/ExecutionGraph
 {:Name {:type :String :id true}
  :Graph :Text
  :Created {:type :String :default dt/now}})

(ln/event :Agentlang.Kernel.Eval/CreateExecutionGraph {:Name :String :Graph :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/CreateExecutionGraph
 [:delete {:Agentlang.Kernel.Eval/ExecutionGraph
           {:Name? :Agentlang.Kernel.Eval/CreateExecutionGraph.Name}}]
 [:delete :Agentlang.Kernel.Eval/ExecutionGraph :purge]
 {:Agentlang.Kernel.Eval/ExecutionGraph
  {:Name :Agentlang.Kernel.Eval/CreateExecutionGraph.Name
   :Graph :Agentlang.Kernel.Eval/CreateExecutionGraph.Graph}})

(ln/event :Agentlang.Kernel.Eval/LoadExecutionGraph {:Name :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/LoadExecutionGraph
 {:Agentlang.Kernel.Eval/ExecutionGraph
  {:Name? :Agentlang.Kernel.Eval/LoadExecutionGraph.Name}})

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
   {:Name (pr-str (:name g)) :Graph "--"}))

(def ^:private saved-graphs (u/make-cell []))

(defn graph? [x] (and (map? x) (:graph x) (:patterns x)))
(defn event-graph? [g] (and (graph? g) (= :event (:graph g))))
(defn agent-graph? [g] (and (graph? g) (= :agent (:graph g))))
(def graph-name :name)
(def graph-result :result)
(def graph-nodes :patterns)

(defn pattern? [x] (and (map? x) (:pattern x)))
(def pattern :pattern)
(def pattern-result :result)

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
        ag (find-real-agent-graph n nodes)]
    (or ag g)))

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
               #(:result (gs/evaluate-dataflow
                          {:Agentlang.Kernel.Eval/CreateExecutionGraph
                           {:Name (pr-str (:name g)) :Graph (pr-str g)}})))
              (make-empty-exec-graph g))]
      (when-not (cn/instance-of? :Agentlang.Kernel.Eval/ExecutionGraph r)
        (log/error (str "Failed to save graph for " (:name g))))
      (when save? (u/safe-set saved-graphs (conj @saved-graphs (:name g))))
      (reset-current-graph!)
      (reset-graph-stack!)))
  true)

(defn load-graph
  ([graph-name]
   (when-let [g (call-disabled
                 #(first
                   (:result
                    (gs/evaluate-dataflow
                     {:Agentlang.Kernel.Eval/LoadExecutionGraph
                      {:Name (pr-str graph-name)}}))))]
     (u/parse-string (:Graph g))))
  ([]
   (when-let [n (peek @saved-graphs)]
     (load-graph n))))

(defn saved-graph-names [] @saved-graphs)

(defn pop-saved-graph-name []
  (let [sgs @saved-graphs]
    (when-let [n (peek sgs)]
      (u/safe-set saved-graphs (pop sgs)))))

(defn reset-saved-graph-names []
  (let [sgs @saved-graphs]
    (u/safe-set saved-graphs [])
    sgs))

