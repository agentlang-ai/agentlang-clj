(ns agentlang.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.global-state :as gs]
            [agentlang.lang.datetime :as dt]
            [agentlang.component :as cn]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(def ^:private exec-graph-enabled-flag (atom nil))

(defn- exec-graph-enabled? []
  (let [f @exec-graph-enabled-flag]
    (if (nil? f)
      (let [f (:exec-graph-enabled? (gs/get-app-config))]
        (reset! exec-graph-enabled-flag f)
        f)
      f)))

(defn call-without-exec-graph [f]
  (let [v @exec-graph-enabled-flag]
    (if v
      (do (reset! exec-graph-enabled-flag false)
          (try
            (f)
            (finally
              (reset! exec-graph-enabled-flag true))))
      (f))))

(defn call-with-exec-graph [f]
  (let [v @exec-graph-enabled-flag]
    (if v
      (f)
      (do (reset! exec-graph-enabled-flag true)
          (try
            (f)
            (finally
              (reset! exec-graph-enabled-flag v)))))))

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
  :Graph :String
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

(defn save-current-graph []
  (when (exec-graph-enabled?)
    (let [g (get-current-graph)
          r (call-without-exec-graph
             #(:result (gs/evaluate-dataflow
                        {:Agentlang.Kernel.Eval/CreateExecutionGraph
                         {:Name (pr-str (:name g)) :Graph (pr-str g)}})))]
      (when-not (cn/instance-of? :Agentlang.Kernel.Eval/ExecutionGraph r)
        (log/error (str "Failed to save graph for " (:name g))))
      (reset-current-graph!)
      (reset-graph-stack!)))
  true)

(defn load-graph [graph-name]
  (when-let [g (call-without-exec-graph
                #(first
                  (:result
                   (gs/evaluate-dataflow
                    {:Agentlang.Kernel.Eval/LoadExecutionGraph
                     {:Name (pr-str graph-name)}}))))]
    (u/parse-string (:Graph g))))

(defn graph? [x] (and (map? x) (:graph x) (:patterns x)))
(defn event-graph? [g] (and (graph? g) (= :event (:graph g))))
(defn agent-graph? [g] (and (graph? g) (= :agent (:graph g))))
(def graph-name :name)
(def graph-result :result)
(def graph-nodes :patterns)

(defn pattern? [x] (and (map? x) (:pattern x)))
(def pattern :pattern)
(def pattern-result :result)
