(ns agentlang.evaluator.exec-graph
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.lang :as ln]
            [agentlang.lang.datetime :as dt]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.lang.internal :as li]
            [agentlang.compiler.context :as ctx]
            [agentlang.evaluator.state :as es]
            [agentlang.evaluator.suspend :as sp]))

(def ^:private active-exec-graph #?(:clj (ThreadLocal.) :cljs (atom)))
(def ^:private exec-graph-stack #?(:clj (ThreadLocal.) :cljs (atom)))
(def ^:private event-stack #?(:clj (ThreadLocal.) :cljs (atom)))

(ln/entity
 :Agentlang.Kernel.Eval/ExecutionGraph
 {:Key {:type :String :guid true}
  :Graph :Any
  :Created {:type :DateTime :default dt/now}})

(ln/dataflow
 :Agentlang.Kernel.Eval/DeleteAllExecutionGraphs
 [:delete :Agentlang.Kernel.Eval/ExecutionGraph :*])

(def ^:private exec-graph-enabled-flag (atom false))

(defn enable-exec-graph! [] (reset! exec-graph-enabled-flag true))
(defn disable-exec-graph! [] (reset! exec-graph-enabled-flag false))

(def ^:dynamic internal-event-mode false)

(defn exec-graph-enabled? []
  (and @exec-graph-enabled-flag
       (not internal-event-mode)))

(defn call-with-exec-graph [f]
  (enable-exec-graph!)
  (try
    (f)
    (finally
      (disable-exec-graph!))))

(defn- evaluator []
  (fn [evt]
    (let [ev (es/get-safe-eval-atomic)]
      (binding [internal-event-mode true]
        (ev evt)))))

(defn delete-all-execution-graphs []
  (let [event {:Agentlang.Kernel.Eval/DeleteAllExecutionGraphs {}}]
    (or ((evaluator) event) true)))

(defn delete-execution-graph [k]
  (let [event {:Agentlang.Kernel.Eval/Delete_ExecutionGraph {:Key k}}]
    (or ((evaluator) event) true)))

(declare root-event)

(defn- is-user-event? [g]
  (let [evt-name (cn/instance-type-kw (root-event g))
        [c _] (li/split-path evt-name)
        parts (map keyword (s/split (name c) #"\."))]
    (not= :Agentlang (first parts))))

(defn save-execution-graph [k g]
  (when (is-user-event? g)
    (let [event {:Agentlang.Kernel.Eval/Create_ExecutionGraph
                 {:Instance
                  {:Agentlang.Kernel.Eval/ExecutionGraph
                   {:Key k :Graph g}}}}]
      ((evaluator) event))))

(defn lookup-execution-graph [k]
  (let [event {:Agentlang.Kernel.Eval/Lookup_ExecutionGraph
               {:Key k}}]
    (first ((evaluator) event))))

(defn lookup-all-execution-graphs []
  (let [event {:Agentlang.Kernel.Eval/LookupAll_ExecutionGraph {}}]
    ((evaluator) event)))

(defn lookup-top-n-execution-graphs [n]
  (take n (sort-by :Created (lookup-all-execution-graphs))))

(defn- cache-execution-graph [k g]
  (when (delete-execution-graph k)
    (save-execution-graph k g)))

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

(declare cleanup-graph)

(defn- reset-exec-graph! [k]
  (let [g (get-active-exec-graph)
        parent (pop-exec-graph)]
    (if parent
      (update-active-exec-graph! (assoc parent :nodes (conj (:nodes parent) g)))
      (do (cache-execution-graph k (cleanup-graph g))
          (update-active-exec-graph! nil)))
    (or parent g)))

(defn init [event-instance]
  (when (exec-graph-enabled?)
    (let [parent-graph (get-active-exec-graph)
          g {:nodes [[event-instance (cn/inference? (cn/instance-type-kw event-instance))]]}]
      (when parent-graph
        (push-exec-graph parent-graph))
      #?(:clj (.set active-exec-graph g)
         :cljs (reset! active-exec-graph g))
      (push-event event-instance))))

(defn- cleanup [result]
  (if (map? result)
    (let [env (env/cleanup (:env result) false)]
      (dissoc (assoc result :env env) :message))
    result))

(defn add-step! [pat result userpat?]
  (when (exec-graph-enabled?)
    (when-let [g (get-active-exec-graph)]
      (let [can-add? (if (map? pat)
                       (let [recname (li/record-name pat)]
                         (not (cn/event? recname)))
                       true)]
        (when can-add?
          (update-active-exec-graph! (assoc g :nodes (conj (:nodes g) [pat result userpat?]))))
        pat))))

(defn finalize! []
  (when (exec-graph-enabled?)
    (when-let [event-instance (pop-event)]
      (let [k (or (get-in event-instance [:EventContext :ExecId])
                  (u/uuid-string))]
        (reset-exec-graph! k)
        k))))

(defn init-node [event-instance]
  (when (get-active-exec-graph)
    (init event-instance)))

(defn finalize-node! []
  (when (get-active-exec-graph)
    (finalize!)))

(defn get-exec-graph [k]
  (:Graph (lookup-execution-graph k)))

(defn get-graphs [rs] (mapv :Graph rs))

(def delete-exec-graph delete-execution-graph)

(defn graph? [x] (and (map? x) (:nodes x)))
(defn event-info [g] (first (:nodes g)))
(defn root-event [g] (ffirst (:nodes g)))
(defn nodes [g] (vec (rest (:nodes g))))
(def node-pattern first)
(defn node-result [n] (:result (second n)))
(defn node-status [n] (:status (second n)))
(defn node-is-user-pattern? [n] (last n))

(defn- get-suspension [g]
  (when-let [g0 (last (:nodes g))]
    (when-let [n (when (graph? g0) (last (:nodes g0)))]
      (let [obj (first (node-result n))]
        (and (map? obj) (cn/instance-of? :Agentlang.Kernel.Eval/Suspension obj) obj)))))

(defn suspended? [g]
  (if (get-suspension g)
    true
    false))

(defn terminated-by-error? [g]
  (let [n (last (:nodes g))]
    (when-not (graph? n)
      (= :error (node-status n)))))

(defn filter-suspensions [graphs]
  (filter suspended? graphs))

(defn filter-errors [graphs]
  (filter terminated-by-error? graphs))

(defn root-events []
  (mapv (fn [g]
          (let [k (:Key g), v (:Graph g)]
            {:event-instance (cn/unmake-instance (root-event v))
             :graph-key k
             :suspended? (suspended? v)
             :error? (terminated-by-error? v)}))
        (lookup-all-execution-graphs)))

(defn root-event-by-graph-key [root-evts k]
  (:event-instance (first (filter #(= k (:graph-key %)) root-evts))))

(defn suspended-root-events [root-evts]
  (filter :suspended? root-evts))

(defn root-events-with-error [root-evts]
  (filter :error? root-evts))

(defn restart-suspension
  ([g restart-value]
   (binding [internal-event-mode true]
     (let [g (if (map? g) g (get-exec-graph g))]
       (when-let [susp (get-suspension g)]
         (sp/restart-suspension susp restart-value)))))
  ([g] (restart-suspension g nil)))

(defn- graph-to-event-pattern [g]
  (let [event-inst (root-event g)]
    [{(cn/instance-type-kw event-inst)
      (cn/instance-attributes event-inst)}
     (node-result (last (:nodes g)))]))

(defn drop-nodes [g n]
  (when-let [nodes (seq (rest (:nodes g)))]
    (mapv #(if (map? %) (graph-to-event-pattern %) %) (drop n nodes))))

(defn cleanup-graph [g]
  (let [ei (event-info g)
        ns0 (nodes g)
        ns (mapv (fn [n] (if (graph? n)
                           (cleanup-graph n)
                           (let [[p r userpat?] n]
                             [p (cleanup r) userpat?])))
                 ns0)]
    (assoc g :nodes (into [] (concat [ei] ns)))))

(defn eval-nodes [nodes]
  (let [env (env/prepare-for-lookups (:env (second (first nodes))))]
    (binding [ctx/dynamic-context (ctx/from-bindings env)]
      ((es/get-evaluate-patterns) env :Agentlang.Kernel.Eval (mapv first nodes)))))

(ln/event
 :Agentlang.Kernel.Eval/GetExecGraph
 {:Key :String})

(ln/dataflow
 :Agentlang.Kernel.Eval/GetExecGraph
 [:eval '(agentlang.evaluator.exec-graph/lookup-execution-graph :Agentlang.Kernel.Eval/GetExecGraph.Key)])

(ln/event
 :Agentlang.Kernel.Eval/RestartSuspensionInGraph
 {:Graph :Any :Value :Any})

(ln/dataflow
 :Agentlang.Kernel.Eval/RestartSuspensionInGraph
 [:eval '(agentlang.evaluator.exec-graph/restart-suspension
          :Agentlang.Kernel.Eval/RestartSuspensionInGraph.Graph
          :Agentlang.Kernel.Eval/RestartSuspensionInGraph.Value)])

(ln/event
 :Agentlang.Kernel.Eval/LookupRecentExecutionGraphs
 {:N :Int})

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupRecentExecutionGraphs
 [:eval '(agentlang.evaluator.exec-graph/lookup-top-n-execution-graphs
          :Agentlang.Kernel.Eval/LookupRecentExecutionGraphs.N)])

(ln/event :Agentlang.Kernel.Eval/LookupAllExecutionGraphs {})

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupAllExecutionGraphs
 [:eval '(agentlang.evaluator.exec-graph/lookup-all-execution-graphs)])

(ln/event :Agentlang.Kernel.Eval/LookupEventsWithGraphs {})

(ln/dataflow
 :Agentlang.Kernel.Eval/LookupEventsWithGraphs
 [:eval '(agentlang.evaluator.exec-graph/root-events)])
