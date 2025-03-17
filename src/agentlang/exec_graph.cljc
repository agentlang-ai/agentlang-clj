(ns agentlang.exec-graph
  (:require [agentlang.util :as u]
            [agentlang.global-state :as gs]))

(def ^:private exec-graph-enabled-flag (atom nil))

(defn- exec-graph-enabled? []
  (if (gs/kernel-mode?)
    false
    (let [f @exec-graph-enabled-flag]
      (if (nil? f)
        (let [f (:exec-graph-enabled? (gs/get-app-config))]
          (reset! exec-graph-enabled-flag f)
          f)
        f))))

(def ^:private current-graph #?(:clj (ThreadLocal.) :cljs (atom nil)))

(defn- set-current-graph! [g]
  #?(:clj (.set current-graph g)
     :cljs (reset! current-graph g))
  g)

(defn- reset-current-graph! [] (set-current-graph! nil))

(defn- get-current-graph [g]
  #?(:clj (.get current-graph [])
     :cljs (or @current-graph [])))

(def ^:private graph-stack #?(:clj (ThreadLocal.) :cljs (atom nil)))

(defn- set-graph-stack! [s]
  #?(:clj (.set graph-stack s)
     :cljs (reset! graph-stack s))
  s)

(defn- get-graph-stack [g]
  #?(:clj (.get graph-stack [])
     :cljs (or @graph-stack [])))

(defn- reset-graph-stack! [] (set-graph-stack! nil))

(defn- push-graph! [g]
  (set-graph-stack! (vec (conj (get-graph-stack) g))))

(defn- pop-graph! []
  (let [s (get-graph-stack)]
    (if-let [g (peek s)]
      (do (set-graph-stack! (pop s))
          g)
      (u/throw-ex "Cannot pop empty graph-stack"))))

(defn- push-node [tag n]
  (let [oldg (get-current-graph)
        newg {:graph tag :name n :patterns []}]
    (push-graph! oldg)
    (set-current-graph! newg)))

(defn- pop-node []
  (let [currg (get-current-graph)
        oldg (pop-graph!)
        pats (:patterns oldg)]
    (set-current-graph! (assoc oldg :patterns (vec (conj (:patterns oldg) currg))))))

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

(defn exit-node []
  (if (exec-graph-enabled?)
    (pop-node)
    true))

(defn save-current-graph []
  (when (exec-graph-enabled?)
    (u/pprint (get-current-graph)) ;; TODO: save to db
    (reset-current-graph!)
    (reset-graph-stack!))
  true)
