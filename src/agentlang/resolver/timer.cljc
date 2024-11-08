(ns agentlang.resolver.timer
  (:require [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.component :as cn]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
            [agentlang.evaluator.state :as es]
            [agentlang.lang.datetime :as dt])
  #?(:clj
     (:import [java.util.concurrent ExecutorService Executors
               Future TimeUnit])))

(def ^:private handles (u/make-cell {}))

#?(:clj
   (def ^:private ^ExecutorService executor (Executors/newCachedThreadPool)))

(defn- update-task-handle! [task-name handle]
  (swap! handles assoc task-name handle))

(defn- expiry-as-ms [inst]
  (let [n (:Expiry inst)]
    (case (u/string-as-keyword (:ExpiryUnit inst))
      :Seconds (* 1000 n)
      :Minutes (* 60 1000 n)
      :Hours (* 60 60 1000 n)
      :Days (* 24 60 60 1000 n))))

(defn- sleep [task-name secs]
  #?(:clj
     (try
       (.sleep TimeUnit/SECONDS secs)
       (catch Exception ex
         (log/error (str "task " - task-name " sleep interrupted - " ex))))))

(defn- set-status! [status task-name]
  (let [result ((es/get-active-evaluator) (cn/make-instance {:Agentlang.Kernel.Lang/SetTimerStatus
                                                             {:TimerName task-name :Status status}}))]
    (u/pprint result)
    result))

(def set-status-ok! "term-ok")
(def set-status-error! "term-error")
(def set-status-terminating! "terminating")

(defn- cancel-task! [task-name]
  (when-let [handle (get @handles task-name)]
    #?(:clj
       (.cancel ^Future handle true)
       :cljs
       (js/clearTimeout handle))))

(defn- timer-expiry-as-seconds [inst]
  (let [unit (u/string-as-keyword (:ExpiryUnit inst))
        expiry (:Expiry inst)]
    (case unit
      :Seconds expiry
      :Minutes (* expiry 60)
      :Hours (* expiry 3600)
      :Days (* expiry 86400))))

(defn- run-task [inst]
  (let [n (:Name inst)]
    (log/info (str "running timer task - " n))
    (try
      (let [result ((es/get-active-evaluator) (cn/make-instance (:ExpiryEvent inst)))]
        (set-status-ok! n)
        result)
      (catch #?(:clj Exception :cljs js/Error) ex
        (set-status-error! n)
        (log/error (str "error in task callback - " ex))))))

(defn- timer-cancelled? [n]
  (let [result ((es/get-active-evaluator)
                (cn/make-instance
                 {:Agentlang.Kernel.Lang/Lookup_Timer {:Name n}}))]
    (println "$$$$$$$$$$$$$$$$$$$$$$$" result)
    (= "term-cancel" (:Status result))))

(def ^:private heartbeat-secs 5)

(defn- make-callback [inst]
  (let [expire-secs (timer-expiry-as-seconds inst)
        n (:Name inst)]
    (fn []
      (loop [rem-secs expire-secs, check-cancel false]
        (if (and check-cancel (timer-cancelled? n))
          (cancel-task! n)
          (if (< rem-secs heartbeat-secs)
            (do (sleep n rem-secs)
                (set-status-terminating! n)
                (run-task inst))
            (do (sleep n heartbeat-secs)
                (update-heartbeat! inst)
                (recur (- rem-secs heartbeat-secs) true))))))))

(defn timer-upsert [inst]
  #?(:clj
     (let [callback (make-callback inst)
           handle (.submit executor ^Callable callback)]
       (update-task-handle! (:Name inst) handle)
       (assoc inst :Status "running"))
     :cljs inst))

(def ^:private resolver-fns
  {:create {:handler timer-upsert}})

(defmake :timer
  (fn [_ _]
    (r/make-resolver :timer resolver-fns)))
