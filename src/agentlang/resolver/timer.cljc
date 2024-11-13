(ns agentlang.resolver.timer
  (:require [agentlang.util :as u]
            [agentlang.util.seq :as su]
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

;; TODO:
;; If the timer-manager flag is set in config:
;; 1. On startup, check for runnable timers in the store and start them.
;; 2. Periodically check for failing nodes and restart them.

(def ^:private handles (u/make-cell {}))

#?(:clj
   (def ^:private ^ExecutorService executor (Executors/newCachedThreadPool)))

(defn- update-task-handle! [task-name handle]
  (u/safe-set handles (assoc @handles task-name handle)))

(defn- expiry-as-ms [inst]
  (let [n (:Expiry inst)]
    (case (u/string-as-keyword (:ExpiryUnit inst))
      :Seconds (* 1000 n)
      :Minutes (* 60 1000 n)
      :Hours (* 60 60 1000 n)
      :Days (* 24 60 60 1000 n))))

(defn- sleep [task-name secs]
  (when (pos? secs)
    #?(:clj
       (try
         (.sleep TimeUnit/SECONDS secs)
         (catch Exception ex
           (log/error (str "task " - task-name " sleep interrupted - " ex)))))))

(defn- update-heartbeat! [task-name]
  (let [result
        (first
         ((es/get-safe-eval-patterns)
          :Agentlang.Kernel.Lang
          [{:Agentlang.Kernel.Lang/SetTimerHeartbeat
            {:TimerName task-name}}]))]
    (if result
      task-name
      (log/warn (str "failed to update heartbeat for timer " task-name ", " (:status result))))))

(defn- set-status! [status task-name]
  (let [result
        (first
         ((es/get-safe-eval-patterns)
          :Agentlang.Kernel.Lang
          [{:Agentlang.Kernel.Lang/SetTimerStatus
            {:TimerName task-name :Status status}}]))]
    (if result
      task-name
      (log/warn (str "failed to set status for timer " task-name ", " (:status result))))))

(def set-status-ok! (partial set-status! "term-ok"))
(def set-status-error! (partial set-status! "term-error"))
(def set-status-terminating! (partial set-status! "terminating"))
(def set-status-running! (partial set-status! "running"))

(defn- cancel-task! [task-name]
  (when-let [handle (get @handles task-name)]
    #?(:clj
       (.cancel ^Future handle true)
       :cljs
       (js/clearTimeout handle))))

(defn- timer-expiry-as-seconds [inst]
  (let [unit (u/string-as-keyword (:ExpiryUnit inst))
        expiry (:Expiry inst)
        expiry-secs
        (case unit
          :Seconds expiry
          :Minutes (* expiry 60)
          :Hours (* expiry 3600)
          :Days (* expiry 86400))
        current-time-secs (dt/unix-timestamp)]
    (if (> current-time-secs (+ (:CreatedTimeSecs inst) expiry-secs))
      1
      expiry-secs)))

(defn- run-task [inst]
  (let [n (:Name inst)]
    (log/info (str "running timer task - " n))
    (try
      (let [result ((es/get-active-evaluator) (cn/make-instance (:ExpiryEvent inst)))]
        (set-status-ok! n)
        (log/info (str "timer " n " result: " result))
        result)
      (catch #?(:clj Exception :cljs js/Error) ex
        (set-status-error! n)
        (log/error (str "error in task callback - " ex))))))

(defn- timer-cancelled? [n]
  (let [inst (first
              ((es/get-safe-eval-patterns)
               :Agentlang.Kernel.Lang
               [{:Agentlang.Kernel.Lang/Lookup_Timer {:Name n}}]))]
    (if inst
      (= "term-cancel" (:Status inst))
      (do (log/warn (str "failed to check cancelled status of timer " n))
          true))))

(def ^:private heartbeat-secs 5)

(defn- make-callback [inst]
  (let [expire-secs (timer-expiry-as-seconds inst)
        n (:Name inst)]
    (fn []
      (try
        (loop [rem-secs expire-secs, check-cancel false]
          (if (<= rem-secs heartbeat-secs)
            (do (sleep n rem-secs)
                (if (and check-cancel (timer-cancelled? n))
                  (cancel-task! n)
                  (do (set-status-terminating! n)
                      (let [r (run-task inst)]
                        (if (:Restart inst)
                          (do (set-status-running! n)
                              (recur expire-secs true))
                          r)))))
            (do (sleep n heartbeat-secs)
                (if (and check-cancel (timer-cancelled? n))
                  (cancel-task! n)
                  (do (update-heartbeat! n)
                      (recur (- rem-secs heartbeat-secs) true))))))
        (catch #?(:clj Exception :cljs js/Error) ex
          #?(:clj (log/error ex)))))))

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

(defn- no-heartbeat? [timer]
  (let [secs (dt/unix-timestamp)]
    (> (- secs (:LastHeartbeatSecs timer)) 10)))

(defn- start-timer [timer]
  (when (timer-upsert timer)
    (set-status-running! (:Name timer))
    (assoc timer :Status "running")))

(defn- extract-result [result]
  (when-let [rs (first result)]
    (when (= :ok (:status rs))
      (:result rs))))

(defn restart-all-runnable []
  (su/nonils
   (mapv
    (fn [timer]
      (case (:Status timer)
        "ready" (start-timer timer)
        "running" (when (no-heartbeat? timer) (start-timer timer))))
    (extract-result ((es/get-active-evaluator) (cn/make-instance {:Agentlang.Kernel.Lang/FindRunnableTimers {}}))))))
