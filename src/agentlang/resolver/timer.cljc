(ns agentlang.resolver.timer
  (:require [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.component :as cn]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry
             #?(:clj :refer :cljs :refer-macros)
             [defmake]]
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
         (:result
          (gs/evaluate-dataflow-internal
           [{:Agentlang.Kernel.Lang/SetTimerHeartbeat
             {:TimerName task-name}}])))]
    (if result
      task-name
      (log/warn (str "failed to update heartbeat for timer " task-name)))))

(defn- set-status! [status task-name]
  (let [result
        (first
         (:result
          (gs/evaluate-dataflow-internal
           [{:Agentlang.Kernel.Lang/SetTimerStatus
             {:TimerName task-name :Status status}}])))]
    (if result
      task-name
      (log/warn (str "failed to set status for timer " task-name)))))

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
    (log/debug (str "running timer task - " n))
    (try
      (let [result (:result (gs/evaluate-dataflow-atomic (cn/make-instance (:ExpiryEvent inst))))]
        (set-status-ok! n)
        (log/debug (str "timer " n " result: " result))
        result)
      (catch #?(:clj Exception :cljs js/Error) ex
        (set-status-error! n)
        (log/error (str "error in task callback - " ex))
        {:status :error}))))

(defn- timer-cancelled? [n]
  (let [inst (first
              (:result
               (gs/evaluate-dataflow-internal
                [{:Agentlang.Kernel.Lang/LookupTimer {:TimerName n}}])))]
    (if inst
      (= "term-cancel" (:Status inst))
      (do (log/warn (str "failed to check cancelled status of timer " n))
          true))))

(defn- expiry-event-successfull? [result]
  (cond
    (map? result)
    (= :ok (:status result))

    (vector? result)
    (expiry-event-successfull? (first result))

    ;; assume success, if non-nil result
    :else result))

(def ^:private heartbeat-secs 5)

(defn- make-callback [inst]
  (let [expire-secs (timer-expiry-as-seconds inst)
        n (:Name inst)]
    (fn []
      (try
        (loop [rem-secs expire-secs, check-cancel false, retries (:Retries inst)]
          (if (<= rem-secs heartbeat-secs)
            (do (sleep n rem-secs)
                (if (and check-cancel (timer-cancelled? n))
                  (cancel-task! n)
                  (do (set-status-terminating! n)
                      (let [r (run-task inst)]
                        (cond
                          (:Restart inst)
                          (do (set-status-running! n)
                              (recur expire-secs true retries))
                          (and (not (expiry-event-successfull? r))
                               (pos? retries))
                          (do (set-status-running! n)
                              (recur expire-secs true (dec retries)))
                          :else r)))))
            (do (sleep n heartbeat-secs)
                (if (and check-cancel (timer-cancelled? n))
                  (cancel-task! n)
                  (do (update-heartbeat! n)
                      (recur (- rem-secs heartbeat-secs) true retries))))))
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

(defn restart-all-runnable []
  (su/nonils
   (mapv
    (fn [timer]
      (case (:Status timer)
        "ready" (start-timer timer)
        "running" (when (no-heartbeat? timer) (start-timer timer))))
    (:result (gs/evaluate-dataflow-internal (cn/make-instance {:Agentlang.Kernel.Lang/FindRunnableTimers {}}))))))
