(ns agentlang.global-state
  (:require [environ.core :as environ]
            #?(:clj [clojure.java.io :as io])))

(def ^:private app-config (atom nil))

(defn set-app-config! [config]
  (reset! app-config config))

(defn merge-app-config! [config]
  (reset! app-config (merge @app-config config)))

(defn get-app-config []
  @app-config)

(defn audit-trail-enabled? []
  (:enable-audit-trail @app-config))

(def ^:dynamic active-event-context nil)

(defn call-with-event-context [ctx f]
  (if ctx
    (binding [active-event-context ctx]
      (f))
    (f)))

(defn active-user [] (:User active-event-context))

(defn active-session-info []
  (get-in active-event-context [:UserDetails :session-info]))

#?(:clj
   (def ^:private active-txn (ThreadLocal.))
   :cljs
   (def ^:dynamic active-txn nil))

#?(:clj
   (defn set-active-txn! [txn] (.set active-txn txn))
   :cljs
   (defn set-active-txn! [_] nil))

#?(:clj
   (defn get-active-txn [] (.get active-txn))
   :cljs
   (defn get-active-txn [] nil))

(def ^:private script-mode (atom false))

(defn in-script-mode! []
  (reset! script-mode true))

(defn in-script-mode? [] @script-mode)

(def ^:dynamic migration-mode nil)

#?(:clj
   (def ^ThreadLocal error-code (ThreadLocal.))
   :cljs
   (def error-code (atom nil)))

(defn set-error-code! [code]
  #?(:clj (.set error-code code)
     :cljs (reset! error-code code)))

(defn reset-error-code! [] (set-error-code! nil))

(defn get-error-code []
  #?(:clj (.get error-code)
     :cljs @error-code))

#?(:clj
   (def agentlang-version
     (memoize (fn []
                (or (:agentlang-version environ/env)
                    (let [projfile (io/resource "META-INF/leiningen/com.github.agentlang-ai/agentlang/project.clj")
                          project (read-string (slurp projfile))]
                      (nth project 2))))))

   :cljs
   (def agentlang-version
     (memoize (fn [] (:agentlang-version environ/env)))))

(def standalone-patterns (atom []))
(def install-init-pattern! (partial swap! standalone-patterns conj))
(defn uninstall-standalone-patterns! [] (reset! standalone-patterns nil))

(def fire-post-events (atom nil))

(def ^:dynamic kernel-mode nil)

(defn kernel-mode? [] kernel-mode)

(defn kernel-call [f]
  (binding [kernel-mode true]
    (f)))

(defn rbac-enabled? []
  (if kernel-mode
    false
    (:rbac-enabled @app-config)))

(def ^:private evaluate-dataflow-fn (atom nil))
(def ^:private evaluate-dataflow-internal-fn (atom nil))
(def ^:private evaluate-pattern-fn (atom nil))

(defn set-evaluate-dataflow-fn! [f] (reset! evaluate-dataflow-fn f))
(defn set-evaluate-pattern-fn! [f] (reset! evaluate-pattern-fn f))

(defn evaluate-dataflow
  ([event-instance]
   (@evaluate-dataflow-fn event-instance))
  ([env event-instance]
   (@evaluate-dataflow-fn nil env event-instance)))

(defn evaluate-dataflow-internal [event-instance]
  (kernel-call #(evaluate-dataflow event-instance)))

(defn evaluate-pattern
  ([env pat] (@evaluate-pattern-fn env pat))
  ([pat] (evaluate-pattern nil pat)))

(def evaluate-patterns evaluate-dataflow)

(defn evaluate-pattern-internal [env pat]
  (kernel-call #(evaluate-pattern env pat)))

(defn evaluate-dataflow-atomic
  ([evaluator arg]
   (let [txn (get-active-txn)]
     (set-active-txn! nil)
     (try
       (evaluator arg)
       (finally
         (set-active-txn! txn)))))
  ([event-instance] (evaluate-dataflow-atomic evaluate-dataflow event-instance)))

#?(:clj
   (def ^:private dataflow-suspend-flag (ThreadLocal.))
   :cljs
   (def ^:dynamic dataflow-suspend-flag nil))

(defn set-dataflow-suspended! [flag]
  #?(:clj
     (.set dataflow-suspend-flag flag)
     :cljs
     (reset! dataflow-suspend-flag flag)))

(defn dataflow-suspended? []
  #?(:clj
     (.get dataflow-suspend-flag)
     :cljs
     @dataflow-suspend-flag))
