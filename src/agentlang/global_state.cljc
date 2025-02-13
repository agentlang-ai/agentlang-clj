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

(defn active-user [] (:User active-event-context))

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

(def ^:dynamic audit-trail-mode nil)

(def ^:dynamic migration-mode nil)

#?(:clj
   (def ^ThreadLocal error-code (ThreadLocal.))
   :cljs
   (def error-code (atom nil)))

(defn set-error-code! [code]
  #?(:clj (.set error-code code)
     :cljs (reset! error-code code)))

(defn get-error-code []
  #?(:clj (.get error-code)
     :cljs @error-code))

(defn set-error-no-perm! []
  (set-error-code! :no-permission))

(defn error-no-perm? []
  (= (get-error-code) :no-permission))

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

(defn kernel-call [f]
  (binding [kernel-mode true]
    (f)))

(defn rbac-enabled? []
  (if kernel-mode
    false
    (:rbac @app-config)))

(def ^:private evaluate-dataflow-fn (atom nil))
(def ^:private evaluate-dataflow-internal-fn (atom nil))

(defn set-evaluate-dataflow-fn! [f] (reset! evaluate-dataflow-fn f))
(defn set-evaluate-dataflow-internal-fn! [f] (reset! evaluate-dataflow-internal-fn f))

(defn evaluate-dataflow [event-instance]
  (@evaluate-dataflow-fn event-instance))

(defn evaluate-dataflow-internal [event-instance]
  (@evaluate-dataflow-internal-fn event-instance))
