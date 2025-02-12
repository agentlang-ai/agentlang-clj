(ns agentlang.intercept.core
  (:require [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.global-state :as gs]))

(def ^:private interceptors (u/make-cell []))

(defn add-interceptor! [spec]
  (let [n (:name spec)
        f (:fn spec)]
    (if (and n f)
      (do (u/call-and-set
           interceptors
           (fn []
             (let [ins @interceptors]
               (if-not (some #{n} (mapv :name ins))
                 (conj ins spec)
                 (u/throw-ex (str "duplicate interceptor - " n))))))
          (:name spec))
      (u/throw-ex (str :name " and " :fn " required in interceptor spec - " spec)))))

(defn reset-interceptors! [] (u/safe-set interceptors []))

(defn- do-intercept-opr [intercept-fn env data continuation]
  (if (env/interceptors-blocked? env)
    (continuation data)
    (intercept-fn env data continuation)))
