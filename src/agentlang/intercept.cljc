(ns agentlang.intercept
  (:require #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.intercept.rbac :as irbac]
            [agentlang.intercept.core :as interceptors]))

(def ^:private makers {:rbac irbac/make})

(defn- make-interceptor [[n config]]
  (when-let [make (makers n)]
    (log/info (str "initializing interceptor - " (name n)))
    (make config)))

(defn- normalize-config [interceptor-config]
  (if (map? interceptor-config)
    interceptor-config
    (into {} (mapv (fn [n] [n nil]) interceptor-config))))

(defn init-interceptors [interceptor-config]
  (mapv
   #(interceptors/add-interceptor!
     (make-interceptor %))
   (normalize-config interceptor-config)))

(def reset-interceptors! interceptors/reset-interceptors!)
