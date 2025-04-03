(ns agentlang.intercept
  (:require #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.intercept.core :as ic]))

(def ^:private makers {})

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
   #(ic/add-interceptor!
     (make-interceptor %))
   (normalize-config interceptor-config)))

(def reset-interceptors! ic/reset-interceptors!)

(def call-interceptors-for-create (partial ic/call-interceptors :create))
(def call-interceptors-for-read (partial ic/call-interceptors :read))
(def call-interceptors-for-update (partial ic/call-interceptors :update))
(def call-interceptors-for-delete (partial ic/call-interceptors :delete))
