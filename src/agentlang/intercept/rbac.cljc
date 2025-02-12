(ns agentlang.intercept.rbac
  (:require [clojure.set :as set]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn- run [env opr arg]
  (if-not gs/audit-trail-mode
    arg))

(defn make [_] ; config is not used
  {:name :rbac :fn run})
