(ns agentlang.lang.tools.mcp.client
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]            
            [agentlang.component :as cn]
            [agentlang.datafmt.json :as json] 
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.core :as rc]
            [agentlang.lang.tools.openapi :as openapi]
            [agentlang.util.logger :as log])
  (:import [agentlang.mcp Client]))

(defn- format-client-object [obj]
  {:handle (get obj "handle")
   :tools (mapv (fn [[n t]]
                  {:name n
                   :description (get t "description")
                   :schema (openapi/attributes-from-properties (json/decode (get t "schema")))})
                (get obj "tools"))})

(declare call-tool)

(defn- invoke-tool [client instance]
  (let [[_ n] (li/split-path (cn/instance-type instance))]
    (call-tool
     client (name n)
     (into {} (mapv (fn [[k v]] [(u/keyword-as-string k) v]) (cn/instance-user-attributes instance))))))

(defn- component-from-client [server-name client]
  (let [cn (ln/component server-name)
        ents (mapv (fn [tool]
                     (ln/entity
                      (li/make-path cn (keyword (:name tool)))
                      (:schema tool)))
                   (:tools client))
        res (li/make-path cn :Resolver)
        paths (rr/override-resolver
               (vec ents)
               (rc/make-resolver res {:create (partial invoke-tool client)}))]
    (log/info (str "MCP component created: " cn))
    (when (seq paths)
      (log/info (str "Resolver " res " registered for paths " paths)))
    (assoc client :component cn :resolver res)))

(defn make-client-transport [cmd cmd-args]
  (Client/makeTransport cmd cmd-args))

(defn init [server-name transport]
  (when-let [obj (Client/makeSyncClient transport)]
    (component-from-client server-name (format-client-object obj))))

(defn call-tool [client tool-name tool-params]
  (Client/callTool (:handle client) tool-name tool-params))

(defn close [client]
  (when (Client/close (:handle client))
    (dissoc client :handle)))
