(ns agentlang.store.h2
  "The storage layer implementation for H2 database.
   Ideal for testing the storage protocol integration with
   the runtime resolver, demos etc."
  (:require [agentlang.util :as u]
            [agentlang.store.protocol :as p]
            [agentlang.store.util :as su]
            [agentlang.store.h2-internal :as h2i]
            [agentlang.store.jdbc-cp :as cp]
            [agentlang.store.db-common :as db]))

(def ^:private driver-class "org.h2.Driver")
(def ^:private url-prefix "jdbc:h2:")

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (parse-connection-info [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              jdbc-url (str url-prefix (:dbname connection-info) ";MODE=PostgreSQL")
              username (or (:username connection-info) "sa")
              password (or (:password connection-info) "sa")]
          {:url jdbc-url :username username :password password}))
      (open-connection [store connection-info]
        (let [{jdbc-url :url username :username password :password}
              (p/parse-connection-info store connection-info)]
          (u/safe-set-once
           datasource
           #(let [dbspec {:driver-class driver-class
                          :jdbc-url jdbc-url
                          :username username
                          :password password}]
              (cp/open-pooled-datasource dbspec)))
          true))
      (close-connection [_]
        (try
          (do (u/call-and-set
               datasource
               #(when @datasource
                  (cp/close-pooled-datasource @datasource)
                  nil))
              true)
          (catch Exception _ false)))
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (drop-entity [_ entity-name]
        (db/drop-entity @datasource entity-name))
      (force-create-instance [_ entity-name instance]
        (db/force-create-instance @datasource entity-name instance))
      (update-instance [_ entity-name instance]
        (db/update-instance @datasource entity-name instance))
      (create-instance [_ entity-name instance]
        (db/create-instance @datasource entity-name instance))
      (delete-by-id [_ entity-name id-attr-name id]
        (db/delete-by-id @datasource entity-name id-attr-name id))
      (delete-all [_ entity-name purge]
        (db/delete-all @datasource entity-name purge))
      (delete-children [_ entity-name path]
        (db/delete-children @datasource entity-name path))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id @datasource entity-name query ids))
      (query-by-unique-keys [store entity-name unique-keys unique-values]
        (db/query-by-unique-keys @datasource entity-name unique-keys unique-values))
      (query-all [_ entity-name query]
        (db/query-all @datasource entity-name query))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (call-in-transaction [_ f]
        (db/transact-fn! @datasource f))
      (compile-query [_ query-pattern]
        (db/compile-query query-pattern))
      (get-reference [_ path refs])
      (execute-migration [_ progress-callback from-vers to-vers components]
        (db/execute-migration @datasource progress-callback from-vers to-vers components)))))
