(ns agentlang.store.alasql
  "The storage layer implementation for SQL.js database."
  (:require [agentlang.util :as u]
            [agentlang.store.util :as su]
            [agentlang.store.protocol :as p]
            [agentlang.store.alasql-internal :as i]
            [agentlang.store.db-common :as db]))

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (open-connection [store connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)]
          (u/safe-set-once
            datasource
            #(let [dbname (if (:dbname connection-info)
                            (:dbname connection-info)
                            (str (gensym "agentlang_db")))]
               (i/create-db dbname)))
          true))
      (close-connection [_]
        (try
          (do (u/safe-set
                datasource
                nil)
              true)
          (catch (js/Error) _ false)))
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (fetch-schema [_]
        nil)
      (update-instance [_ entity-name instance]
        (db/upsert-instance @datasource entity-name instance))
      (force-create-instance [_ entity-name instance]
        (db/force-create-instance @datasource entity-name instance))
      (delete-by-id [_ entity-name id-attr-name id]
        (db/delete-by-id @datasource entity-name id-attr-name id))
      (delete-all [_ entity-name]
        (db/delete-all @datasource entity-name))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id @datasource entity-name query ids))
      (query-by-unique-keys [store entity-name unique-keys unique-values]
        (db/query-by-unique-keys @datasource entity-name unique-keys unique-values))
      (query-all [_ entity-name query]
        (db/query-all @datasource entity-name query))
      (do-query [_ query params]
        (db/do-query @datasource query params))
      (compile-query [_ query-pattern]
        (db/compile-query query-pattern))
      (call-in-transaction [_ f]
        (f @datasource))
      (get-reference [_ path refs]))))
