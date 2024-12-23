(ns agentlang.store.sqlite
  "The storage layer implementation for SQLite."
  (:require [agentlang.util :as u]
            [agentlang.store.protocol :as p]
            [agentlang.store.util :as su]
            [agentlang.store.jdbc-cp :as cp]
            [agentlang.store.db-common :as db]
            [agentlang.store.sqlite-internal :as pi]))

(def ^:private dbtype "sqlite")

(defn- maybe-uuid-from-str [x]
  (if (string? x)
    (u/uuid-from-string x)
    x))

(defn- default-sqlite-db-name []
  (str "agentlang.sqlite." (System/currentTimeMillis) ".db"))

(defn  make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (parse-connection-info [_ connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              dbname (or (:dbname connection-info)
                         (System/getenv "SQLITE_DB_PATH")
                         (default-sqlite-db-name))]
          {:dbname dbname}))
      (open-connection [store connection-info]
        (let [{dbname :dbname}
              (p/parse-connection-info store connection-info)]
          (u/safe-set-once
           datasource
           #(let [dbspec {:dbtype dbtype
                          :dbname dbname}]
              (cp/open-non-pooled-datasource dbspec)))
          true))
      (close-connection [_])
      (connection-info [_]
        (or @datasource {}))
      (create-schema [_ component-name]
        (db/create-schema @datasource component-name false))
      (drop-schema [_ component-name]
        (db/drop-schema @datasource component-name))
      (drop-entity [_ entity-name]
        (db/drop-entity @datasource entity-name))
      (upsert-instance [_ entity-name instance]
        (db/upsert-instance
         pi/upsert-inst-statement
         @datasource entity-name instance))
      (create-instance [_ entity-name instance]
        (db/create-instance @datasource entity-name instance))
      (update-instance [_ entity-name instance]
        (db/update-instance @datasource entity-name instance))
      (delete-by-id [_ entity-name id-attr-name id]
        (db/delete-by-id
         db/delete-by-id-statement
         @datasource entity-name id-attr-name id))
      (delete-all [_ entity-name purge]
        (db/delete-all @datasource entity-name purge))
      (delete-children [_ entity-name path]
        (db/delete-children @datasource entity-name path))
      (query-by-id [_ entity-name query ids]
        (db/query-by-id
         pi/query-by-id-statement
         @datasource entity-name query (map maybe-uuid-from-str ids)))
      (query-by-unique-keys [_ entity-name unique-keys unique-values]
        (db/query-by-unique-keys
         pi/query-by-id-statement @datasource
         entity-name unique-keys unique-values))
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
