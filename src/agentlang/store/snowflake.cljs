
(ns agentlang.store.snowflake
  "The storage layer implementation for Snowflake."
  (:require [agentlang.util :as u]
            [agentlang.store.protocol :as p]
            [agentlang.store.util :as su]
            [agentlang.store.jdbc-cp :as cp]
            [agentlang.store.db-common :as db]
            [agentlang.store.snowflake-internal :as si]
            [agentlang.component :as cn]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]))

(def ^:private driver-class "net.snowflake.client.jdbc.SnowflakeDriver")
(def ^:private jdbc-url-prefix "jdbc:snowflake://")

(defn make []
  (let [datasource (u/make-cell)]
    (reify p/Store
      (parse-connection-info [_ connection-info]
        (let [connection-info (su/normalize-connection-info connection-info)
              account (or (:account connection-info)
                          (System/getenv "SNOWFLAKE_ACCOUNT"))
              warehouse (or (:warehouse connection-info)
                            (System/getenv "SNOWFLAKE_WAREHOUSE"))
              jdbc-url (str jdbc-url-prefix
                            account
                            ".snowflakecomputing.com"
                            "?warehouse=" warehouse
                            "&db=" (or (:dbname connection-info)
                                       (System/getenv "SNOWFLAKE_DB"))
                            "&schema=" (or (:schema connection-info)
                                           (System/getenv "SNOWFLAKE_SCHEMA")
                                           "PUBLIC")
                            "&JDBC_QUERY_RESULT_FORMAT=JSON"
                            "&SESSION_TIMEZONE=UTC")
              username (or (:username connection-info)
                           (System/getenv "SNOWFLAKE_USER"))
              password (or (:password connection-info)
                           (System/getenv "SNOWFLAKE_PASSWORD"))]
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
        (let [schema-name (su/db-schema-for-component component-name)]
          (db/execute-fn!
           @datasource
           (fn [txn]
             (si/with-json-results txn)
             (si/create-schema-if-not-exists txn schema-name)

             (doseq [ename (cn/entity-names component-name false)]
               (when-not (cn/entity-schema-predefined? ename)
                 (let [schema (su/find-entity-schema ename)
                       table-name (su/entity-table-name ename)]
                   (si/create-table txn table-name schema))))
             component-name))))

      (drop-schema [_ component-name]
        (let [schema-name (su/db-schema-for-component component-name)]
          (db/execute-fn!
           @datasource
           (fn [txn]
             (si/drop-schema-if-exists txn schema-name)))
          component-name))

      (drop-entity [_ entity-name]
        (db/drop-entity @datasource entity-name))

      (upsert-instance [_ entity-name instance]
        (db/upsert-instance
         si/upsert-inst-statement
         @datasource entity-name instance))

      (create-instance [_ entity-name instance]
        (db/create-instance @datasource entity-name instance))

      (update-instance [_ entity-name instance]
        (db/update-instance @datasource entity-name instance))

      (delete-by-id [_ entity-name id-attr-name id]
        (db/delete-by-id @datasource entity-name id-attr-name id))

      (delete-all [_ entity-name purge]
        (db/delete-all @datasource entity-name purge))

      (delete-children [_ entity-name path]
        (db/delete-children @datasource entity-name path))

      (query-by-id [_ entity-name query ids]
        (let [schema (su/find-entity-schema entity-name)]
          (db/execute-fn!
           @datasource
           (fn [conn]
             (si/with-json-results conn)
             (->> (for [id ids]
                    (let [[pstmt params schema]
                          (si/query-by-id-statement conn query id schema)]
                      (si/handle-query-result
                       (jdbc/execute! (jdbcp/set-parameters pstmt params))
                       schema)))
                  (filter seq)
                  (apply concat)
                  distinct
                  vec)))))

      (query-by-unique-keys [_ entity-name unique-keys unique-values]
        (db/query-by-unique-keys
         @datasource entity-name unique-keys unique-values))

      (query-all [_ entity-name query]
        (let [schema (su/find-entity-schema entity-name)
              destructuring (:destructuring query)]
          (db/execute-fn!
           @datasource
           (fn [conn]
             (si/with-json-results conn)
             (let [results (si/handle-query-result
                            (jdbc/execute! conn [(if (map? query)
                                                   (:query query)
                                                   query)])
                            schema)]
               (if destructuring
                 (si/process-destructuring-result results destructuring)
                 results))))))

      (do-query [_ query params]
        (db/execute-fn!
         @datasource
         (fn [conn]
           (si/with-json-results conn)
           (let [[stmt params] (db/do-query-statement conn query params)]
             (db/execute-stmt-once! conn stmt params)))))

      (call-in-transaction [_ f]
        (db/transact-fn! @datasource
                         (fn [conn]
                           (si/with-json-results conn)
                           (si/begin-transaction! conn)
                           (try
                             (let [result (f conn)]
                               (si/commit-transaction! conn)
                               result)
                             (catch Exception e
                               (si/rollback-transaction! conn)
                               (throw e))))))

      (compile-query [_ query-pattern]
        (db/compile-query query-pattern))

      (get-reference [_ path refs])

      (execute-migration [_ progress-callback from-vers to-vers components]
        (db/execute-migration
         @datasource
         progress-callback
         from-vers
         to-vers
         components)))))