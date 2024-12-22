
(ns agentlang.store.snowflake-internal
  (:require [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [clojure.string :as s]
            [clojure.set :as set]
            [agentlang.util :as u]
            [agentlang.lang.datetime :as dt]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.store.util :as su]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k])
  (:import [java.sql Connection PreparedStatement]
           (java.time LocalDate LocalDateTime LocalTime)))

(def ^:private max-varchar-length "16777216")

(defn snowflake-sql-type [attr-type]
  (let [parts (li/split-path attr-type)
        tp (if (= (count parts) 1) (first parts) (second parts))]
    (case tp
      (:UUID :Identity) "VARCHAR(36)"
      (:String :Keyword :Email :Password) (str "VARCHAR(" max-varchar-length ")")
      (:DateTime :Date :Time) "TIMESTAMP_NTZ"
      (:List :Edn :Any :Map :Path) "VARIANT"
      :Int "INTEGER"
      (:Int64 :Integer :BigInteger) "BIGINT"
      :Float "FLOAT"
      :Double "DOUBLE"
      :Decimal "NUMBER(38,6)"
      :Boolean "BOOLEAN"
      (if-let [root-type (k/find-root-attribute-type attr-type)]
        (recur root-type)
        (str "VARCHAR(" max-varchar-length ")")))))

(defn- convert-result-value [v col-type]
  (cond
    (nil? v) nil
    (and (number? v) (= col-type :Boolean)) (not (zero? v))
    (and (string? v) (= col-type :Boolean)) (Boolean/parseBoolean v)
    (and (string? v) (= col-type :UUID)) (u/uuid-from-string v)
    (and (string? v) (= col-type :Keyword)) (keyword v)
    (and (string? v) (= col-type :DateTime)) (dt/parse-date-time v)
    (and (string? v) (= col-type :Date)) (dt/parse-date v)
    (and (string? v) (= col-type :Time)) (dt/parse-time v)
    (and (string? v) (or (= col-type :List)
                         (= col-type :Map)
                         (= col-type :Edn))) (read-string v)
    :else v))

(defn- convert-value [v]
  (cond
    (nil? v) nil
    (uuid? v) (str v)
    (keyword? v) (name v)
    (vector? v) (pr-str v)
    (map? v) (pr-str v)
    (seq? v) (pr-str (vec v))
    (instance? LocalDateTime v) (dt/as-string v)
    (instance? LocalDate v) (dt/as-string v)
    (instance? LocalTime v) (dt/as-string v)
    (boolean? v) v
    :else v))

(defn- destructuring-pattern-info [pattern]
  (let [parts (s/split (str pattern) #":")
        fixed-parts (take-while #(not= "&" %) parts)
        rest-var? (some #(when (= "&" %) true) parts)]
    [(count fixed-parts) rest-var?]))

(defn process-destructuring-result [results pattern]
  (when (seq results)
    (let [[fixed-count has-rest?] (destructuring-pattern-info pattern)]
      (if has-rest?
        (let [fixed (vec (take fixed-count results))
              rest-part (vec (drop fixed-count results))]
          (vec (concat fixed [rest-part])))
        (vec results)))))

(defn process-result-set [rs entity-schema]
  (->> rs
       (map #(into {} (for [[k v] %]
                        [k (convert-result-value v (get-in entity-schema [k :type]))])))
       distinct
       vec))

(defn handle-query-result [result entity-schema]
  (when (seq result)
    (process-result-set result entity-schema)))

(def ^:private jdbc-props
  {"JDBC_QUERY_RESULT_FORMAT" "JSON"
   "JDBC_TIMEZONE" "UTC"})

(defn begin-transaction! [conn]
  (jdbc/execute! conn ["BEGIN TRANSACTION"]))

(defn commit-transaction! [conn]
  (jdbc/execute! conn ["COMMIT"]))

(defn rollback-transaction! [conn]
  (jdbc/execute! conn ["ROLLBACK"]))

(defn with-json-results [conn]
  (if (instance? PreparedStatement conn)
    (doto (.unwrap conn Connection)
      (.setString "JDBC_QUERY_RESULT_FORMAT" "JSON"))
    conn))

(defn format-indexed-query [table-name indexed-attrs values]
  (str "SELECT DISTINCT * FROM " table-name
       " WHERE "
       (s/join " AND "
               (map #(str "_" (name %) " = ?")
                    indexed-attrs))
       " AND _" su/deleted-flag-col " = FALSE"))

(defn- attribute-is-optional? [entity-schema attr]
  (let [attr-schema (get entity-schema attr)
        ascm (cn/find-attribute-schema attr-schema)]
    (or (:optional ascm)
        (and (:meta entity-schema)
             (seq (:required-attributes (:meta entity-schema)))
             (not (some #{attr} (:required-attributes (:meta entity-schema))))))))

(defn- format-create-table [table-name entity-schema]
  (let [attrs (sort (keys entity-schema))
        id-attr (first (cn/identity-attributes entity-schema))
        col-defs (for [a attrs
                       :let [atype (cn/attribute-type entity-schema a)
                             sql-type (snowflake-sql-type atype)
                             is-id (= a id-attr)
                             is-unique (some #{a} (cn/unique-attributes entity-schema))
                             is-optional (attribute-is-optional? entity-schema a)]]
                   (str "_" (name a) " " sql-type
                        (when is-id " PRIMARY KEY")
                        (when is-unique " UNIQUE")
                        (when-not (or is-optional is-id) " NOT NULL")))]
    (str "CREATE TABLE IF NOT EXISTS " table-name " ("
         (s/join ", " col-defs)
         ", _" su/deleted-flag-col " BOOLEAN DEFAULT FALSE)")))

(defn create-table [conn table-name entity-schema]
  (let [create-sql (format-create-table table-name entity-schema)]
    (jdbc/execute! conn [create-sql])

    ;; Create indexes for indexed attributes
    (doseq [attr (cn/indexed-attributes entity-schema)
            :let [idx-name (str table-name "_" (name attr) "_idx")
                  col-name (str "_" (name attr))]]
      (try
        (jdbc/execute!
         conn
         [(str "CREATE INDEX " idx-name " ON " table-name "(" col-name ")")])
        (catch Exception _)))))

(defn upsert-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        schema (su/find-entity-schema entity-name)
        uk-attrs (cn/unique-attributes schema)
        id-attr (cn/identity-attribute-name entity-name)
        ks (keys (cn/instance-attributes instance))
        col-names (mapv #(str "_" (name %)) ks)
        col-vals (mapv #(convert-value (% instance)) ks)
        merge-conditions (s/join " AND "
                                 (map #(str "TARGET._" (name %) " = SOURCE._" (name %))
                                      (or (seq uk-attrs) [id-attr])))]
    [(jdbc/prepare conn
                   [(str "MERGE INTO " table-name " TARGET"
                         " USING (SELECT "
                         (s/join ", " (map #(str "? AS " %) col-names))
                         " FROM DUAL) SOURCE"
                         " ON " merge-conditions
                         " WHEN MATCHED THEN UPDATE SET "
                         (s/join ", "
                                 (map #(str "TARGET." % " = SOURCE." %)
                                      (remove #(= % (str "_" (name id-attr))) col-names)))
                         " WHEN NOT MATCHED THEN INSERT ("
                         (s/join ", " col-names)
                         ") VALUES ("
                         (s/join ", " (map #(str "SOURCE." %) col-names))
                         ")")])
     col-vals]))

(defn query-by-id-statement [conn query-sql id entity-schema]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt 1 (str id))
    [pstmt nil entity-schema]))

(defn create-schema-if-not-exists [conn schema-name]
  (jdbc/execute! conn [(str "CREATE SCHEMA IF NOT EXISTS " schema-name)]))

(defn drop-schema-if-exists [conn schema-name]
  (jdbc/execute! conn [(str "DROP SCHEMA IF EXISTS " schema-name " CASCADE")]))
