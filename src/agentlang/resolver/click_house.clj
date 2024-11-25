(ns agentlang.resolver.click-house
  (:require [clojure.string :as s]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry :refer [defmake]]
            [agentlang.store.util :as stu]
            [agentlang.store.sql :as sql]
            [agentlang.store.jdbc-cp :as cp]
            [agentlang.store.jdbc-internal :as ji])
  (:import [java.sql Connection DriverManager PreparedStatement
            ResultSet SQLException Statement]
           [javax.sql DataSource]
           [java.util Collections Properties]
           [com.clickhouse.jdbc ClickHouseDataSource]))

(defn- str-csv [mk xs]
  (reduce (fn [s arg] (str s (when (seq s) ", ") (mk arg))) "" xs))

(defn- as-table-name
  ([entity-name need-db-name]
   (let [[c n] (li/split-path entity-name)
         dbname (s/replace (name c) "." "_")
         tabname (str dbname "." (name n))]
     (if need-db-name
       [tabname dbname]
       tabname)))
  ([entity-name] (as-table-name entity-name false)))

(defn- as-raw-sql-val [v]
  (if (or (string? v) (number? v) (boolean? v))
    v
    (stu/encode-clj-object v)))

(defn- as-quoted-sql-val [v]
  (let [v (as-raw-sql-val v)]
    (if (string? v)
      (str "'" (s/replace v #"'" "''") "'")
      v)))

(defn- execute-sql [ds sql]
  (let [^Connection conn (ds)]
    (try
      (.execute (.createStatement conn) sql)
      (finally
        (.close conn)))))

(defn- ch-create [ds instance]
  (let [n (cn/instance-type-kw instance)
        table-name (as-table-name n)
        attrs (cn/instance-attributes instance)
        anames (keys attrs)
        values (str-csv #(as-quoted-sql-val (% attrs)) anames)
        values (s/replace values #"\\\"" "\\\\\\\\\"")
        sql (str "INSERT INTO " table-name " ("
                 (str-csv name anames)
                 ") VALUES ("
                 values
                 ")")]
    (execute-sql ds sql)
    instance))

(defn- do-update [ds entity-name id-attr-name id-attr-val instance]
  (let [table-name (as-table-name entity-name)
        attrs (dissoc (cn/instance-attributes instance) id-attr-name)
        anames (keys attrs)
        sql (str "ALTER TABLE " table-name " UPDATE "
                 (str-csv #(let [v (% attrs)]
                             (str (name %) " = " (as-quoted-sql-val v)))
                          anames)
                 " WHERE " (name id-attr-name) " = " (as-quoted-sql-val id-attr-val))]
    (execute-sql ds sql)
    instance))

(defn- ch-update [ds instance]
  (let [n (cn/instance-type-kw instance)
        idattr (cn/identity-attribute-name n)]
    (if-let [idval (idattr instance)]
      (do-update ds n idattr idval instance)
      (u/throw-ex (str "update required primary key - " n)))))

(defn- ch-delete [ds instance]
  (let [n (cn/instance-type-kw instance)
        attrs (cn/instance-attributes instance)
        anames (keys attrs)
        table-name (as-table-name n)
        sql (str "ALTER TABLE " table-name " DELETE WHERE "
                 (s/join " AND " (mapv #(str (name %) " = " (as-quoted-sql-val (% attrs))) anames)))]
    (execute-sql ds sql)
    instance))

(defn- format-raw-results [rs]
  (mapv (fn [r]
          (into
           {}
           (mapv (fn [[k v]]
                   (let [[a b] (s/split (str k) #"/")
                         n (or b a)]
                     [(keyword n) v]))
                 r)))
        rs))

(defn- make-select-clause [with-attrs]
  (reduce (fn [s [n k]]
            (let [parts (li/path-parts k)
                  t (li/make-path (:component parts) (:record parts))
                  c (first (:refs parts))]
              (str (if (seq s) ", " "") (as-table-name t) "." (name c) " AS " (name n))))
          "" with-attrs))

(defn- query-by-sql [ds sql params format]
  (let [^Connection conn (ds)]
    (try
      (let [sql (if (string? sql) sql (:query sql))
            [pstmt sanitized-params] (ji/do-query-statement conn sql params)
            results (ji/execute-stmt-once! conn pstmt sanitized-params)]
        (format results))
      (finally
        (.close conn)))))

(defn- as-sql-expr [[opr attr v]]
  [(str (name attr) " " (name opr) " ?") [(as-raw-sql-val v)]])

(defn- compile-query [[entity-name {w :where :as query}]]
  ;; TODO: support patterns with direct `:where` clause.
  (cond
    (or (:join query) (:left-join query))
    [(sql/format-join-sql as-table-name name false (as-table-name (:from query)) query)
     format-raw-results]

    (or (= w :*) (nil? (seq w)))
    (let [wa (:with-attributes query)
          clause (if wa (make-select-clause wa) "*")]
      [(str "SELECT " clause " FROM " (as-table-name entity-name))
       (partial stu/results-as-instances entity-name name)])

    :else
    (let [opr (first w)
          where-clause (case opr
                         (:and :or)
                         (let [sql-exp (mapv as-sql-expr (rest w))
                               exps (mapv first sql-exp)
                               params (flatten (mapv second sql-exp))]
                           [(s/join (str " " (s/upper-case (name opr)) " ") exps) params])
                         (as-sql-expr w))
          table-name (as-table-name entity-name)
          with-attrs (:with-attributes query)
          select-clause (if with-attrs (make-select-clause with-attrs) "*")]
      [(str "SELECT " select-clause " FROM " table-name " WHERE " (first where-clause))
       (second where-clause)
       (if with-attrs format-raw-results (partial stu/results-as-instances entity-name name))])))

(defn- ch-query [ds query-pattern]
  (let [[sql params fmt] (compile-query query-pattern)]
    (query-by-sql ds sql params fmt)))

(defn- as-ch-type [attr-type]
  (if-let [rtp (k/find-root-attribute-type attr-type)]
    (let [[a b] (li/split-path rtp)
          tp (or b a)]
      (case tp
        (:String :Keyword :Email :Password
                 :Time :Edn :Any :Path :Map
                 :List :DateTime :Date) "String"
        (:UUID :Identity) "UUID"
        (:Int :Int64 :Integer :BigInteger) "Int64"
        :Float "Float32"
        (:Double :Decimal) "Float64"
        :Boolean "Boolean"
        "String"))
    "String"))

(defn- create-table-sql [table-name entity-name entity-schema]
  (let [attrs (keys entity-schema)
        atypes (mapv (fn [a] [a (as-ch-type (cn/attribute-type entity-schema a))]) attrs)]
    (str "CREATE TABLE IF NOT EXISTS " table-name " ("
         (str-csv #(str (name (first %)) " " (second %)) atypes)
         ") PRIMARY KEY(" (name (cn/identity-attribute-name entity-name)) ")")))

(defn- create-view-sql [table-name view-query]
  (let [[q _ _] (compile-query view-query)
        sql (if (string? q) q (:query q))]
    (str "CREATE VIEW IF NOT EXISTS " table-name " AS " sql)))

(def ^:private inited-components (atom #{}))

(defn- ch-on-set-path [ds [_ path]]
  (let [^Connection conn (ds)]
    (try
      (let [^Statement stmt (.createStatement conn)
            [table-name dbname] (as-table-name path true)
            [c _] (li/split-path path)]
        (when-not (some #{c} @inited-components)
          (let [sql (str "CREATE DATABASE IF NOT EXISTS " dbname)]
            (.execute stmt sql)
            (swap! inited-components conj c)))
        (let [scm (stu/find-entity-schema path)
              sql (if-let [vq (cn/view-query path)]
                    (create-view-sql table-name [(:from vq) vq])
                    (create-table-sql table-name path scm))]
          (.execute stmt sql)
          path))
      (finally
        (.close conn)))))

(defn- ^Properties as-ch-props [a-map]
  (let [^Properties props (Properties.)]
    (doseq [[k v] a-map]
      (.set props (name k) v))
    props))

(defmake :click-house
  (fn [resolver-name config]
    (let [^Properties props (as-ch-props (:properties config))
          ^DataSource dsobj (cp/as-pooled
                             (ClickHouseDataSource.
                              (or (:url config) (u/getenv "CLICK_HOUSE_URL" "jdbc:ch://localhost"))
                              props)
                             (get config :statement-cache-size 10))
          ds #(.getConnection dsobj
                              (or (:username config)
                                  (u/getenv "CLICK_HOUSE_USER" "default"))
                              (or (:password config) (u/getenv "CLICK_HOUSE_PASSWORD")))
          handlers {:create (partial ch-create ds)
                    :update (partial ch-update ds)
                    :delete (partial ch-delete ds)
                    :query (partial ch-query ds)
                    :on-set-path (partial ch-on-set-path ds)}]
      (r/make-resolver resolver-name handlers))))
