(ns agentlang.store.jdbc-internal
  (:require [clojure.set :as set]
            [next.jdbc :as jdbc]
            [next.jdbc.prepare :as jdbcp]
            [agentlang.global-state :as gs]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.store.util :as su]
            [clojure.string :as str])
  (:import [java.sql PreparedStatement]))

(def prepare jdbc/prepare)

(defn close-pstmt [^PreparedStatement p]
  (.close p))

(defn validate-ref-statement [conn index-tabname colname ref]
  (let [sql (str "SELECT 1 FROM " index-tabname " WHERE _" colname " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [(u/uuid-from-string ref)]]))

(defn create-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        scm (cn/fetch-entity-schema entity-name)
        ks (keys (cn/instance-attributes instance true))
        col-names (mapv #(str "_" (name %)) ks)
        col-vals (u/objects-as-string (mapv #(% instance) ks))
        sql (str "INSERT INTO " table-name " ("
                 (us/join-as-string col-names ", ")
                 ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")")]
    [(jdbc/prepare conn [sql]) col-vals]))

(defn- update-set-exprs [col-names]
  (loop [cs col-names, s ""]
    (if-let [c (first cs)]
      (let [rs (seq (rest cs))
            final-s (str s c " = ?")]
        (if-not rs
          final-s
          (recur rs (str final-s ", "))))
      s)))

(defn update-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        scm (:schema (cn/find-entity-schema entity-name))
        id-attrs (cn/identity-attributes scm)
        immutable-attrs (cn/immutable-attributes scm)
        ignore-attrs (set/intersection
                      (set (mapv su/attribute-column-name id-attrs))
                      (set (mapv su/attribute-column-name immutable-attrs)))
        ks (set/difference
            (set (keys (cn/instance-attributes instance true)))
            ignore-attrs)
        col-names (mapv su/attribute-column-name ks)
        col-vals (u/objects-as-string (mapv #(% instance) ks))
        id-attr-name (cn/identity-attribute-name entity-name)
        id-attr-val (id-attr-name instance)
        sql (str "UPDATE " table-name " SET "
                 (update-set-exprs col-names)
                 " WHERE " (su/attribute-column-name id-attr-name) " = ?")]
    [(jdbc/prepare conn [sql]) (concat col-vals [id-attr-val])]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setString pstmt 1 (str id))
    [pstmt nil]))

(defn purge-by-id-statement [conn table-name id-attr-name id]
  (let [sql (str "DELETE FROM " table-name " WHERE _" (name id-attr-name) " = ? AND _" su/deleted-flag-col " = TRUE")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn delete-by-id-statement [conn table-name id-attr-name id]
  (let [sql (str "UPDATE " table-name " SET _" su/deleted-flag-col " = TRUE WHERE _" (name id-attr-name) " = ?")
        ^PreparedStatement pstmt (jdbc/prepare conn [sql])]
    [pstmt [id]]))

(defn delete-all-statement [conn table-name purge]
  (let [sql (if purge
              (str "DELETE FROM " table-name " WHERE _" su/deleted-flag-col " = TRUE")
              (str "UPDATE " table-name " SET _" su/deleted-flag-col " = TRUE"))]
    (jdbc/prepare conn [sql])))

(defn escape-like-pattern [path]
  (-> path
      (str/replace "\\" "\\\\")
      (str/replace "[" "\\[")
      (str/replace "]" "\\]")
      (str/replace "%" "\\%")
      (str/replace "_" "\\_")
      (str/replace "$" "\\$")))

(defn delete-children-statement [conn table-name path]
  (let [escaped-path (str (escape-like-pattern path) "%")
        sql (str "UPDATE " table-name
                 " SET _" su/deleted-flag-col " = TRUE"
                 " WHERE _" (name li/path-attr) " LIKE ? ESCAPE '\\'")]
    (jdbc/prepare conn [sql escaped-path])))

(defn do-query-statement
  ([conn query-sql query-params]
   (let [^PreparedStatement pstmt
         (jdbc/prepare
          conn (cond
                 (map? query-sql)
                 (:query query-sql)

                 :else
                 [query-sql]))]
     [pstmt (mapv u/keyword-as-string query-params)]))
  ([conn query-sql]
   (jdbc/prepare conn [query-sql])))

(defn transact-fn! [datasource f]
  (with-open [conn (jdbc/get-connection datasource)]
    (jdbc/with-transaction [txn conn]
      (f txn))))

(defn execute-fn! [datasource f]
  (if-let [txn (gs/get-active-txn)]
    (f txn)
    (with-open [conn (jdbc/get-connection datasource)]
      (f conn))))

(defn execute-sql! [conn sql]
  (jdbc/execute! conn sql))

(defn- execute-prepared-stmt! [once _ stmt params]
  (try
    (if (and params (not= (first params) :*))
      (jdbc/execute! (jdbcp/set-parameters stmt params))
      (jdbc/execute! stmt))
    (finally
      (when once
        (.close stmt)))))

(def execute-stmt-once! (partial execute-prepared-stmt! true))
(def execute-stmt! (partial execute-prepared-stmt! false))
