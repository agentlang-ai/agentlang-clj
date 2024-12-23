(ns agentlang.store.sqlite-internal
  (:require [next.jdbc :as jdbc]
            [clojure.set :as set]
            [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.store.util :as su]
            [agentlang.component :as cn])
  (:import [java.sql PreparedStatement]))

(defn- set-excluded-columns [col-names]
  (loop [cs col-names, s ""]
    (if-let [c (first cs)]
      (recur (rest cs)
             (str s " " c " = EXCLUDED." c
                  (when (seq (rest cs))
                    ", ")))
      s)))

(defn upsert-inst-statement [conn table-name id obj]
  (let [[entity-name instance] obj
        scm (:schema (cn/find-entity-schema entity-name))
        id-attrs (cn/identity-attributes scm)
        immutable-attrs (cn/immutable-attributes scm)
        ignore-attrs (set/intersection
                      (set (mapv su/attribute-column-name id-attrs))
                      (set (mapv su/attribute-column-name immutable-attrs)))
        id-attr-nm (su/attribute-column-name (first id-attrs))
        ks (keys (cn/instance-attributes instance))
        col-names (mapv #(str "_" (name %)) ks)
        col-vals (u/objects-as-string (mapv #(% instance) ks))
        sql (str "INSERT INTO " table-name " ("
                 (us/join-as-string col-names ", ")
                 ") VALUES ("
                 (us/join-as-string (mapv (constantly "?") col-vals) ", ")
                 ")  ON CONFLICT (" id-attr-nm ") DO UPDATE SET"
                 (set-excluded-columns
                  (set/difference (set col-names) ignore-attrs)))]
    [(jdbc/prepare conn [sql]) col-vals]))

(defn query-by-id-statement [conn query-sql id]
  (let [^PreparedStatement pstmt (jdbc/prepare conn [query-sql])]
    (.setObject pstmt 1 id)
    [pstmt nil]))
