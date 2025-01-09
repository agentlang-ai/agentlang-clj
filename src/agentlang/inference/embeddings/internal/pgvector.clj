(ns agentlang.inference.embeddings.internal.pgvector
  (:import (org.postgresql.util PGobject)
           (com.pgvector PGvector))
  (:require [clojure.string :as s]
            [next.jdbc :as jdbc]
            [agentlang.inference.embeddings.internal.common :as vc]
            [agentlang.global-state :as gs]))

(def ^:private dbtype "postgresql")

(defn open-connection [config]
  (merge {:dbtype dbtype} config))

(defn close-connection [db-conn]
  (when (= dbtype (:dbtype db-conn))
    true))

(def ^:private init-table
  "CREATE TABLE IF NOT EXISTS text_embedding
(
    embedding_uuid      UUID        DEFAULT gen_random_uuid() PRIMARY KEY,
    embedding_classname VARCHAR(128), -- may be repeated
    text_content        TEXT        NOT NULL,
    meta_content        JSON,
    embedding_model     VARCHAR(64) NOT NULL,
    embedding_1536      VECTOR(1536),
    embedding_3072      VECTOR(3072),
    created_at          TIMESTAMP   NOT NULL DEFAULT NOW(),
    updated_at          TIMESTAMP   NOT NULL DEFAULT NOW(),
    readers             VARCHAR(64)
);")

(defn initialize-vector-table [db-conn]
  (jdbc/execute! db-conn ["CREATE EXTENSION IF NOT EXISTS vector"])
  (jdbc/execute! db-conn [init-table]))

(defn- pg-floats
  "Turn supplied collection of floating-point values into a PostgreSQL
  PGVector object suitable for use as SQL param."
  [float-coll]
  (-> float-coll
      float-array
      (PGvector.)))

(defn- pg-json
  "Turn supplied JSON string into a PostgreSQL PGobject
  object suitable for use as SQL param."
  [json-string]
  (doto (PGobject.)
    (.setType "json")
    (.setValue json-string)))

(def ^:private delete-all-sql
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ?")

(def ^:private delete-selected-sql
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ? AND meta_content -> ? ->> 'type' = ?")

(def ^:private create-object-sql-template
  "INSERT
  INTO text_embedding (
    embedding_classname,
    text_content,
    meta_content,
    embedding_model,
    embedding_%d,
    readers
  ) VALUES (
    ?, ?, ?::json, ?, ?, ?
  )")

(def ^:private find-similar-objects-sql-template
  "SELECT
    text_content,
    (embedding_%d <-> ?) AS euclidean_distance,
    -1 * (embedding_%d <#> ?) AS inner_product,
    1 - (embedding_%d <=> ?) AS cosine_similarity
  FROM text_embedding
  WHERE embedding_classname = ? AND (readers IS NULL %s)
  ORDER BY euclidean_distance
  LIMIT ?")

(def ^:private find-readers-by-document-sql
  "SELECT readers FROM text_embedding WHERE embedding_classname = ? AND meta_content->>'DocumentId' = ?")

(def ^:private update-readers-by-document-sql
  "UPDATE text_embedding SET readers = ? WHERE embedding_classname = ? AND meta_content->>'DocumentId' = ?")

(defn delete-all [db-conn classname]
  (jdbc/execute! db-conn [delete-all-sql classname]))

(defn delete-selected [db-conn app-uuid tag type]
  (let [embedding-classname (vc/get-planner-classname app-uuid)]
    (jdbc/execute! db-conn [delete-selected-sql
                            embedding-classname
                            (name tag)
                            (subs (str type) 1)])))

(defn create-object [db-conn {classname :classname text-content :text-content
                              meta-content :meta-content embedding :embedding
                              embedding-model :embedding-model :as obj}]
  (vc/assert-object! obj)
  (let [create-object-sql (format create-object-sql-template (count embedding))]
    (jdbc/execute! db-conn [create-object-sql
                            classname
                            text-content
                            (pg-json meta-content)
                            embedding-model
                            (pg-floats embedding)
                            (gs/active-user)])))


(defn find-similar-objects [db-conn {classname :classname embedding :embedding :as obj} limit]
  (vc/assert-object! obj)
  (let [embedding-sql-param (pg-floats embedding)
        dimension-count (count embedding)
        user (gs/active-user)
        find-similar-objects-sql (format find-similar-objects-sql-template
                                         dimension-count
                                         dimension-count
                                         dimension-count
                                         (if user (str "OR readers like '%%" user "%%'") ""))]
    (->> [find-similar-objects-sql
          embedding-sql-param
          embedding-sql-param
          embedding-sql-param
          classname
          limit]
         (jdbc/execute! db-conn)
         (mapv :text_embedding/text_content))))

(defn append-reader-for-rbac [db-conn app-uuid document-id user]
  (let [classname (vc/get-document-classname app-uuid)
        rs (first
            (->> [find-readers-by-document-sql
                  classname
                  document-id]
                 (jdbc/execute! db-conn)
                 (mapv :text_embedding/readers)))
        readers (when (seq rs) (set (s/split rs #",")))
        new-readers (if readers
                      (s/join "," (conj readers user))
                      user)]
    (jdbc/execute!
     db-conn
     [update-readers-by-document-sql
      new-readers
      classname
      document-id])))

(defn delete-planner-tool [db-conn {app-uuid :app-uuid tag :tag type :type}]
  (delete-selected db-conn app-uuid tag type))

(defn add-planner-tool [db-conn spec]
  (vc/add-planner-tool-internal spec (partial create-object db-conn)))

(defn update-planner-tool [db-conn spec]
  (delete-planner-tool db-conn spec)
  (add-planner-tool db-conn spec))

(defn embed-planner-tool [db-conn spec]
  (vc/embed-planner-tool-internal
   spec
   (partial update-planner-tool db-conn)
   (partial delete-planner-tool db-conn)))

(defn add-document-chunk [db-conn app-uuid text-chunk]
  (vc/add-document-chunk-internal app-uuid text-chunk (partial create-object db-conn)))
