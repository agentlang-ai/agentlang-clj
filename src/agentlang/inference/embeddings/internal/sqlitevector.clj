(ns agentlang.inference.embeddings.internal.sqlitevector
  (:require [clojure.string :as s]
            [next.jdbc :as jdbc]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.embeddings.internal.common :as vc]
            [agentlang.global-state :as gs]
            [clojure.java.io :as io])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

(def ^:private dbtype "sqlite")
(def ^:private vec-script-name "maybe-download-vec0.sh")

(defn- extract-script []
  (let [temp-dir (Files/createTempDirectory "vec0-script" (make-array FileAttribute 0))
        script-name vec-script-name
        temp-script (.toFile (.. temp-dir (resolve script-name)))
        resource (io/resource (str "scripts/" script-name))]
    (if resource
      (do
        (with-open [in (io/input-stream resource)]
          (io/copy in temp-script))
        (.setExecutable temp-script true)
        (.getAbsolutePath temp-script))
      (throw (Exception. (str "Could not find resource: scripts/" script-name))))))

(def ^:private vec0-script-path
  (delay (extract-script)))

(defn open-connection [config]
  (jdbc/get-connection
   (assoc
    (dissoc config :llm-provider)
    :enable_load_extension true :dbtype dbtype)))

(defn close-connection [db-conn]
  (when (= dbtype (:dbtype db-conn))
    (.close db-conn)
    true))

(defn load-sqlite-vec0-extension [conn]
  (log/info "checking if vec0 extension exists")
  (try
    (u/execute-script @vec0-script-path)
    (catch Exception ex
      (log/error "load-sqlite-vec0-extension: vec0 sqlite extension installation failed")
      (log/error ex)))
  (let [extension-path (-> (io/file (System/getProperty "user.dir") "lib" "vec0.so")
                          (.getAbsolutePath))]
    (log/debug (str "Loading extension from: " extension-path))
    (jdbc/execute! conn [(str "SELECT load_extension ('" extension-path "');")]))
  (log/debug "vec0 extension loaded"))

(def ^:private init-table
  "CREATE VIRTUAL TABLE IF NOT EXISTS text_embedding USING vec0
(
    embedding_classname TEXT, -- may be repeated
    text_content        TEXT       ,
    meta_content        TEXT,
    embedding_model     TEXT,
    embedding_1536      float[1536],
    readers             TEXT
);")

(defn initialize-vector-table [db-conn]
  (load-sqlite-vec0-extension db-conn)
  (jdbc/execute! db-conn [init-table]))

(defn- sqvec-floats
  "Turn supplied collection of floating-point values into a Sqlite
  object suitable for use as SQL param."
  [float-coll]
  (str "[" (s/join "," (into [] (float-array float-coll))) "]"))

(def ^:private delete-all-sql
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ?")

(def ^:private delete-selected-sql-template
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ? AND json_extract(meta_content, '$.%s.type') = ?")

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
    ?, ?, ?, ?, ?, ?
  )")

(def ^:private find-similar-objects-sql-template
  "SELECT
   readers, text_content
  FROM text_embedding
  WHERE embedding_classname = ? AND embedding_%d match ?
  LIMIT ?")

(def ^:private find-readers-by-document-sql
  "SELECT readers FROM text_embedding WHERE embedding_classname = ? AND json_extract(meta_content, '$.DocumentId') = ?")

(def ^:private update-readers-by-document-sql
  "UPDATE text_embedding SET readers = ? WHERE embedding_classname = ? AND json_extract(meta_content, '$.DocumentId') = ?")

(defn delete-all [db-conn classname]
  (jdbc/execute! db-conn [delete-all-sql classname]))

(defn delete-selected [db-conn app-uuid tag type]
  (let [embedding-classname (vc/get-planner-classname app-uuid)
        delete-selected-sql (format delete-selected-sql-template (name tag))]
    (jdbc/execute! db-conn [delete-selected-sql
                            embedding-classname
                            (subs (str type) 1)])))

(defn create-object [db-conn {classname :classname text-content :text-content
                              meta-content :meta-content embedding :embedding
                              embedding-model :embedding-model :as obj}] 
  (vc/assert-object! obj)
  (let [create-object-sql (format create-object-sql-template (count embedding))]
    (jdbc/execute! db-conn [create-object-sql
                            classname
                            text-content
                            (str meta-content)
                            embedding-model
                            (sqvec-floats embedding)
                            (or (gs/active-user) "")])))

(defn find-similar-objects [db-conn {classname :classname embedding :embedding :as obj} limit]
  (vc/assert-object! obj)
  (let [embedding-sql-param (sqvec-floats embedding)
        dimension-count (count embedding)
        user (gs/active-user)
        find-similar-objects-sql (format find-similar-objects-sql-template
                                         dimension-count)]
    (->> [find-similar-objects-sql
          classname
          embedding-sql-param
          limit]
         (jdbc/execute! db-conn)
         (filter
          #(let [readers (:text_embedding/readers %)]
            (or
             (= readers "")
             (if user (s/includes? readers user) false))))
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
