(ns agentlang.inference.embeddings.internal.sqlitevector
  (:require [clojure.string :as s]
            [next.jdbc :as jdbc]
            [cheshire.core :as json]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.global-state :as gs]
            [agentlang.inference.provider :as provider]
            [agentlang.inference.embeddings.internal.model :as model]))

(def ^:private dbtype "sqlite")
(def ^:private vec0-script-path "./scripts/maybe-download-vec0.sh")

(defn open-connection [config]
  (jdbc/get-connection
   (assoc
    (dissoc config :llm-provider)
    :enable_load_extension true :dbtype dbtype)))

(defn close-connection [db-conn]
  (when (= dbtype (:dbtype db-conn))
    true))

(defn load-sqlite-vec0-extension [conn]
  (log/info (str "checking if vec0 extension exists")) 
  (try
    (u/execute-script vec0-script-path)
    (catch Exception ex
      (log/error (str "load-sqlite-vec0-extension: vec0 sqlite extension installation failed"))
      (log/error ex)))
  (jdbc/execute! conn ["SELECT load_extension ('vec0');"])
  (log/debug (str "vec0 extension loaded")))

(defn- pg-floats
  "Turn supplied collection of floating-point values into a Sqlite
  object suitable for use as SQL param."
  [float-coll]
  (str "[" (s/join "," (into [] (float-array float-coll))) "]"))

(defn get-document-classname [app-uuid]
  (str "KnowledgeDoc_" (s/replace app-uuid "-" "")))

(defn get-planner-classname [app-uuid]
  (str "PlannerTools_" (s/replace app-uuid "-" "")))

(def ^:private delete-selected-sql-template
  "DELETE
  FROM text_embedding
  WHERE embedding_classname = ? AND json_extract(meta_content, '$.%s.type') = ?")

(defn delete-selected [db-conn app-uuid tag type]
  (let [embedding-classname (get-planner-classname app-uuid)
        delete-selected-sql (format delete-selected-sql-template (name tag))]
    (jdbc/execute! db-conn [delete-selected-sql
                            embedding-classname
                            (subs (str type) 1)])))

(defn- assert-object! [obj]
  (when-not (model/object? obj)
    (u/throw-ex (str "Invalid embedding object: " obj))))

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

(defn create-object [db-conn {classname :classname text-content :text-content
                              meta-content :meta-content embedding :embedding
                              embedding-model :embedding-model :as obj}] 
  (assert-object! obj)
  (let [create-object-sql (format create-object-sql-template (count embedding))]
    (jdbc/execute! db-conn [create-object-sql
                            classname
                            text-content
                            (str meta-content)
                            embedding-model
                            (pg-floats embedding)
                            (or (gs/active-user) "")])))

(def ^:private find-similar-objects-sql-template
  "SELECT
   readers, text_content
  FROM text_embedding
  WHERE embedding_classname = ? AND embedding_%d match ?
  LIMIT ?")

(defn find-similar-objects [db-conn {classname :classname embedding :embedding :as obj} limit]
  (assert-object! obj)
  (let [embedding-sql-param (pg-floats embedding)
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

(defn delete-planner-tool [db-conn {app-uuid :app-uuid tag :tag type :type}]
  (delete-selected db-conn app-uuid tag type))

(defn- form-to-json [{data-key :type entity-type :tag attributes :schema}]
  (let [process (fn [v]
                  (cond
                    (map? v) (into {}
                                   (for [[k v] v]
                                     [(name k)
                                      (if (keyword? v)
                                        {:type (name (second (s/split (str v) #"\:")))}
                                        v)]))
                    (keyword? v) {:type (name (second (s/split (str v) #"\:")))}
                    (string? v) {:type v}))]
    (json/generate-string
     {(name entity-type)
      {"type" data-key
       "attributes" (into {} (map (fn [[k v]] [(name k) (process v)]) attributes))}})))

(defn add-planner-tool [db-conn {app-uuid :app-uuid tool-spec :tool-spec meta-content :meta-content}]
  (let [document-classname (get-planner-classname app-uuid)
        tool-text (pr-str tool-spec)
        [embedding embedding-model] (provider/make-embedding {:text-content tool-text})]
    (create-object db-conn (model/as-object {:classname document-classname
                                             :text-content tool-text
                                             :meta-content (form-to-json meta-content)
                                             :embedding-model embedding-model
                                             :embedding embedding}))))

(defn update-planner-tool [db-conn spec]
  (delete-planner-tool db-conn spec)
  (add-planner-tool db-conn spec))

(defn embed-planner-tool [db-conn {tool-name :tool-name tool-spec :tool-spec
                                   tag :tag operation :operation :as spec}]
  (log/debug (str "Ingesting planner tool: " spec))
  (if (or (and (nil? tool-name)
               (nil? tool-spec))
          (= tag 'component))
    (log/info (str "Ignoring insertion of component for now..."))
    (case operation
      :add
      (let [spec (if (and tool-spec tool-name)
                   (assoc spec :tool-spec (assoc tool-spec :tool-name tool-name))
                   spec)]
        (update-planner-tool db-conn spec))
      :delete (delete-planner-tool db-conn spec)
      (throw (ex-info "Expected operation 'add' or 'delete'" {:operation operation})))))

(defn add-document-chunk [db-conn app-uuid text-chunk]
  (let [document-classname (get-document-classname app-uuid)
        text-content (json/generate-string (dissoc text-chunk :Id :AppUuid))
        [embedding embedding-model] (provider/make-embedding {:text-content text-content})]
    (create-object db-conn {:classname document-classname
                            :text-content text-content
                            :meta-content (json/generate-string
                                           {:Title (get text-chunk :Title "")
                                            :DocumentId (get text-chunk :Id)})
                            :embedding embedding
                            :embedding-model embedding-model})))

(def ^:private find-readers-by-document-sql
  "SELECT readers FROM text_embedding WHERE embedding_classname = ? AND json_extract(meta_content, '$.DocumentId') = ?")

(def ^:private update-readers-by-document-sql
  "UPDATE text_embedding SET readers = ? WHERE embedding_classname = ? AND json_extract(meta_content, '$.DocumentId') = ?")

(defn append-reader-for-rbac [db-conn app-uuid document-id user]
  (let [classname (get-document-classname app-uuid)
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
