(ns agentlang.inference.embeddings.sqlitevector
  (:require [agentlang.util :as u]
            [agentlang.inference.provider.core :as provider]
            [agentlang.inference.embeddings.protocol :as p]
            [agentlang.inference.embeddings.internal.registry :as r]
            [agentlang.inference.embeddings.internal.sqlitevector :as sqv]
            [agentlang.inference.embeddings.internal.common :as vc]))

;;;; sample config.edn entry:
;; {:embeddings {:vectordb :sqlitevector
;;               :config {:dbname "./test1.db"}]}}}

(defn make []
  (let [db-conn (u/make-cell)
        provider-name (u/make-cell)
        cwp #(provider/call-with-provider @provider-name %1)]
    (reify p/EmbeddingDb
      (open-connection [this config]
        (let [conn (sqv/open-connection config)] 
          (sqv/initialize-vector-table conn)
          (u/safe-set db-conn conn))
         (u/safe-set-once provider-name #(:llm-provider config))
        this)
      (close-connection [_]
        (when (sqv/close-connection @db-conn)
          (u/safe-set db-conn nil)
          true))
      (embed-tool [_ spec]
        (cwp #(sqv/embed-planner-tool @db-conn spec)))
      (update-tool [_ spec]
        (cwp #(sqv/update-planner-tool @db-conn spec)))
      (delete-tool [_ spec]
        (cwp #(sqv/delete-planner-tool @db-conn spec)))
      (embed-document-chunk [_ app-uuid text-chunk]
        (cwp #(sqv/add-document-chunk @db-conn app-uuid text-chunk)))
      (get-document-classname [_ app-uuid]
        (vc/get-document-classname app-uuid))
      (get-planner-classname [_ app-uuid]
        (vc/get-planner-classname app-uuid))
      (append-reader-for-rbac [db app-uuid document-id user]
        (sqv/append-reader-for-rbac @db-conn app-uuid document-id user))
      (find-similar-objects [_ query-spec limit]
        (sqv/find-similar-objects @db-conn query-spec limit)))))

(def make-db
  (memoize
   (fn [config]
     (let [db (make)]
      (when (p/open-connection db config)
        db)))))

(r/register-db :sqlitevector make-db)
