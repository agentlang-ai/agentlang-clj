(ns agentlang.inference.embeddings.internal.common 
  (:require [clojure.string :as s]
            [cheshire.core :as json]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.inference.provider :as provider]
            [agentlang.inference.embeddings.internal.model :as model]))

(defn get-entities-classname [app-uuid]
  (str "EntitySchema_" (s/replace app-uuid "-" "")))

(defn get-document-classname [app-uuid]
  (str "KnowledgeDoc_" (s/replace app-uuid "-" "")))

(defn get-planner-classname [app-uuid]
  (str "PlannerTools_" (s/replace app-uuid "-" "")))

(defn assert-object! [obj]
  (when-not (model/object? obj)
    (u/throw-ex (str "Invalid embedding object: " obj))))

(defn form-to-json [{data-key :type entity-type :tag attributes :schema}]
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

(defn embed-planner-tool-internal [{tool-name :tool-name tool-spec :tool-spec
                                   tag :tag operation :operation :as spec} update-func delete-func]
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
        (update-func spec))
      :delete (delete-func spec)
      (throw (ex-info "Expected operation 'add' or 'delete'" {:operation operation})))))

(defn add-document-chunk-internal [app-uuid text-chunk create-obj-func]
  (let [document-classname (get-document-classname app-uuid)
        text-content (json/generate-string (dissoc text-chunk :Id :AppUuid))
        [embedding embedding-model] (provider/make-embedding {:text-content text-content})]
    (create-obj-func {:classname document-classname
                      :text-content text-content
                      :meta-content (json/generate-string
                                     {:Title (get text-chunk :Title "")
                                      :DocumentId (get text-chunk :Id)})
                      :embedding embedding
                      :embedding-model embedding-model})))

(defn add-planner-tool-internal [{app-uuid :app-uuid tool-spec :tool-spec meta-content :meta-content} create-obj-func]
  (let [document-classname (get-planner-classname app-uuid)
        tool-text (pr-str tool-spec)
        [embedding embedding-model] (provider/make-embedding {:text-content tool-text})]
    (create-obj-func (model/as-object {:classname document-classname
                                       :text-content tool-text
                                       :meta-content (form-to-json meta-content)
                                       :embedding-model embedding-model
                                       :embedding embedding}))))
