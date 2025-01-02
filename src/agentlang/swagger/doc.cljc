(ns agentlang.swagger.doc
  (:require [agentlang.component :as cn]
            [agentlang.util.http :as uh]
            [agentlang.datafmt.json :as json]
            [clojure.string :as str]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [clojure.set :refer [union]]))

(def agentlangType->swaggerType
  {:Agentlang.Kernel.Lang/String {:type "string"}
   :Agentlang.Kernel.Lang/DateTime {:type "string"
                                 :format "date-time"}
   :Agentlang.Kernel.Lang/Date {:type "string"
                             :format "date"}
   :Agentlang.Kernel.Lang/Password {:type "string"
                                 :format "password"}
   :Agentlang.Kernel.Lang/Time {:type "string"
                             :format "time"}
   :Agentlang.Kernel.Lang/UUID {:type "string"
                             :format "uuid"}
   :Agentlang.Kernel.Lang/Int {:type "integer"}
   :Agentlang.Kernel.Lang/Int64 {:type "integer"
                              :format "int64"}
   :Agentlang.Kernel.Lang/Float {:type "number"
                              :format "float"}
   :Agentlang.Kernel.Lang/Double {:type "number"
                               :format "double"}
   :Agentlang.Kernel.Lang/Boolean {:type "boolean"}
   :Agentlang.Kernel.Lang/Email {:type "string"
                              :format "email"}
   :Agentlang.Kernel.Lang/Map {:type "object"}
   :Agentlang.Kernel.Lang/Any {:type "any"}})

(defn get-clojure-type [attr]
  (if-let [type (get agentlangType->swaggerType attr)]
    type
    (if-let [attr (cn/find-attribute-schema attr)]
      (get agentlangType->swaggerType (get attr :type) {:type "string"})
      {:type "string"})))

(defn agentlang-entity-to-swagger
  "Agentlang entity to Ring Swagger compatible schema"
  [entity-name]
  (let [entity-obj (:schema (cn/find-entity-schema entity-name)) 
        entity-obj (if entity-obj (dissoc entity-obj :PATH :INSTMETA)
                       (cn/record-schema entity-name))]
    (zipmap
     (map name (keys entity-obj))
     (map get-clojure-type (vals entity-obj)))))

(defn- get-unneeded-entity-dataflows [component]
  (let [entities (union (cn/entity-names component)
                        (cn/relationship-names component))]
    (into
     #{}
     (apply
      concat
      (map
       (fn [entity]
         (let [nm (name entity)
               ns (namespace entity)]
           (mapv #(keyword ns %)
                 [(str "Lookup_Internal_" nm)
                  (str "LookupAll_" nm)
                  (str "Lookup_" nm)
                  (str "Delete_" nm)
                  (str "Create_" nm)
                  (str "Update_" nm)
                  (str "Lookup_" nm "Meta")
                  (str "Delete_" nm "Meta")
                  (str "Create_" nm "Meta")
                  (str "Update_" nm "Meta")
                  (str "Lookup_Internal_" nm "Meta")
                  (str nm "_OnCreate_After")
                  (str nm "_OnCreate_Before")
                  (str nm "_OnUpsert_After")
                  (str nm "_OnUpsert_Before")
                  (str nm "_OnDelete_After")
                  (str nm "_OnDelete_Before")])))
       entities)))))

(defn get-all-events [component]
  (let [all-events (cn/event-names component)
        comp-str (name component)
        def-events (conj (get-unneeded-entity-dataflows component)
                         (keyword comp-str (str comp-str "_Init")))]
    (filter (fn [e]
              (not (contains? def-events e))) all-events)))

(defn get-event-return-type [event]
  (if-let
   [pattern
    (try (let [patterns (-> (cn/dataflows-for-event event) first second :patterns)
               patterns (if (vector? (last patterns))
                          (if (= (first (last patterns)) :match)
                            (last patterns) patterns) patterns)
               patterns (if (vector? (last patterns))
                          (if (= (first (last patterns)) :delete)
                            [{(second (last patterns)) {}}] patterns) patterns)
               comp (namespace event)
               patterns (if (vector? (last patterns)) (last patterns) patterns)
               last-entity (if (map? (last patterns)) (last patterns) (last (butlast patterns)))
               last-entity-name (-> last-entity first first)
               last-entity-name
               (if (= \? (last (name last-entity-name)))
                 (keyword (str comp "/" (apply str (butlast (name last-entity-name)))))
                 (keyword (str comp "/" (name last-entity-name))))]
           (agentlang-entity-to-swagger last-entity-name))
         (catch Exception _
           (log/info (str "Could not infer return type of dataflow: " event))))]
    pattern {}))

(defn get-entities-info [entities]
  (map
   (fn [entity-full-name]
     (let [entity-name (name entity-full-name)
           rest-path-vars (uh/get-child-entity-path entity-full-name)
           rest-path (:path rest-path-vars)
           swagger-types (agentlang-entity-to-swagger entity-full-name)
           route-params-vars (into [] (map
                                       (fn [entity]
                                         {:name (str/lower-case entity),
                                          :in "path",
                                          :required true,
                                          :schema
                                          {:type "string"}})
                                       (:vars rest-path-vars)))
           route-params-all (concat
                             [{:name (str/lower-case entity-name),
                               :in "path",
                               :required true,
                               :schema
                               {:type "string"}}]
                             route-params-vars)]

       [entity-full-name entity-name rest-path
        swagger-types route-params-vars route-params-all]))
   entities))

(defn get-events-info [events]
  (map
   (fn [event]
     (let [event-schema (dissoc (cn/event-schema event) :EventContext :inferred)
           swagger-schema (zipmap (map name (keys event-schema))
                                  (map get-clojure-type (vals event-schema))) 
           event-return-type (get-event-return-type event)]
       [event swagger-schema event-return-type])) events))

(defn generate-swagger-json
  "Generate swagger JSON based on the information in a Agentlang 
   component (events, entities, dataflows)"
  [component]
  (let [entities (get-entities-info (cn/entity-names component))
        events (get-events-info (get-all-events component))
        swag-spec
        {:openapi "3.0.3",
         :info
         {:title (name component),
          :version "1.0.0"},
         :paths
         (apply
          merge
          (concat
           (map
            (fn [[event-name _ _]]
              {(str "/api/" (namespace event-name) "/" (name event-name))
               {:post
                {:tags
                 [event-name]
                 :requestBody
                 {"$ref" (str "#/components/requestBodies/" (name event-name))},
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" (name event-name))}}}}})
            events)
           (map
            (fn [[_ entity-name rest-path _ route-params-vars route-params-all]]
              {(str "/" rest-path "/" "{" (str/lower-case entity-name) "}")
               {:get
                {:tags
                 [entity-name],
                 :parameters route-params-all,
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" entity-name)}}},
                :delete
                {:tags
                 [entity-name],
                 :parameters route-params-all,
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" entity-name)}}},
                :put
                {:tags
                 [entity-name],
                 :parameters route-params-all,
                 :requestBody
                 {"$ref" (str "#/components/requestBodies/" entity-name)},
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" entity-name)}}}},
               (str "/" rest-path)
               {:get
                {:tags
                 [entity-name],
                 :parameters route-params-vars,
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" entity-name)}}},
                :post
                {:tags
                 [entity-name],
                 :parameters route-params-vars,
                 :requestBody
                 {"$ref" (str "#/components/requestBodies/" entity-name)},
                 :responses
                 {"200"
                  {"$ref" (str "#/components/responses/" entity-name)}}}}})
            entities))),
         :components
         {:requestBodies
          (apply
           merge
           (concat
            (map
             (fn [[event-name event-schema _]]
               {(keyword (name event-name))
                {:required true,
                 :content
                 {"application/json"
                  {:schema
                   {:type "object",
                    :properties
                    {event-name
                     {:type "object",
                      :properties event-schema}}}}}}})
             events)
            (map
             (fn [[entity-full-name entity-name _ swagger-types _ _]]
               {(keyword entity-name)
                {:required true,
                 :content
                 {"application/json"
                  {:schema
                   {:type "object",
                    :properties
                    {(str (namespace entity-full-name) "/" entity-name)
                     {:type "object",
                      :properties (dissoc swagger-types "__Id__" "__instmeta__" "__path__")}}}}}}})
             entities)))
          :responses
          (apply
           merge
           (concat
            (map
             (fn [[event-name _ event-return-type]]
               {(keyword (name event-name))
                {:description "Success",
                 :content
                 {"application/json"
                  {:schema
                   {:type "object",
                    :properties
                    {"status"
                     {:type "string",
                      :example "ok"},
                     "result"
                     {:type "array",
                      :items
                      {:type "object",
                       :properties event-return-type}},
                     "message"
                     {:type "string",
                      :nullable true,
                      :example nil}}}}}}})
             events)
            (map
             (fn [[_ entity-name _ swagger-types _ _]]
               {(keyword entity-name)
                {:description "Success",
                 :content
                 {"application/json"
                  {:schema
                   {:type "object",
                    :properties
                    {"status"
                     {:type "string",
                      :example "ok"},
                     "result"
                     {:type "array",
                      :items
                      {:type "object",
                       :properties (dissoc swagger-types "__instmeta__" "__path__")}},
                     "message"
                     {:type "string",
                      :nullable true,
                      :example nil}}}}}}}) entities)))}}]
    (json/encode swag-spec)))
