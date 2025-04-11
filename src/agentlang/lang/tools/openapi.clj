(ns agentlang.lang.tools.openapi
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.util.logger :as log])
  (:import [io.swagger.parser OpenAPIParser]
           [io.swagger.v3.parser OpenAPIV3Parser]
           [io.swagger.v3.parser.core.models SwaggerParseResult]
           [io.swagger.v3.oas.models.media ObjectSchema]
           [io.swagger.v3.oas.models.security SecurityScheme]
           [io.swagger.v3.oas.models.parameters Parameter]
           [io.swagger.v3.oas.models OpenAPI PathItem Components Operation]))

;; Useful references and links:
;; 1. https://swagger.io/specification/
;; 2. https://github.com/swagger-api/swagger-parser
;; 3. https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md
;; 4. https://github.com/swagger-api/swagger-core/blob/master/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/OpenAPI.java

(defn- as-al-type [t req default]
  (let [optional (if req false true)]
    (merge
     (case t
       :string {:type :String}
       :integer {:type :Int}
       :number {:type :Double}
       :boolean {:type :Boolean}
       {:type :Any})
     (when optional
       {:optional optional})
     (when-not (nil? default)
       {:default default}))))

(defn- create-component [open-api]
  (let [extns (.getExtensions (.getInfo open-api))
        provider-name (get extns "x-providerName")
        service-name (get extns "x-serviceName")]
    (if (and provider-name service-name)
      (let [n (keyword (str provider-name "." service-name))]
        (and (ln/component n) n))
      (u/throw-ex (str "Cannot create component - x-providerName and x-serviceName are required in open-api specification")))))

(defn- fetch-security-schemes [^Components comps]
  (reduce (fn [scms [^String n ^SecurityScheme sec-scm]]
            (assoc scms n {:name (.getName sec-scm)
                           :type (keyword (.toString (.getType sec-scm)))
                           :in (keyword (.toString (.getIn sec-scm)))
                           :description (.getDescription sec-scm)}))
          {} (.getSecuritySchemes comps)))

(defn- path-to-event-name [p]
  (loop [p (if (= (first p) \/) (subs p 1) p), cap-mode? true, r []]
    (if-let [c (first p)]
      (case c
        (\/ \. \_ \-) (recur (rest p) true r)
        (recur (rest p) false (conj r (if cap-mode? (Character/toUpperCase c) c))))
      (keyword (apply str r)))))

(defn- path-to-operation [^PathItem path-item]
  (let [oprs [[:get (.getGet path-item)]
              [:post (.getPost path-item)]
              [:put (.getPut path-item)]
              [:delete (.getDelete path-item)]]]
    (when-let [[method ^Operation opr] (first (filter (fn [[_ v]] (not (nil? v))) oprs))]
      (let [params (mapv (fn [^Parameter p]
                           {:name (keyword (.getName p))
                            :in (keyword (.getIn p))
                            :optional (if (.getRequired p) false true)})
                         (.getParameters opr))
            attrs (into {} (mapv (fn [{n :name opt :optional}]
                                   [n {:type :Any :optional opt}])
                                 params))
            attrs-meta (into {} (mapv (fn [{n :name in :in}]
                                        [n {:in in}])
                                      params))]
        {:method method
         :attributes attrs
         :attributes-meta attrs-meta}))))

(defn- paths-to-events [component-name ^OpenAPI open-api]
  (reduce (fn [events [^String path-n ^PathItem path-item]]
            (let [evt-name (li/make-path component-name (path-to-event-name path-n))]
              (if-let [operation (path-to-operation path-item)]
                (conj events {evt-name (merge {:meta {:path-info {:endpoint path-n
                                                                  :attributes-meta (:attributes-meta operation)
                                                                  :method (:method operation)}}}
                                              (:attributes operation))})
                events)))
          [] (.getPaths open-api)))

(def ^:private sec-schemes (u/make-cell {}))

(defn- register-security-schemes [component-name security-schemes]
  (u/safe-set sec-schemes (assoc @sec-schemes component-name security-schemes)))

(defn- get-component-security-schemes [component-name]
  (get @sec-schemes component-name))

(defn- handle-openai-event [event-instance]
  (u/pprint event-instance)
  event-instance)

(defn- register-resolver [component-name events]
  (ln/resolver
   (li/make-path component-name :Resolver)
   {:paths events
    :with-methods
    {:eval handle-openai-event}}))

(defn parse [spec-url]
  (let [^OpenAPIParser parser (OpenAPIParser.)
        ^SwaggerParseResult result  (.readLocation parser spec-url nil nil)
        ^OpenAPI open-api (.getOpenAPI result)]
    (when-let [msgs (.getMessages result)]
      (log/warn (str "Errors or warnings in parsing " spec-url))      
      (doseq [msg (seq msgs)]
        (log/warn (str "Validation error: " msg))))
    (if open-api
      (let [cn (create-component open-api)
            ^Components comps (.getComponents open-api)
            security-schemes (fetch-security-schemes comps)
            entities
            (mapv
             ln/entity
             (mapv (fn [[^String k ^ObjectSchema v]]
                     {(li/make-path cn (keyword k))
                      (into {} (mapv (fn [[pk pv]]
                                       [(keyword pk) (as-al-type (keyword (.getType pv)) (.getRequired pv) (.getDefault pv))])
                                     (.getProperties v)))})
                   (.getSchemas comps)))
            events (mapv ln/event (paths-to-events cn open-api))
            sec-schemes (register-security-schemes cn security-schemes)]
        (when (seq entities) (log/info (str "Entities - " (s/join ", " entities))))
        (when (seq events)
          (log/info (str "Events - " (s/join ", " events)))
          (when-let [r (register-resolver cn events)] (log/info (str "Resolver - " r))))
        (when sec-schemes (log/info (str "Security-schemes - " sec-schemes)))
        cn)
      (u/throw-ex (str "Failed to parse " spec-url)))))
