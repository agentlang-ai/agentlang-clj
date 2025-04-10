(ns agentlang.lang.tools.openapi
  (:require [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.util.logger :as log])
  (:import [io.swagger.parser OpenAPIParser]
           [io.swagger.v3.parser OpenAPIV3Parser]
           [io.swagger.v3.parser.core.models SwaggerParseResult]
           [io.swagger.v3.oas.models OpenAPI]))

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

(defn parse [spec-url]
  (let [^OpenAPIParser parser (OpenAPIParser.)
        ^SwaggerParseResult result  (.readLocation parser spec-url nil nil)
        ^OpenAPI open-api (.getOpenAPI result)]
    (when-let [msgs (.getMessages result)]
      (log/warn (str "Errors or warnings in parsing " spec-url))      
      (doseq [msg (seq msgs)]
        (log/warn (str "Validation error: " msg))))
    (if open-api
      (let [cn (create-component open-api)]
        (mapv
         ln/entity 
         (mapv (fn [[^String k v]]
                 {(li/make-path cn (keyword k))
                  (into {} (mapv (fn [[pk pv]]
                                   [(keyword pk) (as-al-type (keyword (.getType pv)) (.getRequired pv) (.getDefault pv))])
                                 (.getProperties v)))})
               (.getSchemas (.getComponents open-api))))
        #_(doseq [[path-n path-item] (.getPaths open-api)]
          (println ">>>>>>>>>>>>>>>>>>>>>>> " path-n)
          (doseq [param (.getParameters (.getGet path-item))]
            (println "#######################" (.getName param) " " (.getRequired param)))))
      (u/throw-ex (str "Failed to parse " spec-url)))))
