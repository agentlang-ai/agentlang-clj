(ns agentlang.lang.tools.openapi
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [agentlang.util :as u]
            [agentlang.lang :as ln]
            [agentlang.component :as cn]
            [agentlang.util.http :as http]
            [agentlang.lang.internal :as li]
            [agentlang.datafmt.json :as json]
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
        #_(doseq [[n resp] (.getResponses opr)]
          (println n)
          (doseq [[mn mt] (.getContent resp)]
            (println mn)
            (println (.getSchema mt))))
        {:method method
         :attributes attrs
         :attributes-meta attrs-meta}))))

(defn- paths-to-events [component-name ^OpenAPI open-api]
  (reduce (fn [events [^String path-n ^PathItem path-item]]
            (let [evt-name (li/make-path component-name (path-to-event-name path-n))]
              (if-let [operation (path-to-operation path-item)]
                (conj events {evt-name (merge {:meta {:api-info {:type :openapi
                                                                 :endpoint path-n
                                                                 :attributes-meta (:attributes-meta operation)
                                                                 :method (:method operation)}}}
                                              (:attributes operation))})
                events)))
          [] (.getPaths open-api)))

(def ^:private cn-meta (u/make-cell {}))

(defn- register-meta [cn ^OpenAPI open-api]
  (let [servers (mapv #(.getUrl %) (.getServers open-api))]
    (u/safe-set cn-meta (assoc @cn-meta cn {:servers servers}))))

(def ^:private sec-schemes (u/make-cell {}))

(defn- register-security-schemes [component-name security-schemes]
  (u/safe-set sec-schemes (assoc @sec-schemes component-name security-schemes)))

(defn get-component-security-schemes [component-name]
  (get @sec-schemes component-name))

(def ^:private sec-scheme-values (u/make-cell {}))

(defn set-security [component-name security-scheme-name security-object]
  (if-let [scm (get (get-component-security-schemes component-name) security-scheme-name)]
    (let [obj {:value security-object :scheme scm}
          sc-vals (assoc (get @sec-scheme-values component-name {}) security-scheme-name obj)]
      (u/safe-set sec-scheme-values (assoc @sec-scheme-values component-name sc-vals)))
    (u/throw-ex (str "Security scheme not found - " [component-name security-scheme-name]))))

(defn get-security [cn]
  (get @sec-scheme-values cn))

(defn get-server [cn]
  (let [srvs (:servers (get @cn-meta cn))]
    (or (first (filter #(s/starts-with? % "https") srvs))
        (first srvs))))

(defn- build-http-request-from-attributes [api-info attrs]
  (let [attrs-meta (:attributes-meta api-info)
        query-attrs (set/union
                     (set (mapv first (filter #(= :query (:in %)) attrs-meta)))
                     (set (keys attrs)))
        q (when (seq query-attrs) (mapv (fn [a] (str (name a) "=" (get attrs a))) query-attrs))
        ;; TODO: build request-body from attributes
        ;; TODO: add required headers
        ]
    {:url (if q (s/join "&" q) "")
     :headers nil
     :body nil}))

(defn- filter-sec-in [tag sec-specs]
  (mapv (fn [s] [(get-in s [:scheme :name]) (:value s)])
        (filter #(= tag (:in (:scheme %))) sec-specs)))

(defn- maybe-beader-token-header [sec-specs]
  (when-let [s (first (filter #(= "bearer" (:scheme %)) sec-specs))]
    (when-let [tok (:value s)]
      {:Authorization (str "Bearer " tok)})))

(defn- build-http-request-from-securities [sec]
  (let [sec-specs (mapv second sec)
        in-q (filter-sec-in :query sec-specs)
        q (when (seq in-q) (mapv (fn [[k v]] (str k "=" v)) in-q))
        in-hdr (filter-sec-in :header sec-specs)
        hdrs0 (when (seq in-hdr) (into {} in-hdr))
        hdrs (or hdrs0 (maybe-beader-token-header sec-specs))]
    {:url (if q (s/join "&" q) "")
     :headers hdrs}))

(defn- handle-get [api-info server securities attrs]
  (let [req0 (build-http-request-from-attributes api-info attrs)
        req1 (build-http-request-from-securities securities)
        q0 (:url req0), q1 (:url req1)
        q0? (seq q0), q1? (seq q1)
        q (str (if q0? q0 "")
               (if (and q0? q1?)
                 "&"
                 "")
               (if q1? q1 ""))
        url (str server (:endpoint api-info) (if (seq q) (str "?" q) ""))
        headers (merge (:headers req0) (:headers req1))
        resp (http/do-get url (when (seq headers) {:headers headers}))
        status (:status resp)]
    (if (= 200 status)
      (let [opts (:opts resp)
            ctype (get-in opts [:headers "Content-Type"])]
        (if (= ctype "application/json")
          (json/decode (:body resp))
          (:body resp)))
      (do (log/warn (str "GET request to " url " failed with status - " status))
          (log/warn (:body resp))
          nil))))

(defn- handle-openai-event [event-instance]
  (let [event-name (cn/instance-type-kw event-instance)
        [cn _] (li/split-path event-name)
        sec (get-security cn)
        server (get-server cn)
        api-info (:api-info (cn/fetch-meta event-name))]
    (case (:method api-info)
      :get (handle-get api-info server sec (cn/instance-user-attributes event-instance))
      (u/throw-ex (str "method " (:method api-info) "not yet supported")))))

(defn- register-resolver [component-name events]
  (ln/resolver
   (li/make-path component-name :Resolver)
   {:paths events
    :with-methods
    {:eval handle-openai-event}}))

(defn invocation-event [en]
  (let [[c n] (li/split-path en)]
    (li/make-path c (keyword (str "Invoke" (name n))))))

(defn- register-invocation-dataflows [events]
  (mapv
   (fn [[event-name invocation-event-name]]
     (ln/dataflow
      invocation-event-name
      {event-name {} :from (li/make-ref invocation-event-name :Parameters)}))
   (mapv (fn [en] [en (invocation-event en)]) events)))

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
            sec-schemes (register-security-schemes cn security-schemes)
            cn-meta (register-meta cn open-api)]
        (when cn-meta (log/info (str "Meta - " cn-meta)))
        (when (seq entities) (log/info (str "Entities - " (s/join ", " entities))))
        (when (seq events)
          (log/info (str "Events - " (s/join ", " events)))
          (when-let [evts (register-invocation-dataflows events)] (log/info (str "Invocation events - " (s/join ", " evts))))
          (when-let [r (register-resolver cn events)] (log/info (str "Resolver - " r))))
        (when sec-schemes (log/info (str "Security-schemes - " sec-schemes)))
        cn)
      (u/throw-ex (str "Failed to parse " spec-url)))))
