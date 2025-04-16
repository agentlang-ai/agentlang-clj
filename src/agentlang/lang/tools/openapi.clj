(ns agentlang.lang.tools.openapi
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clj-yaml.core :as yaml]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.lang :as ln]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.util.http :as http]
            [agentlang.lang.internal :as li]
            [agentlang.datafmt.json :as json]
            [agentlang.util.logger :as log]))

;; Useful references and links:
;; 1. https://swagger.io/specification/
;; 2. https://github.com/swagger-api/swagger-parser
;; 3. https://github.com/OAI/OpenAPI-Specification/blob/3.0.1/versions/3.0.1.md
;; 4. https://github.com/swagger-api/swagger-core/blob/master/modules/swagger-models/src/main/java/io/swagger/v3/oas/models/OpenAPI.java

(def ^:private spec-registry (u/make-cell {}))

(defn- put-spec! [component-name spec]
  (u/safe-set spec-registry (assoc @spec-registry component-name spec)))

(defn- get-spec [component-name]
  (get @spec-registry component-name))

(defn- config-entity-name [component-name]
  (li/make-path component-name :ApiConfig))

(defn invocation-event [event-name]
  (let [[c n] (li/split-path event-name)]
    (li/make-path c (keyword (str "Invoke" (name n))))))

(defn- register-event [event-spec]
  (let [n (li/record-name event-spec)
        attrs (li/record-attributes event-spec)
        inv-event (invocation-event n)]
    (ln/dataflow
     inv-event
     {n {} :from (li/make-ref inv-event :Parameters)})
    (ln/event n attrs)))

(defn- as-al-type [t]
  (if (nil? t)
    :Any
    (case t
      :string :String
      :integer :Int
      :number :Double
      :boolean :Boolean
      :Any)))

(defn- component-name-from-title [open-api]
  (when-let [title (get-in open-api [:info :title])]
    (let [s (apply str (filter #(or (Character/isLetter %) (Character/isDigit %)) title))]
      (when (seq s)
        (keyword s)))))

(defn- create-component [open-api]
  (if-let [n (component-name-from-title open-api)]
    (and (ln/component n) n)
    (u/throw-ex (str "Cannot create component - failed to infer title from specification"))))

(defn- path-to-event-name [p]
  (loop [p (if (= (first p) \/) (subs p 1) p), cap-mode? true, r []]
    (if-let [c (first p)]
      (case c
        (\/ \. \_ \-) (recur (rest p) true r)
        (recur (rest p) false (conj r (if cap-mode? (Character/toUpperCase c) c))))
      (keyword (apply str r)))))

(defn- path-spec-to-attrs [spec]
  (mapv (fn [p]
          {(keyword (:name p))
           {:meta {:doc (:description p) :in (keyword (:in p))}
            :type (as-al-type (keyword (get-in p [:schema :type])))
            :optional (not (:required p))}})
        (:parameters spec)))

(defn- parse-reqresp-spec [component-name schema]
  (or
   (when schema
     (let [arr? (= "array" (:type schema))
           ref-path (if arr?
                      (get-in schema [:items :$ref])
                      (:$ref schema))]
       (when ref-path
         (let [n (last (mapv keyword (s/split (subs ref-path 2) #"/")))
               typ (li/make-ref component-name n)]
           (if arr? {:listof typ} {:type typ})))))
   {:type :Any}))

(defn- parse-request-body-spec [component-name req-body-spec]
  (parse-reqresp-spec component-name (get-in req-body-spec [:content :application/json :schema])))

(defn- parse-responses-spec [component-name resp-spec]
  (parse-reqresp-spec component-name (get-in resp-spec ["200" :content :application/json :schema])))

(defn- paths-to-events [component-name open-api]
  (let [sec (:security open-api)]
    (apply
     concat
     (mapv (fn [[k v]]
             (mapv (fn [[method spec]]
                     (let [event-name (or (:operationId spec)
                                          (path-to-event-name (str (s/capitalize (name method)) "_" (name k))))
                           attr-spec cn/inferred-event-schema #_(path-spec-to-attrs spec)]
                       {(li/make-path component-name event-name)
                        (apply merge {:meta
                                      {:doc (:description spec)
                                       :api (name k)
                                       :requestBody (parse-request-body-spec component-name (:requestBody spec))
                                       :responses (parse-responses-spec component-name (:responses spec))
                                       :security (or (:security spec) sec)
                                       :method method}}
                               attr-spec)}))
                   v))
           (:paths open-api)))))

(defn- components-to-records [component-name open-api]
  (reduce
   (fn [recs [k v]]
     (if (= "object" (:type v))
       (let [required (mapv u/string-as-keyword (:required v))]
         (conj
          recs
          {(li/make-path component-name k)
           (reduce
            (fn [attrs [k v]]
              (let [req (or (some #{k} required)
                            (:required v))
                    props (merge
                           {:optional (not req)}
                           (when-let [d (:default v)]
                             {:default d})
                           (if-let [xs (:enum v)]
                             {:oneof (vec xs)}
                             {:type (as-al-type (u/string-as-keyword (:type v)))}))]
                (assoc attrs k props)))
            {} (:properties v))}))
       recs))
   [] (get-in open-api [:components :schemas])))

(def ^:private invoke-event-meta (u/make-cell {}))

(defn- cache-invocation-meta [event-name tag data]
  (let [cache (assoc (get @invoke-event-meta event-name {}) tag data)]
    (u/safe-set invoke-event-meta (assoc @invoke-event-meta event-name cache))
    data))

(defn- cached-invocation-meta [event-name tag]
  (get-in @invoke-event-meta [event-name tag]))

(defn- fetch-server [event-name open-api]
  (or (cached-invocation-meta event-name :server)
      (cache-invocation-meta
       event-name :server
       (let [srvs (:servers open-api)]
         (:url
          (if (= 1 (count srvs))
            (first srvs)
            (or (first (filter #(s/starts-with? (:url %) "https") srvs))
                (first srvs))))))))

(defn- security-headers [security]
  (let [hsecs
        (su/nonils
         (mapv (fn [[spec v]]
                 (if-let [tok (:bearer_token v)]
                   [:bearer_token tok]
                   (when (= :header (:in spec))
                     (let [n (:name spec)]
                       (if-let [secv (get v n)]
                         [n secv]
                         (u/throw-ex
                          (str "Failed to generate security-headers, required parameter "
                               n " not found in security-object")))))))
               security))]
    (when (seq hsecs)
      (reduce
       (fn [headers [n v]]
         (if (= :bearer_token n)
           (assoc headers "Authorization" (str "Bearer " v))
           (assoc headers n v)))
       {} hsecs))))

(defn- security-query-params [security]
  (let [qsecs (mapv (fn [[spec v]]
                      (when (= :query (:in spec))
                        (let [n (:name spec)]
                          (if-let [secv (get v n)]
                            [n secv]
                            (u/throw-ex (str "Required parameter " n " not found in security-object"))))))
                    security)]
    (when (seq qsecs)
      (s/join "&" (mapv (fn [[n v]] (str (name n) "=" v)) qsecs)))))

(defn- attribute-names-in [in-tag event-schema]
  (mapv first (filter (fn [[k v]] (= in-tag (:in v))) event-schema)))

(defn- attach-query-params [url event-schema security event-attrs]
  (let [anames (attribute-names-in :query event-schema)
        sec-params (security-query-params security)
        has-params (seq anames)]
    (str
     (if has-params
       (let [params (s/join "&" (mapv (fn [a] (str (name a) "=" (get event-attrs a))) anames))]
         (str url "?" params))
       url)
     (if sec-params (str (if has-params "&" "?") sec-params) ""))))

(defn- format-api-endpoint [api-endpoint event-schema event-attrs]
  (if-let [anames (seq (attribute-names-in :path event-schema))]
    [(reduce
      (fn [s f a]
        (s/replace s f (get event-attrs a)))
      api-endpoint (mapv (fn [n] (str "{" (name n) "}")) anames) anames)
     (dissoc event-attrs anames)]
    [api-endpoint event-attrs]))

(defn- make-request-body [open-api event-meta event-attrs]
  (if-let [spec (:requestBody event-meta)]
    (if-let [typ (:type spec)]
      (if (= :Any typ)
        event-attrs
        (let [attr-names (set (cn/user-attribute-names (cn/find-record-schema typ)))]
          (first (set/project [event-attrs] attr-names))))
      (if-let [typ (:listof spec)]
        (let [attr-names (set (cn/user-attribute-names (cn/find-record-schema typ)))]
          (mapv #(first (set/project [%] attr-names)) event-attrs))
        event-attrs))
    event-attrs))

(defn- make-request [open-api event-name event-meta event-attrs security]
  (let [schema (into
                {}
                (mapv
                 (fn [[k v]]
                   [k (cn/fetch-attribute-meta v)])
                 (filter (fn [[k _]] (some #{k} (keys event-attrs))) (cn/fetch-event-schema event-name))))
        [api event-attrs] (format-api-endpoint (:api event-meta) schema event-attrs)
        url (attach-query-params
             (str (fetch-server event-name open-api) "/" api)
             schema security event-attrs)
        headers (security-headers security)
        request-body (make-request-body open-api event-meta event-attrs)]
    {:url url :headers headers :requestBody request-body}))

(defn- process-response [event-meta resp]
  (if-let [spec (:response event-meta)]
    (if-let [typ (:type spec)]
      (if (= :Any typ)
        resp
        (cn/make-instance typ resp false))
      (if-let [typ (:listof spec)]
        (mapv #(cn/make-instance typ % false) resp)
        resp))
    resp))

(defn- handle-response [method event-meta url resp]
  (if (map? resp)
    (if-let [status (:status resp)]
      (if (= 200 status)
        (let [opts (:opts resp)
              ctype (get-in opts [:headers "Content-Type"])]
          (if (= ctype "application/json")
            (process-response event-meta (json/decode (:body resp)))
            (:body resp)))
        (do (log/warn (str (name method) " request to " url " failed with status - " status))
            (log/warn (:body resp))
            nil))
      (process-response event-meta resp))
    resp))

(defn- handle-post [open-api security event-name event-meta event-attrs]
  (let [{url :url headers :headers reqbody :requestBody}
        (make-request open-api event-name event-meta event-attrs security)
        resp (http/POST url (when (seq headers) {:headers headers}) reqbody :json)]
    (handle-response :POST event-meta url resp)))

(defn- handle-get [open-api security event-name event-meta event-attrs]
  (let [{url :url headers :headers}
        (make-request open-api event-name event-meta event-attrs security)
        resp (http/do-get url (when (seq headers) {:headers headers}))]
    (handle-response :GET event-meta url resp)))

(defn- normalize-sec-spec [spec]
  (into
   {}
   (mapv (fn [[k v]] [k (u/string-as-keyword v)]) spec)))

(defn- handle-openapi-event [event-instance]
  (let [event-name (cn/instance-type-kw event-instance)
        event-meta (cn/fetch-meta event-name)
        method (:method event-meta)
        [cn _] (li/split-path event-name)
        open-api (get-spec cn)
        _ (when-not open-api
            (u/throw-ex (str "Event " event-name ", no OpenAPI specification found for component " cn)))
        event-sec (:security (or (:EventContext event-instance) gs/active-event-context))
        security (when event-sec
                   (let [sec-scms (get-in open-api [:components :securitySchemes])]
                     (mapv (fn [[k v]]
                             (if-let [ss (get sec-scms k)]
                               [(normalize-sec-spec ss) v]
                               (u/throw-ex (str "Invalid security-scheme " k " for " event-name))))
                           event-sec)))]
    (when-let [handler
               (case method
                 :get handle-get
                 :post handle-post
                 (u/throw-ex (str "Event " event-name ", method " method " not yet supported")))]
      (handler open-api security event-name event-meta (cn/instance-user-attributes event-instance)))))

(defn- register-resolver [component-name events]
  (ln/resolver
   (li/make-path component-name :Resolver)
   {:paths events
    :with-methods
    {:eval handle-openapi-event}}))

(defn- register-config-entity [cn open-api]
  (let [attrs0 (reduce
                (fn [attrs [k v]]
                  (if-let [in (:in v)]
                    (assoc attrs k {:meta (normalize-sec-spec v)
                                    :type :String
                                    :optional true})
                    attrs))
                {} (get-in open-api [:components :securitySchemes]))
        attrs (assoc attrs0 :servers {:listof :String :default (vec (:servers open-api))})]
    (if-let [n (ln/entity {(config-entity-name cn) attrs})]
      n
      (log/warn (str "Failed to register config-entity for " cn)))))

(defn- register-model [cn open-api config-entity]
  (cn/register-model
   cn
   {:name cn
    :components [cn]
    :version (:version open-api)
    :agentlang-version "current"
    :config-entity config-entity}))

(defn- read-yml-file [spec-url]
  (if (s/starts-with? spec-url "http")
    (let [result (http/do-get spec-url)]
      (if (= 200 (:status result))
        (:body result)
        (u/throw-ex (str "Failed to GET " spec-url ", status - " (:status result)))))
    (slurp spec-url)))

(defn- parse-openapi-spec [spec-url]
  (yaml/parse-string (read-yml-file spec-url)))

(defn parse [spec-url]
  (if-let [open-api (parse-openapi-spec spec-url)]
    (let [cn (create-component open-api)
          _ (put-spec! cn open-api)
          events (mapv register-event (paths-to-events cn open-api))
          recs (mapv ln/record (components-to-records cn open-api))
          config-entity (register-config-entity cn open-api)]
      (when config-entity (log/info (str "Config entity - " config-entity)))
      (when (seq recs) (log/info (str "Records - " recs)))
      (when (seq events)
        (log/info (str "Events - " (s/join ", " events)))
        (when-let [r (register-resolver cn events)] (log/info (str "Resolver - " r))))
      (when-let [n (register-model cn open-api config-entity)]
        (log/info (str "Model registered - " n)))
      cn)
    (u/throw-ex (str "Failed to parse " spec-url))))
