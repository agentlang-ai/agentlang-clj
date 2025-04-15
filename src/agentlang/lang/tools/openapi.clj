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

(defn- paths-to-events [component-name open-api]
  (let [sec (:security open-api)]
    (apply
     concat
     (mapv (fn [[k v]]
             (mapv (fn [[method spec]]
                     (let [event-name (or (:operationId spec)
                                          (path-to-event-name (str (s/capitalize (name method)) "_" (name k))))
                           attr-spec (path-spec-to-attrs spec)]
                       {(li/make-path component-name event-name)
                        (apply merge {:meta
                                      {:doc (:description spec)
                                       :api (name k)
                                       :security (or (:security spec) sec)
                                       :method method}}
                               attr-spec)}))
                   v))
           (:paths open-api)))))

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

(defn- handle-post [open-api security event-name event-meta event-attrs]
  )

(defn- handle-get [open-api security event-name event-meta event-attrs]
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
          events (mapv register-event (paths-to-events cn open-api))]
      (when (seq events)
        (log/info (str "Events - " (s/join ", " events)))
        (when-let [r (register-resolver cn events)] (log/info (str "Resolver - " r))))
      cn)
    (u/throw-ex (str "Failed to parse " spec-url))))
