(ns agentlang.util.http
  (:require #?(:clj [org.httpkit.client :as http]
               :cljs [cljs-http.client :as http])
            #?(:clj [org.httpkit.sni-client :as sni-client])
            [clojure.string :as s]
            [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.datafmt.json :as json]
            [agentlang.datafmt.transit :as t]
            [agentlang.global-state :as gs]
            #?(:clj [ring.util.codec :as codec])
            #?(:cljs [cljs.core.async :refer [<!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go]])))

#?(:clj
   (alter-var-root #'org.httpkit.client/*default-client* (fn [_] sni-client/default-client)))

(def ^:private enc-dec
  {:json [json/decode json/encode]
   :transit+json [t/decode t/encode]})

(def content-types
  {"application/json" :json
   "application/transit+json" :transit+json})

(def ^:private datafmt-content-types (us/map-mirror content-types))

(defn encoder [data-fmt]
  (second (data-fmt enc-dec)))

(defn decoder [data-fmt]
  (first (data-fmt enc-dec)))

(def content-type (partial get datafmt-content-types))

(def entity-event-prefix "/api/")
(def graphql-prefix "/graphql")
(def nrepl-prefix "/nrepl")
(def login-prefix "/login")
(def logout-prefix "/logout")
(def signup-prefix "/signup")
(def get-user-prefix "/get-user")
(def update-user-prefix "/update-user")
(def forgot-password-prefix "/forgot-password")
(def confirm-forgot-password-prefix "/confirm-forgot-password")
(def confirm-sign-up-prefix "/confirm-sign-up")
(def change-password-prefix "/change-password")
(def refresh-token-prefix "/refresh-token")
(def resend-confirmation-code-prefix "/resend-confirmation-code")
(def query-prefix "/q")
(def dynamic-eval-prefix "/dynamic")
(def auth-prefix "/auth")
(def auth-callback-prefix "/auth/callback")
(def ai-prefix "/ai")
(def debug-prefix "/debug")
(def register-magiclink-prefix "/register-magiclink")
(def get-magiclink-prefix "/get-magiclink")
(def preview-magiclink-prefix "/preview-magiclink")
(def post-inference-service-question "/post-inference-service-question")

(def url-encode u/url-encode)
(def url-encode-plain u/url-encode-plain)
(def url-decode-plain u/url-decode-plain)

(defn- remote-resolver-error [response]
  (u/throw-ex (str "remote service error - " (or (:error response) response))))

(defn- response-handler [format callback response]
  ((or callback identity)
   (if (map? response)
     (if-let [status (:status response)]
       (if (< 199 status 299)
         #?(:clj
            ((decoder format) (:body response))
            :cljs (:body response))
         (remote-resolver-error response))
       (remote-resolver-error response))
     response)))

(defn- fetch-auth-token [options]
  (if-let [t (:auth-token options)]
    [t (dissoc options :auth-token)]
    [nil options]))

#?(:cljs
   (defn make-http-request [format body token]
     (merge {format body}
            (when token {:with-credentials? false
                         :oauth-token token}))))

(defn do-post
  ([url options request-obj format response-handler]
   (let [[token options] (fetch-auth-token options)
         body ((encoder format) request-obj)]
     #?(:clj
        (let [headers (apply
                       assoc
                       (:headers options)
                       "Content-Type" (content-type format)
                       (when token
                         ["Authorization" (str "Bearer " token)]))
              options (assoc options :headers headers)]
          (response-handler @(http/post url (assoc options :body body))))
        :cljs (go
                (let [k (if (= format :transit+json) :transit-params :json-params)]
                  (response-handler
                   (<! (http/post url (make-http-request k body token)))))))))
  ([url options request-obj]
   (do-post url options request-obj :json identity))
  ([url request-obj]
   (do-post url nil request-obj)))

(defn do-get
  ([url options format response-handler]
   (let [[token options] (fetch-auth-token options)]
     #?(:clj
        (let [headers (apply
                       assoc
                       (:headers options)
                       "Content-Type" (content-type format)
                       (when token
                         ["Authorization" (str "Bearer " token)]))
              options (assoc options :headers headers)]
          (response-handler @(http/get url options)))
        :cljs (go
                (let [k (if (= format :transit+json) :transit-params :json-params)]
                  (response-handler
                   (<! (http/get url (make-http-request k nil token)))))))))
  ([url options]
   (do-get url options :json identity))
  ([url] (do-get url nil)))

(defn do-raw-request [args-map]
  #?(:clj @(http/request args-map)
     :cljs (go (callback (<! (http/request args-map))))))

(defn do-request
  ([method callback url headers body]
   (let [req (merge {:url url :method method :headers headers} (when body {:body body}))]
     #?(:clj @(http/request req)
        :cljs (go (callback (<! (http/request req)))))))
  ([method url headers body] (do-request method identity url headers body))
  ([method url headers] (do-request method url headers nil))
  ([method url] (do-request method url nil)))

(defn POST
  ([url options request-obj format]
   (do-post
    url (dissoc options :callback)
    request-obj format (partial response-handler format (:callback options))))
  ([url options request-obj]
   (POST url options request-obj :transit+json)))

(defn normalize-post-options [arg]
  (if (fn? arg) {:callback arg} arg))

#?(:clj
   (do
     (defn- get-env-var [var-name]
       (let [var-value (System/getenv var-name)]
         (if (nil? var-value)
           (throw (Exception. (str "Environment variable \"" var-name "\" not found.")))
           var-value)))

     (defn get-aws-config []
       (let [aws-config {:region       (get-env-var "AWS_REGION")
                         :access-key   (get-env-var "AWS_ACCESS_KEY")
                         :secret-key   (get-env-var "AWS_SECRET_KEY")
                         :client-id    (get-env-var "AWS_COGNITO_CLIENT_ID")
                         :user-pool-id (get-env-var "AWS_COGNITO_USER_POOL_ID")
                         :whitelist?   (or (get-in (gs/get-app-config) [:authentication :whitelist?])
                                           false)}]
         ;;TODO: Need to revisit this and add a layer to check for domains
         ;;      that are whitelisted.
         (if (true? (:whitelist? aws-config))
           (assoc aws-config
                  :s3-bucket (get-env-var "AWS_S3_BUCKET")
                  :whitelist-file-key (get-env-var "WHITELIST_FILE_KEY"))
           aws-config)))))

(defn- fully-qualified-name [base-component n]
  (let [[c en] (s/split n #"\$")]
    (if (and c en)
      (keyword (str c "/" en))
      (keyword (str base-component "/" n)))))

(defn- normalize-path [uri] (s/split uri #"/"))

(defn parse-rest-uri [uri]
  (let [parts (s/split uri #"/")
        base-component (first parts)
        fqn (partial fully-qualified-name base-component)]
    (loop [ss (rest parts), n 2, result []]
      (if-let [s (first ss)]
        (if (= n 3)
          (recur (rest ss) 1 (conj result s))
          (recur (rest ss) (inc n) (conj result (fqn s))))
        {:component (keyword base-component)
         :entity (if (= 3 n) (last result) (last (drop-last result)))
         :path (vec result)}))))

(defn- add-path-vars [path]
  (mapcat #(vector % (str "{" (s/lower-case (name %)) "}")) path))

(defn get-child-entity-path [entity]
  (when (cn/entity? entity)
    (loop [path '()]
      (let [parent-entity
            (cn/containing-parents (or (first path) entity))]
        (if (empty? parent-entity)
          {:path (str "api/" (namespace entity)
                      (when (seq path)
                        (let [path (add-path-vars path)]
                          (str "/"
                               (apply
                                str
                                (interpose
                                 "/" (map name path))))))
                      "/" (name entity))
           :vars (map name path)}
          (recur (conj path (-> parent-entity first last))))))))

#?(:clj
   (defn form-decode [s]
     (w/keywordize-keys (codec/form-decode s))))

(defn parse-cookies [cookie-string]
  (when cookie-string
    (into {}
          (for [cookie (.split cookie-string ";")]
            (let [keyval (map #(.trim %) (.split cookie "=" 2))]
              [(first keyval) (second keyval)])))))

(defn create-pattern-from-path [entity-name obj path parent]
  (let [attrs (li/record-attributes obj)
        idn (cn/identity-attribute-name entity-name)
        has-parent (map? parent)
        relname (when has-parent (last (drop-last path)))]
    (when-not (= (last path) entity-name)
      (u/throw-ex (str "Invalid path " path)))
    (when (and relname (not (cn/contains-relationship? relname)))
      (u/throw-ex (str relname " is not a contains relationship")))
    (let [id-val (or (idn attrs) li/id-attr-s)
          path (concat path [id-val])
          pat {entity-name
               (merge
                (assoc attrs li/path-attr (li/vec-to-path path))
                (when has-parent
                  {li/parent-attr (li/path-attr parent)}))}
          create-event (cn/entity-create-action entity-name)]
      (if create-event
        {create-event
         {:Instance (cn/make-instance pat)}}
        pat))))
