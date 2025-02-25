(ns agentlang.auth.okta
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.http :as http]
            [agentlang.util.logger :as log]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.lang.b64 :as b64]
            [agentlang.datafmt.json :as json]
            [agentlang.global-state :as gs]
            [agentlang.auth.jwt :as jwt]
            [agentlang.auth.core :as auth]
            [agentlang.user-session :as sess]))

(def ^:private tag :okta)

;; config required for okta/auth:
#_{:rbac-enabled true
   :authentication {:service :okta
                    :is-identity-store true ; use okta as the sole identity/user store
                    :superuser-email <email>
                    :domain <okta-domain>
                    :auth-server <okta-auth-server-name> ; or "default"
                    :client-id <okta-app-client-id>
                    :client-secret <okta-app-secret>
                    :api-token <okta-api-token>
                    :scope "openid offline_access"
                    :introspect true ; if false token will be verified locally
                    :authorize-redirect-url "http://localhost:3000/auth/callback"
                    :client-url "http://localhost:3000/order"
                    :logout-redirect "http://localhost:3000/bye"

                    :role-claim :user-roles
                    ;; "user-roles" is the custom attribute set for Okta users.
                    ;; More than one role or group can be specified as a comma-delmited string
                    ;; or as a vector of strings.
                    ;; Okta groups can be automatically set as a role-claim attribute by following
                    ;; these steps:
                    ;; In Okta console, navigate to Security > API. Select your authorization server and go to the Claims tab.
                    ;; Set these values: Name: groups, Include in: Access Token, Value type: Groups,
                    ;; Filter: Select Matches regex and use .* as the value
                    ;; Then, click Create. In this config set `:role-claim` to `:groups`.

                    :default-role "guest"

                    ;; cache is optional
                    :cache {:host <REDIS_HOST>
                            :port <REDIS_PORT>}}}

(defmethod auth/make-client tag [_config]
  _config)

(def ^:private openid-config (u/make-cell nil))

(defn get-jwks-url [domain auth-server client-id]
  (let [cfg
        (or @openid-config
            (u/safe-set
             openid-config
             (let [resp (http/do-get
                         (str "https://" domain
                              "/oauth2/" auth-server
                              "/.well-known/openid-configuration?client_id=" client-id))]
               (if (= (:status resp) 200)
                 (json/decode (:body resp))
                 (let [msg "Okta jwks fetch failed"]
                   (log/warn (str msg ": " resp))
                   (u/throw-ex (str msg " with status: " (:status resp))))))))]
    (:jwks_uri cfg)))

(defn- verify-and-extract [{domain :domain auth-server :auth-server client-id :client-id} token]
  (try
    (jwt/verify-and-extract
     (get-jwks-url domain auth-server client-id)
     token)
    (catch Exception e
      (log/warn e))))

(declare introspect)

(defn- refresh-tokens [{domain :domain
                        client-id :client-id
                        client-secret :client-secret
                        auth-server :auth-server
                        authorize-redirect-url :authorize-redirect-url
                        :as auth-config}
                       token sid]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/token")
        req (str "client_id=" client-id "&client_secret=" client-secret
                 "&grant_type=refresh_token&redirect_uri=" authorize-redirect-url
                 "&refresh_token=" token)
        resp (http/do-request :post url {"Content-Type" "application/x-www-form-urlencoded"} req)]
    (if (= 200 (:status resp))
      (let [r (us/snake-to-kebab-keys (json/decode (:body resp)))
            data (sess/session-cookie-update-tokens sid r)]
        (if data
          (introspect auth-config [sid data])
          (log/warn (str "failed to refresh tokens for " sid))))
      (do (log/error resp)
          (u/throw-ex (str "failed to refresh access token with error: " (:status resp)))))))

(defn- do-introspect [token-hint domain auth-server client-id client-secret token]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/introspect")
        _ (log/debug (str "okta introspect for client " client-id ", " url))
        req (str "token=" token "&token_type_hint=" token-hint)
        resp (http/do-request
              :post url
              {"Content-Type" "application/x-www-form-urlencoded"
               "Authorization" (str "Basic " (b64/encode-string (str client-id ":" client-secret)))}
              req)]
    (if (= 200 (:status resp))
      (let [result (json/decode (:body resp))]
        (log/debug (str "okta token for client " client-id " active flag is " (:active result)))
        (if (:active result)
          result
          (log/warn (str token-hint " introspect returned inactive state: " result))))
      (log/warn (str token-hint " introspect returned status: " (:status resp))))))

(def ^:private introspect-access-token (partial do-introspect "access_token"))
(def ^:private introspect-refresh-token (partial do-introspect "refresh_token"))

(defn- introspect [{domain :domain
                    auth-server :auth-server
                    client-id :client-id
                    client-secret :client-secret
                    :as auth-config}
                   [sid data]]
  (let [authres (:authentication-result data)
        access-token (:access-token authres)
        resp (introspect-access-token
              domain auth-server client-id client-secret
              access-token)]
    (or resp
        (let [reftok (:refresh-token authres)]
          (log/debug (str "okta starting refresh token request for client " client-id))
          (when (introspect-refresh-token
                 domain auth-server client-id client-secret
                 reftok)
            (refresh-tokens auth-config reftok sid))))))

(defn- introspection-required? [auth-config cookie-created-millis]
  (if cookie-created-millis
    (if-let [ttl-ms (:cookie-ttl-ms auth-config)]
      (< ttl-ms (- (System/currentTimeMillis) cookie-created-millis))
      (:introspect auth-config))
    (:introspect auth-config)))

(defmethod auth/verify-token tag [auth-config [data cookie-created-millis]]
  (if (vector? data)
    (let [result
          (if (introspection-required? auth-config cookie-created-millis)
            (do (log/debug (str "auth/okta: introspecting token remotely - " data))
                (introspect auth-config data))
            (do (log/debug (str "auth/okta: verifying token locally - " data))
                (verify-and-extract auth-config (:id-token (:authentication-result (second data))))))]
      (if-not (:username result)
        (assoc result :username (:username (second data)))
        result))
    (verify-and-extract auth-config data)))

(defmethod auth/make-authfn tag [auth-config]
  (fn [_ token] (verify-and-extract auth-config token)))

(defn- extract-sid [cookie]
  (when-let [i (s/index-of cookie "sid=")]
    (let [j (s/index-of cookie ";" i)
          sid (subs cookie i j)]
      sid)))

(defn- code-to-tokens [{domain :domain
                        client-id :client-id
                        client-secret :client-secret
                        auth-server :auth-server
                        authorize-redirect-url :authorize-redirect-url
                        server-redirect-host :server-redirect-host}
                       code]
  (let [url (str "https://" domain "/oauth2/" auth-server "/v1/token")
        req (str "client_id=" client-id "&client_secret=" client-secret
                 "&grant_type=authorization_code&redirect_uri=" (or server-redirect-host authorize-redirect-url)
                 "&code=" code)
        resp (http/do-request :post url {"Content-Type" "application/x-www-form-urlencoded"} req)]
    (if (= 200 (:status resp))
      (json/decode (:body resp))
      (do (log/error resp)
          (u/throw-ex (str "failed to get access token with error: " (:status resp)))))))

(def ^:private user-state-delim "._.")

(defn- make-authorize-url [{domain :domain
                            auth-server :auth-server
                            authorize-redirect-url :authorize-redirect-url
                            server-redirect-host :server-redirect-host
                            client-id :client-id
                            no-prompt :no-prompt
                            scope :scope
                            user-state :user-state
                            session-token :session-token}]
  (let [nonce (str "n-" (us/generate-code 3) "_" (us/generate-code 6))
        s0 (us/generate-code 10)
        state (if user-state (str s0 user-state-delim user-state) s0)
        url0 (str "https://" domain "/oauth2/" auth-server "/v1/authorize?client_id=" client-id
                  "&response_type=code&scope=" (http/url-encode-plain scope) "&redirect_uri="
                  (http/url-encode (or server-redirect-host authorize-redirect-url)) "&state=" state "&nonce=" nonce)
        url (if no-prompt (str url0 "&prompt=none") url0)]
    [(if session-token
       (str url "&sessionToken=" session-token)
       url) state]))

(defn- fetch-tokens [config login-result]
  (let [session-token (:sessionToken login-result)
        [url state] (make-authorize-url (assoc config :no-prompt true :session-token session-token))
        result (http/do-get url {:follow-redirects false})]
    (log/debug (str "okta fetch-tokens from " url " returned status " (:status result)))
    (if (= 302 (:status result))
      (let [loc (:location (:headers result))
            s (subs loc (inc (s/index-of loc "?")))
            login-info (http/form-decode s)]
        (when (not= state (:state login-info))
          (u/throw-ex (str "okta/authorize failed with state mismatch - " state " <> " (:state login-info))))
        (let [tokens (code-to-tokens config (:code login-info))]
          {:authentication-result
           (merge
            {:state state
             :user-data {:cookie (extract-sid (:set-cookie (:headers result)))}}
            (us/snake-to-kebab-keys tokens))}))
      (do (log/error result)
          (u/throw-ex (str "okta/authorize call failed. expected redirect, not " (:status result)))))))

(defmethod auth/user-login tag [{event :event domain :domain :as config}]
  (let [url (str "https://" domain "/api/v1/authn")
        _ (log/debug (str "auth/user-login user: " (:Username event) ", " url))
        result
        (try
          (http/do-post url {"username" (:Username event)
                             "password" (:Password event)
                             "options"
                             {"multiOptionalFactorEnroll" false
                              "warnBeforePasswordExpired" false}})
          (catch Exception ex
            (log/error ex)))]
    (if (= (:status result) 200)
      (let [r (fetch-tokens config (json/decode (:body result)))]
        (auth/on-user-login (:Username event))
        r)
      (log/warn (str "login failed: " result)))))

(defmethod auth/upsert-user tag [req]
  (:instance req))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  ;; TODO: implement refresh-token
  )

(defn- cookie-to-sid [cookie]
  (when cookie
    (when-let [kvs (http/parse-cookies cookie)]
      (get kvs "sid"))))

(defmethod auth/cookie-to-session-id tag [_ cookie]
  (cookie-to-sid cookie))

(defmethod auth/authenticate-session tag [{cookie :cookie
                                           client-url :client-url
                                           server-redirect-host :server-redirect-host
                                           :as auth-config}]
  (let [user-state (str (when client-url (b64/encode-string client-url)) user-state-delim
                        (when server-redirect-host (b64/encode-string server-redirect-host)))
        auth-config (assoc auth-config :user-state user-state)]
    (if-let [sid (auth/cookie-to-session-id auth-config cookie)]
      (do
        (log/debug (str "auth/authenticate-session with cookie " sid))
        (if (sess/lookup-session-cookie-user-data sid)
          {:status :redirect-found
           :location client-url}
          {:status :redirect-found}))
      {:status :redirect-found
       :location (first (make-authorize-url auth-config))})))

(defn- cleanup-roles [roles default-role]
  (let [roles (if (vector? roles)
                (vec (filter #(not= "Everyone" %) roles))
                roles)]
    (if (seq roles)
      roles
      default-role)))

(defn- get-nth-state [state n]
  (let [st (get (s/split state (re-pattern user-state-delim)) n)]
    (when (seq st)
      (b64/decode-to-string st))))

(defn client-url-from-state [state]
  (get-nth-state state 1))

(defn server-redirect-host-from-state [state]
  (get-nth-state state 2))

(defmethod auth/handle-auth-callback tag [{client-url :client-url args :args :as auth-config}]
  (let [request (:request args)
        current-sid (cookie-to-sid (get-in request [:headers "cookie"]))
        params (http/form-decode (:query-string request))
        server-redirect-host (server-redirect-host-from-state (:state params))
        tokens (code-to-tokens (assoc auth-config :server-redirect-host server-redirect-host) (:code params))
        session-id (or current-sid (u/uuid-string))
        result {:authentication-result (us/snake-to-kebab-keys tokens)}
        auth-status (auth/verify-token auth-config [[session-id result] nil])
        client-url (or (client-url-from-state (:state params)) client-url)
        user (:username auth-status)]
    (log/debug (str "auth/handle-auth-callback returning session-cookie " session-id " to " client-url))
    (when-not (sess/ensure-local-user
               user
               (cleanup-roles
                (get auth-status (:role-claim auth-config))
                (:default-role auth-config)))
      (log/warn (str "okta - failed to ensure local user " user)))
    (if (and user (sess/upsert-user-session user true)
             ((if current-sid
                sess/session-cookie-replace
                sess/session-cookie-create)
              session-id (assoc result :username user)))
      (do
        (auth/on-user-login user)
        {:status :redirect-found
         :location client-url
         :set-cookie (str "sid=" session-id)})
      {:error "failed to create session"})))

(defmethod auth/session-user tag [{req :request cookie :cookie :as auth-config}]
  (if-let [sid (auth/cookie-to-session-id auth-config cookie)]
    (let [[session-data ttl] (sess/lookup-session-cookie-user-data sid)
          result (auth/verify-token auth-config [[sid session-data] ttl])
          user (:sub result)
          username (or (:username result) user)]
      {:email username
       :sub user
       :username username
       :session-info (:authentication-result session-data)})
    (let [user (get-in req [:identity :sub])]
      {:email user
       :sub user
       :username user})))

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/user-logout tag [{domain :domain
                                  auth-server :auth-server
                                  req :request
                                  cookie :cookie
                                  logout-redirect :logout-redirect}]
  (let [redirect-uri (http/url-encode logout-redirect)
        id-token (if cookie
                   (get-in (second cookie) [:authentication-result :id-token])
                   (jwt/remove-bearer (get (:headers req) "authorization")))
        url (str "https://" domain "/oauth2/" auth-server "/v1/logout?"
                 "id_token_hint=" id-token "&post_logout_redirect_uri=" redirect-uri)
        resp (http/do-get url {:follow-redirects false})]
    (if (= 200 (:status resp))
      :bye
      (do (log/error resp)
          (str "logout failed with status " (:status resp))))))

(defmethod auth/delete-user tag [_]
  (u/throw-ex "auth/delete-user not implemented for okta"))

(defmethod auth/get-user tag [_]
  (u/throw-ex "auth/get-user not implemented for okta"))

(defn- as-identity-user [okta-user]
  (let [profile (:profile okta-user)]
    (cn/make-instance
     :Agentlang.Kernel.Identity/User
     {:Name (:login profile)
      :FirstName (:firstName profile)
      :LastName (:lastName profile)
      :Email (:email profile)
      :UserData okta-user})))

(defn- parse-opr [opr]
  (case opr
    := "eq"
    :like "sw" ; starts-with
    :< "lt"
    :> "gt"
    :<= "lteq"
    :>= "gteq"
    (u/throw-ex (str "operator not supported by okta: " opr))))

(defn- clause-as-query [clause]
  (let [opr (first clause)]
    (when (or (= opr :and) (= opr :or))
      (u/throw-ex (str "logical oprerator not supported by okta search - " opr)))
    (let [opr (parse-opr opr)
          n (second clause)
          attr (case n
                 :Email "profile.email"
                 :FirstName "profile.firstName"
                 :LastName "profile.lastName"
                 (name n))]
      (http/url-encode-plain (str attr " " opr " \"" (last clause) "\"")))))

(defmethod auth/lookup-users tag [{domain :domain api-token :api-token :as config}]
  (when-not api-token
    (u/throw-ex "okta api token is required to lookup users"))
  (let [clause (auth/query-key config)
        q (clause-as-query clause)
        url (str "https://" domain "/api/v1/users?search=" q)
        resp (http/do-get url {:headers {"Authorization" (str "SSWS " api-token)}})]
    (if (= 200 (:status resp))
      (mapv as-identity-user (json/decode (:body resp)))
      (u/throw-ex (str "okta lookup-users failed with status " (:status resp))))))

(defmethod auth/lookup-all-users tag [{domain :domain api-token :api-token}]
  (when-not api-token
    (u/throw-ex "okta api token is required to lookup users"))
  (let [url (str "https://" domain "/api/v1/users")
        resp (http/do-get url {:headers {"Authorization" (str "SSWS " api-token)}})]
    (if (= 200 (:status resp))
      (mapv as-identity-user (json/decode (:body resp)))
      (u/throw-ex (str "okta lookup-users failed with status " (:status resp))))))

(defmethod auth/resend-confirmation-code tag [_]
  (u/throw-ex "auth/resend-confirmation-code not implemented for okta"))

(defmethod auth/confirm-sign-up tag [_]
  (u/throw-ex "auth/confirm-sign-up not implemented for okta"))

(defmethod auth/forgot-password tag [_]
  (u/throw-ex "auth/forgot-password not implemented for okta"))

(defmethod auth/confirm-forgot-password tag [_]
  (u/throw-ex "auth/confirm-forgot-password not implemented for okta"))

(defmethod auth/change-password tag [_]
  (u/throw-ex "auth/change-password not implemented for okta"))

(defmethod auth/create-role tag [_]
  (log/warn "auth/create-role not implemented for okta")
  true)

(defmethod auth/delete-role tag [_]
  (u/throw-ex "auth/delete-role not implemented for okta"))

(defmethod auth/add-user-to-role tag [_]
  (log/warn "auth/add-user-to-role not implemented for okta")
  true)

(defmethod auth/remove-user-from-role tag [_]
  (log/warn "auth/remove-user-from-role not implemented for okta")
  true)
