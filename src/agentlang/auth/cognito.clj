(ns agentlang.auth.cognito
  (:require [amazonica.aws.cognitoidp
             :refer [admin-add-user-to-group admin-delete-user admin-get-user admin-remove-user-from-group
                     admin-update-user-attributes admin-user-global-sign-out change-password
                     confirm-forgot-password confirm-sign-up create-group delete-group admin-list-groups-for-user
                     forgot-password initiate-auth list-users resend-confirmation-code sign-up]]
            [clojure.string :as str]
            [clojure.walk :as w]
            [org.httpkit.client :as hc]
            [ring.util.codec :as codec]
            [agentlang.datafmt.json :as json]
            [agentlang.auth.core :as auth]
            [agentlang.auth.jwt :as jwt]
            [agentlang.component :as cn]
            [agentlang.user-session :as sess]
            [agentlang.lang.internal :as li]
            [agentlang.global-state :as gs]
            [agentlang.util :as u]
            [agentlang.util.http :as uh]
            [agentlang.util.logger :as log]))

(def ^:private tag :cognito)

(defmethod auth/make-client tag [{:keys [access-key secret-key region] :as _config}]
  (or (auth/client-key _config)
      {:access-key access-key
       :secret-key secret-key
       :endpoint region}))

(defn make-jwks-url [region user-pool-id]
  (str "https://cognito-idp."
       region
       ".amazonaws.com/"
       user-pool-id
       "/.well-known/jwks.json"))

(defn- get-error-msg-and-log [cognito-exception]
  (let [error-msg (ex-message cognito-exception)]
    (log/error cognito-exception)
    (subs
     error-msg
     0
     (or (str/index-of error-msg  "(Service: AWSCognitoIdentityProvider")
         (count error-msg)))))

(defn- parse-user-filter [filter]
  (if filter
    (let [opr (first filter)
          n (str/lower-case (name (second filter)))
          opr (case opr
                := "="
                :like "^="
                (u/throw-ex (str "operator not support bu cognito list-users - " opr)))]
      (str n " " opr " \"" (last filter) "\""))
    ""))

(defn- find-cognito-users
  ([client user-pool-id filter]
   (let [filter (if (string? filter) filter (parse-user-filter filter))]
     (:users (list-users client :user-pool-id user-pool-id :filter filter))))
  ([client user-pool-id]
   (find-cognito-users client user-pool-id nil)))

(defn- find-cognito-username-by-email [client user-pool-id email]
  (let [users (find-cognito-users client user-pool-id (str "email = \"" email "\""))
        user (first users)]
    (:username user)))

(defn- verify-and-extract [{region :region user-pool-id :user-pool-id} token]
  (try
    (jwt/verify-and-extract
     (make-jwks-url region user-pool-id)
     token)
    (catch Exception e
      (log/warn e))))

(defn- assign-user-roles [client user-pool-id username default-role]
  (let [user-groups (admin-list-groups-for-user client :user-pool-id user-pool-id :username username)
        roles (mapv :group-name (:groups user-groups))
        roles-for-user (if-not (seq roles) [(or default-role "user")] roles)]
    (sess/maybe-assign-roles username roles-for-user)))

(defmethod auth/verify-token tag [_config token]
  (verify-and-extract (uh/get-aws-config) token))

(defmethod auth/make-authfn tag [_config]
  (let [aws-config (uh/get-aws-config)]
    (fn [_req token]
      (verify-and-extract aws-config token))))

(defmethod auth/user-login tag [{:keys [event default-role] :as req}]
  (let [{:keys [client-id user-pool-id] :as aws-config} (uh/get-aws-config)
        client (auth/make-client (merge req aws-config))]
    (try
      (let [login-response
            (initiate-auth client
                           :auth-flow "USER_PASSWORD_AUTH"
                           :auth-parameters {"USERNAME" (:Username event)
                                             "PASSWORD" (:Password event)}
                           :client-id client-id)]
        (assign-user-roles client user-pool-id (:Username event) default-role) 
        login-response)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defn- sign-up-user [req aws-config client-id user-pool-id whitelist? user]
  (let [{:keys [Name FirstName LastName Password Email]} user]
    (try
      (sign-up
       (auth/make-client (merge req aws-config))
       :client-id client-id
       :password Password
       :user-attributes [["given_name" FirstName]
                         ["family_name" LastName]
                         ["email" Email]
                         ["name" Name]]
       :username Email)
      (catch com.amazonaws.services.cognitoidp.model.UsernameExistsException ex
        (log/error ex))
      (catch com.amazonaws.services.cognitoidp.model.CodeDeliveryFailureException ex
        (log/error ex))
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/upsert-user tag [{:keys [instance operation] :as req}]
  (let [{:keys [client-id user-pool-id whitelist?] :as aws-config} (uh/get-aws-config)
        req-type (last (li/split-path (cn/instance-type instance)))]
    (cond
      ;; Create User
      (= :create operation)
      (let [user instance]
        (when (:Password user)
          (sign-up-user req aws-config client-id user-pool-id whitelist? user))
        nil)

      ;; Update user
      (or (= :update operation) (= req-type :UpdateUser))
      (if (= req-type :User)
        (let [{:keys [FirstName LastName Email AppId]} instance]
          (try
            (let
              [queried-user (list-users :user-pool-id user-pool-id
                                        :filter (str "email = \"" (str Email) "\""))
               user (first (:users queried-user))
               {:keys [username]} user]
              (admin-update-user-attributes
                (auth/make-client (merge req aws-config))
                :username username
                :user-pool-id user-pool-id
                :user-attributes [["given_name" FirstName]
                                  ["family_name" LastName]
                                  ["custom:app_id" AppId]]))
            (catch Exception e
              (throw (Exception. (get-error-msg-and-log e))))))
        (let [user-details (:UserDetails instance)
              cognito-username (get-in req [:user :username])
              inner-user-details (:User user-details)
              {:keys [FirstName LastName AppId]} inner-user-details
              github-details (get-in user-details [:OtherDetails :GitHub])
              {:keys [Username Org Token]} github-details
              refresh-token (get-in user-details [:OtherDetails :RefreshToken])
              open-ai-info (get-in user-details [:OtherDetails :OpenAI])
              {:keys [Key FinetunedModel]} open-ai-info]
          (try
            (admin-update-user-attributes
              (auth/make-client (merge req aws-config))
              :username cognito-username
              :user-pool-id user-pool-id
              :user-attributes [["given_name" FirstName]
                                ["family_name" LastName]
                                ["custom:github_org" Org]
                                ["custom:github_token" Token]
                                ["custom:github_username" Username]
                                ["custom:openai_key" Key]
                                ["custom:openai_tuned_model" FinetunedModel]
                                ["custom:app_id" AppId]])
            ;; Refresh credentials
            (initiate-auth
              (auth/make-client (merge req aws-config))
              :auth-flow "REFRESH_TOKEN_AUTH"
              :auth-parameters {"USERNAME"      cognito-username
                                "REFRESH_TOKEN" refresh-token}
              :client-id client-id)
            (catch Exception e
              (throw (Exception. (get-error-msg-and-log e)))))))

      :else nil)))

(defmethod auth/refresh-token tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        cognito-username (get-in req [:user :username])
        refresh-token (:RefreshToken event)]
    (try
      (initiate-auth
       (auth/make-client (merge req aws-config))
       :auth-flow "REFRESH_TOKEN_AUTH"
       :auth-parameters {"USERNAME" cognito-username
                         "REFRESH_TOKEN" refresh-token}
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defn- make-session-info [token]
  (let [parts (str/split token #" ")]
    {:token-type (first parts) :id-token (last parts)}))

(defmethod auth/session-user tag [all-stuff-map]
  (let [user-details (get-in all-stuff-map [:request :identity])]
    {:github-username (:custom:github_username user-details)
     :github-token (:custom:github_token user-details)
     :github-org (:custom:github_org user-details)
     :openai-key (:custom:openai_key user-details)
     :openai-fine-tuned-model (:custom:openai_tuned_model user-details)
     :app-id (:custom:app_id user-details)
     :session-info (make-session-info (get-in all-stuff-map [:request :headers "authorization"]))
     :email (or (:email user-details) (:username user-details))
     :sub (:sub user-details)
     :username (or (:cognito:username user-details) (:sub user-details))}))

(defmethod auth/session-sub tag [req]
  (auth/session-user req))

(defmethod auth/authenticate-session tag [_]
  (u/throw-ex (str "authenticate-session not implemented for " tag)))

(defn- create-event [event-name]
  {cn/type-tag-key :event
   cn/instance-type (keyword event-name)})

(defmethod auth/handle-auth-callback tag [auth-config]
  (let [{call-post-signup :call-post-signup
         request :request} (:args auth-config)
        query (when-let [s (:query-string request)]
                (w/keywordize-keys (codec/form-decode s)))
        code (:code query)
        redirect-query (:redirect query)
        cognito-domain (u/getenv "AWS_COGNITO_DOMAIN")
        api-url (u/getenv "AGENTLANG_API_URL")
        ui-url (u/getenv "AGENTLANG_UI_URL")
        client-id (u/getenv "AWS_COGNITO_CLIENT_ID")]
    (try
      (let [resp
            @(hc/post
              (str cognito-domain "/oauth2/token")
              {:headers {"Content-Type" "application/x-www-form-urlencoded"}
               :query-params
               {:grant_type "authorization_code"
                :code code
                :client_id client-id
                :redirect_uri (str api-url "/auth/callback"
                                   (when redirect-query (str "?redirect=" redirect-query)))}})
            tokens (json/decode (:body resp))]
        (if-let [token (:id_token tokens)]
          (if-let [user (auth/verify-token auth-config token)]
            (when (:email user)
              (let [user-obj {:Email (:email user)
                              :Name (str (:given_name user) " " (:family_name user))
                              :FirstName (:given_name user)
                              :LastName (:family_name user)}
                    sign-up-request
                    {:Agentlang.Kernel.Identity/SignUp
                     {:User {:Agentlang.Kernel.Identity/User user-obj}}}
                    new-sign-up
                    (not
                     (first
                      (:result
                       (gs/evaluate-dataflow
                        {:Agentlang.Kernel.Identity/FindUser
                         {:Email (:Email user-obj)}}))))]
                (when new-sign-up
                  (let [sign-up-result (:result (gs/evaluate-dataflow sign-up-request))]
                    (when call-post-signup
                      (gs/evaluate-dataflow
                       (assoc
                        (create-event :Agentlang.Kernel.Identity/PostSignUp)
                        :SignupResult sign-up-result :SignupRequest {:User user-obj})))))
                (sess/upsert-user-session (:sub user) true)
                {:status :redirect-found
                 :location (str (or redirect-query ui-url)
                                "/?id_token=" (:id_token tokens)
                                "&refresh_token=" (:refresh_token tokens))}))
            {:error "id_token not valid"})
          {:error "error fetching tokens"}))
      (catch Exception e
        (log/info (str e))
        {:error "error fetching tokens"}))))

(defmethod auth/user-logout tag [{:keys [sub] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)]
    (try
      (admin-user-global-sign-out
       (auth/make-client (merge req aws-config))
       :user-pool-id user-pool-id
       :username (:username sub))
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/delete-user tag [{:keys [instance] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)]
    (when-let [email (:Email instance)]
      (try
        (admin-delete-user
         :username email
         :user-pool-id user-pool-id)
        (catch Exception e
          (throw (Exception. (get-error-msg-and-log e))))))))

(defmethod auth/get-user tag [{:keys [user] :as req}]
  (let [{:keys [user-pool-id] :as aws-config} (uh/get-aws-config)
        resp (admin-get-user
              (auth/make-client (merge req aws-config))
              :username (:username user)
              :user-pool-id user-pool-id)
        user-attributes (:user-attributes resp)
        user-attributes-map (reduce
                             (fn [attributes-map attribute-map]
                               (assoc attributes-map (keyword (:name attribute-map)) (:value attribute-map)))
                             {}
                             user-attributes)
        {:keys [custom:github_username
                custom:github_token
                custom:github_org
                custom:openai_key
                custom:openai_tuned_model
                custom:app_id
                given_name family_name email]} user-attributes-map]
    {:GitHub {:Username custom:github_username
              :Token custom:github_token
              :Org custom:github_org}
     :OpenAI {:Key            custom:openai_key
              :FinetunedModel custom:openai_tuned_model}
     :AppId custom:app_id
     :FirstName given_name
     :LastName family_name
     :Email email}))

(defmethod auth/resend-confirmation-code tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)]
    (try
      (resend-confirmation-code
       (auth/make-client (merge req aws-config))
       :username (:Username event)
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/confirm-sign-up tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        {:keys [Username ConfirmationCode]} event]
    (try
      (confirm-sign-up
       (auth/make-client (merge req aws-config))
       :username Username
       :confirmation-code ConfirmationCode
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)]
    (try
      (forgot-password
       (auth/make-client (merge req aws-config))
       :username (:Username event)
       :client-id client-id)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/confirm-forgot-password tag [{:keys [event] :as req}]
  (let [{:keys [client-id] :as aws-config} (uh/get-aws-config)
        {:keys [Username ConfirmationCode Password]} event]
    (try
      (confirm-forgot-password
       (auth/make-client (merge req aws-config))
       :username Username
       :confirmation-code ConfirmationCode
       :client-id client-id
       :password Password)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/change-password tag [{:keys [event] :as req}]
  (let [aws-config (uh/get-aws-config)
        {:keys [AccessToken CurrentPassword NewPassword]} event]
    (try
      (change-password
       (auth/make-client (merge req aws-config))
       :access-token AccessToken
       :previous-password CurrentPassword
       :proposed-password NewPassword)
      (catch Exception e
        (throw (Exception. (get-error-msg-and-log e)))))))

(defmethod auth/create-role tag [{:keys [role-name] :as req}]
  (try
    (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
      (create-group
       (auth/make-client (merge req cfg))
       :group-name role-name
       :user-pool-id user-pool-id))
    (catch com.amazonaws.services.cognitoidp.model.GroupExistsException ex
      req)))

(defmethod auth/delete-role tag [{:keys [client role-name] :as req}]
  (try
    (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)]
      (delete-group
       (or client (auth/make-client (merge req cfg)))
       :group-name role-name
       :user-pool-id user-pool-id))
    (catch com.amazonaws.services.cognitoidp.model.ResourceNotFoundException ex
      req)))

(defmethod auth/add-user-to-role tag [{:keys [client role-name username] :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)
        client (or client (auth/make-client (merge req cfg)))
        user-id (find-cognito-username-by-email client user-pool-id username)]
    (admin-add-user-to-group
     client
     :group-name role-name
     :username user-id
     :user-pool-id user-pool-id)))

(defmethod auth/remove-user-from-role tag [{:keys [client role-name username] :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)
        client (or client (auth/make-client (merge req cfg)))]
    (if-let [user-id (find-cognito-username-by-email client user-pool-id username)]
      (admin-remove-user-from-group
       client
       :group-name role-name
       :username user-id
       :user-pool-id user-pool-id)
      (do (log/warn (str "remove-user-from-role: cognito user not found - " username))
          username))))

(defn- find-attribute-value [user-attrs n]
  (let [n (name n)]
    (:value (first (filter (fn [x] (= n (:name x))) user-attrs)))))

(defn- as-identity-user [user]
  (let [attrs (:attributes user)
        f (partial find-attribute-value attrs)
        user-name (or (f :name) (:username user))]
    (cn/make-instance
     :Agentlang.Kernel.Identity/User
     {:Name user-name
      :FirstName (or (f :given_name) user-name)
      :LastName (or (f :family_name) user-name)
      :Email (f :email)
      :UserData attrs}
     false)))

(defmethod auth/lookup-users tag [{client :client :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)
        client (or client (auth/make-client (merge req cfg)))
        clause (auth/query-key req)]
    (mapv as-identity-user (find-cognito-users client user-pool-id clause))))

(defmethod auth/lookup-all-users tag [{client :client :as req}]
  (let [{user-pool-id :user-pool-id :as cfg} (uh/get-aws-config)
        client (or client (auth/make-client (merge req cfg)))]
    (mapv as-identity-user (find-cognito-users client user-pool-id))))
