#_(do (ns agentlang.test.auth
  (:require #?(:clj [clojure.test :refer [deftest is]]
               :cljs [cljs.test :refer-macros [deftest is]])
            [agentlang.util.logger :as log]
            [agentlang.util :as u]
            [agentlang.auth.oauth2 :as auth]))

;; This test should not be enabled on CI.
;; Follow these steps to run locally:
;; 1. Go to https://api.slack.com/apps and create an app.
;; 2. Set app scopes as: channels:read,groups:read,im:read,mpim:read.
;; 3. Set redirect url as: https://localhost/slack/redirect
;; 4. $ export AGENTLANG_AUTH_TEST_CLIENT_ID=client-id-of-the-app
;; 5. $ export AGENTLANG_AUTH_TEST_CLIENT_SECRET=client-secret-of-the-app
;; 6. $ lein test :only agentlang.test.auth/basic
(deftest basic
  #?(:clj
     (let [client-id (u/getenv "AGENTLANG_AUTH_TEST_CLIENT_ID" "")
           client-secret (u/getenv "AGENTLANG_AUTH_TEST_CLIENT_SECRET" "")]
       (when (and (seq client-id) (seq client-secret))
         (let [auth-obj (auth/initialize
                         auth/slack
                         {:client-id client-id
                          :client-secret client-secret
                          :scope "channels:read,groups:read,im:read,mpim:read"
                          :callback "https://localhost/slack/redirect"})]
           (is (auth/oauth2? auth-obj))
           (println (str "please go to " (auth/authorization-url auth-obj) " to authorize the client."))
           (print "once authorized, please enter the code here: ")
           (flush)
           (let [code (read-line)]
             (print "enter the secret: ")
             (flush)
             (let [secret (read-line)
                   api-obj (auth/enable-access auth-obj code secret)]
               (is (auth/access-enabled? api-obj))
               (log/info (str "trying to list issues using the access-token " (:raw-token api-obj) " ..."))
               (let [response (auth/http-get api-obj "https://slack.com/api/conversations.list")
                     status (:status response)]
                 (is (or (= status 200) (= status 404))))))
           (is (auth/release auth-obj))))))))
