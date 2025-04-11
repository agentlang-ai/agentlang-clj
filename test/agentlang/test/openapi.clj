(ns agentlang.test.openapi
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.tools.openapi :as openapi]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.test.util :as tu :refer [defcomponent]]))

(defn- openapi-test-enabled? []
  (System/getenv "OPENAPI_TEST_ENABLED"))

(def spec-url "https://raw.githubusercontent.com/APIs-guru/openapi-directory/refs/heads/main/APIs/nytimes.com/article_search/1.0.0/openapi.yaml")

(deftest parse-to-model
  (when (openapi-test-enabled?)
    (let [cn (openapi/parse spec-url)]
      (u/run-init-fns)
      (is (cn/component-exists? cn))
      (let [event-name (first (cn/api-event-names cn))
            [sec-name _] (first (openapi/get-component-security-schemes cn))]
        (is (= :nytimes.com.article_search/ArticlesearchJson event-name))
        (openapi/set-security cn sec-name (u/getenv "NYT_API_KEY"))
        ;; TODO: automate handling of response (see L81 of agentlang.lang.tools.openapi)
        (is
         (seq
          (get-in
           (tu/invoke
            {(openapi/invocation-event event-name) {:Parameters {:q "election"}}})
           [:response :docs])))))))
