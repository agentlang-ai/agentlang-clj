(ns agentlang.test.openapi
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.tools.openapi :as openapi]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.test.util :as tu :refer [defcomponent]]))

(def spec-url "https://raw.githubusercontent.com/APIs-guru/openapi-directory/refs/heads/main/APIs/nytimes.com/article_search/1.0.0/openapi.yaml")

(deftest parse-to-model
  (let [cn (openapi/parse spec-url)]
    (u/run-init-fns)
    (is (cn/component-exists? cn))
    (let [event-name (first (cn/api-event-names cn))]
      (is (= :nytimes.com.article_search/ArticlesearchJson event-name))
      (tu/invoke
       {(openapi/invocation-event event-name) {:Parameters {:q "election"}}}))))
