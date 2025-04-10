(ns agentlang.test.openapi
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.tools.openapi :as openapi]))

(def spec-url "https://raw.githubusercontent.com/APIs-guru/openapi-directory/refs/heads/main/APIs/nytimes.com/article_search/1.0.0/openapi.yaml")

(deftest parse-to-model
  (openapi/parse spec-url)
  (is (cn/component-exists? :nytimes.com.article_search)))
