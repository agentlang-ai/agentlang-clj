(ns agentlang.test.openapi
  (:require [clojure.test :refer [deftest is]]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.lang.tools.openapi :as openapi]
            [agentlang.lang
             :refer [component attribute event
                     entity record dataflow]]
            [agentlang.test.util :as tu :refer [defcomponent]]))

(defn- openapi-test-enabled? []
  (System/getenv "OPENAPI_TEST_ENABLED"))

(def spec-url "https://raw.githubusercontent.com/APIs-guru/openapi-directory/refs/heads/main/APIs/nytimes.com/article_search/1.0.0/openapi.yaml")

(when (openapi-test-enabled?)

  (defn- parse-spec [spec-url]
    (let [model (openapi/parse spec-url)
          cn (first (:components model))]
      (is (cn/component-exists? cn))
      (u/run-init-fns)
      [cn model]))

  (deftest basic-api-call
    (let [[cn model] (parse-spec spec-url)
          config-entity (:config-entity model)]
      (let [event-name (first (cn/api-event-names cn))
            cfg (tu/invoke
                 {(cn/crud-event-name config-entity :Create)
                  {:Instance
                   {config-entity
                    {:apikey {:api-key (u/getenv "NYT_API_KEY")}}}}})]
        (is (cn/instance-of? config-entity cfg))
        (is
         (seq
          (get-in
           (tu/invoke
            {(openapi/invocation-event event-name)
             {:Parameters {:q "election"}}})
           ;;; If the config-entity is not set, add security to the event as,
           ;; {:EventContext {:security {:apikey {:api-key (u/getenv "NYT_API_KEY")}}}}
           [:response :docs]))))))

  (deftest petstore
    (let [[cn _] (parse-spec "test/sample/petstore.yaml")]
      (is (= :SwaggerPetstoreOpenAPI30 cn))
      (let [recnames (cn/record-names cn)]
        (is (> (count recnames) 1))
        (is (some #{(li/make-path cn :Pet)} recnames)))
      (let [pet? (partial cn/instance-of? :SwaggerPetstoreOpenAPI30/Pet)
            p? (fn [r]
                 (is (pet? r))
                 (is (= 102 (:id r))))]
        (p? (tu/invoke {(openapi/invocation-event :SwaggerPetstoreOpenAPI30/addPet)
                        {:Parameters
                         {:id 102
                          :category {:id 1 :name "my-pets"}
                          :name "kittie"
                          :photoUrls ["https://mypets.com/imgs/kittie.jpg"]
                          :tags [{:id 1, :name "cats"}]
                          :status "available"}}}))
        (p? (tu/invoke {(openapi/invocation-event :SwaggerPetstoreOpenAPI30/getPetById)
                        {:Parameters
                         {:petId 102}}}))
        (is (= "Pet deleted" (tu/invoke {(openapi/invocation-event :SwaggerPetstoreOpenAPI30/deletePet)
                                         {:Parameters
                                          {:petId 102}}})))
        (let [rs (tu/invoke {(openapi/invocation-event :SwaggerPetstoreOpenAPI30/findPetsByStatus)
                             {:Parameters
                              {:status "available"}}})]
          (is (every? pet? rs))))))

  ) ; (when (openapi-test-enabled?)
