(ns agentlang.test.errors
  (:require [agentlang.util.errors :as errors]
            [agentlang.test.util :as test-util]
            [clojure.spec.alpha :as s]
            #?(:clj [clojure.test :refer [deftest is testing]]
               :cljs [cljs.test :refer-macros [deftest is testing]])
            [agentlang.api :as api]
            [agentlang.evaluator :as e]
            #?(:clj [agentlang.test.util :as tu :refer [defcomponent]]
               :cljs [agentlang.test.util :as tu :refer-macros [defcomponent]])))

;; specs for the expected structure of the error response
(s/def ::status (s/and keyword? #(= % :error)))
(s/def ::message string?)
(s/def ::result-map (s/keys :req-un [::status ::result ::env ::message]))

(def raised-error-test-cases
  [{:error-key :check-failed
    :args ["invalid-value" "attribute-name"]
    :expected-internal "Check failed, invalid value invalid-value for attribute-name"
    :expected-client "Check failed, invalid value invalid-value for attribute-name"}
   {:error-key :format-mismatch
    :args ["attribute-name"]
    :expected-internal "Format mismatch - attribute-name"
    :expected-client "Format mismatch - attribute-name"}
   {:error-key :type-mismatch
    :args ["attribute-name" "type-name"]
    :expected-internal "Expected type for attribute-name is type-name"
    :expected-client "Expected type for attribute-name is type-name"}
   {:error-key :invalid-list-element
    :args ["attribute-name"]
    :expected-internal "Invalid list element for attribute-name"
    :expected-client "Invalid list element for attribute-name"}
   {:error-key :not-a-set
    :args ["attribute-name"]
    :expected-internal "Not a set - attribute-name"
    :expected-client "Not a set - attribute-name"}
   {:error-key :invalid-set-element
    :args ["attribute-name"]
    :expected-internal "Invalid set element for attribute-name"
    :expected-client "Invalid set element for attribute-name"}
   {:error-key :no-default-value
    :args ["attribute-name"]
    :expected-internal "No default value defined for attribute-name"
    :expected-client "No default value defined for attribute-name"}
   {:error-key :attribute-type-mismatch
    :args ["attribute-name" "record-name"]
    :expected-internal "Attribute attribute-name is not of type record-name"
    :expected-client "Attribute attribute-name is not of type record-name"}
   {:error-key :no-record-set
    :args ["attribute-name"]
    :expected-internal "No record set for attribute attribute-name"
    :expected-client "No record set for attribute attribute-name"}
   {:error-key :schema-not-found
    :args ["record-name"]
    :expected-internal "Schema not found for record-name"
    :expected-client "Schema not found for record-name"}
   {:error-key :attribute-not-in-schema
    :args ["attribute-name"]
    :expected-internal "Attribute not in schema - attribute-name"
    :expected-client "Attribute not in schema - attribute-name"}
   {:error-key :invalid-operator
    :args ["condition-expression"]
    :expected-internal "Invalid condition in event pattern - condition-expression"
    :expected-client "Invalid condition in event pattern - condition-expression"}])

(defn evaluate-request-and-verify-client-error-response
  [input expected-message]
  (api/component :Api.Test)
  (api/entity
    :Api.Test/F
    {:Id :Identity
     :Name {:type :String :id true}
     :Y :Int})
  (api/event
    :Api.Test/MakeF
    {:Name :String :Y :Int})
  (api/dataflow
    :Api.Test/MakeF
    {:Api.Test/F
     {:Name :Api.Test/MakeF.Name
      :Y :Api.Test/MakeF.Y}})
  (tu/finalize-component :Api.Test)
  (let [result (first (e/eval-all-dataflows (test-util/maybe-as-map input)))]
    (is (s/valid? ::result-map result))
    (is (= (:message result) expected-message))))

(defn test-error-scenario-for-raised-error
  "Tests an error scenario with configurable client message settings.
  This function allows testing different client message configurations for raised errors.
  - error-key: The key identifying the error.
  - args: Arguments to be passed to the error function.
  - expected-internal: The expected internal error message.
  - expected-client: The expected client-facing error message.
  - client-error-fn (optional): A function to override the default client message.
  - disabled-errors (optional): A set of error keys for which the client message should be disabled."
  [error-key args expected-internal expected-client & [client-error-fn disabled-errors]]
  (with-redefs [errors/client-error-functions (if client-error-fn {error-key client-error-fn} errors/client-error-functions)
                errors/disabled-client-errors (or disabled-errors errors/disabled-client-errors)]
    (try
      (errors/raise-error error-key args)
      (catch Exception e
        (let [ex-data (ex-data e)]
          (is (= expected-internal (:message (get-in ex-data [:error]))))
          (is (= expected-client (errors/extract-client-message-from-ex e))))))))

(defn evaluate-raised-error-test-case
  "Creates a test case for a specific error scenario."
  [error-key args expected-internal expected-client & [client-error-fn disabled-errors]]
  (test-error-scenario-for-raised-error error-key args expected-internal expected-client client-error-fn disabled-errors))

(deftest verify-API-error-responses
  "End to end API tests to verify error responses"
  (testing "Check failed error response"
    (evaluate-request-and-verify-client-error-response {:Api.Test/MakeF {:Name 1 :Y 100}}
                                                       "Check failed, invalid value 1 for :Name"))

  (testing "Invalid attributes error response"
    (evaluate-request-and-verify-client-error-response {:Api.Test/MakeF {:Name "Nice Name"
                                                                         :Y 100
                                                                         :ExtraUndefinedAttribute "Some value"}}
                                                       "[:Api.Test :MakeF] - invalid attribute(s) found - (:ExtraUndefinedAttribute)")))

(deftest verify-various-client-msg-configuration-scenarios-for-raised-errors
  (testing "With client message same as internal error message"
    (evaluate-raised-error-test-case :invalid-attribute
                                     ["TestRecord" "attr1"]
                                     "TestRecord - invalid attribute(s) found - attr1"
                                     "TestRecord - invalid attribute(s) found - attr1"))

  (testing "With overridden client message"
    (let [client-invalid-attribute-error (ns-resolve 'agentlang.util.errors 'client-invalid-attribute-error)]
      (evaluate-raised-error-test-case :invalid-attribute
                                       ["TestRecord" "attr1"]
                                       "TestRecord - invalid attribute(s) found - attr1"
                                       "Invalid attribute(s) found - attr1"
                                       client-invalid-attribute-error)))

  (testing "With disabled client message"
    (evaluate-raised-error-test-case :invalid-attribute
                                     ["TestRecord" "attr1"]
                                     "TestRecord - invalid attribute(s) found - attr1"
                                     "Internal error on server"
                                     nil
                                     #{:invalid-attribute})))

(deftest test-various-raised-errors
  (doseq [test-case raised-error-test-cases]
    (testing (str (name (:error-key test-case)) " error")
      (evaluate-raised-error-test-case (:error-key test-case)
                                       (:args test-case)
                                       (:expected-internal test-case)
                                       (:expected-client test-case)))))
