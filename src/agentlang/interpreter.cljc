(ns agentlang.interpreter
  (:require [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.store :as store]
            [agentlang.store.util :as su]
            [agentlang.global-state :as gs]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.core :as r]))

(defn- make-result [env result]
  {:env env :result result})

(def ^:private resolve-reference env/lookup)

(declare evaluate-dataflow-in-environment)

(defn- evaluate-attr-expr [env attrs exp]
  (let [final-exp (mapv #(if (keyword? %)
                           (or (% attrs) (resolve-reference env %))
                           %)
                        exp)]
    (li/evaluate (seq final-exp))))

(defn- assoc-fn-attributes [env attrs fns]
  (loop [fns fns, raw-obj attrs]
    (if-let [[a f] (first fns)]
      (recur (rest fns) (assoc raw-obj a (f env raw-obj)))
      raw-obj)))

(defn- resolve-attribute-values [env recname attrs]
  (let [has-exp? (first (filter (fn [[_ v]] (list? v)) attrs))
        attrs1 (into
                {}
                (mapv (fn [[k v]]
                        [k (if (keyword? v)
                             (resolve-reference env v)
                             v)])
                      attrs))
        new-attrs
        (if has-exp?
          (into
           {}
           (mapv (fn [[k v]]
                   [k (if (list? v) (evaluate-attr-expr env attrs1 v) v)])
                 attrs1))
          attrs1)
        [efns _] (cn/all-computed-attribute-fns recname nil)]
    (assoc-fn-attributes env new-attrs efns)))

(defn- normalize-query-comparison [k v]
  (let [c (count v)]
    (cond
      (= c 3) v
      (= c 2) [(first v) k v]
      :else (u/throw-ex (str "Invalid query syntax: " v)))))

(defn- as-column-name [k]
  (keyword (su/attribute-column-name (li/normalize-name k))))

(defn- parse-query-value [env k v]
  (let [k (as-column-name k)]
    (cond
      (keyword? v) [:= k (resolve-reference env v)]
      (vector? v) (normalize-query-comparison k (vec (concat [(first v)] (mapv parse-query-value (rest v)))))
      :else [:= k v])))

(defn- process-query-attribute-value [env [k v]]
  [k (parse-query-value env k v)])

(defn- preprocess-select-clause [env entity-name clause]
  (let [attr-names (cn/entity-attribute-names entity-name)]
    (w/postwalk #(if (keyword? %)
                   (if (su/sql-keyword? %)
                     %
                     (if (some #{%} attr-names)
                       (as-column-name %)
                       (resolve-reference env %)))
                   %)
                clause)))

(defn- handle-query-pattern [env recname attrs alias]
  (let [attrs0 (when (seq attrs)
                 (if-let [select-clause (:? attrs)]
                   {:? (preprocess-select-clause env recname select-clause)}
                   (into {} (mapv (partial process-query-attribute-value env) attrs))))
        resolver (rr/resolver-for-path recname)
        result (if resolver
                 (r/call-resolver-query resolver env [recname attrs0])
                 (store/do-query (env/get-store env) nil [recname attrs0]))
        env0 (if (seq result) (env/bind-instances env recname result) env)
        env1 (if alias (env/bind-instance-to-alias env0 alias result) env0)]
    (make-result env1 result)))

(defn- handle-entity-crud-pattern [env recname attrs alias]
  (let [inst (cn/make-instance recname (resolve-attribute-values env recname attrs))
        resolver (rr/resolver-for-path recname)
        final-inst (if resolver
                     (r/call-resolver-create resolver env inst)
                     (store/create-instance (env/get-store env) inst))
        env0 (env/bind-instance env recname final-inst)
        env1 (if alias (env/bind-variable env0 alias final-inst) env0)]
    (make-result env1 final-inst)))

(defn- handle-event-pattern [env recname attrs alias]
  (let [inst (cn/make-instance recname (resolve-attribute-values env recname attrs))
        resolver (rr/resolver-for-path recname)
        final-result (if resolver
                       (r/call-resolver-eval resolver env inst)
                       (evaluate-dataflow-in-environment env inst))
        env0 (:env final-result)
        env1 (if alias (env/bind-variable env0 alias (:result final-result)) env0)]
    (make-result env1 final-result)))

(defn- handle-record-pattern [env recname attrs alias]
  (let [inst (cn/make-instance recname (resolve-attribute-values env recname attrs))
        env0 (if alias (env/bind-variable env alias inst) env)]
    (make-result env0 inst)))

(defn- crud-handler [env pat]
  (let [recname (li/record-name pat)
        recattrs (li/record-attributes pat)
        alias (:as pat)
        result
        (if (cn/entity-schema (li/normalize-name recname))
          (if (li/query-instance-pattern? pat)
            (handle-query-pattern env recname recattrs alias)
            (handle-entity-crud-pattern env recname recattrs alias))
          (if (cn/event-schema recname)
            (handle-event-pattern env recname recattrs alias)
            (when (cn/record-schema recname)
              (handle-record-pattern env recname recattrs alias))))]
    (when-not result
      (u/throw-ex (str "Schema not found for " recname ". Cannot evaluate " pat)))
    result))

(defn- expr-handler [env pat]
  )

(defn- ref-handler [env pat]
  (make-result env (resolve-reference env pat)))

(defn- pattern-handler [pat]
  (cond
    (map? pat) crud-handler
    (vector? pat) expr-handler
    (keyword? pat) ref-handler
    :else nil))

(defn evaluate-dataflow
  ([store env event-instance is-internal]
   (let [env0 (or env (env/bind-instance (env/make store nil) event-instance))
         env (if is-internal
               (env/block-interceptors env0)
               (env/assoc-active-event env0 event-instance))]
     (store/call-in-transaction
      store
      (fn [txn]
        (let [txn-set? (when (and txn (not (gs/get-active-txn)))
                         (gs/set-active-txn! txn)
                         true)]
          (try
            (loop [df-patterns (cn/fetch-dataflow-patterns event-instance), env env, result nil]
              (if-let [pat (first df-patterns)]
                (if-let [handler (pattern-handler pat)]
                  (let [{env1 :env r :result} (handler env pat)]
                    (recur (rest df-patterns) env1 r))
                  (u/throw-ex (str "Cannot handle invalid pattern " pat)))
                (make-result env result)))
            (finally
              (when txn-set? (gs/set-active-txn! nil)))))))))
  ([store event-instance] (evaluate-dataflow store nil event-instance false)))

(defn evaluate-dataflow-in-environment [env event-instance]
  (evaluate-dataflow nil env event-instance false))
