(ns agentlang.interpreter
  (:require [clojure.set :as set]
            [clojure.walk :as w]
            #?(:clj [clojure.core.cache.wrapped :as cache])
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

(defn- evaluate-attr-expr [env attrs attr-name exp]
  (let [final-exp (mapv #(if (keyword? %)
                           (if (= % attr-name)
                             (u/throw-ex (str "Unqualified self-reference " % " not allowed in " exp))
                             (or (% attrs) (resolve-reference env %)))
                           %)
                        exp)]
    (li/evaluate (seq final-exp))))

(defn- assoc-fn-attributes [env attrs fn-exprs]
  (loop [fns fn-exprs, raw-obj attrs]
    (if-let [[a exp] (first fns)]
      (recur (rest fns) (assoc raw-obj a (evaluate-attr-expr env attrs a exp)))
      raw-obj)))

(defn- find-deps [k all-deps]
  (second (first (filter #(= k (first %)) all-deps))))

(defn- build-ordered-deps
  ([k deps all-deps result]
   (if (nil? deps)
     (if (some #{k} result) result (conj result k))
     (let [r (vec (apply concat (mapv (fn [d]
                                        (if (some #{k} result)
                                          result
                                          (build-ordered-deps d (find-deps d all-deps) all-deps result)))
                                      deps)))]
       (if (some #{k} r) result (vec (concat r [k]))))))
  ([attrs-deps]
   (loop [ads attrs-deps, result []]
     (if-let [[k deps] (first ads)]
       (if (some #{k} result)
         (recur (rest ads) result)
         (recur (rest ads) (build-ordered-deps k deps attrs-deps result)))
       result))))

(def ^:private eval-cache
  #?(:clj (cache/lru-cache-factory {} :threshold 1000)
     :cljs (atom {})))

(defn- eval-cache-lookup [k]
  #?(:clj (cache/lookup eval-cache k)
     :cljs (get @eval-cache k)))

(defn- eval-cache-update [k v]
  #?(:clj (cache/through-cache eval-cache k (constantly v))
     :cljs (swap! eval-cache assoc k v))
  v)

(defn- order-by-dependencies [env attrs]
  (let [k [(cn/instance-type-kw (env/active-event env)) (env/eval-state-counter env)]]
    (or (eval-cache-lookup k)
        (eval-cache-update
         k
         (let [exp-attrs (into {} (filter (fn [[_ v]] (list? v)) attrs))
               ks (set (keys exp-attrs))
               attrs-deps (mapv (fn [[k v]]
                                  (if-let [deps (seq (set/intersection (set v) (set/difference ks #{k})))]
                                    [k deps]
                                    [k nil]))
                                exp-attrs)
               ordered-deps (build-ordered-deps attrs-deps)]
           (mapv (fn [k] [k (get exp-attrs k)]) ordered-deps))))))

(defn- resolve-attribute-values
  ([env recname attrs compute-compound-attributes?]
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
           (loop [exp-attrs (order-by-dependencies env attrs1), attrs attrs1]
             (if-let [[k v] (first exp-attrs)]
               (let [newv (evaluate-attr-expr env attrs k v)]
                 (recur (rest exp-attrs) (assoc attrs k newv)))
               attrs))
           attrs1)]
     (if compute-compound-attributes?
       (if-let [[efns _] (cn/all-computed-attribute-fns recname nil)]
         (assoc-fn-attributes env new-attrs efns)
         new-attrs)
       new-attrs)))
  ([env recname attrs] (resolve-attribute-values env recname attrs true)))

(defn- resolve-instance-values [env recname inst]
  (let [attrs (resolve-attribute-values env recname (cn/instance-attributes inst))]
    (cn/make-instance recname attrs false)))

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

(defn- query-attribute? [[k _]] (li/query-pattern? k))

(defn- lift-attributes-for-update [attrs]
  (if-let [upattrs (seq (filter (complement query-attribute?) attrs))]
    [(into {} upattrs) (into {} (filter query-attribute? attrs))]
    [nil attrs]))

(defn- handle-upsert [env resolver recname update-attrs instances]
  (when (seq instances)
    (let [updated-instances (mapv #(resolve-instance-values env recname (merge % update-attrs)) instances)
          rs
          (if resolver
            (every? identity (mapv #(r/call-resolver-update resolver env %) updated-instances))
            (store/update-instances (env/get-store env) recname updated-instances))]
      (when rs updated-instances))))

(defn- handle-query-pattern [env recname attrs alias]
  (let [select-clause (:? attrs)
        [update-attrs query-attrs] (when-not select-clause (lift-attributes-for-update attrs))
        attrs (if query-attrs query-attrs attrs)
        attrs0 (when (seq attrs)
                 (if select-clause
                   {:? (preprocess-select-clause env recname select-clause)}
                   (into {} (mapv (partial process-query-attribute-value env) attrs))))
        resolver (rr/resolver-for-path recname)
        result0 (if resolver
                  (r/call-resolver-query resolver env [recname attrs0])
                  (store/do-query (env/get-store env) nil [recname attrs0]))
        env0 (if (seq result0) (env/bind-instances env recname result0) env)
        result (if update-attrs (handle-upsert env0 resolver recname update-attrs result0) result0)
        env1 (if (seq result) (env/bind-instances env0 recname result) env0)
        env2 (if alias (env/bind-instance-to-alias env1 alias result) env1)]
    (make-result env2 result)))

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

(defn- crud-handler [env pat sub-pats]
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

(defn- delete-instance [env entity-name params]
  (if-let [resolver (rr/resolver-for-path entity-name)]
    (r/call-resolver-delete [entity-name params])
    (let [store (env/get-store env)]
      (cond
        (= :* params)
        (store/delete-all store entity-name false)

        (= :purge params)
        (store/delete-all store entity-name true)

        :else
        (let [attrs (resolve-attribute-values env entity-name params false)
              n (first (keys attrs))]
          (store/delete-by-id store entity-name n (get attrs n)))))))

(defn- parse-expr-pattern [pat]
  (let [[h t] (split-with #(not= % :as) pat)]
    (if (seq t)
      (let [t (rest t)]
        (when-not (seq t)
          (u/throw-ex (str "Alias not specified after `:as` in " pat)))
        (when (> (count t) 1)
          (u/throw-ex (str "Alias must appear last in " pat)))
        [(vec h) (first t)])
      [(vec h) nil])))

(defn- expr-handler [env pat _]
  (let [[pat alias] (parse-expr-pattern pat)
        result
        (apply
         (case (first pat)
           :delete delete-instance
           (u/throw-ex (str "Invalid expression - " pat)))
         env (rest pat))
        env (if alias (env/bind-variable env alias result) env)]
    (make-result env result)))

(defn- ref-handler [env pat _]
  (make-result env (resolve-reference env pat)))

(defn- pattern-handler [pat]
  (cond
    (map? pat) crud-handler
    (vector? pat) expr-handler
    (keyword? pat) ref-handler
    :else nil))

(defn- maybe-lift-relationship-patterns [pat]
  (if-let [rel-names (seq (map first (filter (fn [[k _]]
                                               (cn/relationship? (li/normalize-name k)))
                                             pat)))]
    [(apply dissoc pat rel-names)
     {:rels (into {} (mapv (fn [k] [k (get pat k)]) rel-names))}]
    [pat]))

(defn- maybe-preprocecss-pattern [env pat]
  (if (map? pat)
    (if-let [from (:from pat)]
      (let [alias (:as pat)
            pat (dissoc pat :from :alias)
            data0 (if (keyword? from) (resolve-reference env from) from)
            data1 (if (map? data0) data0 (u/throw-ex (str "Failed to resolve " from " in " pat)))
            data (if (cn/an-instance? data1) (cn/instance-attributes data1) data1)
            k (first (keys pat))
            attrs (merge (get pat k) data)]
        [(merge {k attrs} (when alias {:as alias}))])
      (maybe-lift-relationship-patterns pat))
    [pat]))

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
            (loop [df-patterns (cn/fetch-dataflow-patterns event-instance),
                   pat-count 0, env env, result nil]
              (if-let [pat0 (first df-patterns)]
                (let [pat-count (inc pat-count)
                      env (env/bind-eval-state env pat0 pat-count)
                      [pat sub-pats] (maybe-preprocecss-pattern env pat0)]
                  (if-let [handler (pattern-handler pat)]
                    (let [{env1 :env r :result} (handler env pat sub-pats)]
                      (recur (rest df-patterns) pat-count env1 r))
                    (u/throw-ex (str "Cannot handle invalid pattern " pat))))
                (make-result env result)))
            (finally
              (when txn-set? (gs/set-active-txn! nil)))))))))
  ([store event-instance] (evaluate-dataflow store nil event-instance false)))

(defn evaluate-dataflow-in-environment [env event-instance]
  (evaluate-dataflow nil env event-instance false))
