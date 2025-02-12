(ns agentlang.interpreter
  (:require [clojure.set :as set]
            [clojure.walk :as w]
            #?(:clj [clojure.core.cache.wrapped :as cache])
            [agentlang.model]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.store :as store]
            [agentlang.store.util :as su]
            [agentlang.intercept :as intercept]
            [agentlang.global-state :as gs]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.core :as r]))

(defn- make-result [env result]
  {:env env :result result})

(declare evaluate-dataflow-in-environment evaluate-pattern
         evaluate-attr-expr)

(def ^:private resolve-reference env/lookup)

(defn- resolve-references-in-attributes-helper [env attrs]
  (into
   {}
   (mapv (fn [[k v]]
           [k (cond
                (keyword? v) (resolve-reference env v)
                (vector? v) (mapv #(resolve-reference env %) v)
                (list? v) (evaluate-attr-expr env nil k v)
                :else v)])
         attrs)))

(defn- resolve-references-in-attributes [env pat]
  (if-let [recname (li/record-name pat)]
    (let [alias (:as pat)
          attrs (li/record-attributes pat)
          new-attrs (resolve-references-in-attributes-helper env attrs)]
      (merge {recname (into {} new-attrs)}
             (when alias {:as alias})))
    pat))

(defn- resolve-references-in-map [env m]
  (let [res (mapv (fn [[k v]]
                    [k (cond
                         (= k :as) v
                         (map? v)
                         (if (li/instance-pattern? v)
                           (resolve-references-in-attributes env v)
                           (resolve-references-in-attributes-helper env v))
                         (keyword? v) (resolve-reference env v)
                         :else v)])
                  m)]
    (into {} res)))

(defn- resolve-all-references [env pat]
  (if (keyword? pat)
    (resolve-reference env pat)
    (w/postwalk
     #(if (map? %)
        (resolve-references-in-map env %)
        %)
     pat)))

(defn- as-query-pattern [pat]
  (let [alias (:as pat)
        n (li/record-name pat)
        attrs (li/record-attributes pat)]
    (merge
     {n (into {} (mapv (fn [[k v]]
                         [(if (li/query-pattern? k)
                            k
                            (li/name-as-query-pattern k))
                          v])
                       attrs))}
     (when alias {:as alias}))))

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

(defn- normalize-query-comparison [k v] `[~(first v) ~k ~@(rest v)])

(defn- as-column-name [k]
  (keyword (su/attribute-column-name (li/normalize-name k))))

(defn- resolve-query-value [env v]
  (cond
    (keyword? v) (resolve-reference env v)
    (vector? v) `[~(first v) ~@(mapv (partial resolve-query-value env) (rest v))]
    :else v))

(defn- parse-query-value [env k v]
  (let [k (as-column-name k)]
    (cond
      (keyword? v) [:= k (resolve-reference env v)]
      (vector? v) (normalize-query-comparison k (vec (concat [(first v)] (mapv (partial resolve-query-value env) (rest v)))))
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
    (let [can-update-all (intercept/call-interceptors-for-update env recname)
          updated-instances (mapv #(resolve-instance-values env recname (merge % update-attrs)) instances)
          rs
          (if resolver
            (every? identity (mapv #(r/call-resolver-update resolver env %) updated-instances))
            (store/update-instances (env/get-store env) recname updated-instances))]
      (when rs updated-instances))))

(defn- fetch-parent [relname child-recname relpat]
  (when-not (cn/contains-relationship? relname)
    (u/throw-ex (str "Not a contains-relationship " relname " in " relpat)))
  (let [parent (cn/containing-parent relname)
        child (cn/contained-child relname)]
    (when (not= child (li/normalize-name child-recname))
      (u/throw-ex (str "Error in query " relpat ", "
                       child-recname " is not a child of "
                       parent " via the contains-relationship "
                       relname)))
    parent))

(defn- force-fetch-only-id [recname attrs]
  (when (= 1 (count (keys attrs)))
    (let [idattr (cn/identity-attribute-name recname)]
      (idattr attrs))))

(def ^:private c-parent-attr (keyword (su/attribute-column-name li/parent-attr)))

(defn- maybe-merge-cont-rels-query-to-attributes [[recname attrs rels-query :as args]]
  (or (when rels-query
        (let [[k _ :as ks] (keys rels-query)]
          (when (and (= 1 (count ks)) (cn/contains-relationship? k))
            (when-let [parent (fetch-parent k recname rels-query)]
              (when-let [pat (get-in rels-query [k parent])]
                (when-let [pid (and (= 1 (count (keys pat)))
                                    (force-fetch-only-id parent pat))]
                  [(li/normalize-name recname) (assoc attrs li/parent-attr? [:= c-parent-attr (pr-str [parent pid])]) nil]))))))
      args))

(defn- handle-query-pattern [env recname [attrs sub-pats] alias]
  (let [can-read-all (intercept/call-interceptors-for-read env recname)
        select-clause (:? attrs)
        [update-attrs query-attrs] (when-not select-clause (lift-attributes-for-update attrs))
        attrs (if query-attrs query-attrs attrs)
        attrs0 (when (seq attrs)
                 (if select-clause
                   {:? (preprocess-select-clause env recname select-clause)}
                   (into {} (mapv (partial process-query-attribute-value env) attrs))))
        resolver (rr/resolver-for-path recname)
        cont-rels-query0 (when-let [rels (:cont-rels sub-pats)] (resolve-all-references env rels))
        [recname attrs0 cont-rels-query] (maybe-merge-cont-rels-query-to-attributes [recname attrs0 cont-rels-query0])
        bet-rels-query (when-let [rels (:bet-rels sub-pats)] (resolve-all-references env rels))
        rels-query {:cont-rels cont-rels-query
                    :bet-rels bet-rels-query}
        result0 (if resolver
                  (r/call-resolver-query resolver env [recname attrs0 rels-query can-read-all])
                  (store/do-query (env/get-store env) nil [recname attrs0 rels-query can-read-all]))
        env0 (if (seq result0) (env/bind-instances env recname result0) env)
        result (if update-attrs (handle-upsert env0 resolver recname update-attrs result0) result0)
        env1 (if (seq result) (env/bind-instances env0 recname result) env0)
        env2 (if alias (env/bind-instance-to-alias env1 alias result) env1)]
    (make-result env2 result)))

(defn- handle-entity-create-pattern [env recname attrs alias]
  (let [_ (intercept/call-interceptors-for-create env recname)
        inst (cn/make-instance recname (resolve-attribute-values env recname attrs))
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

(defn- resolve-pattern [env pat]
  (if (keyword? pat)
    (resolve-reference env pat)
    (first (:result (evaluate-pattern env (as-query-pattern pat))))))

(defn- maybe-set-parent [env relpat recname recattrs]
  (let [k (first (keys relpat))]
    #_(when-not (li/query-pattern? k)
      (u/throw-ex (str "Relationship name " k " should be a query in " relpat)))
    (let [relname (li/normalize-name k)
          parent (fetch-parent relname recname relpat)]
      (if-let [result (resolve-pattern env (k relpat))]
        (do (when-not (cn/instance-of? parent result)
              (u/throw-ex (str "Result of " relpat " is not of type " parent)))
            (let [pid (li/path-attr result)
                  ppath (u/parse-string (li/path-attr result))]
              (assoc recattrs li/parent-attr
                     pid li/path-attr
                     (pr-str (concat ppath [relname recname li/id-attr])))))
        (u/throw-ex (str "Query " relpat " failed to lookup " parent " for " recname))))))

(defn- create-between-relationships [env bet-rels recname result]
  (when-let [inst (when-let [r (:result result)]
                    (let [inst (if (map? r) r (first r))]
                      (when-not (cn/instance-of? recname inst)
                        (u/throw-ex (str "Cannot create relationship " recname " for " inst)))
                      inst))]
    (doseq [[relname relspec] bet-rels]
      (let [other-inst (resolve-pattern env relspec)
            _ (when-not (cn/an-instance? other-inst)
                (u/throw-ex (str "Cannot create between-relationship " relname ". "
                                 "Query failed - " relspec)))
            a1 (first (cn/find-between-keys relname recname))
            other-recname (cn/instance-type-kw other-inst)
            a2 (first (cn/find-between-keys relname other-recname))]
        (when-not (or a1 a2)
          (u/throw-ex (str "No relationship " relname " between " recname " and " other-recname)))
        (:result (evaluate-pattern env {relname {a1 (li/path-attr inst) a2 (li/path-attr other-inst)}}))))))

(defn- crud-handler [env pat sub-pats]
  (let [recname (li/record-name pat)
        recattrs (li/record-attributes pat)
        alias (:as pat)]
    (cond
      (cn/entity-schema (li/normalize-name recname))
      (let [q? (li/query-instance-pattern? pat)
            f (if q? handle-query-pattern handle-entity-create-pattern)
            [cont-rels bet-rels]
            (and (seq sub-pats) [(:cont-rels sub-pats) (:bet-rels sub-pats)])
            attrs
            (if q?
              [recattrs sub-pats]
              (if (seq cont-rels)
                (maybe-set-parent env cont-rels recname recattrs)
                recattrs))
            result (f env recname attrs alias)]
        (when (and (not q?) (seq bet-rels))
          (create-between-relationships env bet-rels recname result))
        result)

      (cn/event-schema recname)
      (handle-event-pattern env recname recattrs alias)

      (cn/record-schema recname)
      (handle-record-pattern env recname recattrs alias)

      :else (u/throw-ex (str "Schema not found for " recname ". Cannot evaluate " pat)))))

(defn- call-resolver-delete [entity-name args]
  (when-let [resolver (rr/resolver-for-path entity-name)]
    (r/call-resolver-delete [entity-name args])))

(defn- extract-entity-name [pattern]
  (let [pattern (dissoc pattern :as :from)
        ks (keys pattern)]
    (first (filter #(cn/entity? (li/normalize-name %)) ks))))

(defn- delete-instances [env pattern & params]
  (let [store (env/get-store env)
        params (first params)
        purge? (= :purge params)
        delall? (= :* params)]
    (when (or purge? delall?)
      (when-not (and (keyword? pattern)
                     (cn/entity? pattern))
        (u/throw-ex (str "Second element must be a valid entity name - [:delete " pattern " " params "]"))))
    (let [can-delete-all (intercept/call-interceptors-for-delete env (if (keyword? pattern) pattern (extract-entity-name pattern)))]
      (if (or purge? delall?)
        (or (call-resolver-delete pattern params) (store/delete-all store pattern purge?))
        (let [r (evaluate-pattern env pattern)
              env (:env r), insts (:result r)]
          (when-let [entity-name (and (seq insts) (cn/instance-type-kw (first insts)))]
            (let [insts (if can-delete-all insts (intercept/call-interceptors-for-delete env insts))]
              (or (call-resolver-delete entity-name insts)
                  (doseq [inst insts]
                    (store/delete-by-id store entity-name li/path-attr (li/path-attr inst)))
                  insts))))))))

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
           :delete delete-instances
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

(defn- filter-relationships [predic? pats]
  (into {} (filter (fn [[k _]] (predic? (li/normalize-name k))) pats)))

(def ^:private filter-between-relationships (partial filter-relationships cn/between-relationship?))
(def ^:private filter-contains-relationships (partial filter-relationships cn/contains-relationship?))

(defn- maybe-lift-relationship-patterns [pat]
  (let [bet-rels (filter-between-relationships pat)
        cont-rels (filter-contains-relationships pat)]
    [(apply dissoc pat (keys (merge bet-rels cont-rels)))
     {:cont-rels (when (seq cont-rels) cont-rels)
      :bet-rels (when (seq bet-rels) bet-rels)}]))

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
      (if (cn/between-relationship? (li/record-name pat))
        [pat]
        (maybe-lift-relationship-patterns pat)))
    [pat]))

(defn evaluate-pattern [env pat]
  (let [[pat sub-pats] (maybe-preprocecss-pattern env pat)]
    (if-let [handler (pattern-handler pat)]
      (handler env pat sub-pats)
      (u/throw-ex (str "Cannot handle invalid pattern " pat)))))

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
              (if-let [pat (first df-patterns)]
                (let [pat-count (inc pat-count)
                      env (env/bind-eval-state env pat pat-count)
                      {env1 :env r :result} (evaluate-pattern env pat)]
                  (recur (rest df-patterns) pat-count env1 r))
                (make-result env result)))
            (finally
              (when txn-set? (gs/set-active-txn! nil)))))))))
  ([store event-instance] (evaluate-dataflow store nil event-instance false))
  ([event-instance] (evaluate-dataflow (store/get-default-store) nil event-instance false)))

(defn evaluate-dataflow-in-environment [env event-instance]
  (evaluate-dataflow nil env event-instance false))

(defn evaluate-dataflow-internal [event-instance]
  (evaluate-dataflow (store/get-default-store) nil event-instance true))

(gs/set-evaluate-dataflow-fn! evaluate-dataflow)
(gs/set-evaluate-dataflow-internal-fn! evaluate-dataflow-internal)
