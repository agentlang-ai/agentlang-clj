(ns agentlang.interpreter
  (:require [clojure.set :as set]
            [clojure.walk :as w]
            [clojure.core.async :as async]
            #?(:clj [clojure.core.cache.wrapped :as cache])
            [agentlang.model]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.env :as env]
            [agentlang.store :as store]
            [agentlang.store.util :as su]
            [agentlang.intercept.rbac :as rbac]
            [agentlang.global-state :as gs]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.core :as r]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn- make-result [env result]
  {:env env :result result})

(declare evaluate-dataflow-in-environment evaluate-pattern
         evaluate-attr-expr)

(defn- follow-reference [env k]
  (let [v (env/lookup env k)]
    (if (li/quoted? v)
      (li/quoted-value v)
      v)))

(defn- evaluate-attribute-value [env k v]
  (cond
    (keyword? v) (follow-reference env v)
    (li/quoted? v) (:result (evaluate-pattern env v))
    (vector? v) `[~(let [k (first v)]
                     (if (su/sql-keyword? k)
                       k
                       (follow-reference env k)))
                  ~@(mapv #(evaluate-attribute-value env k %) (rest v))]
    (list? v) (evaluate-attr-expr env nil k v)
    :else v))

(defn- follow-references-in-attributes-helper [env attrs]
  (into
   {}
   (mapv (fn [[k v]] [k (evaluate-attribute-value env k v)]) attrs)))

(defn- follow-references-in-attributes [env pat]
  (if-let [recname (li/record-name pat)]
    (let [alias (:as pat)
          attrs (li/record-attributes pat)
          new-attrs (follow-references-in-attributes-helper env attrs)]
      (merge {recname (into {} new-attrs)}
             (when alias {:as alias})))
    pat))

(declare realize-all-references)

(defn- follow-references-in-map [env m]
  (let [res (mapv (fn [[k v]]
                    [k (cond
                         (= k :as) v
                         (map? v)
                         (if (li/instance-pattern? v)
                           (follow-references-in-attributes env v)
                           (follow-references-in-attributes-helper env v))
                         (keyword? v) (follow-reference env v)
                         :else v)])
                  m)]
    (into {} res)))

(defn- realize-all-references [env pat]
   (if (keyword? pat)
     (follow-reference env pat)
     (w/postwalk
      #(if (map? %)
         (follow-references-in-map env %)
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
                             (or (% attrs) (follow-reference env %)))
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

(defn- realize-attribute-values
  ([env recname attrs compute-compound-attributes?]
   (let [has-exp? (first (filter (fn [[_ v]] (list? v)) attrs))
         attrs1 (into
                 {}
                 (mapv (fn [[k v]]
                         [k (if (keyword? v)
                              (follow-reference env v)
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
     (into
      {}
      (mapv (fn [[k v]]
              [k (evaluate-attribute-value env k v)])
            (if compute-compound-attributes?
              (if-let [[efns _] (cn/all-computed-attribute-fns recname nil)]
                (assoc-fn-attributes env new-attrs efns)
                new-attrs)
              new-attrs)))))
  ([env recname attrs] (realize-attribute-values env recname attrs true)))

(defn- realize-instance-values [env recname inst]
  (let [attrs (realize-attribute-values env recname (cn/instance-attributes inst))]
    (cn/make-instance recname attrs false)))

(defn- normalize-query-comparison [k v] `[~(first v) ~k ~@(rest v)])

(defn- as-column-name [k]
  (keyword (su/attribute-column-name (li/normalize-name k))))

(defn- realize-query-value [env v]
  (cond
    (keyword? v) (follow-reference env v)
    (vector? v) `[~(first v) ~@(mapv (partial realize-query-value env) (rest v))]
    :else v))

(defn- parse-query-value [env k v]
  (let [k (as-column-name k)]
    (cond
      (keyword? v) [:= k (follow-reference env v)]
      (vector? v) (normalize-query-comparison k (vec (concat [(first v)] (mapv (partial realize-query-value env) (rest v)))))
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
                       (follow-reference env %)))
                   %)
                clause)))

(defn- query-attribute? [[k _]] (li/query-pattern? k))

(defn- lift-attributes-for-update [attrs]
  (if-let [upattrs (seq (filter (complement query-attribute?) attrs))]
    [(into {} upattrs) (into {} (filter query-attribute? attrs))]
    [nil attrs]))

(defn- call-resolver [resolver-fn resolver store-f env arg]
  (if (rr/composed? resolver)
    (when-let [rs (reduce (fn [arg r] (call-resolver resolver-fn r store-f env arg)) arg resolver)]
      (store-f rs))
    (or (:result (resolver-fn resolver env arg))
        arg)))

(defn- handle-upsert [env resolver recname update-attrs instances]
  (when (seq instances)
    (let [updated-instances (mapv #(realize-instance-values env recname (merge % update-attrs)) instances)
          store-f (fn [updated-instances] (store/update-instances (env/get-store env) recname updated-instances))
          rs
          (if resolver
            (call-resolver r/call-resolver-update resolver store-f env updated-instances)
            (store-f updated-instances))]
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

(defn- all-entities [recname sub-pats]
  (let [crels (:cont-rels sub-pats)
        brels (:bet-rels sub-pats)]
    (set
     (if (or (seq crels) (seq brels))
       (let [names (atom [recname])]
         (w/postwalk
          #(do (when (map? %)
                 (when-let [n (li/record-name %)]
                   (let [n (li/normalize-name n)]
                     (when (cn/entity? n)
                       (swap! names conj n)))))
               %)
          [crels brels])
         @names)
       [recname]))))

(defn- handle-query-pattern [env recname [attrs sub-pats] alias]
  (let [select-clause (:? attrs)
        [update-attrs query-attrs] (when-not select-clause (lift-attributes-for-update attrs))
        _ (when (and (li/query-pattern? recname) (seq query-attrs))
            (u/throw-ex (str "Cannot have attribute specific queries for " recname)))
        recname (li/normalize-name recname)
        attrs (if query-attrs query-attrs attrs)
        attrs0 (when (seq attrs)
                 (if select-clause
                   {:? (preprocess-select-clause env recname select-clause)}
                   (into {} (mapv (partial process-query-attribute-value env) attrs))))
        resolver (rr/resolver-for-path recname)
        cont-rels-query0 (when-let [rels (:cont-rels sub-pats)] (realize-all-references env rels))
        [recname attrs0 cont-rels-query] (maybe-merge-cont-rels-query-to-attributes [recname attrs0 cont-rels-query0])
        qfordel? (:*query-for-delete* env)
        all-ents (all-entities recname sub-pats)
        can-read-all (every? #(rbac/can-read? %) all-ents)
        can-update-all (when update-attrs (rbac/can-update? recname))
        can-delete-all (:*can-delete-all* env)
        qparams {:entity-name recname
                 :query-attributes attrs0
                 :sub-query sub-pats
                 :rbac {:read-on-entities (set/difference all-ents #{recname})
                        :can-read-all? can-read-all
                        :can-update-all? can-update-all
                        :can-delete-all? can-delete-all
                        :follow-up-operation (or (when qfordel? :delete)
                                                 (when update-attrs :update))}}
        result0 (if (and resolver (not (rr/composed? resolver)))
                  (r/call-resolver-query resolver env qparams)
                  (store/do-query (env/get-store env) nil qparams))
        env0 (if (seq result0) (env/bind-instances env recname result0) env)
        result (if update-attrs (handle-upsert env0 resolver recname update-attrs result0) result0)
        env1 (if (seq result) (env/bind-instances env0 recname result) env0)
        env2 (if alias (env/bind-instance-to-alias env1 alias result) env1)]
    (make-result env2 result)))

(defn- handle-entity-create-pattern [env recname attrs alias]
  (if-not (rbac/can-create? recname)
    (make-result env {:status :forbidden})
    (let [inst (cn/make-instance recname (realize-attribute-values env recname attrs))
          resolver (rr/resolver-for-path recname)
          store (env/get-store env)
          store-f #(and (store/create-instance store %) %)
          final-inst (if resolver
                       (call-resolver r/call-resolver-create resolver store-f env inst)
                       (store-f inst))
          _ (when (and (gs/rbac-enabled?) (cn/instance-of? recname final-inst))
              (store/assign-owner store recname final-inst))
          env0 (env/bind-instance env recname final-inst)
          env1 (if alias (env/bind-variable env0 alias final-inst) env0)]
      (make-result env1 final-inst))))

(defn- handle-event-pattern [env recname attrs alias]
  (let [inst (cn/make-instance recname (realize-attribute-values env recname attrs))
        env (env/bind-instance env recname inst)
        resolver (rr/resolver-for-path recname)
        final-result (if resolver
                       (r/call-resolver-eval resolver env inst)
                       (evaluate-dataflow-in-environment env inst))
        env0 (:env final-result)
        r (:result final-result)
        env1 (if alias (env/bind-variable env0 alias r) env0)]
    (make-result env1 r)))

(defn- handle-record-pattern [env recname attrs alias]
  (let [inst (cn/make-instance recname (realize-attribute-values env recname attrs))
        env0 (if alias (env/bind-variable env alias inst) env)]
    (when (cn/instance-of? :Agentlang.Kernel.Rbac/InstancePrivilegeAssignment inst)
      (rbac/handle-instance-privilege-assignment env inst))
    (make-result env0 inst)))

(defn- realize-pattern [env pat]
  (if (keyword? pat)
    (follow-reference env pat)
    (first (:result (evaluate-pattern env (as-query-pattern pat))))))

(defn- maybe-set-parent [env relpat recname recattrs]
  (let [k (first (keys relpat))]
    #_(when-not (li/query-pattern? k)
      (u/throw-ex (str "Relationship name " k " should be a query in " relpat)))
    (let [relname (li/normalize-name k)
          parent (fetch-parent relname recname relpat)]
      (if-let [result (realize-pattern env (k relpat))]
        (do (when-not (cn/instance-of? parent result)
              (u/throw-ex (str "Result of " relpat " is not of type " parent)))
            (let [ppath (li/path-attr result)]
              (assoc recattrs
                     li/parent-attr ppath
                     li/path-attr (str ppath "," (li/vec-to-path [relname recname li/id-attr])))))
        (u/throw-ex (str "Failed to lookup " parent " for " recname))))))

(defn- create-between-relationships [env bet-rels recname result]
  (when-let [inst (when-let [r (:result result)]
                    (let [inst (if (map? r) r (first r))]
                      (when-not (cn/instance-of? recname inst)
                        (u/throw-ex (str "Cannot create relationship " recname " for " inst)))
                      inst))]
    (doseq [[relname relspec] bet-rels]
      (let [other-inst (realize-pattern env relspec)
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

(defn- call-resolver-delete [env store-f entity-name args]
  (when-let [resolver (rr/resolver-for-path entity-name)]
    (call-resolver r/call-resolver-delete resolver store-f env [entity-name args])))

(defn- extract-entity-name [pattern]
  (let [pattern (li/normalize-instance-pattern pattern)
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
    (let [ent-name (if (keyword? pattern) pattern (extract-entity-name pattern))
          can-delete-all (rbac/can-delete? ent-name)
          store-f (fn [_] (store/delete-all store pattern purge?))]
      (if (or purge? delall?)
        (if can-delete-all
          (or (call-resolver-delete env store-f pattern params) (store-f nil))
          (u/throw-ex (str "No permission to delete all instances of " ent-name)))
        (let [enriched-env (if can-delete-all
                             (assoc env :*can-delete-all* true)
                             (assoc env :*query-for-delete* true))
              r (evaluate-pattern enriched-env pattern)
              env (:env r), insts (:result r)]
          (when-let [entity-name (and (seq insts) (cn/instance-type-kw (first insts)))]
            (let [store-f (fn [_]
                            (doseq [inst insts]
                              (store/delete-by-id store entity-name li/path-attr (li/path-attr inst))))]
              (or (call-resolver-delete env store-f entity-name insts)
                  (store-f nil)
                  insts))))))))

(defn- handle-quote [env pat]
  (w/prewalk
   #(if (li/unquoted? %)
      (:result (evaluate-pattern env (li/unquoted-value %)))
      %)
   pat))

(defn- call-function [env pat]
  (let [fname (first pat)
        args (mapv #(:result (evaluate-pattern env %)) (rest pat))]
    (li/evaluate `(~fname ~@args))))

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
        tag (first pat)
        result
        (apply
         (case tag
           :> call-function
           :delete delete-instances
           :q# handle-quote
           (u/throw-ex (str "Invalid expression - " pat)))
         env (rest pat))
        env (if alias (env/bind-variable env alias result) env)]
    (make-result env result)))

(defn- ref-handler [env pat _]
  (make-result env (follow-reference env pat)))

(defn- pattern-handler [pat]
  (cond
    (map? pat) crud-handler
    (vector? pat) expr-handler
    (keyword? pat) ref-handler
    :else pat))

(defn- filter-relationships [predic? pats]
  (into {} (filter (fn [[k _]] (predic? (li/normalize-name k))) pats)))

(def ^:private filter-between-relationships (partial filter-relationships cn/between-relationship?))
(def ^:private filter-contains-relationships (partial filter-relationships cn/contains-relationship?))

(defn- filter-query-attributes [attrs]
  (when-let [xs (seq (filter (fn [[k _]] (li/query-pattern? k)) attrs))]
    (into {} xs)))

(defn- walk-query-pattern [env pat qmode]
  (let [ks (keys pat)
        names (mapv li/normalize-name ks)
        entity-name (first (filter cn/entity? names))
        cont-rels (filter cn/contains-relationship? names)
        bet-rels (filter cn/between-relationship? names)
        rf (partial realize-all-references env)
        f (fn [r]
            (let [subpat (or (get pat r)
                             (get pat (li/name-as-query-pattern r)))
                  [alias subpat] (if (map? subpat) [(:as subpat) (dissoc subpat :as)] [nil subpat])]
              (when (and alias (cn/relationship? r))
                (li/register-alias! r (li/record-name subpat) alias))
              [r (walk-query-pattern env (rf subpat) true) alias]))]
    {:select [entity-name
              (let [attrs ((if qmode identity filter-query-attributes) (get pat entity-name))]
                (if-let [select-clause (:? attrs)]
                  {:? (preprocess-select-clause env entity-name select-clause)}
                  (into {} (mapv (partial process-query-attribute-value env) attrs))))]
     :contains-join (mapv f cont-rels)
     :between-join (mapv f bet-rels)}))

(defn- maybe-lift-relationship-patterns [env pat]
  (let [alias (:as pat)
        into (:into pat)
        pat (li/normalize-instance-pattern pat)
        _ (li/reset-alias-db!)
        q (walk-query-pattern env pat false)
        bet-rels (filter-between-relationships pat)
        cont-rels (filter-contains-relationships pat)]
    [(let [p (apply dissoc pat (keys (merge bet-rels cont-rels)))]
       (if alias
         (assoc p :as alias)
         p))
     {:cont-rels (when (seq cont-rels) cont-rels)
      :bet-rels (when (seq bet-rels) bet-rels)
      :abstract-query q
      :into into}]))

(defn- maybe-preprocecss-pattern [env pat]
  (if (map? pat)
    (if-let [from (:from pat)]
      (let [alias (:as pat)
            pat (dissoc pat :from :alias)
            data0 (if (keyword? from) (follow-reference env from) from)
            data1 (if (map? data0) data0 (u/throw-ex (str "Failed to resolve " from " in " pat)))
            data (if (cn/an-instance? data1) (cn/instance-attributes data1) data1)
            k (first (keys pat))
            attrs (merge (get pat k) data)
            pat (merge {k attrs} (when alias {:as alias}))]
        (maybe-lift-relationship-patterns env pat))
      (if (cn/between-relationship? (li/record-name pat))
        [pat]
        (maybe-lift-relationship-patterns env pat)))
    [pat]))

(defn- maybe-normalize-pattern [pat]
  (if (li/query-pattern? pat)
    {pat {}}
    pat))

(defn- literal? [x]
  (or (number? x) (string? x) (boolean? x)))

(defn evaluate-pattern
  ([env pat]
   (if (literal? pat)
     (make-result env pat)
     (let [env (or env (env/make (store/get-default-store) nil))
           pat (maybe-normalize-pattern pat)
           [condition-handlers pat]
           (if (map? pat)
             [(li/except-tag pat) (dissoc pat li/except-tag)]
             [nil pat])
           [pat sub-pats] (maybe-preprocecss-pattern env pat)]
       (if-let [handler (pattern-handler pat)]
         (try
           (let [r (handler env pat sub-pats)
                 res (:result r)
                 no-data (or (nil? res) (and (seqable? res) (not (seq res))))]
             (if-let [on-not-found (and no-data (:not-found condition-handlers))]
               (evaluate-pattern env on-not-found)
               r))
           (catch #?(:clj Exception :cljs js/Error) ex
             (if-let [on-error (:error condition-handlers)]
               (evaluate-pattern env on-error)
               (throw ex))))
         (u/throw-ex (str "Cannot handle invalid pattern " pat))))))
  ([pat] (evaluate-pattern nil pat)))

(defn evaluate-dataflow
  ([store env event-instance-or-patterns]
   (let [patterns (when (vector? event-instance-or-patterns)
                    event-instance-or-patterns)
         event-instance (if-not patterns
                          (if (cn/an-instance? event-instance-or-patterns)
                            event-instance-or-patterns
                            (cn/make-instance event-instance-or-patterns))
                          {})]
     (gs/call-with-event-context
      (:EventContext event-instance)
      (fn []
        (let [env0 (or env (env/bind-instance (env/make store nil) event-instance))
              env (env/assoc-active-event env0 event-instance)
              store (or (env/get-store env) store)]
          (store/call-in-transaction
           store
           (fn [txn]
             (if (and (seq event-instance) (cn/instance-of? :Agentlang.Kernel.Rbac/DeleteInstancePrivilegeAssignment event-instance))
               (rbac/delete-instance-privilege-assignment env event-instance)
               (let [txn-set? (when (and txn (not (gs/get-active-txn)))
                                (gs/set-active-txn! txn)
                                true)]
                 (try
                   (loop [df-patterns (or patterns (cn/fetch-dataflow-patterns event-instance))
                          pat-count 0, env env, result nil]
                     (if-let [pat (first df-patterns)]
                       (let [pat-count (inc pat-count)
                             env (env/bind-eval-state env pat pat-count)
                             {env1 :env r :result} (evaluate-pattern env pat)]
                         (recur (rest df-patterns) pat-count env1 r))
                       (make-result env result)))
                   (finally
                     (when txn-set? (gs/set-active-txn! nil)))))))))))))
  ([store event-instance] (evaluate-dataflow store nil event-instance))
  ([event-instance] (evaluate-dataflow (store/get-default-store) nil event-instance)))

(defn evaluate-dataflow-in-environment [env event-instance]
  (evaluate-dataflow nil env event-instance))

#?(:clj
   (defn async-evaluate-pattern [op-code pat result-chan]
     (async/go
       (try
         (let [evaluation-result (case op-code
                                   "eval" (cond
                                            (map? pat) (evaluate-pattern pat)
                                            (list? pat) (eval pat)
                                            :else (println "Cannot evaluate this pattern: " pat))
                                   "add" (eval pat)
                                   (println "Wrong op-code for the pattern - op-code: " op-code))]
           (log/info (str "Evaluation result from async-evaluate-pattern is: " evaluation-result))
           (async/>! result-chan evaluation-result))
         (catch Exception e
           (do
             (log/warn (str "Exception during evaluation on async-evaluate-pattern: " (.getMessage e)))
             (async/>! result-chan
                       (str "Error during evaluation:"
                            (.getMessage e))))))
       (async/close! result-chan))))

(gs/set-evaluate-dataflow-fn! evaluate-dataflow)
(gs/set-evaluate-pattern-fn! evaluate-pattern)
