(ns agentlang.graphql.resolvers
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [agentlang.auth.core :as auth]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.graphql.generator :as gg]
            [agentlang.lang :as lang]
            [agentlang.lang.internal :as fl]
            [agentlang.lang.raw :as lr]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.lang.internal :as li]))

(defn- find-schema [fetch-names find-schema]
  (mapv (fn [n] {n (cn/encode-expressions-in-schema (find-schema n))}) (fetch-names)))

(defn schema-info [component]
  {:records       (find-schema #(cn/record-names component) lr/find-record)
   :entities      (find-schema #(cn/entity-names component) lr/find-entity)
   :relationships (find-schema #(cn/relationship-names component) lr/find-relationship)})

(defn first-result [r]
  (:result (first r)))

(defn form-pattern-name [component-name pattern-str entity]
  (keyword (str (name component-name) "/" pattern-str "_" entity)))

(defn form-entity-name
  "Forms a keyword for an entity name from a component name.
   If the entity is already namespaced keyword, returns it as is."
  [component-name sep entity]
  (cond
    ;; if entity is a keyword and is namespaced, return as is
    (and (keyword? entity) (namespace entity)) entity

    ;; if entity is a keyword without a namespace, form a new keyword
    (keyword? entity) (keyword (str (name component-name) sep (name entity)))

    ;; if entity is a symbol, form a new keyword
    :else (keyword (str (name component-name) sep (name entity)))))

(defn append-question-mark [k]
  (if-let [ns (namespace k)]
    (keyword ns (str (name k) "?"))
    (keyword (str (name k) "?"))))

(defn append-question-to-keys [m]
  (into {} (map (fn [[k v]] [(append-question-mark k) v]) m)))

(defn get-combined-key-value
  "Finds and returns a key-value pair from arg-map where the key is formed by combining entity-name and attribute-name.
   Example:
     Input: arg-map: {:Name \"NameVal\", :Content \"ContentVal\", :UserEmail \"UserEmailVal\"}
            entity-name: :User
            attribute-name: :Email
     Output: {:UserEmail \"UserEmailVal\"}"
  [arg-map entity-name attribute-name]
  (let [combined-key (keyword (str (name entity-name) (name attribute-name)))]
    (when (contains? arg-map combined-key)
      {combined-key (arg-map combined-key)})))

(defn extract-entity-name [kw]
  "input: :WordCount.Core/Document
  output: Document"
  (let [parts (str/split (name kw) #"/")]
    (last parts)))

(defn- temp-event-name [component]
  (fl/make-path
    component
    (fl/unq-name #_"generates unique name as a keyword")))

(defn- register-event [component pats]
  (let [event-name (temp-event-name component)]
    (and (apply lang/dataflow event-name pats)
         event-name)))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn separate-primitive-types [entity-meta]
  (reduce-kv
    (fn [acc attr-name attr-info]
      (let [type-info (:type attr-info)
            base-type (if (and (coll? type-info)
                               (= (first type-info) 'non-null))
                        (second type-info)
                        type-info)
            category (if (contains? gg/graphql-primitive-types base-type)
                       :primitive
                       :non-primitive)]
        (update acc category conj attr-name)))
    {:primitive #{}, :non-primitive #{}}
    entity-meta))

(declare apply-filter)

(defn apply-comparison [instance field op value]
  (let [instance-value (get instance field)]
    (let [result (case op
      :eq (= instance-value value)
      :ne (not= instance-value value)
      :gt (if (and (string? instance-value) (string? value))
            (pos? (compare instance-value value))
            (> instance-value value))
      :gte (if (and (string? instance-value) (string? value))
             (not (neg? (compare instance-value value)))
             (>= instance-value value))
      :lt (if (and (string? instance-value) (string? value))
            (neg? (compare instance-value value))
            (< instance-value value))
      :lte (if (and (string? instance-value) (string? value))
             (not (pos? (compare instance-value value)))
             (<= instance-value value))
      :in (contains? (set value) instance-value)
      :startsWith (str/starts-with? (str instance-value) (str value))
      :endsWith (str/ends-with? (str instance-value) (str value))
      :between (and (>= instance-value (first value)) (<= instance-value (second value)))
      :not (not (apply-comparison instance field (first (keys value)) (first (vals value))))
      :contains (if (and (sequential? instance-value) (number? value))
                  (some #(== % value) instance-value)
                  (str/includes? (str instance-value) (str value)))
      :containsAny (when (sequential? instance-value)
                     (some #(if (number? %)
                              (some (fn [x] (== % x)) instance-value)
                              (some (fn [x] (= (str %) (str x))) instance-value))
                           value))
      :containsAll (when (sequential? instance-value)
                     (every? #(if (number? %)
                                (some (fn [x] (== % x)) instance-value)
                                (some (fn [x] (= (str %) (str x))) instance-value))
                             value))
      :isEmpty (let [is-empty (if (coll? instance-value)
                                (empty? instance-value)
                                (nil? instance-value))]
                 (if value
                   is-empty
                   (not is-empty)))
      (= value op))]
      result)))

(defn apply-attribute-filter [attribute filter]
  (every? (fn [[k v]]
            (if (map? v)
              (every? #(apply-comparison attribute k % (get v %)) (keys v))
              (apply-comparison attribute k :eq v)))
          filter))

(defn apply-list-comparison [attribute-list comparison-type filter]
  (case comparison-type
    :some (some #(apply-filter % filter) attribute-list)
    :every (every? #(apply-filter % filter) attribute-list)
    :none (not-any? #(apply-filter % filter) attribute-list)
    :count (let [count-value (count attribute-list)]
             (apply-comparison {:count count-value} :count (first (keys filter)) (first (vals filter))))
    :isEmpty (let [is-empty (empty? attribute-list)]
               (if filter
                 is-empty
                 (not is-empty)))
    :containsAll (every? #(some (fn [attr] (apply-filter attr %)) attribute-list) filter)
    :containsAny (some #(some (fn [attr] (apply-filter attr %)) attribute-list) filter)))

(defn apply-filter [instance filter]
  (if (map? filter)
    (every? (fn [[k v]]
              (let [result (case k
                             :and (every? #(apply-filter instance %) v)
                             :or (some #(apply-filter instance %) v)
                             :not (not (apply-filter instance v))
                             :Addresses (let [[comparison-type attr-filter] (first v)]
                                          (apply-list-comparison (:Addresses instance) comparison-type attr-filter))
                             (if (map? v)
                               (if (some #(map? (val %)) v)
                                 (apply-filter (get instance k) v)
                                 (apply-attribute-filter instance {k v}))
                               (apply-comparison instance k :eq v)))]
                result))
            filter)
    true))

(declare process-node)

(defn process-comparison [attr op-or-map complex-attributes]
  (let [attr-keyword (keyword attr)]
    (if (contains? complex-attributes attr)
      nil
      (if (map? op-or-map)
        (let [comparisons (map (fn [[op val]]
                                 (case op
                                   :eq [:= attr-keyword val]
                                   :ne [:not= attr-keyword val]
                                   :gt [:> attr-keyword val]
                                   :lt [:< attr-keyword val]
                                   :gte [:>= attr-keyword val]
                                   :lte [:<= attr-keyword val]
                                   :in [:in attr-keyword val]
                                   :contains [:like attr-keyword (str "%" val "%")]
                                   :startsWith [:like attr-keyword (str val "%")]
                                   :endsWith [:like attr-keyword (str "%" val)]
                                   :between [:between attr-keyword (first val) (second val)]
                                   [op attr-keyword val]))
                               op-or-map)]
          (if (= 1 (count comparisons))
            (first comparisons)
            (into [:and] comparisons)))
        [op-or-map attr-keyword]))))

(defn flatten-logic [op nodes]
  (reduce (fn [acc node]
            (if (and (sequential? node) (= (first node) op))
              (into acc (rest node))
              (conj acc node)))
          []
          nodes))

(defn process-node [node complex-attributes]
  (cond
    (and (map? node) (= 1 (count node)))
    (let [[k v] (first node)]
      (cond
        (= k :filter) (process-node v complex-attributes)
        (#{:and :or} k) (let [processed (keep #(process-node % complex-attributes) v)
                              flattened (flatten-logic k processed)]
                          (when (seq flattened)
                            (if (= 1 (count flattened))
                              (first flattened)
                              (into [k] flattened))))
        (= k :not) (when-let [processed (process-node v complex-attributes)]
                     [:not processed])
        :else (process-comparison k v complex-attributes)))
    (map? node)
    (into {} (keep (fn [[k v]]
                     (when-let [processed (process-node v complex-attributes)]
                       [k processed]))
                   node))
    :else node))

(defn generate-fractl-query [entity-name filters complex-attributes & {:keys [alias order-by limit offset]}]
  (let [processed-filters (process-node filters complex-attributes)
        query {(append-question-mark entity-name)
               (cond-> {}
                 processed-filters (assoc :where processed-filters)
                 alias (assoc :as alias)
                 order-by (assoc :order-by order-by)
                 limit (assoc :limit limit)
                 offset (assoc :offset offset))}]
    query))

(defn apply-filters [instances filters]
  (if (nil? filters)
    instances
    (if (empty? instances)
      []
      (let [filtered (filter #(apply-filter % filters) instances)]
        filtered))))

(defn make-auth-event-context [context]
  (let [auth-config (:auth-config context)
        request (:request context)]
    (if (and auth-config request)
      (let [user (auth/session-user (assoc auth-config :request request))]
        (when user
          {:User        (:email user)
           :Sub         (:sub user)
           :UserDetails user})))))

(defn eval-patterns [component patterns context]
  (let [event (register-event component (as-vec patterns))
        auth-event-context (make-auth-event-context context)]
    (try
      (let [result (first (gs/evaluate-dataflow {event {:EventContext auth-event-context}}))]
        (if (= (:status result) :error)
          (u/throw-ex (str "Error: " (:message result)))
          (:result result)))
      (finally
        (cn/remove-event event)))))

(defn extract-entity-meta-context [entity-name context]
  (let [{:keys [core-component entity-metas]} context
        entity-meta ((keyword (extract-entity-name entity-name)) entity-metas)]
    [core-component entity-meta]))

(defn extract-args [args]
  (let [{:keys [attributes filter limit offset]} args]
    [attributes filter limit (or offset 0)]))

(defn build-dataflow-query [entity-name attrs filters non-primitive-attrs]
  (if (empty? attrs)
    (generate-fractl-query entity-name filters non-primitive-attrs)
    {entity-name (append-question-to-keys attrs)}))

(defn normalize-results
  "Normalizes the results of a dataflow pattern"
  [results]
  (if results
    (cond
      (map? results) [results]
      (coll? results) results
      :else [results])
    []))

(defn apply-final-filters [results filters offset limit]
  (cond->> (apply-filters results filters)
    true (drop offset)
    limit (take limit)))

(defn query-entity-by-attribute-resolver
  [entity-name]
  (fn [context args value]
    (let [[core-component entity-meta] (extract-entity-meta-context entity-name context)
          {_ :primitive non-primitive-attrs :non-primitive} (separate-primitive-types entity-meta)
          [attrs filters limit offset] (extract-args args)
          dataflow-query (build-dataflow-query entity-name attrs filters non-primitive-attrs)
          results (eval-patterns core-component dataflow-query context)
          normalized-results (if results
                               (cond
                                 (map? results) [results]
                                 (coll? results) results
                                 :else [])
                               [])]
      (apply-final-filters normalized-results filters offset limit))))

(defn query-parent-children-resolver
  []
  (fn [context args parent]
    ; simply return parent to be used by children resolver
    [parent]))

(defn find-guid-or-id-attribute [schema entity-name]
  (let [entities (:entities schema)
        entity-map (first (filter #(contains? % entity-name) entities))]
    (when entity-map
      (let [entity-def (entity-map entity-name)
            identity-attr (some (fn [[attr details]]
                                  (when (= details :Identity)
                                    attr))
                                entity-def)
            guid-attr (some (fn [[attr details]]
                              (when (= (get details :guid) true)
                                attr))
                            entity-def)
            id-attr (some (fn [[attr details]]
                            (when (= (get details :id) true)
                              attr))
                          entity-def)]
        (or identity-attr guid-attr id-attr)))))

(defn- as-fully-qualified-path [_] (u/raise-not-implemented 'as-fully-qualified-path))
(defn- path-string [_] (u/raise-not-implemented 'path-string))
(defn- uri-join-parts [_] (u/raise-not-implemented 'uri-join-parts))

(defn- query-all-children [core-component parent-name parent-id relationship-name child-name context]
  (let [fq (partial as-fully-qualified-path core-component)
        all-children-pattern {(form-pattern-name core-component "LookupAll" child-name)
                              {li/path-attr (fq (str "path://" parent-name "/" parent-id "/" relationship-name "/" child-name "/%"))}}]
    (eval-patterns core-component all-children-pattern context)))

(defn- query-children-using-attributes [core-component parent-name child-name relationship-name parent-guid-attribute parent-instance attrs context]
  (let [query-params (append-question-to-keys attrs)
        dataflow-query [{parent-name
                         {(append-question-mark parent-guid-attribute) (parent-guid-attribute parent-instance)}
                         :as :Parent}
                        {child-name query-params
                         :->        [[relationship-name :Parent]]}]]
    (eval-patterns core-component dataflow-query context)))

(defn query-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args parent-instance]
    (let [{:keys [core-component]} context
          schema (schema-info core-component)
          [attrs filters limit offset] (extract-args args)
          parent-guid-attribute (find-guid-or-id-attribute schema parent-name)
          parent-id (parent-guid-attribute parent-instance)
          [extracted-parent-name extracted-child-name extracted-relationship-name]
          (map extract-entity-name [parent-name child-name relationship-name])

          dataflow-result
          (if (nil? attrs)
            (query-all-children core-component extracted-parent-name parent-id extracted-relationship-name extracted-child-name context)
            (query-children-using-attributes core-component parent-name child-name relationship-name parent-guid-attribute parent-instance attrs context))

          results (normalize-results dataflow-result)]
      (apply-final-filters results filters offset limit))))

(defn query-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [[attrs filters limit offset] (extract-args args)
          core-component (:core-component context)
          query-params (append-question-to-keys attrs)
          query (if (empty? query-params)
                  {(append-question-mark relationship-name) {}}
                  {relationship-name query-params})
          results (eval-patterns core-component query context)]
      (apply-final-filters results filters offset limit))))

(defn transform-pattern
  "Transforms a pattern by wrapping maps with their corresponding record types."
  [records pattern]
  (letfn [(find-record-type [value]
            (some (fn [record]
                    (when (= (set (keys (val (first record)))) (set (keys value)))
                      (key (first record))))
                  records))
          (wrap-record [record-type data]
            (if (map? data)
              {record-type data}
              data))
          (process-value [value]
            (cond
              (vector? value)
              (mapv process-value value)

              (map? value)
              (if-let [record-type (find-record-type value)]
                (wrap-record record-type (into {} (map (fn [[k v]] [k (process-value v)]) value)))
                (into {} (map (fn [[k v]] [k (process-value v)]) value)))
              :else
              value))]
    (process-value pattern)))

(defn create-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema-info (schema-info core-component)
          records (:records schema-info)
          create-pattern {entity-name args}
          attr-map-key (first (keys create-pattern))
          attr-map-value (get create-pattern attr-map-key)
          transformed-attr-map (transform-pattern records attr-map-value)
          create-pattern {attr-map-key transformed-attr-map}]
      (first (eval-patterns core-component create-pattern context)))))

(defn create-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)
          [parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
          child-params (dissoc args parent-guid-attribute)
          fetch-parent-query {parent-name {(append-question-mark parent-guid) parent-guid-value}}
          create-child-query {child-name child-params
                              :->        [[relationship-name fetch-parent-query]]}
          results (eval-patterns core-component create-child-query context)]
      (assoc results parent-guid-attribute parent-guid-value))))

(defn create-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          create-pattern {relationship-name args}]
      (first (eval-patterns core-component create-pattern context)))))

(defn transform-update-pattern
  [records pattern]
  (let [update-key (first (keys pattern))
        update-value (get pattern update-key)
        id (:Id update-value)
        data (:Data update-value)
        transformed-data (transform-pattern records data)
        result {update-key {:Id id :Data transformed-data}}]
    result))

(defn update-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema-info (schema-info core-component)
          entity-guid-attr (find-guid-or-id-attribute schema-info entity-name)
          entity-guid-val (entity-guid-attr args)
          update-pattern {(form-pattern-name core-component "Update" (extract-entity-name entity-name))
                          {entity-guid-attr entity-guid-val
                           :Data            (dissoc args entity-guid-attr)}}
          records (:records schema-info)
          update-pattern (transform-update-pattern records update-pattern)]
      (first (eval-patterns core-component update-pattern context)))))

(defn update-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          child-guid (find-guid-or-id-attribute schema child-name)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)]

      (when-not parent-guid-arg-pair
        (throw (Exception. (str "Error: " parent-guid " not provided for " parent-name))))

      (let [[parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
            child-params (dissoc args parent-guid-attribute child-guid)
            extracted-child-name (keyword (extract-entity-name child-name))
            [child-id _] (gg/find-attribute extracted-child-name (:entity-metas context) :id)
            child-id-value (child-id args)]

        (when-not child-id-value
          (throw (Exception. (str "Error: " child-id " not provided for " child-name ". It is needed to identify contained
          entity."))))

        (let [path-attr fl/path-attr
              child-path (path-string
                          (as-fully-qualified-path
                           core-component
                           (uri-join-parts [(extract-entity-name parent-name)
                                            parent-guid-value
                                            (extract-entity-name relationship-name)
                                            (extract-entity-name child-name)
                                            child-id-value])))
              update-pattern {(form-pattern-name core-component "Update" (extract-entity-name child-name))
                              {:Data     child-params
                               path-attr (str "path:/" child-path)}}
              results (eval-patterns core-component update-pattern context)]
          (assoc (first results) parent-guid-attribute parent-guid-value))))))

(defn update-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          extracted-entity1-name (keyword (extract-entity-name entity1-name))
          extracted-entity2-name (keyword (extract-entity-name entity2-name))
          entity1-guid-value (extracted-entity1-name args)
          entity2-guid-value (extracted-entity2-name args)]

      (when-not entity1-guid-value
        (throw (Exception.
                 (str "Error: GUID for '" entity1-name "' not provided. It is needed to update the associated entity."))))
      (when-not entity2-guid-value
        (throw (Exception.
                 (str "Error: GUID for '" entity2-name "' not provided. It is needed to update the associated entity."))))

      (let [query-params {(append-question-mark extracted-entity1-name) entity1-guid-value
                          (append-question-mark extracted-entity2-name) entity2-guid-value}
            relationship-params (dissoc args extracted-entity1-name extracted-entity2-name)
            update-pattern {relationship-name
                            (merge query-params relationship-params)}]
        (first (eval-patterns core-component update-pattern context))))))


(defn delete-entity-resolver
  [entity-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          entity-guid (find-guid-or-id-attribute schema entity-name)
          delete-pattern [[:delete entity-name {entity-guid (entity-guid args)}]]]
      (eval-patterns core-component delete-pattern context))))

(defn delete-contained-entity-resolver
  [relationship-name parent-name child-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          schema (schema-info core-component)
          parent-guid (find-guid-or-id-attribute schema parent-name)
          parent-guid-arg-pair (get-combined-key-value args parent-name parent-guid)
          [parent-guid-attribute parent-guid-value] (first (seq parent-guid-arg-pair))
          child-guid (find-guid-or-id-attribute schema child-name)
          child-params (dissoc args parent-guid-attribute)]
      (when (or (nil? parent-guid-attribute) (nil? parent-guid-value))
        (throw (Exception. (str "Error: " (extract-entity-name parent-name) (name parent-guid) " not provided for " parent-name
                                ". It is needed to identify the parent entity."))))
      (let [query-children-pattern {child-name (append-question-to-keys child-params)
                                    :-> [[(append-question-mark relationship-name)
                                          {parent-name {(append-question-mark parent-guid)
                                                        parent-guid-value}}]] :as :Cs}
            delete-children-pattern [:for-each :Cs
                                     [:delete child-name {child-guid (keyword (str "%." (name child-guid)))}]]
            results (flatten (eval-patterns core-component [query-children-pattern delete-children-pattern] context))]
        (if (and (seq results) (map? (first results)))      ;; map isn't returned when no records are deleted
          ;; we received guid of parent, thus returning
          (vec (map #(assoc % parent-guid-attribute parent-guid-value) results))
          {})))))

(defn delete-between-relationship-resolver
  [relationship-name entity1-name entity2-name]
  (fn [context args value]
    (let [args (:input args)
          core-component (:core-component context)
          create-pattern [[:delete relationship-name args]]]
      (eval-patterns core-component create-pattern context))))

(defn generate-query-resolver-map [schema]
  (let [entities (mapv (fn [entity] (key (first entity))) (:entities schema))
        relationships (into {} (map (fn [rel] [(key (first rel)) (:meta (val (first rel)))]) (:relationships schema)))
        entity-resolvers (reduce (fn [acc entity-key]
                                   (assoc acc
                                     (keyword (str "Query/"
                                                   (name (last (str/split (name entity-key) #"\.")))))
                                     (agentlang.graphql.resolvers/query-entity-by-attribute-resolver entity-key)))
                                 {} entities)
        relationship-resolvers (reduce-kv (fn [acc rel {:keys [contains between]}]
                                            (cond
                                              contains
                                              (let [[parent child] contains
                                                    parent-name (name (last (str/split (name parent) #"\.")))
                                                    child-name (name (last (str/split (name child) #"\.")))
                                                    relation-name (name (last (str/split (name rel) #"\.")))]
                                                (-> acc
                                                    (assoc (keyword (str parent-name "/" relation-name))
                                                           (agentlang.graphql.resolvers/query-parent-children-resolver))
                                                    (assoc (keyword (str relation-name "/" child-name))
                                                           (agentlang.graphql.resolvers/query-contained-entity-resolver rel parent child))))
                                              between
                                              (let [[entity1 entity2] between
                                                    entity1-name (name (last (str/split (name entity1) #"\.")))
                                                    entity2-name (name (last (str/split (name entity2) #"\.")))
                                                    relation-name (name (last (str/split (name rel) #"\.")))]
                                                (assoc acc
                                                  (keyword (str "Query/" relation-name))
                                                  (agentlang.graphql.resolvers/query-between-relationship-resolver rel entity1 entity2)))))
                                          {} relationships)]
    (merge entity-resolvers relationship-resolvers)))

(defn generate-mutation-resolvers
  ([schema mutation-type]
   (generate-mutation-resolvers schema mutation-type true))
  ([schema mutation-type include-relationship-resolvers?]
   (let [mutation-type-lower-case (str/lower-case mutation-type)
         entities (mapv (fn [entity] (key (first entity))) (:entities schema))
         relationships (map (fn [rel] [(key (first rel)) (:meta (val (first rel)))]) (:relationships schema))
         entity-mutation-resolvers (reduce (fn [acc entity-key]
                                             (assoc acc
                                               (keyword (str "Mutation/" mutation-type (name (last (str/split (name entity-key) #"\.")))))
                                               ((resolve (symbol (str "agentlang.graphql.resolvers/" mutation-type-lower-case "-entity-resolver"))) entity-key)))
                                           {} entities)
         relationship-mutation-resolvers (if include-relationship-resolvers?
                                           (reduce (fn [acc [rel-key {:keys [contains between]}]]
                                                     (cond
                                                       contains
                                                       (let [[parent child] contains
                                                             relation-name (name (last (str/split (name rel-key) #"\.")))]
                                                         (assoc acc
                                                           (keyword (str "Mutation/" mutation-type relation-name))
                                                           ((resolve (symbol (str "agentlang.graphql.resolvers/" mutation-type-lower-case "-contained-entity-resolver"))) rel-key parent child)))

                                                       between
                                                       (let [[entity1 entity2] between
                                                             relation-name (name (last (str/split (name rel-key) #"\.")))]
                                                         (assoc acc
                                                           (keyword (str "Mutation/" mutation-type relation-name))
                                                           ((resolve (symbol (str "agentlang.graphql.resolvers/" mutation-type-lower-case "-between-relationship-resolver"))) rel-key entity1 entity2)))
                                                       :else acc))
                                                   {} relationships))]
     (merge entity-mutation-resolvers relationship-mutation-resolvers))))

(defn remove-child-entities [schema contains-graph]
  (let [children (set (mapcat val contains-graph))
        filtered-entities (vec (filter
                                 (fn [entity-map]
                                   (let [entity-key (keyword (name (first (keys entity-map))))]
                                     (not (children entity-key))))
                                 (:entities schema)))]
    (assoc schema :entities filtered-entities)))

(defn generate-resolver-map [schema contains-graph]
  (let [schema-without-children (remove-child-entities schema contains-graph)
        query-resolvers (generate-query-resolver-map schema)
        create-mutation-resolvers (generate-mutation-resolvers schema-without-children "Create")
        update-mutation-resolvers (generate-mutation-resolvers schema-without-children "Update")
        delete-mutation-resolvers (generate-mutation-resolvers schema-without-children "Delete")]
    (merge query-resolvers create-mutation-resolvers update-mutation-resolvers delete-mutation-resolvers)))
