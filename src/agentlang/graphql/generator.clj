(ns agentlang.graphql.generator
  (:require [clojure.string :as str]
            [agentlang.util.logger :as log]
            [clojure.set :as set]
            )
  (:import (clojure.lang LazySeq PersistentList)))

(def query-input-object-name-postfix "QueryAttributes")

(def filter-input-object-name-postfix "Filter")

(def mutation-input-object-name-postfix "MutationAttributes")

(def record-names (atom #{}))

(def entity-names (atom #{}))

(def relationship-names (atom #{}))

(def between-relationship-names (atom #{}))

(def element-names (atom #{}))

(def contains-graph (atom {}))

(def records-code (atom {}))

(def entities-code (atom {}))

(def relationships-code (atom {}))

(def enums-code (atom {}))

;; stores the metadata maps for each attribute of each entity
(def entity-metas (atom {}))

(defn cleanup-atoms []
  (reset! record-names #{})
  (reset! entity-names #{})
  (reset! relationship-names #{})
  (reset! between-relationship-names #{})
  (reset! element-names #{})
  (reset! contains-graph {})
  (reset! records-code {})
  (reset! entities-code {})
  (reset! relationships-code {})
  (reset! enums-code {})
  (reset! entity-metas {}))

(def graphql-primitive-types #{:String :Int :Float :Boolean})

(def agentlangType->graphQL
  {:String     :String
   :DateTime   :String
   :Date       :String
   :Password   :String
   :Time       :String
   :UUID       :String
   :Int        :Int
   :Int64      :Int
   :Float      :Float
   :Double     :Float
   :Decimal    :Float
   :Boolean    :Boolean
   :Email      :String
   :Any        :String
   :Identity   :String
   :Keyword    :String
   :Path       :String
   :BigInteger :Int
   :Map        :EDNMap
   :Edn        :EDNMap})

(defn type->GraphQL [type-info]
  (let [type-kw (cond
                  (string? type-info) (keyword type-info)
                  (keyword? type-info) type-info
                  (map? type-info) (keyword (:type type-info))
                  :else nil)]                               ;; unsupported type-info
    (if (and type-kw (not (@element-names type-kw)))
      (let [mapped-type (get agentlangType->graphQL type-kw)]
        (if mapped-type
          (keyword mapped-type)
          :String))                                         ;; default to :String if type not found
      type-kw)))                                            ;; if a record, keep original

(defn field-type
  ([type-name type-info]
   ;; Lacinia and Agentlang have different rules for Enum value formats. So, disabling enum parsing for now and treating
   ;; enum as a string in GraphQL schema. Agentlang will automatically parse and handle value validation

   ;; More context:
   ;; Lacinia Enum values must be GraphQL Names: they may contain only letters, numbers, and underscores.
   ;; Enums are case-sensitive; by convention they are in all upper-case.

   ;; We're keeping enum mapping logic in case we'd like to handle this difference using resolvers by sanitizing
   ;; values to generate their corresponding GraphQL name forms.

   (field-type type-name type-info false))
  ([type-name type-info enums-enabled?]
   (letfn [(transform-type [info]
             (cond
               (:oneof info)
               (let [oneof-values (mapv keyword (:oneof info))]
                 (if enums-enabled?
                   [type-name {:meta {:type :enum :values oneof-values}}]
                   [:String {:meta {:type :basic}}]))

               (:listof info)
               (let [listof-type (type->GraphQL (:listof info))]
                 [(list 'list listof-type) {:meta {:type :list :item-type listof-type}}])

               (:setof info)
               (let [setof-type (type->GraphQL (:setof info))]
                 [(list 'list setof-type) {:meta {:type :set :item-type setof-type}}])

               :else
               [(type->GraphQL info) {:meta {:type :basic}}]))]
     ;; parsing type-info to further determine the field type structure
     (let [parsed-info (transform-type type-info)
           non-null? (not (:optional type-info))
           is-guid? (or (:guid type-info) (= :Identity type-info))
           ;; adding GUID metadata if applicable
           updated-meta (if is-guid?
                          (assoc-in (second parsed-info) [:meta :guid] true)
                          (second parsed-info))]

       ;; wrapping in 'non-null' if not optional
       (if non-null?
         [{:type (list 'non-null (first parsed-info))} updated-meta]
         [{:type (first parsed-info)} updated-meta])))))

(defn extract-between-relationship-names [relationships]
  (->> relationships
       (filter #(contains? (:meta (val (first %))) :between))
       (map (comp keyword name key first))
       (into [])))

(defn strip-irrelevant-attributes [fields]
  "Removes any attributes besides :type"
  (let [output (into {} (map (fn [[k v]] [k {:type (:type v)}]) fields))]
    output))

(defn find-attribute
  "Returns vector containing the first attribute name and details found, with fallback mechanism.
   Accepts an entity name, entities map, a primary attribute, and any number of fallback attributes.
   For example: [:Email {:guid true, :type (non-null :String)}]"
  [entity-name entities primary-attr & fallback-attrs]
  (let [entity-details (get entities entity-name)
        find-attr (fn [attr]
                    (some (fn [[attr-name attr-props]]
                            (when (attr attr-props)
                              [attr-name attr-props]))
                          entity-details))
        search-attrs (cons primary-attr fallback-attrs)
        search (fn [attrs]
                 (when-let [attr (first attrs)]
                   (or (find-attr attr)
                       (recur (rest attrs)))))]
    (search search-attrs)))

(defn make-graphql-fields-optional [entity-name input-map make-primary-non-null]
  (let [fields (:fields input-map)
        [primary-attr-name _] (find-attribute entity-name @entity-metas :guid :id)]
    (assoc input-map :fields
                     (reduce-kv (fn [acc key val]
                                  (let [type-val (:type val)
                                        is-primary (= key primary-attr-name)
                                        already-non-null? (and (coll? type-val) (= 'non-null (first type-val)))]
                                    (assoc acc key
                                               (cond
                                                 ;; make primary key non-null if needed
                                                 (and is-primary make-primary-non-null)
                                                 (assoc val :type (if already-non-null? type-val (list 'non-null type-val)))

                                                 ;; make others optional
                                                 already-non-null?
                                                 (assoc val :type (second type-val))

                                                 ;; else, leave the field as is
                                                 :else
                                                 val))))
                                {}
                                fields))))

(defn build-contains-relationship-graph [edn-input]
  "Returns a map where keys are parent entities and values are list of contained entities.
  For example: if user contains document and profile, and document further contains page, index:
  {:Document [:Index :Page], :User [:Document :Profile]}"
  (let [relationships (into []
                            (mapcat (fn [entry]
                                      (let [[entity details] (first entry)
                                            meta (get-in details [:meta])]
                                        (when-let [contains (get meta :contains)]
                                          (map (fn [child] [(first contains) child])
                                               (rest contains))))))
                            edn-input)]
    (reduce (fn [acc [parent child]]
              (update acc parent (fnil conj []) child))
            {}
            relationships)))

(defn find-parents [graph entity]
  "Recursively finds all parent entities of the given entity."
  (letfn [(recursive-find [entity]
            (reduce-kv (fn [acc key val]
                         (if (some #(= entity %) val)
                           (into acc (cons key (recursive-find key)))
                           acc))
                       []
                       graph))]
    (recursive-find entity)))

(defn remove-wrapper-functions [type-val]
  "(non-null (list :String)) will become :String."
  (let [unwrap (fn [val] (if (coll? val) (last val) val))]
    (loop [current-val type-val]
      (if (coll? current-val)
        (let [next-val (unwrap current-val)]                ;; use the unwrap helper to get the next value
          (recur next-val))                                 ;; continue with the next value
        current-val))))

(defn replace-wrapped-type [type-val new-root-type]
  "Replaced datatype with new one. (non-null (list :String)) will become (non-null (list :NewString))."
  (let [unwrap (fn [val] (if (coll? val) (butlast val) val))
        replace-last (fn [coll new-val] (conj (vec (butlast coll)) new-val))]
    (loop [current-val type-val]
      (if (coll? current-val)
        (let [next-val (unwrap current-val)]
          (if (empty? next-val)                             ;; if we've unwrapped everything, replace the root
            new-root-type
            (recur (replace-last next-val new-root-type))))
        new-root-type))))                                   ;; if not a collection, just return the new root type

(defn update-nested-type [type-val update-fn]
  (if (list? type-val)
    (let [inner-type (last type-val)
          wrapper-fns (butlast type-val)]
      (concat wrapper-fns [(update-nested-type inner-type update-fn)]))
    (update-fn type-val)))

(defn append-postfix-to-field-names [input-map postfix remove-wrappers]
  "Appends a postfix to field names if attribute is of entity/record/relationship type. Preserves list and non-null wrappers."
  (let [fields (:fields input-map)]
    (assoc input-map :fields
           (reduce-kv (fn [acc key val]
                        (let [type-val (:type val)
                              type-name (remove-wrapper-functions type-val)
                              updated-type-val (if (contains? @element-names type-name)
                                                 (update-nested-type type-val #(keyword (str (name %) postfix)))
                                                 type-val)]
                          (assoc acc key (assoc val :type updated-type-val))))
                      {}
                      fields))))

(defn update-entity-meta
  ([entity-name]
   ;; init with {}
   (swap! entity-metas
          (fn [current-metas]
            (if (contains? current-metas entity-name)
              current-metas
              (assoc current-metas entity-name {})))))
  ([entity-name attribute-name meta]
   (swap! entity-metas
          (fn [current-metas]
            (update current-metas entity-name
                    (fn [entity-meta]
                      (assoc (or entity-meta {}) attribute-name meta)))))))

(defn enrich-field-meta [type-info meta]
  "Additional fields inside the agentlang field type should flow to metadata for later code extension."
  (if (map? type-info)
    (let [additional-fields (apply dissoc type-info [:type :item-type :guid :values])]
      ;; merge parsed meta with unused field key value pairs
      (merge meta additional-fields))
    meta))

(defn generate-enum-edn [enum-name values]
  {enum-name {:values (vec values)}})

(defn sanitize-attributes [attributes]
  ;; check if :rbac is present and handle operations
  (dissoc attributes :rbac :meta))

(defn process-attributes [entity-name attributes]
  (if (empty? attributes)
    ;; only true when between relationship doesn't have attributes
    {})
  (reduce-kv (fn [acc key val]
               (let [type-info (field-type key val)
                     field-datatype (first type-info)
                     field-meta (enrich-field-meta val (:meta (second type-info)))]
                 ;; save attribute meta
                 (update-entity-meta entity-name key (assoc (dissoc field-meta :type) :type (:type field-datatype)))
                 (cond
                   (= (:type field-meta) :basic)
                   (assoc acc key field-datatype)

                   (= (:type field-meta) :enum)
                   (do
                     (swap! enums-code merge (generate-enum-edn key (:values field-meta)))
                     (assoc acc key field-datatype))

                   (= (:type field-meta) :list)
                   (assoc acc key field-datatype)

                   :else acc)
                 ;; save attribute meta
                 ))
             {}
             attributes))

(defn process-entity [entity-name attributes]
  (let [entity-key (keyword (name entity-name))
        fields (process-attributes entity-name (sanitize-attributes attributes))]
    {entity-key {:fields fields}}))

(defn entities->GraphQL-schema [entities code-atom]
  (swap! code-atom (fn [_]
                     (reduce (fn [acc entity]
                               (reduce-kv (fn [a entity-name attributes]
                                            (merge a (process-entity entity-name attributes)))
                                          acc
                                          entity))
                             {}
                             entities))))

(defn process-relationship [relationship-name body]
  (let [relationship-key (keyword (name relationship-name))
        fields (:meta body)]
    (cond
      (contains? fields :contains)
      (let [[parent-name child-name] (map keyword (:contains fields))
            parent-guid-details (find-attribute parent-name @entity-metas :guid :id)
            parent-guid (first parent-guid-details)
            parent-guid-datatype (:type (second parent-guid-details))]
        ;; update the parent entity to include contained entity
        (swap! entities-code
               update-in [parent-name :fields]
               assoc
               relationship-key
               {:type (list 'list relationship-key)})
        ;; update the child entity to include the primary key of parent entity
        (swap! entities-code
               update-in [child-name :fields]
               assoc
               (keyword (str (name parent-name) (name parent-guid)))
               {:type parent-guid-datatype})
        ;; create a new entity representing contains relationship
        (swap! entities-code
               assoc
               relationship-key
               {:fields {(keyword (name child-name)) {:type (list 'list child-name)
                                                      :args {:attributes {:type (keyword (str (name child-name) query-input-object-name-postfix))}
                                                             :filter      {:type (keyword (str (name child-name) filter-input-object-name-postfix))}
                                                             :limit      {:type :Int}
                                                             :offset     {:type :Int}}}}}))

      (contains? fields :between)
      (let [between-value (:between fields)
            ;; split the between-value based on presence of :as to extract entity names and aliases
            ;; example between value: (:DocumentName1 :DocumentName2 :as (:Document1 :Document2))
            [entities aliases] (if (contains? (set between-value) :as)
                                 (split-with #(not= % :as) between-value)
                                 [between-value nil])
            [entity1-name entity2-name] entities
            alias-pair (when aliases (first (drop 1 aliases)))
            [alias1 alias2] (if alias-pair alias-pair [entity1-name entity2-name])
            processed-attributes (process-attributes relationship-name (sanitize-attributes (dissoc body :meta)))
            entity1-guid-details (find-attribute entity1-name @entity-metas :guid)
            entity2-guid-details (find-attribute entity2-name @entity-metas :guid)
            entity1-guid-datatype (remove-wrapper-functions (:type (second entity1-guid-details)))
            entity2-guid-datatype (remove-wrapper-functions (:type (second entity2-guid-details)))]
        ;; create a new entity representing between relationship
        (swap! entities-code
               (fn [current-code]
                 (let [current-fields (:fields (get current-code relationship-key {}))
                       merged-fields (merge current-fields processed-attributes)
                       updated-fields (assoc merged-fields alias1 {:type entity1-guid-datatype} alias2 {:type entity2-guid-datatype})]

                   (if (empty? processed-attributes)
                     (do
                       (update-entity-meta relationship-name)
                       (update-entity-meta relationship-key alias1 {:type entity1-guid-datatype})
                       (update-entity-meta relationship-key alias2 {:type entity2-guid-datatype})))
                   (assoc current-code relationship-key {:fields updated-fields})))))

      :else
      (log/warn (str "Warning: unexpected relationships keys - relationship name: " relationship-name "relationship body: " body)))))

(defn relationships->GraphQL-schema [entities code-atom]
  (swap! code-atom (fn [_]
                     (reduce (fn [acc entity]
                               (reduce-kv (fn [a name body]
                                            (merge a (process-relationship name body)))
                                          acc
                                          entity))
                             {}
                             entities))))

(defn generate-queries [entities]
  (let [fields (reduce-kv (fn [acc k v]
                            (assoc acc k {:type (list 'list k)
                                          :args {:attributes {:type (keyword (str (name k) query-input-object-name-postfix))}
                                                 :filter     {:type (keyword (str (name k) filter-input-object-name-postfix))}
                                                 :limit      {:type :Int}
                                                 :offset     {:type :Int}}}))
                          {} entities)]
    {:Query {:fields fields}}))

(defn generate-query-input-objects [entities-records]
  (let [input-objects (reduce-kv (fn [acc k v]
                                   (assoc acc
                                     (keyword (str (name k) query-input-object-name-postfix))
                                     ;; making fields optional to allow user query using any attribute
                                     {:fields (strip-irrelevant-attributes (:fields (append-postfix-to-field-names (make-graphql-fields-optional k v false) query-input-object-name-postfix true)))}))
                                 {}
                                 entities-records)]
    input-objects))

(defn generate-mutation-input-objects
  [entities-records mutation-type make-fields-optional keep-primary-non-null]
  (let [input-objects (reduce-kv
                        (fn [acc k v]
                          (let [processed-v (if make-fields-optional
                                              (make-graphql-fields-optional k v keep-primary-non-null)
                                              v)
                                fields-modified (append-postfix-to-field-names processed-v
                                                                               (str mutation-type mutation-input-object-name-postfix)
                                                                               false)]
                            (assoc acc
                              (keyword (str (name k) (str mutation-type mutation-input-object-name-postfix)))
                              {:fields (strip-irrelevant-attributes (:fields fields-modified))})))
                        {}
                        entities-records)]
    input-objects))

(defn generate-mutations [entities mutation-type can-return-list?]
  (let [mutation-fields (reduce-kv
                          (fn [acc k v]
                            (let [mutation-input-object-name (keyword (str (name k) (str mutation-type mutation-input-object-name-postfix)))
                                  type (if can-return-list?
                                         (list 'list k)
                                         k)]
                              (assoc acc (keyword (str mutation-type (name k)))
                                         {:type type
                                          :args {:input {:type mutation-input-object-name}}})))
                          {}
                          entities)]
    {:Mutation {:fields mutation-fields}}))

(defn generate-delete-mutations [entities]
  "Allows deletion only using guid or id."
  (let [find-arg (fn [entity-name entity-details]
                   ;; use :guid, else fallback to :id
                   (let [guid-attr (first (find-attribute entity-name entities :guid))
                         id-attr (when (not guid-attr)
                                   (some (fn [[attr-name attr-props]]
                                           (when (:id attr-props) attr-name))
                                         entity-details))]
                     (if guid-attr
                       {guid-attr {:type (:type (get-in entities [entity-name guid-attr]))}}
                       (when id-attr
                         {id-attr {:type (:type (get-in entities [entity-name id-attr]))}}))))
        make-mutation (fn [[entity-name attrs]]
                        ;; generate Delete mutation map if guid or id found
                        (when-let [args-map (find-arg entity-name attrs)]
                          {(keyword (str "Delete" (name entity-name)))
                           {:type (keyword (name entity-name))
                            :args args-map}}))]
    {:Mutation
     {:fields
      ;; Use comp to filter out nils from entities without a guid or id
      (into {} (comp (map make-mutation) (filter identity)) entities)}}))

(defn merge-mutations [& maps]
  (reduce (fn [acc-map current-map]
            (let [acc-fields (get-in acc-map [:Mutation :fields] {})
                  current-fields (get-in current-map [:Mutation :fields] {})
                  merged-fields (merge acc-fields current-fields)]
              (assoc-in acc-map [:Mutation :fields] merged-fields)))
          {:Mutation {:fields {}}}
          maps))

(defn initialize-atom [ref value]
  (reset! ref (set value)))

(defn merge-element-names []
  (let [combined-names (set/union @record-names @entity-names @relationship-names)]
    (reset! element-names combined-names)))

(defn extract-names [schema-map]
  (map #(first (keys %)) schema-map))

(defn normalize-schema [element]
  (cond
    (map? element)                                          ;; recursively normalize its keys and values
    (into {} (map (fn [[k v]] [(normalize-schema k) (normalize-schema v)]) element))

    (keyword? element)                                      ;; convert namespaced keyword to non-namespaced keyword
    (keyword (name element))

    (coll? element)                                         ;; normalize elements
    (map normalize-schema element)

    :else
    element))

(defn dissoc-if-empty [m k]
  (if (empty? (get m k))
    (dissoc m k)
    m))

(defn remove-empty-graphql-constructs [schema]
  (-> schema
      (update :objects (fn [objs]
                         (-> objs
                             (dissoc-if-empty :Query)
                             (dissoc-if-empty :Mutation)
                             (dissoc-if-empty :Subscription))))
      (dissoc-if-empty :enums)
      (dissoc-if-empty :input-objects)))

(defn generate-contains-graph [schema-info]
  (build-contains-relationship-graph (:relationships (normalize-schema schema-info))))

(defn update-child-mutations [mutations relationships mutation-type]
  "Firstly, this function removes child mutations because children are mutated in the context of parents - via relationship 
  mutations. For example, if a document is contained by a user and a category, :Mutation/CreateDocument won't work. 
  Instead, we should identify document via relationship name, such as: :Mutation/CreateUserDocument where UserDocument is
  the name of contains relationship. Secondly, simplify relationship mutation to use attributes of child mutation 
  (which includes parent guid/id), instead of having all attributes of parent as well."
  ;; collect and apply removal
  (let [mutations-map (:fields (:Mutation mutations))
        update-info (reduce (fn [acc relationship]
                              (let [relationship-name (key (first relationship))
                                    contains (get-in (val (first relationship)) [:meta :contains])]
                                (if contains
                                  (let [child (second contains)
                                        relationship-mutation-key (keyword (str mutation-type (name relationship-name)))
                                        child-mutation-key (keyword (str mutation-type (name child)))
                                        child-value (mutations-map child-mutation-key)]
                                    (assoc acc
                                      :updates (conj (acc :updates) [relationship-mutation-key child-value])
                                      :removals (conj (acc :removals) child-mutation-key)))
                                  acc)))
                            {:updates [] :removals []}
                            relationships)
        updates-map (reduce (fn [m [k v]]
                              (assoc m k v))
                            mutations-map
                            (:updates update-info))
        final-map (reduce dissoc updates-map (:removals update-info))]
    {:Mutation {:fields final-map}}))

(defn remove-rbac-from-relationships [relationships]
  (map (fn [relationship]
         (let [key (first (keys relationship))
               value-map (first (vals relationship))]
           {key (dissoc value-map :rbac)}))
       relationships))

(defn make-fields-with-default-vals-optional
  [schema]
  (letfn [(transform [item]
            (cond
              (map? item) (into {} (map (fn [[k v]]
                                          (if (= k :default)
                                            [:optional true]
                                            [k (transform v)])) item))
              (vector? item) (mapv transform item)
              (seq? item) (map transform item)
              :else
                (let [special-set #{:Now :Identity}
                      type-dict {:Now :Now :Identity :String}]
                  (if (contains? special-set item)
                    (cond-> {:type (get type-dict item item)
                             :optional true}
                      (= item :Identity) (assoc :guid true))
                    item))))]
    (transform schema)))

(defn preprocess-schema-info [schema-info]
  (let [schema-info (assoc schema-info :relationships (remove-rbac-from-relationships (:relationships schema-info)))
        schema-info (make-fields-with-default-vals-optional schema-info)]
    (normalize-schema schema-info)))

(def scalars
  {:AnyScalar
   {:parse     (fn [value]
                 (cond
                   (string? value) value
                   (number? value) (str value)
                   (boolean? value) (str value)
                   :else (str value)))
    :serialize str}

   :EDNMap
   {:parse     (fn [value]
                 (cond
                   (map? value) value
                   (string? value) (try
                                    (read-string value)
                                    (catch Exception _
                                      ((:parse (:AnyScalar scalars)) value)))
                   :else ((:parse (:AnyScalar scalars)) value)))
    :serialize (fn [value]
                 (if (map? value)
                   (pr-str value)
                   ((:serialize (:AnyScalar scalars)) value)))}})

(def filter-input-objects
  {:EDNComparison
    {:fields
     {:eq {:type :String}
      :ne {:type :String}
      :contains {:type :String}
      :isEmpty {:type :Boolean}
      :hasKey {:type :String}}}

   :StringComparison
   {:fields
    {:eq         {:type :String}
     :ne         {:type :String}
     :in         {:type '(list :String)}
     :contains   {:type :String}
     :startsWith {:type :String}
     :endsWith   {:type :String}
     :gt         {:type :String}
     :gte        {:type :String}
     :lt         {:type :String}
     :lte        {:type :String}}}

   :IntComparison
   {:fields
    {:eq      {:type :Int}
     :ne      {:type :Int}
     :gt      {:type :Int}
     :gte     {:type :Int}
     :lt      {:type :Int}
     :lte     {:type :Int}
     :in      {:type '(list :Int)}
     :between {:type '(list :Int)}}}

   :FloatComparison
   {:fields
    {:eq      {:type :Float}
     :ne      {:type :Float}
     :gt      {:type :Float}
     :gte     {:type :Float}
     :lt      {:type :Float}
     :lte     {:type :Float}
     :in      {:type '(list :Float)}
     :between {:type '(list :Float)}}}

   :BooleanComparison
   {:fields
    {:eq  {:type :Boolean}
     :ne  {:type :Boolean}}}

   :ListComparison
   {:fields
    {:contains    {:type :AnyScalar}
     :containsAll {:type '(list :AnyScalar)}
     :containsAny {:type '(list :AnyScalar)}
     :eq          {:type '(list :AnyScalar)}
     :ne          {:type '(list :AnyScalar)}
     :isEmpty     {:type :Boolean}}}})

(defn extract-entities-inside-between-rel [relationship-name]
  "Returns names of entities part of given between relationship as a set."
  (let [found-value (get @relationships-code relationship-name)
        fields (get found-value :fields)]
    (into #{} (filter #(contains? fields %) @entity-names))))

(defn generate-comparison-type [base-type element-names relationship-names]
  (cond
    (= base-type :EDNMap) :EDNComparison
    (= base-type :Boolean) :BooleanComparison
    (= base-type :Int) :IntComparison
    (= base-type :Float) :FloatComparison
    (or (= (class base-type) PersistentList)
        (= (class base-type) LazySeq))
    (let [filter-obj-name (second base-type)
          inner-type (keyword (str/replace (name filter-obj-name) #"Filter$" ""))
          is-element-name? (contains? @element-names inner-type)]
      (if (and is-element-name? (not (contains? @relationship-names inner-type)))
        (keyword (str (name inner-type) "ListComparison"))
        :ListComparison))
    (str/includes? (name base-type) "Filter") base-type
    :else :StringComparison))

(defn generate-filter-fields [base-fields element-names relationship-names filter-name]
  (merge
   {:and {:type (list 'list filter-name)}
    :or  {:type (list 'list filter-name)}
    :not {:type filter-name}}
   (into {}
         (comp
          (map (fn [[field-name {:keys [type]}]]
                 (let [base-type (if (and (list? type) (= (first type) 'non-null))
                                   (second type)
                                   type)
                       comparison-type (generate-comparison-type base-type element-names relationship-names)]
                   [(keyword (name field-name))
                    {:type comparison-type}])))
          (filter identity))
         base-fields)))

(defn generate-list-comparison-fields [list-comparison-type]
  (let [inner-type (str/replace (name list-comparison-type) #"ListComparison$" "")]
    {:some        {:type (keyword (str inner-type "Filter"))}
     :every       {:type (keyword (str inner-type "Filter"))}
     :none        {:type (keyword (str inner-type "Filter"))}
     :count       {:type :IntComparison}
     :isEmpty     {:type :Boolean}
     :containsAll {:type (list 'list (keyword (str inner-type "Filter")))}
     :containsAny {:type (list 'list (keyword (str inner-type "Filter")))}}))

(defn create-filter-name [entity-name]
  (keyword (str (name entity-name) filter-input-object-name-postfix)))

(defn find-list-comparison-field [filter-fields]
  (first (filter (fn [[_ v]]
                   (and (map? v)
                        (:type v)
                        (keyword? (:type v))
                        (str/ends-with? (name (:type v)) "ListComparison")
                        (not= (:type v) :ListComparison)))
                 filter-fields)))

(defn add-list-comparison-fields [acc filter-name filter-fields [_ list-comparison-field]]
  (let [list-comparison-type (:type list-comparison-field)
        list-comparison-fields (generate-list-comparison-fields list-comparison-type)]
    (-> acc
        (assoc filter-name {:fields filter-fields})
        (assoc list-comparison-type {:fields list-comparison-fields}))))

(defn generate-entity-filter-input-objects [entities-records]
  (reduce-kv (fn [acc entity-name entity-spec]
               (let [filter-name (create-filter-name entity-name)
                     base-fields (-> entity-name
                                     (make-graphql-fields-optional entity-spec false)
                                     (append-postfix-to-field-names filter-input-object-name-postfix true)
                                     :fields
                                     strip-irrelevant-attributes)
                     filter-fields (generate-filter-fields base-fields element-names relationship-names filter-name)
                     list-comparison-field (find-list-comparison-field filter-fields)]
                 (if list-comparison-field
                   (add-list-comparison-fields acc filter-name filter-fields list-comparison-field)
                   (assoc acc filter-name {:fields filter-fields}))))
             {}
             entities-records))

(defn get-filter-input-objects [entities-records]
  (merge filter-input-objects (generate-entity-filter-input-objects entities-records)))

(defn generate-graphql-schema-code [schema-info]
  (let [data (preprocess-schema-info schema-info)
        records (:records data)
        entities (:entities data)
        relationships (:relationships data)]

    (initialize-atom record-names (extract-names records))
    (initialize-atom entity-names (extract-names entities))
    (initialize-atom relationship-names (extract-names relationships))
    (initialize-atom between-relationship-names (extract-between-relationship-names relationships))
    (reset! contains-graph (build-contains-relationship-graph relationships))
    (merge-element-names)

    ;; generate components
    (entities->GraphQL-schema entities entities-code)
    (entities->GraphQL-schema records records-code)
    (relationships->GraphQL-schema relationships relationships-code)

    ;; generate combined schema
    (let [combined-objects (merge @entities-code @records-code)
          queries (generate-queries @entities-code)
          create-mutations (update-child-mutations (generate-mutations @entities-code "Create" false) relationships "Create")
          update-mutations (update-child-mutations (generate-mutations @entities-code "Update" false) relationships "Update")
          delete-mutations (update-child-mutations (generate-mutations @entity-metas "Delete" true) relationships "Delete")
          query-input-objects (generate-query-input-objects combined-objects)
          create-mutation-input-objects (generate-mutation-input-objects combined-objects "Create" false false)
          update-mutation-input-objects (generate-mutation-input-objects combined-objects "Update" true false)
          delete-mutation-input-objects (generate-mutation-input-objects combined-objects "Delete" true false)
          filter-input-objects (get-filter-input-objects combined-objects)
          input-objects (merge filter-input-objects query-input-objects create-mutation-input-objects update-mutation-input-objects delete-mutation-input-objects)]
      (let [initial-schema {:objects       (merge {:Query        (:Query queries)
                                                   :Mutation     (:Mutation (merge-mutations create-mutations update-mutations delete-mutations))
                                                   :Subscription {}} combined-objects)
                            :enums         @enums-code
                            :scalars scalars
                            :input-objects input-objects}
            schema (remove-empty-graphql-constructs initial-schema)]
        [schema @entity-metas]))))

(defn generate-graphql-schema [schema-info]
  (try
    (generate-graphql-schema-code schema-info)
    (finally
      (cleanup-atoms))))
