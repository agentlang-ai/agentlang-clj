(ns agentlang.store.db-common
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [clojure.walk :as w]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.lang.internal :as li]
            [agentlang.lang.raw :as raw]
            [agentlang.store.util :as stu]
            [agentlang.store.sql :as sql]
            [agentlang.util.seq :as su]
            [agentlang.global-state :as gs]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            #?(:clj [agentlang.store.jdbc-internal :as ji])
            #?(:cljs [agentlang.store.alasql-internal :as aqi])))

(def transact-fn! #?(:clj ji/transact-fn! :cljs aqi/execute-fn!))
(def execute-fn! #?(:clj ji/execute-fn! :cljs aqi/execute-fn!))
(def execute-sql! #?(:clj ji/execute-sql! :cljs aqi/execute-sql!))
(def execute-stmt-once! #?(:clj ji/execute-stmt-once! :cljs aqi/execute-stmt-once!))
(def execute-stmt! #?(:clj ji/execute-stmt! :cljs aqi/execute-stmt!))
(def create-inst-statement #?(:clj ji/create-inst-statement :cljs aqi/upsert-inst-statement))
(def update-inst-statement #?(:clj ji/update-inst-statement :cljs aqi/upsert-inst-statement))
(def purge-by-id-statement #?(:clj ji/purge-by-id-statement :cljs aqi/delete-by-id-statement))
(def delete-by-id-statement #?(:clj ji/delete-by-id-statement :cljs aqi/delete-by-id-statement))
(def delete-all-statement #?(:clj ji/delete-all-statement :cljs aqi/delete-all-statement))
(def delete-children-statement #?(:clj ji/delete-children-statement :cljs aqi/delete-children-statement))
(def query-by-id-statement #?(:clj ji/query-by-id-statement :cljs aqi/query-by-id-statement))
(def do-query-statement #?(:clj ji/do-query-statement :cljs aqi/do-query-statement))
(def validate-ref-statement #?(:clj ji/validate-ref-statement :cljs aqi/validate-ref-statement))
(def prepare #?(:clj ji/prepare :cljs aqi/prepare))
(def close-pstmt #?(:clj ji/close-pstmt :cljs aqi/close-pstmt))

(def id-type (sql/attribute-to-sql-type :Agentlang.Kernel.Lang/UUID))

(defn compile-query [query-pattern]
  (let [ename (:from query-pattern)
        eversion (:version query-pattern)
        query-pattern (dissoc query-pattern :version)]
    (sql/format-sql
     (stu/entity-table-name ename eversion)
     (cn/view? ename)
     (if (> (count (keys query-pattern)) 2)
       (dissoc query-pattern :from)
       (let [where-clause (:where query-pattern)]
         (when (not= :* where-clause) where-clause))))))

(defn query-attributes-to-sql [recname attrs]
  (let []))

(defn- norm-constraint-name [s]
  (s/replace s "_" ""))

(defn- as-col-name [attr-name]
  (str "_" (name attr-name)))

(defn- append-fkeys [table-name [attr-name [refspec cascade-on-delete]]]
  (let [n (name attr-name)
        ename [(:component refspec) (:record refspec)]]
    (let [constraint-name (norm-constraint-name (str "_" table-name "_" n "FK"))]
      [(str "ALTER TABLE " table-name " DROP CONSTRAINT IF EXISTS " constraint-name)
       (str "ALTER TABLE " table-name " ADD CONSTRAINT " constraint-name
            " FOREIGN KEY(_" n ") "
            "REFERENCES " (stu/entity-table-name ename)
            "(_" (name (first (:refs refspec))) ")"
            (when cascade-on-delete
              " ON DELETE CASCADE"))])))

(defn- concat-sys-cols [s]
  (str s ", _" stu/deleted-flag-col " BOOLEAN DEFAULT false"))

(defn- uk [table-name col-name]
  (norm-constraint-name (str table-name col-name "UK")))

(defn- idx [table-name col-name]
  (norm-constraint-name (str table-name col-name "_IDX")))

(defn- pk [table-name]
  (norm-constraint-name (str table-name "PK")))

(defn- concat-post-init-sql! [out-table-data sqls]
  (let [d (concat (:post-init-sqls @out-table-data) sqls)]
    (swap! out-table-data assoc :post-init-sqls d)))

(defn- create-relational-table-sql [table-name entity-schema
                                    indexed-attributes unique-attributes
                                    compound-unique-attributes out-table-data]
  (let [afk (partial append-fkeys table-name)
        post-init-sql! (partial concat-post-init-sql! out-table-data)
        compound-unique-attributes (if (keyword? compound-unique-attributes)
                                     [compound-unique-attributes]
                                     compound-unique-attributes)]
    (concat
     [(str stu/create-table-prefix " " table-name " ("
           (loop [attrs (sort (keys entity-schema)), col-types [], cols ""]
             (if-let [a (first attrs)]
               (let [atype (cn/attribute-type entity-schema a)
                     sql-type (sql/attribute-to-sql-type atype)
                     is-ident (= a li/path-attr)
                     is-uk (some #{a} unique-attributes)
                     attr-ref (cn/attribute-ref entity-schema a)
                     col-name (as-col-name a)
                     uq (if is-ident
                          (str "CONSTRAINT " (pk table-name) " PRIMARY KEY")
                          (when is-uk
                            (str "CONSTRAINT " (uk table-name col-name) " UNIQUE")))]
                 #?(:clj
                    (when attr-ref
                      (post-init-sql! (afk [a attr-ref]))))
                 (recur
                  (rest attrs)
                  (conj col-types [col-name sql-type (or is-ident is-uk)])
                  (str cols (str col-name " " sql-type " " uq)
                       (when (seq (rest attrs))
                         ", "))))
               (do (swap! out-table-data assoc :columns col-types)
                   (concat-sys-cols cols))))
           (when (seq compound-unique-attributes)
             (str ", CONSTRAINT " (str table-name "_compound_uks")
                  " UNIQUE "
                  "(" (s/join ", " (mapv as-col-name compound-unique-attributes)) ")"))
           ")")]
     (when (seq indexed-attributes)
       (mapv (fn [attr]
               (let [n (as-col-name attr)]
                 (str "CREATE INDEX "
                      #?(:clj "IF NOT EXISTS "
                         :cljs "")
                      (idx table-name n) " ON " table-name "(" n ")")))
             indexed-attributes)))))

(defn- create-relational-table [connection entity-schema table-name
                                indexed-attrs unique-attributes
                                compound-unique-attributes out-table-data]
  (let [ss (create-relational-table-sql
            table-name entity-schema indexed-attrs
            unique-attributes compound-unique-attributes out-table-data)]
    (doseq [sql ss]
      (when-not (execute-sql! connection [sql])
        (u/throw-ex (str "Failed to create table - " sql))))
    table-name))

(defn- create-db-schema!
  "Create a new schema (a logical grouping of tables), if it does not already exist."
  [connection db-schema-name]
  (if (execute-sql! connection [(stu/create-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to create schema - " db-schema-name))))

(defn rename-db-table! [connection db-table-name-new db-table-name-old]
  (if (execute-sql! connection [(stu/rename-table-sql db-table-name-new db-table-name-old)])
    db-table-name-new
    (u/throw-ex (str "Failed to rename table - " db-table-name-old " to " db-table-name-new))))

(defn- drop-db-schema! [connection db-schema-name]
  (if (execute-sql! connection [(stu/drop-schema-sql db-schema-name)])
    db-schema-name
    (u/throw-ex (str "Failed to drop schema - " db-schema-name))))

(defn- create-component-meta-table-sql [table-name]
  (str "CREATE TABLE IF NOT EXISTS " table-name
       " (KEY VARCHAR(100) PRIMARY KEY, VALUE VARCHAR("
       sql/default-max-varchar-length "))"))

(defn- insert-entity-meta-sql [comp-meta-table entity-table meta-data]
  (str "INSERT INTO " comp-meta-table " VALUES ('" entity-table "', '" meta-data "')"
       " ON CONFLICT DO NOTHING"))

(defn- normalize-meta-data [[c t u]]
  [c (s/upper-case t) u])

(defn- create-relational-view [connection view-name view-query]
  (let [q (compile-query view-query)
        select (if (string? q) q (:query q))
        sql-view-init (str "DROP VIEW IF EXISTS " view-name)
        sql (str "CREATE VIEW " view-name " AS " (if (string? select) select (first select)))]
    (try 
      (execute-sql! connection [sql-view-init])
      (execute-sql! connection [sql])
      (catch Exception ex
        (.printStackTrace ex)))
    view-name))

(defn create-schema
  "Create the schema, tables and indexes for the component."
  ([datasource component-name post-init]
   (let [scmname (stu/db-schema-for-component component-name)
         table-data (atom nil)
         create-views (atom nil)
         component-meta-table (stu/component-meta-table-name component-name)]
     (execute-fn!
      datasource
      (fn [txn]
        (execute-sql! txn [(create-component-meta-table-sql component-meta-table)])
        (doseq [ename (cn/entity-names component-name false)]
          (when-not (cn/entity-schema-predefined? ename)
            (let [tabname (stu/entity-table-name ename)]
              (if-let [q (cn/view-query ename)]
                (swap! create-views conj #(create-relational-view txn tabname q))
                (let [schema (stu/find-entity-schema ename)]
                  (create-relational-table
                   txn schema tabname
                   (cn/indexed-attributes schema)
                   (cn/unique-attributes schema)
                   (cn/compound-unique-attributes ename)
                   table-data)
                  (execute-sql!
                   txn [(insert-entity-meta-sql
                         component-meta-table tabname
                         {:columns
                          (mapv normalize-meta-data (:columns @table-data))})]))))))
        (when post-init
          (doseq [sql (:post-init-sqls @table-data)]
            (execute-sql! txn [sql])))
        (doseq [cv @create-views] (cv))
        component-name))))
  ([datasource component-name]
   (create-schema datasource component-name true)))

(defn drop-schema
  "Remove the schema from the database, perform a non-cascading delete."
  [datasource component-name]
  (let [scmname (stu/db-schema-for-component component-name)]
    (execute-fn! datasource
                 (fn [txn]
                   (drop-db-schema! txn scmname)))
    component-name))

(defn drop-entity
  [datasource entity-name]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn! datasource
                 (fn [txn]
                   (let [sql (str "DROP TABLE IF EXISTS " tabname " CASCADE")]
                     (execute-sql! txn [sql]))))))

(defn- remove-unique-attributes [indexed-attrs entity-schema]
  (if-let [uq-attrs (seq (cn/unique-attributes entity-schema))]
    (set/difference (set indexed-attrs) (set uq-attrs))
    indexed-attrs))

(defn- upsert-relational-entity-instance [upsert-inst-statement create-mode
                                          datasource entity-name instance]
  (let [tabname (stu/entity-table-name entity-name)
        inst (stu/serialize-objects instance)]
    (execute-fn!
     datasource
     #(do (when create-mode
            (let [id-val (li/path-attr instance)
                  [id-attr-name id-val] (if id-val
                                          [li/path-attr id-val]
                                          (let [n (cn/identity-attribute-name entity-name)]
                                            [n (n instance)]))
                  [pstmt params] (purge-by-id-statement % tabname id-attr-name id-val)]
              (execute-stmt-once! % pstmt params)))
          (let [[pstmt params] (upsert-inst-statement % tabname nil [entity-name inst])]
            (execute-stmt-once! % pstmt params))))
    instance))

(defn upsert-instance [upsert-inst-statement create-mode datasource entity-name instance]
  (upsert-relational-entity-instance
   upsert-inst-statement create-mode datasource entity-name instance))

(def create-instance (partial upsert-instance create-inst-statement true))
(def update-instance (partial upsert-instance update-inst-statement false))

(defn- delete-inst!
  "Delete an entity instance."
  [conn tabname id-attr-name id delete-by-id-statement]
  (let [[pstmt params] (delete-by-id-statement conn tabname id-attr-name id)]
    (execute-stmt-once! conn pstmt params)))

(defn delete-children [datasource entity-name path]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (delete-children-statement conn tabname path)]
         (execute-stmt-once! conn pstmt nil))))
    entity-name))

(defn- delete-children-cascade [datasource child-entity-names path]
  (when (seq child-entity-names)
    (doseq [en child-entity-names]
      (delete-children-cascade datasource (cn/contained-children-names en) path)
      (delete-children datasource en path))))

(defn delete-by-id
  ([delete-by-id-statement datasource entity-name id-attr-name id]
   (when (cn/check-cascade-delete-children entity-name)
     (delete-children-cascade datasource (cn/contained-children-names entity-name) id))
   (let [tabname (stu/entity-table-name entity-name)]
     (execute-fn!
      datasource
      (fn [conn]
        (delete-inst! conn tabname id-attr-name id delete-by-id-statement)))
     id))
  ([datasource entity-name id-attr-name id]
   (delete-by-id delete-by-id-statement datasource entity-name id-attr-name id)))

(defn delete-all [datasource entity-name purge]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (delete-all-statement conn tabname purge)]
         (execute-stmt-once! conn pstmt nil))))
    entity-name))

(defn- raw-results [query-fns]
  (flatten (mapv u/apply0 query-fns)))

(defn- query-instances [entity-name query-fns]
  (let [results (raw-results query-fns)]
    (stu/results-as-instances entity-name results)))

(defn query-by-id
  ([query-by-id-statement datasource entity-name query-sql ids]
   (execute-fn!
    datasource
    (fn [conn]
      (query-instances
       entity-name
       (mapv #(let [[pstmt params] (query-by-id-statement conn query-sql %)]
                (fn [] (execute-stmt-once! conn pstmt params)))
             (set ids))))))
  ([datasource entity-name query-sql ids]
   (query-by-id query-by-id-statement datasource entity-name query-sql ids)))

(defn- as-table-name [entity-name]
  (keyword (stu/entity-table-name entity-name nil)))

(defn- entity-attributes-as-queries [attrs sql-alias]
  (mapv (fn [[k v]]
          (let [c (li/make-ref sql-alias (stu/attribute-column-name-kw k))]
            (if (vector? v)
              `[~(first v) ~c ~(last v)]
              [:= c v])))
        attrs))

(defn- parse-names-from-rel-query [qpat]
  [(first (filter #(cn/entity? (first %)) qpat))
   (first (filter #(cn/relationship? (first %)) qpat))])

(def ^:private path-col (stu/attribute-column-name-kw li/path-attr))
(def ^:private parent-col (stu/attribute-column-name-kw li/parent-attr))

(defn- insert-deleted-clause [w sql-alias]
  (let [f (first w)]
    (if (= :and f)
      `[~f [:= ~(li/make-ref sql-alias stu/deleted-flag-col-kw) false] ~@(rest w)]
      `[:and [:= ~(li/make-ref sql-alias stu/deleted-flag-col-kw) false] ~w])))

(defn- fix-refs [sql-alias args]
  (mapv (fn [arg]
          `[~(first arg)
            ~@(mapv #(if (keyword? %)
                       (li/make-ref sql-alias %)
                       %)
                    (rest arg))])
        args))

(defn- ref-from-canonical-name [n]
  (let [parts (li/path-parts n)
        refs (seq (:refs parts))]
    (when-not refs
      (u/throw-ex (str "Invalid attribute reference - " n)))
    (let [cn (:component parts), recname (:record parts)]
      (li/make-ref
       (if (and cn recname)
         (as-table-name (li/make-path cn recname))
         (:path parts))
       (stu/attribute-column-name-kw (first refs))))))

(defn- select-into [into-spec]
  (mapv (fn [[k v]]
          [(ref-from-canonical-name v) k])
        into-spec))

(defn- entity-column-names [entity-name sql-alias]
  (let [attr-names (cn/query-attribute-names entity-name)]
    (mapv #(li/make-ref sql-alias (stu/attribute-column-name-kw %)) attr-names)))

(def ^:private ipa-path (fn [ipa-alias] (li/make-ref ipa-alias (stu/attribute-column-name-kw :ResourcePath))))
(def ^:private ipa-user (fn [ipa-alias] (li/make-ref ipa-alias (stu/attribute-column-name-kw :Assignee))))

(def ^:private ipa-flag-cols
  {:read #(li/make-ref % (stu/attribute-column-name-kw :CanRead))
   :update #(li/make-ref % (stu/attribute-column-name-kw :CanUpdate))
   :delete #(li/make-ref % (stu/attribute-column-name-kw :CanDelete))})

(defn- maybe-add-rbac-joins
  ([oprs user entity-name read-on-entities sql-pat]
   (let [join (:join sql-pat)
         ipa-table (keyword (stu/inst-priv-table entity-name))
         ipa-alias ipa-table
         inv-privs-join
         [[ipa-table ipa-alias]
          (concat
           [:and
            [:like (li/make-ref (or (li/get-alias entity-name)
                                    (keyword (as-table-name entity-name))) path-col) (ipa-path ipa-alias)]
            [:= (ipa-user ipa-alias) user]]
           (mapv (fn [opr] [:= ((opr ipa-flag-cols) ipa-alias) true]) oprs))]
         additional-joins (when (seq read-on-entities)
                            (mapv (comp :join #(maybe-add-rbac-joins [:read] user % nil nil)) read-on-entities))]
     (assoc sql-pat :join (concat join inv-privs-join (first additional-joins)))))
  ([oprs user entity-name sql-pat]
   (maybe-add-rbac-joins oprs user entity-name nil sql-pat)))

(defn- get-alias [relname entity-name]
  (when-let [alias (li/get-alias relname entity-name)]
    (when (stu/sql-keyword? alias)
      (u/throw-ex (str "SQL keyword " alias " cannot be used as an alias")))
    alias))

(defn- between-join [src-entity relname [target-entity attrs]]
  (let [n1 (first (cn/find-between-keys relname src-entity))
        n2 (first (cn/find-between-keys relname target-entity))]
    (when-not (or n1 n2)
      (u/throw-ex (str "Query failed, "
                       "no relationship " relname " between " src-entity " and " target-entity)))
    (let [rel-alias (keyword (as-table-name relname))
          rel-ref (partial li/make-ref rel-alias)
          this-alias (or (get-alias relname src-entity) (keyword (as-table-name src-entity)))
          this-ref (partial li/make-ref this-alias)
          that-alias (or (get-alias relname target-entity) (keyword (as-table-name target-entity)))
          that-ref (partial li/make-ref that-alias)
          p (when-let [p (li/path-attr attrs)]
              (and (string? p) p))
          main-joins
          [[(as-table-name relname) rel-alias]
           [:and
            [:= (rel-ref stu/deleted-flag-col-kw) false]
            [:= (rel-ref (stu/attribute-column-name-kw n1)) (this-ref path-col)]
            [:= (rel-ref (stu/attribute-column-name-kw n2)) (or p (that-ref path-col))]]]
          sub-joins
          (when-not p
            [[(as-table-name target-entity) that-alias]
             (vec
              (concat
               [:and
                [:= (that-ref stu/deleted-flag-col-kw) false]]
               (entity-attributes-as-queries attrs that-alias)))])]
      (vec (concat sub-joins main-joins)))))

(defn- contains-join [src-entity relname [target-entity attrs]]
  (let [traverse-up? (not (cn/child-in? (li/normalize-name relname) src-entity))
        src-alias (or (get-alias relname src-entity) (keyword (as-table-name src-entity)))
        target-alias (or (get-alias relname target-entity) (keyword (as-table-name target-entity)))
        join-pat
        (concat
         [[(as-table-name target-entity) target-alias]
          (vec
           (concat
            [:and [:= (li/make-ref target-alias stu/deleted-flag-col-kw) false]]
            [(if traverse-up?
               [:= (li/make-ref target-alias parent-col) (li/make-ref src-alias path-col)]
               [:= (li/make-ref src-alias parent-col) (li/make-ref target-alias path-col)])]
            (entity-attributes-as-queries attrs target-alias)))])]
    join-pat))

(declare handle-joins-for-contains handle-joins-for-between)

(defn- handle-joins-for-contains [entity-name cjs]
  (mapv
   (fn [[relname spec]]
     (let [[target-entity _ :as sel] (:select spec)
           r0 (contains-join entity-name relname sel)
           r1 (if-let [cjs (:contains-join spec)]
                (apply concat r0 (handle-joins-for-contains target-entity cjs))
                r0)]
       (if-let [bjs (:between-join spec)]
         (apply concat r1 (handle-joins-for-between target-entity bjs))
         r1)))
   cjs))

(defn- handle-joins-for-between [entity-name bjs]
  (mapv
   (fn [[relname spec]]
     (let [[target-entity _ :as sel] (:select spec)
           r0 (between-join entity-name relname sel)
           r1 (if-let [bjs (:between-join spec)]
                (apply concat r0 (handle-joins-for-between target-entity bjs))
                r0)]
       (if-let [cjs (:contains-join spec)]
         (apply concat r1 (handle-joins-for-contains target-entity cjs))
         r1)))
   bjs))

(defn- query-from-abstract [abstract-query into-spec]
  (let [[entity-name attrs] (:select abstract-query)
        entity-alias (keyword (as-table-name entity-name))
        q0 (merge
            {:select (or (when into-spec (select-into into-spec))
                         (entity-column-names entity-name entity-alias))
             :from [[(as-table-name entity-name) entity-alias]]}
            (or (:? attrs)
                (when (seq attrs)
                  {:where (vec (concat [:and] (fix-refs entity-alias (vals attrs))))})))
        cont-joins
        (when-let [cjs (:contains-join abstract-query)]
          (vec (first (handle-joins-for-contains entity-name cjs))))
        bet-joins
        (when-let [bjs (:between-join abstract-query)]
          (vec (first (handle-joins-for-between entity-name bjs))))
        q (if (or (seq cont-joins) (seq bet-joins))
            (assoc q0 :join (concat (seq cont-joins) (seq bet-joins)))
            q0)]
    q))

(defn- query-by-attributes [datasource {entity-name :entity-name
                                        attrs :query-attributes
                                        sub-query :sub-query
                                        rbac :rbac}]
  (let [rbac-enabled? (gs/rbac-enabled?)
        [can-read-all can-update-all can-delete-all
         update-delete-tag read-on-entities]
        (when rbac-enabled?
          [(:can-read-all? rbac)
           (:can-update-all? rbac)
           (:can-delete-all? rbac)
           (:follow-up-operation rbac)
           (:read-on-entities rbac)])
        update-delete-tag
        (when update-delete-tag
          (cond
            (and (= :update update-delete-tag)
                 can-update-all) nil
            (and (= :delete update-delete-tag)
                 can-delete-all) nil
            :else update-delete-tag))
        user (when rbac-enabled? (gs/active-user))
        select-all? (and (li/query-pattern? entity-name)
                         (not (seq attrs)))
        entity-name (if select-all?
                      (li/normalize-name entity-name)
                      entity-name)
        entity-alias (keyword (as-table-name entity-name))
        into-spec (:into sub-query)
        sql-pat0 (query-from-abstract (:abstract-query sub-query) into-spec)
        w0 (:where sql-pat0)
        w1 (if w0 (insert-deleted-clause w0 entity-alias) [:= (li/make-ref entity-alias stu/deleted-flag-col-kw) false])
        sql-pat0 (assoc sql-pat0 :where w1)
        sql-pat (if rbac-enabled?
                  (if can-read-all
                    (if update-delete-tag
                      (maybe-add-rbac-joins [update-delete-tag] user entity-name sql-pat0)
                      sql-pat0)
                    (maybe-add-rbac-joins
                     (concat [:read] (when update-delete-tag [update-delete-tag]))
                     user entity-name read-on-entities sql-pat0))
                  sql-pat0)
        sql-params (sql/raw-format-sql sql-pat)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (prepare conn [(first sql-params)])
             rslt (execute-stmt-once! conn pstmt (rest sql-params))]
         (if into-spec
           rslt
           (stu/results-as-instances entity-name rslt)))))))

(defn do-query
  ([datasource query-sql query-params]
   (if-not query-sql
     (query-by-attributes datasource query-params)
     (execute-fn!
      datasource
      (fn [conn]
        (let [[pstmt params] (do-query-statement conn query-sql query-params)]
          (execute-stmt-once! conn pstmt params))))))
   ([datasource query-params] (do-query datasource nil query-params)))

(defn- query-relational-entity-by-unique-keys [datasource entity-name unique-keys attribute-values]
  (let [sql (sql/compile-to-direct-query (stu/entity-table-name entity-name) (mapv name unique-keys) :and)]
    (when-let [rows (seq (do-query datasource sql (mapv #(attribute-values %) unique-keys)))]
      (stu/result-as-instance entity-name (first rows)))))

(defn query-by-unique-keys
  "Query the instance by a unique-key value."
  ([query-by-id-statement datasource entity-name unique-keys attribute-values]
   (query-relational-entity-by-unique-keys
    datasource entity-name unique-keys attribute-values))
  ([datasource entity-name unique-keys attribute-values]
   (query-by-unique-keys nil datasource entity-name unique-keys attribute-values)))

(defn- normalize-join-results [attr-names rs]
  (let [cs (mapv #(keyword (s/upper-case (name %))) attr-names)
        irs (mapv #(into {} (mapv (fn [[k v]] [(keyword (s/upper-case (name k))) v]) %)) rs)]
    (vec
     (mapv
      (fn [r]
        (into
         {}
         (mapv
          (fn [c a]
            [a (get r c)])
          cs attr-names)))
      irs))))

(defn query-all
  ([datasource entity-name rows-to-instances query query-params]
   (let [is-raw (map? query)
         [q wa] (if is-raw
                  [(:query query)
                   (:with-attributes query)]
                  [query nil])
         [query-sql query-params] (if (vector? q)
                                    (let [pp (if is-raw
                                               ((:parse-params query) (rest q))
                                               (rest q))]
                                      [(first q) pp])
                                    [q query-params])]
     (execute-fn!
      datasource
      (fn [conn]
        (let [qfns (let [[pstmt params] (do-query-statement conn query-sql query-params)]
                     [#(execute-stmt-once! conn pstmt params)])]
          (if is-raw
            (normalize-join-results wa (raw-results qfns))
            (rows-to-instances entity-name qfns)))))))
  ([datasource entity-name query]
   (query-all datasource entity-name query-instances query nil)))

(defn- cols-spec-to-multiple-inserts [from-table to-table cols-spec]
  [(str "SELECT * FROM " from-table)
   (str "INSERT INTO " to-table " (" (s/join ", " (mapv first cols-spec)) ") VALUES "
        "(" (s/join "," (repeat (count cols-spec) \?)) ")")
   (mapv (fn [[_ c]]
           (if (and (string? c) (s/starts-with? c "_"))
             (keyword (s/upper-case c))
             c))
         cols-spec)])

(defn generate-migration-commands [from-table to-table cols-spec]
  (if (some #(fn? (second %)) cols-spec)
    (cols-spec-to-multiple-inserts from-table to-table cols-spec)
    (str "INSERT INTO " to-table " (" (s/join "," (mapv first cols-spec)) ") "
         "SELECT " (s/join "," (mapv #(let [v (second %)]
                                        (if (and (string? v) (= "NA" v))
                                          (str "'" v "'")
                                          v))
                                     cols-spec))
         " FROM " from-table)))

(defn- normalize-meta-result [r]
  (let [r (mapv (fn [[k v]]
                  [(second (li/split-path k)) v])
                r)]
    (into {} r)))

(defn- normalize-component-meta [meta]
  (let [[kk vk] (if (:KEY (first meta)) [:KEY :VALUE] [:key :value])]
    (into {} (mapv (fn [r] [(kk r) (u/parse-string (vk r))]) meta))))

(defn- load-component-meta
  ([datasource model-version component-name]
   (let [table-name (stu/component-meta-table-name component-name model-version)]
     (normalize-component-meta
      (mapv normalize-meta-result (execute-sql! datasource [(str "SELECT * FROM " table-name)])))))
  ([datasource component-name]
   (load-component-meta datasource nil component-name)))

(defn- raise-uk-change-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle unique-key conversion for " table-name "." col-name)))

(defn- raise-type-change-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle data-type conversion for " table-name "." col-name)))

(defn- raise-uk-number-error [table-name col-name]
  (u/throw-ex (str "Migration cannot automatically handle addition of unique-numeric column " table-name "." col-name)))

(defn- generate-inserts [[[from-table from-cols] [to-table to-cols]]]
  (generate-migration-commands
   from-table to-table
   (mapv
    (fn [[tc tt tu]]
      (if-let [[c t u] (first (filter #(= tc (first %)) from-cols))]
        (cond
          (and (not u) tu) (raise-uk-change-error to-table tc)
          (= tt t) [tc c]
          :else (raise-type-change-error to-table tc))
        (if tu
          (case tt
            :s [tc u/uuid-string]
            :n (raise-uk-number-error to-table tc))
          (case tt
            :s [tc "NA"]
            :n [tc 0]))))
    to-cols)))

(defn- preproc-cols [{cols :columns}]
  (mapv (fn [[c t u]]
          [c (if (or (s/starts-with? t "VARCHAR")
                     (= t "UUID"))
               :s
               :n)
           u])
        cols))

(defn- compute-diff [from-tables from-meta to-tables to-meta]
  (mapv (fn [f t] [[f (preproc-cols (get from-meta f))]
                   [t (preproc-cols (get to-meta t))]])
        from-tables to-tables))

(defn- migration-commands [datasource from-vers to-vers components]
  (let [load-from (partial load-component-meta datasource from-vers)
        load-to (partial load-component-meta datasource to-vers)]
    (mapv
     (fn [cn]
       (let [from-meta (load-from cn), to-meta (load-to cn),
             from-tables (keys from-meta)
             fvs (stu/escape-graphic-chars from-vers)
             from-base (set (map #(subs % 0 (s/index-of % fvs)) from-tables))
             to-tables (keys to-meta)
             tvs (stu/escape-graphic-chars to-vers)
             to-base (set (map #(subs % 0 (s/index-of % tvs)) to-tables))
             final-tables (mapv (fn [n] [(str n fvs) (str n tvs)]) (set/intersection to-base from-base))
             final-from-tables (mapv first final-tables)
             final-to-tables (mapv second final-tables)]
         [cn (mapv
              generate-inserts
              (compute-diff final-from-tables from-meta
                            final-to-tables to-meta))]))
     components)))

(defn- normalize-raw-results [rs]
  (mapv
   (fn [r]
     (into
      {}
      (mapv
       (fn [[k v]]
         (let [[_ n] (li/split-path k)]
           [(keyword (s/upper-case (name n))) v]))
       r)))
   rs))

(defn- execute-per-row-migration! [txn cmd]
  (when-let [rs (seq (normalize-raw-results (execute-sql! txn [(first cmd)])))]
    (let [pstmt (prepare txn [(second cmd)])
          args (nth cmd 2)]
      (try
        (doseq [r rs]
          (let [params (mapv (fn [k]
                               (cond
                                 (keyword? k) (k r)
                                 (fn? k) (k)
                                 :else k))
                             args)]
            (execute-stmt! txn pstmt params)))
        (finally
          #?(:clj (.close pstmt)))))))

(defn execute-migration [datasource progress-callback from-vers to-vers components]
  (let [commands (migration-commands datasource from-vers to-vers components)]
    (transact-fn!
     datasource
     (fn [txn]
       (doseq [[cn cmds] commands]
         (progress-callback {:component cn})
         (doseq [cmd cmds]
           (progress-callback {:command cmd})
           (if (string? cmd)
             (execute-sql! txn [cmd])
             (execute-per-row-migration! txn cmd))))))
    true))
