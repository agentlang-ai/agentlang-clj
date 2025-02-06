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
            (let [id-attr-name (cn/identity-attribute-name entity-name)
                  id-val (id-attr-name instance)
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

(defn delete-by-id
  ([delete-by-id-statement datasource entity-name id-attr-name id]
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

(defn delete-children [datasource entity-name path]
  (let [tabname (stu/entity-table-name entity-name)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (delete-children-statement conn tabname path)]
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

(defn- insert-deleted-clause [w]
  (let [f (first w)]
    (if (= :and f)
      `[~f [:= ~stu/deleted-flag-col-kw false] ~@(rest w)]
      `[:and [:= ~stu/deleted-flag-col-kw false] ~w])))

(defn- query-by-attributes [datasource [entity-name attrs]]
  (let [select-all? (and (li/query-pattern? entity-name)
                         (not (seq attrs)))
        entity-name (if select-all?
                      (li/normalize-name entity-name)
                      entity-name)
        sql-pat0
        (merge
         {:select [:*] :from [(keyword (stu/entity-table-name entity-name nil))]}
         (or (:? attrs)
             (when-not select-all?
               {:where (vec (concat [:and] (vals attrs)))})))
        w0 (:where sql-pat0)
        w1 (if w0 (insert-deleted-clause w0) [:= stu/deleted-flag-col-kw false])
        sql-pat (assoc sql-pat0 :where w1)
        sql-params (sql/raw-format-sql sql-pat)]
    (execute-fn!
     datasource
     (fn [conn]
       (let [pstmt (prepare conn [(first sql-params)])]
         (stu/results-as-instances entity-name (execute-stmt-once! conn pstmt (rest sql-params))))))))

(defn do-query [datasource query-sql query-params]
  (if-not query-sql
    (query-by-attributes datasource query-params)
    (execute-fn!
     datasource
     (fn [conn]
       (let [[pstmt params] (do-query-statement conn query-sql query-params)]
         (execute-stmt-once! conn pstmt params))))))

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
