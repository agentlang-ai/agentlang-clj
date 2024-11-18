(ns agentlang.store.sql
  "Support for translating dataflow-query patterns to generic SQL."
  (:require [clojure.string :as str]
            [clojure.walk :as w]
            [honeysql.core :as hsql]
            [agentlang.util :as u]
            [agentlang.store.util :as su]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k]))

(defn- attach-column-name-prefixes [where-clause]
  (w/prewalk
   #(if (and (keyword? %) (not (li/operator? %)))
      (keyword (su/attribute-column-name %))
      %)
   where-clause))

(defn- make-wildcard [query]
    (if (su/aggregate-query? query)
      (mapv
       #(let [c (get query %)]
          (keyword (str "%" (name %) "." (when (not= c :*) "_") (name (get query %)))))
       (keys (dissoc query :where :version)))
      [:*]))

(defn- with-deleted-flag [flag where]
  (let [clause [:= su/deleted-flag-col-kw flag]]
    (if where
      [:and clause where]
      clause)))

(def ^:private with-not-deleted-clause (partial with-deleted-flag false))
(def ^:private with-deleted-clause (partial with-deleted-flag true))

(defn- parse-join-table [entity-table-name jquery]
  (let [rec-name (li/record-name jquery)
        rec-version (li/record-version jquery rec-name)
        rec-attr (dissoc (li/record-attributes jquery) 
                         :meta :meta?)]
    [(entity-table-name rec-name rec-version) rec-attr]))

(defn normalize-jattr-name [a]
  (let [n (str a)]
    (if (str/ends-with? n "?")
      (subs n 1 (dec (count n)))
      a)))

(defn- jattrs-as-on-clause [entity-table-name attribute-column-name main-table jtable jattrs entity-version]
  (let [ss (mapv (fn [[k v]]
                   (let [p (li/path-parts v)
                         c (:component p), r (:record p)
                         ref-table (if (and c r)
                                     (entity-table-name (li/make-path c r) entity-version)
                                     main-table)]
                     (str jtable "." (attribute-column-name (normalize-jattr-name k))
                          " = " ref-table "." (attribute-column-name (first (:refs p))))))
                 jattrs)]
    (str/join " AND " ss)))

(defn- split-col-ref [s]
  (let [idx (str/last-index-of s ".")]
    [(subs s 0 idx) (subs s (inc idx))]))

(defn- with-join-attributes [entity-table-name attribute-column-name attrs entity-version]
  (reduce (fn [s [n k]]
            (let [[t c] (split-col-ref (str k))]
              (str s (if (seq s) ", " "")
                   (entity-table-name t entity-version) "." (attribute-column-name c)
                   " AS " (name n) " ")))
          "" attrs))

(defn format-join-sql
  ([entity-table-name attribute-column-name check-is-deleted table-name query]
   (let [wa (:with-attributes query)]
     (when-not wa
       (u/throw-ex (str "join requires with-attributes list - " query)))
     (let [j (:join query)
           lj (:left-join query)
           first-pat (first (or j lj))
           v (li/record-version first-pat (li/record-name first-pat))
           jinfo (mapv (partial parse-join-table entity-table-name) (or j lj))
           d (name su/deleted-flag-col-kw)
           s-join (if j " INNER JOIN " " LEFT JOIN ")
           q (str "SELECT " (with-join-attributes entity-table-name attribute-column-name wa v)
                  " FROM " table-name
                  (reduce (fn [s [jtable jattrs]]
                            (let [on-clause (jattrs-as-on-clause
                                             entity-table-name
                                             attribute-column-name table-name jtable jattrs v)]
                              (str s s-join " " jtable " ON " on-clause " ")))
                          "" jinfo)
                  (when check-is-deleted (str " WHERE " table-name "." d " = FALSE")))]
       {:with-attributes (mapv first wa) :query q})))
  ([table-name query] (format-join-sql su/entity-table-name su/attribute-column-name true table-name query)))

(defn- select-for-attrs [with-attrs]
  (mapv (fn [[n k]]
          (let [[t c] (split-col-ref (str k))
                cn (keyword (str (su/entity-table-name t) "/" (su/attribute-column-name c)))]
            [cn n]))
        with-attrs))

(defn- maybe-remove-where [qpat]
  (if (:where qpat) qpat (dissoc qpat :where)))

(defn format-sql [table-name is-view query]
  (let [qmap (map? query)]
    (if (and qmap (or (:join query) (:left-join query)))
      (format-join-sql table-name query)
      (let [wa (when qmap (:with-attributes query))
            query (if qmap (dissoc query :join :left-join :with-attributes) query)
            group-by (when qmap (:group-by query))
            query (if group-by (dissoc query :group-by) query)
            [deleted query] (if qmap
                              [(:deleted query)
                               (dissoc query :deleted)]
                              [nil query])
            with-deleted-flag (cond is-view identity
                                    deleted with-deleted-clause
                                    :else with-not-deleted-clause)
            wildcard (if wa (select-for-attrs wa) (make-wildcard query))
            interim-pattern
            (maybe-remove-where
             (if qmap
               (merge
                {:select wildcard :from [(keyword table-name)]}
                (let [clause (attach-column-name-prefixes query)
                      where (:where clause)]
                  (assoc clause :where (with-deleted-flag where))))
               (let [where-clause (attach-column-name-prefixes query)
                     p {:select wildcard
                        :from [(keyword table-name)]}]
                 (assoc p :where
                        (with-deleted-flag
                          (when where-clause
                            (let [f (first where-clause)]
                              (cond
                                (string? f)
                                [(keyword f) (keyword (second where-clause)) (nth where-clause 2)]
                                (seqable? f) f
                                :else where-clause))))))))
            final-pattern (if group-by
                            (assoc interim-pattern :group-by (mapv #(keyword (str "_" (name %))) group-by))
                            interim-pattern)
            sql (hsql/format final-pattern)]
        (if wa
          {:query sql :with-attributes (mapv first wa)}
          sql)))))

(defn- concat-where-clauses [clauses]
  (if (> (count clauses) 1)
    (reduce conj [:and] clauses)
    (first clauses)))

(defn compile-to-direct-query
  ([table-name col-names log-opr-tag]
   (let [sql (str "SELECT * FROM " table-name)
         logopr (if (= log-opr-tag :and) "AND " "OR ")]
     (if (= :* col-names)
       (str sql " WHERE _" su/deleted-flag-col " = FALSE")
       (str sql " WHERE _" su/deleted-flag-col " = FALSE AND "
            (loop [cs col-names, s ""]
              (if-let [c (first cs)]
                (recur (rest cs)
                       (str s "_" c " = ? "
                            (when (seq (rest cs))
                              logopr)))
                s))))))
  ([table-name col-names]
   (compile-to-direct-query table-name col-names :and)))

(def default-max-varchar-length "10485760")
(def ^:private default-boolean-type "BOOLEAN")

(defn as-sql-type
  ([max-varchar-length bool-type attr-type]
   (let [parts (li/split-path attr-type)
         tp (if (= (count parts) 1) (first parts) (second parts))]
     (case tp
       (:String
        :Keyword :Email
        :Password :DateTime :Date :Time :List :Edn :Any
        :Map :Path) (str "VARCHAR(" max-varchar-length ")")
       (:UUID :Identity) "UUID"
       :Int "INT"
       (:Int64 :Integer :BigInteger) "BIGINT"
       :Float "REAL"
       :Double "DOUBLE PRECISION"
       :Decimal "DECIMAL"
       :Boolean bool-type
       nil)))
  ([attr-type] (as-sql-type default-max-varchar-length default-boolean-type attr-type)))

(defn attribute-to-sql-type
  ([max-varchar-length bool-type attribute-type]
   (if-let [root-type (k/find-root-attribute-type attribute-type)]
     (if-let [tp (as-sql-type root-type)]
       tp
       (u/throw-ex (str "SQL type mapping failed for " attribute-type
                        ", root type is " root-type)))
     (str "VARCHAR(" max-varchar-length ")")))
  ([attribute-type]
   #?(:clj
      ;; For postgres
      (attribute-to-sql-type default-max-varchar-length default-boolean-type attribute-type)
      ;(attribute-to-sql-type Integer/MAX_VALUE "BOOLEAN" "DATE" attribute-type)
      :cljs (attribute-to-sql-type (.-MAX_SAFE_INTEGER js/Number) "BOOLEAN" "DATE" attribute-type))
   ))
