(ns agentlang.compiler
  "Compile dataflow patterns to calls into the resolver protocol."
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [clojure.pprint :as pp]
            [agentlang.util :as u]
            [agentlang.util.graph :as ug]
            [agentlang.util.seq :as us]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.lang.internal :as li]
            [agentlang.lang.opcode :as op]
            [agentlang.lang.syntax :as ls]
            [agentlang.paths :as paths]
            [agentlang.paths.internal :as pi]
            [agentlang.compiler.context :as ctx]
            [agentlang.component :as cn]
            [agentlang.meta :as mt]
            [agentlang.env :as env]
            [agentlang.store :as store]
            [agentlang.store.util :as stu]
            [agentlang.evaluator.state :as es]
            [agentlang.global-state :as gs]
            [agentlang.compiler.rule :as rule]
            [agentlang.compiler.validation :as cv]
            [agentlang.compiler.internal :as i]))

(defn make-context
  ([with-types]
   (or ctx/dynamic-context (ctx/make with-types)))
  ([] (make-context nil)))

(def ^:dynamic active-event-name nil) ; event for which dataflow is being compiled

(defn- active-event-is-built-in? []
  (when-let [[_ n] active-event-name]
    (s/index-of (str n) "_")))

(def ^:private emit-load-literal op/load-literal)
(def ^:private emit-load-instance-by-name op/load-instance)

(defn- emit-load-references [[rec-name alias :as n] refs validate?]
  (when (if validate? (cv/validate-references rec-name refs) true)
    (op/load-references [n refs])))

(defn- emit-match [match-pattern-code cases-code alternative-code alias]
  (op/match [match-pattern-code cases-code alternative-code alias]))

(defn- emit-for-each [bind-pattern-code elem-alias body-code alias]
  (op/for-each [bind-pattern-code elem-alias body-code alias]))

(defn- emit-delete [recname id-pat-code]
  (op/delete-instance [recname id-pat-code]))

(defn- emit-try [rethrow? body handlers alias-name]
  (op/try_ [rethrow? body handlers alias-name]))

(def ^:private runtime-env-var '--env--)
(def ^:private current-instance-var '--inst--)

(declare expr-with-arg-lookups)

(defn- reference-lookup-call [n]
  (let [parts (li/path-parts n)]
    (cond
      (:path parts)
      `(if-let [r# (get ~current-instance-var ~n)]
         r#
         (let [result# (agentlang.env/lookup-by-alias ~runtime-env-var ~(:path parts))
               r# (if (map? result#) result#
                      (if (i/const-value? result#)
                        result#
                        (if (seqable? result#) (first result#) result#)))]
           (if-let [refs# '~(seq (:refs parts))]
             (get-in r# refs#)
             result#)))

      (seq (:refs parts))
      `(first (agentlang.env/follow-reference ~runtime-env-var ~parts))

      :else
      `(agentlang.env/lookup-instance ~runtime-env-var [(:component ~parts) (:record ~parts)]))))

(defn- arg-lookup [arg]
  (cond
    (i/const-value? arg) arg

    (vector? arg)
    (vec (expr-with-arg-lookups arg))

    (seqable? arg)
    (expr-with-arg-lookups arg)

    (li/name? arg)
    (reference-lookup-call arg)

    (symbol? arg)
    `(agentlang.env/lookup-variable ~runtime-env-var ~arg)

    :else arg))

(defn- map-values-as-exprs [m]
  (let [r (mapv (fn [[k v]]
                  [k (arg-lookup v)])
                m)]
    (into {} r)))

(defn- make-map-expr [expr]
  (let [inst-pat? (li/instance-pattern? expr)
        m (if inst-pat? (li/instance-pattern-attrs expr) expr)
        m-with-exprs (map-values-as-exprs m)]
    (if inst-pat?
      `(agentlang.component/make-instance
        ~(li/instance-pattern-name expr)
        ~m-with-exprs)
      m-with-exprs)))

(defn- expr-with-arg-lookups [expr]
  (cond
    (i/const-value? expr) expr
    (map? expr) (make-map-expr expr)
    (seqable? expr)
    (cond
      (li/quoted? expr)
      (w/prewalk #(if (li/unquoted? %)
                    (if (> (count %) 2)
                      (u/throw-ex (str "too many elements in un-quoted pattern - " %))
                      (expr-with-arg-lookups (second %)))
                    %)
                 expr)
      (not (seq expr)) expr
      :else
      (let [final-args (map arg-lookup (rest expr))]
        `(~(first expr) ~@final-args)))
    :else (arg-lookup expr)))

(defn- expr-as-fn [expr]
  (li/evaluate
   `(fn ~[runtime-env-var current-instance-var]
      ~expr)))

(defn- query-param-lookup [p]
  (let [r (arg-lookup p)]
    (if (i/const-value? r)
      r
      (expr-as-fn r))))

(declare query-param-process)

(defn- param-process-seq-query [attrname query]
  (if (vector? (first query))
    (mapv #(query-param-process [attrname %]) query)
    (concat
     [(first query)]
     (concat [attrname] (mapv query-param-lookup (rest query))))))

(defn- logical-query? [q]
  (and (seqable? q)
       (let [f (first q)]
         (or (= :and f) (= :or f)))))

(declare compound-expr-as-fn)

(defn- query-param-process [[k v]]
  (if (logical-query? v)
    (let [opr (first v)]
      `[~opr ~@(map #(query-param-process [k %]) (rest v))])
    (cond
      (i/const-value? v) [k v]
      (seqable? v)
      (if (symbol? (first v))
        [k (compound-expr-as-fn v)]
        (vec (param-process-seq-query k v)))
      :else [k (query-param-lookup v)])))

(defn- fetch-compile-query-fn [ctx]
  (or (ctx/fetch-compile-query-fn ctx)
      (store/get-default-compile-query)))

(defn- recname-from-opcode [opc]
  (first (:arg opc)))

(defn- compiled-query-from-opcode [opc]
  (stu/compiled-query (second (:arg opc))))

(defn- as-opcode-map [opc]
  (let [r (if (map? opc)
            (:opcode opc)
            (:opcode (first opc)))]
    (if (map? r)
      r
      (first r))))

(declare compile-pattern)

(defn- remove-meta-query [query]
  (cond
    (map? query) (dissoc query :meta :meta?)
    (coll? query) (into [] (filter #(not= (first %) :meta) query))
    :else query))

(defn- get-version-query [query]
  (cond
    (map? query) (get-in query [:meta :version])
    (coll? query)
    (get-in
     (first (filter #(= (first %) :meta) query))
     [1 :version])
    :else nil))

(defn- compile-relational-entity-query [ctx entity-name query]
  (let [version (get-version-query query)
        query (remove-meta-query query)
        q (i/expand-query
           entity-name
           (mapv query-param-process query))
        q (assoc q :version version)
        final-cq ((fetch-compile-query-fn ctx) q)]
    (stu/package-query q final-cq)))

(defn compile-query [ctx entity-name query]
  (let [q (compile-relational-entity-query ctx entity-name query)]
    (ctx/put-fresh-record! ctx entity-name {})
    q))

(defn- compound-expr-as-fn
  "Compile compound expression to a function.
   Arguments are tranlated to lookup calls on
   the runtime context."
  [expr]
  (expr-as-fn (expr-with-arg-lookups expr)))

(defn- build-dependency-graph [attr-pats ctx schema graph]
  ;; make parser/build-dependency-graph callable from util/apply->
  (let [result-graph (i/build-dependency-graph attr-pats ctx schema graph)]
    [ctx schema result-graph]))

(def ^:private appl (partial u/apply-> last))

(defn- normalize-attrs-for-full-query [attrs]
  (let [new-attrs (mapv (fn [[k v]] [(li/normalize-name k) v]) attrs)]
    (into {} new-attrs)))

(defn- normalize-meta-query-pat-attr [pat-attrs]
  (if (:meta pat-attrs)
    (dissoc (assoc pat-attrs :meta? (:meta pat-attrs)) :meta)
    pat-attrs))

(defn- parse-attributes
  "Classify attributes in the pattern as follows:
    1. computable - values can be computed at compile-time, e.g literals.
    2. references - values are references to other attributes, that can be reached from the dataflow.
    3. compound - values has to be computed at runtime, by invoking a function.
    4. queries - a query has to be issued on this attribute.

    Any value or reference is validated against the schema, raise an exception on failure.

    A graph of dependencies is prepared for each attribute. If there is a cycle in the graph,
    raise an error. Otherwise return a map with each attribute group and their attached graphs."
  [ctx pat-name pat-attrs schema version args]
  (if (:full-query? args)
    {:attrs (assoc pat-attrs :query (compile-query
                                     ctx pat-name
                                     (normalize-attrs-for-full-query pat-attrs)))}
    (let [pat-attrs (normalize-meta-query-pat-attr pat-attrs) 
          {computed :computed refs :refs
           compound :compound query :query
           :as cls-attrs} (i/classify-attributes ctx pat-attrs schema version)
          fs (mapv #(partial build-dependency-graph %) [refs compound query])
          deps-graph (appl fs [ctx schema ug/EMPTY])
          compound-exprs (mapv (fn [[k v]] [k (compound-expr-as-fn v)]) compound)
          parsed-refs (mapv (fn [[k v]] [k (if (symbol? v) {:refs v} (li/path-parts v))]) refs)
          compiled-query (when query (compile-query ctx pat-name query))
          final-attrs (if (seq compiled-query)
                        (assoc cls-attrs :query compiled-query)
                        cls-attrs)]
      {:attrs (assoc final-attrs :compound compound-exprs :refs parsed-refs)
       :deps deps-graph})))

(def ^:private set-attr-opcode-fns {:computed op/set-literal-attribute
                                    :refs op/set-ref-attribute
                                    :compound op/set-compound-attribute})

(defn- begin-build-instance [rec-name attrs args]
  (if-let [q (:query attrs)]
    (op/query-instances [rec-name q (:filter-by args)])
    (op/new-instance rec-name)))

(declare compile-list-literal)

(defn- set-literal-attribute [ctx [aname valpat :as attr]]
  (if (vector? valpat)
    (compile-list-literal ctx aname valpat)
    (op/set-literal-attribute attr)))

(defn- build-record-for-upsert? [attrs]
  (when (or (seq (:compound attrs))
            (seq (:computed attrs))
            (seq (:sorted attrs)))
    true))

(defn- emit-build-record-instance [ctx rec-name attrs schema args]
  (let [alias (:alias args)
        event? (= (cn/type-tag-key args) :event)
        timeout-ms (:timeout-ms args)
        local-ref? #(:path (second %))
        ext-refs (filter (complement local-ref?) (:refs attrs))
        local-refs (filter local-ref? (:refs attrs))]
    (concat [(begin-build-instance rec-name attrs args)]
            (distinct
             (apply
              concat
              [(mapv #(op/set-ref-attribute %) ext-refs)
               (mapv (partial set-literal-attribute ctx) (:computed attrs))
               (let [f (:compound set-attr-opcode-fns)] (mapv #(f %) (:compound attrs)))
               (mapv (fn [[k v]] ((k set-attr-opcode-fns) v)) (:sorted attrs))
               (mapv #(op/set-ref-attribute %) local-refs)]))
            [(if event?
               (op/intern-event-instance
                [rec-name alias (ctx/fetch-with-types ctx)
                 timeout-ms])
               (op/intern-instance
                (vec
                 (concat
                  [rec-name alias attrs]
                  (if (ctx/build-partial-instance? ctx)
                    [false false]
                    [true (and (cn/entity? rec-name)
                               (build-record-for-upsert? attrs))])))))])))

(defn- sort-attributes-by-dependency [attrs deps-graph]
  (let [sorted (i/sort-attributes-by-dependency attrs deps-graph)
        compound (i/left-out-from-sorted :compound attrs sorted)]
    (assoc attrs :sorted sorted :compound compound)))

(defn- emit-realize-instance
  "Emit opcode for realizing a fully-built instance of a record, entity or event.
  It is assumed that the opcodes for setting the individual attributes were emitted
  prior to this."
  [ctx pat-name pat-attrs schema version args]
  (when-let [xs (cv/invalid-attributes (dissoc pat-attrs :meta :meta?) schema)]
    (if (= (first xs) cn/id-attr)
      (if (= (get schema cn/type-tag-key) :record)
        (u/throw-ex (str "Invalid attribute " cn/id-attr " for type record: " pat-name))
        (u/throw-ex (str "Wrong reference of id in line: " pat-attrs "of " pat-name)))
      (u/throw-ex (str "Invalid attributes in pattern - " xs))))
  (let [{attrs :attrs deps-graph :deps} (parse-attributes ctx pat-name pat-attrs schema version args)
        sorted-attrs (sort-attributes-by-dependency attrs deps-graph)]
    (emit-build-record-instance ctx pat-name sorted-attrs schema args)))

(defn- emit-dynamic-upsert [ctx pat-name pat-attrs _ _ args]
  (op/dynamic-upsert
   [pat-name pat-attrs (partial compile-pattern ctx) (:alias args)]))

(defn- emit-realize-map-literal [_ pat]
  (emit-load-literal pat))

(defn- compile-fetch-all-query
  "Generate code for the wildcard query pattern (:EntityName?) to retrieve
  all instances of an entity. For an SQL backend, this will make the store do
  a `SELECT * FROM entity_table`."
  [ctx pat]
  (let [entity-name (li/split-path (li/query-target-name pat))
        q (compile-query ctx entity-name nil)]
    (op/query-instances [entity-name q nil])))

(defn- compile-pathname
  ([ctx pat alias]
   (if (li/query-pattern? pat)
     (compile-fetch-all-query ctx pat)
     (let [{component :component record :record refs :refs
            path :path :as parts} (if (map? pat) pat (li/path-parts pat))]
       (if path
         (if (= path pat)
           (emit-load-instance-by-name [path path])
           (if-let [p (ctx/dynamic-type ctx (ctx/aliased-name ctx path))]
             (if (= path p)
               (if refs
                 (emit-load-references [path path] refs false)
                 (emit-load-instance-by-name [path path]))
               (compile-pathname ctx (assoc (li/path-parts p) :refs refs) path))
             (u/throw-ex (str "ambiguous reference - " pat))))
         (let [n (ctx/dynamic-type ctx [component record])
               opc (and (cv/find-schema n)
                        (if refs
                          (emit-load-references [n alias] refs true)
                          (emit-load-instance-by-name [n alias])))]
           (ctx/put-record! ctx n {})
           opc)))))
  ([ctx pat] (compile-pathname ctx pat nil)))

(defn- process-complex-query [v]
  (if (li/name? v)
    (let [parts (li/path-parts v)]
      (if (seq (:refs parts))
        (expr-as-fn (arg-lookup v))
        v))
    v))

(defn- query-entity-name [k]
  (let [sk (str k)]
    (when-not (s/ends-with? sk "?")
      (u/throw-ex (str "queried entity-name must end with a `?` - " k)))
    (keyword (subs (apply str (butlast sk)) 1))))

(defn- cleanup-join [pat]
  (dissoc pat :join :left-join :with-attributes))

(defn- ensure-with-attributes [entity-name attrs]
  (if (seq attrs)
    attrs
    (let [anames (cn/entity-attribute-names entity-name)
          prefix (subs (str entity-name) 1)]
      (into {} (mapv (fn [a] [a (keyword (str prefix "." (name a)))]) anames)))))

(defn compile-complex-query
  "Compile a complex query. Invoke the callback
  function with the compiled query as argument.
  The default behavior is to pass the compiled query
  to the query-instances opcode generator"
  ([ctx pat callback]
   (let [k (first (keys (cleanup-join pat)))
         q (k pat)
         version (get-in q [:meta :version])
         n (if ctx
             (ctx/dynamic-type ctx (query-entity-name k))
             (query-entity-name k))]
     (when-not (cn/find-entity-schema n version)
       (u/throw-ex (str "cannot query undefined entity - " n)))
     (let [q (dissoc q :meta :meta?)
           w (when (seq (:where q))
               (w/postwalk process-complex-query (:where q)))
           j (seq (:join pat))
           lj (when-let [lj (seq (:left-join pat))]
                (when j (u/throw-ex (str "join and left-join cannot be mixed - " pat)))
                (vec lj))
           fp (assoc q :from n :where w
                     :join j :left-join lj
                     :version version
                     :with-attributes (if (or j lj)
                                        (ensure-with-attributes
                                         (li/normalize-name k)
                                         (:with-attributes pat))
                                        (:with-attributes q)))]
       (if-let [cq (and ctx (fetch-compile-query-fn ctx))]
         (let [c (stu/package-query fp (cq fp))]
           (callback [(li/split-path n) c nil]))
         (if callback (callback fp) fp)))))
  ([ctx pat]
   (compile-complex-query ctx pat op/query-instances))
  ([pat] (compile-complex-query nil pat nil)))

(defn- query-map->command [pat]
  (if-let [alias (:as pat)]
    [(dissoc pat :as) :as alias]
    [pat]))

(defn- fetch-with-types [pat]
  (when-let [wt (ctx/with-types-tag pat)]
    (when-not (map? wt)
      (u/throw-ex (str "with-types expects a map " - wt)))
    (doseq [[base-type subtype] wt]
      (when-not (cn/inherits? base-type subtype)
        (u/throw-ex
         (str "error in with-types - "
              subtype " is not a subtype of "
              base-type " in " wt))))
    wt))

(defn- from-pattern-typename [pat]
  (first (keys (li/normalize-upsert-pattern pat))))

(defn- from-pattern? [pat]
  (and (:from pat)
       (map? ((from-pattern-typename pat) pat))))

(defn- compile-from-pattern [ctx pat]
  (let [typ (from-pattern-typename pat)]
    (when-not (cn/find-object-schema typ)
      (u/throw-ex (str "undefined type " typ " in " pat)))
    (when (li/rel-tag pat)
      (u/throw-ex (str "cannot mix :from and " li/rel-tag)))
    (let [f (:from pat)
          inst-alias (:as pat)
          opcode (if (or (li/pathname? f) (map? f))
                   (compile-pattern ctx f)
                   (u/throw-ex (str "invalid :from specification " f)))]
      (when inst-alias
        (ctx/add-alias! ctx inst-alias))
      (op/instance-from
       [(li/split-path (ctx/dynamic-type ctx typ))
        (let [np (li/normalize-upsert-pattern pat)]
          (when-let [p (seq (first (vals np)))]
            (ctx/with-build-partial-instance
              ctx
              #(compile-pattern ctx np))))
        opcode inst-alias]))))

(defn- package-opcode [code pat]
  (if (and (map? code) (:opcode code))
    code
    {:opcode code :pattern pat}))

(defn- some-query-attrs? [attrs]
  (when (some li/query-pattern? (keys attrs))
    attrs))

(defn- extract-filter-patterns [spec]
  (when-let [ps (seq
                 (filter
                  #(let [r (first %)]
                     (and (map? r)
                          (li/query-instance-pattern? r)))
                  spec))]
    (vec ps)))

(defn- extract-query-attrs [obj]
  (when-let [filter-pat (and (seqable? obj) (first obj))]
    (when (map? filter-pat)
      (set (mapv #(li/normalize-name (first %))
                 (filter #(li/query-pattern? (first %)) (li/record-attributes filter-pat)))))))

(defn- complex-query-pattern? [pat]
  (when-not (ls/rel-tag pat)
    (let [ks (keys (li/normalize-instance-pattern pat))]
      (and (= 1 (count ks))
           (s/ends-with? (str (first ks)) "?")))))

(defn- query-with-join-pattern? [pat]
  (or (:join pat) (:left-join pat)))

(declare compile-query-command)

(defn- compile-map [ctx pat] 
  (cond
    (complex-query-pattern? pat)
    (let [[k v] [(first (keys pat)) (first (vals pat))]] 
      (if (pi/proper-path? v)
        (compile-map ctx {(li/normalize-name k) {li/path-query-tag v}})
        (compile-query-command ctx (query-map->command pat))))

    (query-with-join-pattern? pat)
    (compile-query-command ctx (query-map->command pat))

    (from-pattern? pat)
    (compile-from-pattern ctx pat)

    (li/instance-pattern? pat)
    (let [rel-spec (li/rel-tag pat)
          [filter-pats pat]
          (if rel-spec
            [(extract-filter-patterns rel-spec) (dissoc pat li/rel-tag)]
            [nil pat])
          orig-nm (ctx/dynamic-type ctx (li/instance-pattern-name pat))
          full-nm (li/normalize-name orig-nm)
          rec-version (or (get-in pat [orig-nm :meta :version])
                          (get-in pat [orig-nm :meta? :version]))
          {component :component record :record
           path :path refs :refs :as parts} (li/path-parts full-nm)
          refs (seq refs)
          nm (if (or path refs)
               parts
               [component record])
          attrs (li/instance-pattern-attrs pat)
          is-query-upsert (or (li/query-pattern? orig-nm)
                              (some li/query-pattern? (keys attrs)))
          alias (:as pat)
          timeout-ms (ls/timeout-ms-tag pat)
          [tag scm] (if (or path refs)
                      [:dynamic-upsert nil]
                      (cv/find-schema nm full-nm rec-version))]
      (let [c (case tag
                (:entity :record) emit-realize-instance
                :event (do
                         (when-let [wt (fetch-with-types pat)]
                           (ctx/bind-with-types! ctx wt))
                         emit-realize-instance)
                :dynamic-upsert emit-dynamic-upsert
                (u/throw-ex (str "not a valid instance pattern - " pat)))
            args (merge {:alias alias cn/type-tag-key tag
                         :full-query? (and (= tag :entity)
                                           (li/query-pattern? orig-nm))}
                        (when timeout-ms
                          {:timeout-ms timeout-ms})
                        (when filter-pats
                          {:filter-by
                           (mapv #(let [opc (compile-pattern ctx %)
                                        qattrs (extract-query-attrs %)]
                                    {:opcodes opc :query-attrs qattrs})
                                 filter-pats)}))
            opc (c ctx nm attrs scm rec-version args)]
        (ctx/put-record! ctx nm pat)
        (when alias
          (let [alias-name (ctx/alias-name alias)]
            (ctx/add-alias! ctx (or nm alias-name) alias)))
        opc))

    :else
    (emit-realize-map-literal ctx pat)))

(defn- compile-user-macro [ctx pat]
  (let [m (first pat)]
    (if (li/macro-name? m)
      (u/throw-ex (str "macro not found - " m))
      (u/throw-ex (str "not a valid macro name - " m)))))

(defn special-form-alias [pat]
  (let [rpat (reverse pat)]
    (if (= :as (second rpat))
      [(vec (reverse (nthrest rpat 2))) (first rpat)]
      [pat nil])))

(defn- maybe-single-pat [p]
  (if (and (vector? p) (= 1 (count p)))
    (first p)
    p))

(declare preproc-patterns)

(defn- normalize-and-preproc
  ([pat]
   (maybe-single-pat
    (preproc-patterns
     (if (vector? pat)
       (if (li/registered-macro? (first pat))
         [pat]
         pat)
       [pat]))))
  ([pat remove-alias]
   (let [p (normalize-and-preproc pat)]
     (if (map? p)
       (dissoc p :as)
       p))))

(defn- compile-for-each-body [ctx body-pats]
  (ctx/add-alias! ctx :% :%)
  (let [new-pats (preproc-patterns body-pats)]
    (mapv #(compile-pattern ctx %) new-pats)))

(defn- parse-for-each-match-pattern [pat]
  (if (vector? pat)
    (if (= :as (second pat))
      [(first pat) (nth pat 2)]
      [pat nil])
    [pat nil]))

(defn- compile-for-each-match-pattern [ctx pat]
  (let [[pat alias] (parse-for-each-match-pattern pat)]
    (when alias
      (ctx/add-alias! ctx alias))
    [(compile-pattern ctx (normalize-and-preproc pat true)) alias]))

(defn- compile-for-each [ctx pat]
  (let [[bind-pat-code elem-alias]
        (compile-for-each-match-pattern ctx (first pat))
        [body-pats alias] (special-form-alias (rest pat))
        body-code (compile-for-each-body ctx body-pats)]
    (when alias
      (ctx/add-alias! ctx alias))
    (emit-for-each bind-pat-code elem-alias body-code alias)))

(defn- extract-match-clauses [pat]
  (let [[pat alias] (special-form-alias pat)]
    (loop [pat pat, result []]
      (if (seq pat)
        (let [case-pat (first pat), conseq (first (rest pat))]
          (if conseq
            (recur (nthrest pat 2) (conj result [case-pat conseq]))
            [result case-pat alias]))
        [result nil alias]))))

(defn- compile-maybe-pattern-list [ctx pat]
  (mapv #(compile-pattern ctx %)
        (if (vector? pat)
          (if (li/registered-macro? (first pat))
            [pat]
            pat)
          [pat])))

(defn- compile-match-cases [ctx cases]
  (loop [cases cases, cases-code []]
    (if-let [[case-pat conseq] (first cases)]
      (recur
       (rest cases)
       (conj
        cases-code
        [[(compile-pattern ctx case-pat)]
         [(compile-maybe-pattern-list ctx (normalize-and-preproc conseq))]]))
      cases-code)))

(defn- case-match?
  "If the first component of the match is a name or literal, it's a
  normal match expression (similar to Clojure `case`),
  otherwise it's a conditional expression.
  Return true for a normal match expression."
  [pat]
  (let [f (first pat)]
    (or (li/name? f)
        (li/literal? f))))

(defn- generate-cond-code [ctx pat]
  (loop [clauses pat, code []]
    (if-let [c (first clauses)]
      (if-not (seq (rest clauses))
        {:clauses code :else (compile-maybe-pattern-list ctx (normalize-and-preproc c))}
        (recur (nthrest clauses 2)
               (conj
                code
                [(rule/compile-rule-pattern c)
                 (compile-maybe-pattern-list ctx (normalize-and-preproc (second clauses)))])))
      {:clauses code})))

(defn- compile-match-cond [ctx pat]
  (let [[pat alias] (special-form-alias pat)
        code (generate-cond-code ctx pat)]
    (when alias
      (ctx/add-alias! ctx alias))
    (emit-match nil (:clauses code) (:else code) alias)))

(defn- compile-match [ctx pat]
  (if (case-match? pat)
    (let [match-pat-code (compile-pattern ctx (first pat))
          [cases alternative alias] (extract-match-clauses (rest pat))
          cases-code (compile-match-cases ctx cases)
          alt-code (when alternative
                     (compile-maybe-pattern-list
                      ctx (normalize-and-preproc alternative)))]
      (when alias
        (ctx/add-alias! ctx alias))
      (emit-match [match-pat-code] cases-code [alt-code] alias))
    (compile-match-cond ctx pat)))

(defn- compile-try-handler [ctx [k pat]]
  (when-not (op/result-tag? k)
    (u/throw-ex (str "invalid try handler " k)))
  [k (compile-maybe-pattern-list ctx pat)])

(defn- distribute-handler-keys [handler-spec]
  (loop [hs handler-spec, final-spec {}]
    (if-let [[k v] (first hs)]
      (recur (rest hs)
             (if (vector? k)
               (reduce #(assoc %1 %2 v) final-spec k)
               (assoc final-spec k v)))
      final-spec)))

(defn- compile-construct-with-handlers
  ([ctx pat default-handlers]
   (let [body (mapv #(compile-pattern ctx %) (preproc-patterns [(first pat)]))
         hpats (us/flatten-map (rest pat))
         handler-pats (if (seq hpats)
                        (distribute-handler-keys
                         (into {} (map vec (partition 2 hpats))))
                        default-handlers)
         handlers (mapv (partial compile-try-handler ctx) handler-pats)]
     (when-not (seq handlers)
       (u/throw-ex (str "missing status handlers in " (u/pretty-str pat))))
     [body (into {} handlers)]))
  ([ctx pat] (compile-construct-with-handlers ctx pat nil)))

(defn- try-alias [pat]
  (let [rpat (reverse pat)]
    (if (= :as (second rpat))
      [(vec (reverse (nthrest rpat 2))) (first rpat)]
      [pat nil])))

(defn- compile-try
  ([rethrow? ctx pat]
   (let [[pat alias-name] (try-alias pat)
         [body handlers] (compile-construct-with-handlers ctx pat)]
     (when alias-name
       (ctx/add-alias! ctx alias-name))
     (emit-try rethrow? body handlers alias-name)))
  ([ctx pat] (compile-try false ctx pat)))

(defn- valid-alias-name? [alias]
  (if (vector? alias)
    (every? #(if (vector? %)
               (every? li/name? %)
               (li/name? %))
            alias)
    (li/name? alias)))

(defn- compile-query-pattern [ctx query-pat alias]
  (when-not (or (map? query-pat) (li/name? query-pat))
    (u/throw-ex (str "invalid query pattern - " query-pat)))
  (let [path (if (keyword? query-pat)
               query-pat
               (when-let [n (query-entity-name (first (keys query-pat)))]
                 (ctx/put-record! ctx (li/split-path n) true)
                 n))
        [nm refs] (when (li/name? path)
                    (let [{component :component
                           record :record refs :refs}
                          (li/path-parts path)]
                      [[component record] refs]))]
    (when alias
      (when-not (valid-alias-name? alias)
        (u/throw-ex (str "not a valid name - " alias)))
      (let [alias-name (ctx/alias-name alias)]
        (ctx/add-alias! ctx (or nm alias-name) alias)))
    (op/evaluate-query
     [#(compile-complex-query
        ctx
        (if (map? query-pat)
          query-pat
          (%2 nm refs))
        identity)
      alias])))

(defn- query-by-function [query-pat]
  (when (map? query-pat)
    (let [k (query-entity-name (first (keys query-pat)))
          f (first (vals query-pat))]
      (when (and (li/name? k) (fn? f))
        [k f]))))

(defn- compile-query-command
  "Compile the command [:query pattern :as result-alias].
   `pattern` could be a query pattern or a reference, making
  it possible to dynamically execute queries received via events.
  If `result-alias` is provided, the query result is bound to that name
  in the local environment"
  [ctx pat]
  (let [query-pat (first pat)
        alias (when (= :as (second pat))
                (nth pat 2))]
    (if-let [[entity-name qfn] (query-by-function query-pat)]
      (op/evaluate-query [(fn [env _]
                            (let [q (stu/package-query (qfn (partial env/lookup env)))]
                              [(li/split-path entity-name)
                               (if (string? q)
                                 [q]
                                 q)]))
                          alias])
      (compile-query-pattern ctx query-pat alias))))

(defn- compile-delete [ctx [recname & id-pat]]
  (cond
    (= (vec id-pat) [:*])
    (emit-delete (li/split-path recname) :*)

    (= (vec id-pat) [:purge])
    (emit-delete (li/split-path recname) :purge)

    :else
    (let [p (first id-pat)
          qpat (if (map? p)
                 p
                 [[(cn/identity-attribute-name recname) p]])
          alias (when (> (count id-pat) 1)
                  (if (= :as (second id-pat))
                    (nth id-pat 2)
                    (u/throw-ex (str "expected alias declaration, found " (second id-pat)))))
          q (compile-query
             ctx recname
             (if (map? qpat)
               (into [] qpat)
               qpat))]
      (when alias
        (ctx/add-alias! ctx recname alias))
      (emit-delete (li/split-path recname) (merge q {ls/alias-tag alias})))))

(defn- compile-quoted-expression [ctx exp]
  (if (li/unquoted? exp)
    (if (> (count exp) 2)
      (u/throw-ex (str "cannot compile rest of unquoted expression - " exp))
      (compile-pattern ctx (second exp)))
    exp))

(defn- compile-quoted-list [ctx pat]
  (w/prewalk (partial compile-quoted-expression ctx) pat))

(defn- compile-await [ctx pat]
  (op/await_ (compile-construct-with-handlers ctx pat {:ok "done"})))

(defn- maybe-as-eval-event [pat]
  (let [f (first pat)]
    (if (or (map? f) (keyword? f))
      `[(agentlang.evaluator/safe-eval ~f) ~@(rest pat)]
      pat)))

(defn- compile-eval [ctx pat]
  (let [pat (maybe-as-eval-event pat)
        m (us/wrap-to-map (rest pat))
        ret-type (:check m)
        result-alias (:as m)]
    (when (keyword? ret-type)
      (ctx/put-fresh-record! ctx (li/split-path ret-type) {}))
    (when result-alias
      (ctx/add-alias! ctx (or ret-type result-alias) result-alias))
    (op/eval_
     [(expr-as-fn (expr-with-arg-lookups (first pat)))
      ret-type result-alias])))

(defn- compile-rethrow-after [ctx pat]
  (op/rethrow-after [(compile-pattern ctx (first pat))]))

(defn- compile-map-entry-in-path
  ([p is-root]
   (let [n (li/record-name p)
         attrs (li/record-attributes p)]
     (when-not (li/name? n)
       (u/throw-ex (str "invalid pattern, cannot fetch entity-name - " p)))
     (when-not (map? attrs)
       (u/throw-ex (str "invalid pattern, cannot fetch attributes - " p)))
     (let [id-attr (if (and is-root (not (seq (cn/containing-parents n))))
                     (cn/identity-attribute-name n)
                     (cn/path-identity-attribute-name n))]
       (when-not id-attr
         (u/throw-ex (str "cannot find identity attribute - " p)))
       (if-let [v (id-attr attrs)]
         [n v (dissoc attrs id-attr)]
         (u/throw-ex (str "failed to find identity value - " p))))))
  ([p] (compile-map-entry-in-path p false)))

(defn- compile-keyword-entry-in-path [p]
  (when-not (cn/contains-relationship? p)
    (u/throw-ex (str "not a contains relationship - " p)))
  p)

(defn- dispatch-compile-path-entry [p]
  (cond
    (map? p)
    (compile-map-entry-in-path p)

    (keyword? p)
    (compile-keyword-entry-in-path p)

    :else (u/throw-ex (str "invalid path query pattern - " p))))

(defn- encode-path-component [pc]
  (cond
    (vector? pc)
    [(pi/encoded-uri-path-part (first pc)) (second pc)]

    (keyword? pc)
    (if (cn/contains-relationship? pc)
      (pi/encoded-uri-path-part pc)
      pc)

    :else pc))

(defn- path-as-expr
  ([path child-id]
   `(~(symbol "clojure.string/join")
     "/" ~(vec (concat (conj path pi/path-prefix) [(if child-id child-id "%")]))))
  ([path] (path-as-expr path nil)))

(defn- finalize-path-query [child-pat result alias]
  (let [path (flatten (mapv encode-path-component result))
        inst-pat
        (cond
          (map? child-pat)
          (let [n (li/record-name child-pat), attrs (li/record-attributes child-pat)]
            (when-not n
              (u/throw-ex (str "not a valid entity pattern, no name found - " child-pat)))
            (when-not attrs
              (u/throw-ex (str "not a valid entity pattern, no attributes found - " child-pat)))
            (let [id-attr (cn/path-identity-attribute-name n)
                  id-val (id-attr attrs)
                  attrs (when (seq attrs)
                          (into
                           {}
                           (mapv (fn [[k v]]
                                   [(if (li/query-pattern? k)
                                      k
                                      (li/name-as-query-pattern k))
                                    v])
                                 (dissoc attrs id-attr))))
                  path (concat path [(pi/encoded-uri-path-part n)])]
              {n (if id-val
                   (assoc attrs li/path-attr? (path-as-expr path id-val))
                   (assoc attrs li/path-attr? [:like (path-as-expr path)]))}))

          (li/name? child-pat)
          (let [path (concat path [(pi/encoded-uri-path-part child-pat)])]
            {child-pat {li/path-attr? [:like (path-as-expr path)]}})

          :else (u/throw-ex (str "invalid child pattern - " child-pat)))]
    (if alias
      (assoc inst-pat :as alias)
      inst-pat)))

(defn- compile-path-query [ctx query-pat]
  (when-not (map? (first query-pat))
    (u/throw-ex (str "root-entry must be an entity pattern - " (first query-pat))))
  (let [[path-pats alias] (try-alias query-pat)]
    (loop [pat (rest path-pats), result [(compile-map-entry-in-path (first path-pats) true)]]
      (if-let [p (first pat)]
        (if-not (seq (rest pat))
          (compile-pattern ctx (finalize-path-query p result alias))
          (recur (rest pat) (conj result (dispatch-compile-path-entry p))))
        (u/throw-ex (str "invalid query, no path to child entity - " pat))))))

(def ^:private special-form-handlers
  {:match compile-match
   :try compile-try
   :throws (partial compile-try true)
   :rethrow-after compile-rethrow-after
   :for-each compile-for-each
   :query compile-query-command
   :delete compile-delete
   :await compile-await
   :eval compile-eval
   :? compile-path-query})

(defn- compile-special-form
  "Compile built-in special-forms (or macros) for performing basic
  conditional and iterative operations."
  [ctx pat]
  (if-let [h ((first pat) special-form-handlers)]
    (h ctx (rest pat))
    (compile-user-macro ctx pat)))

(defn- compile-list-literal [ctx attr-name pat]
  (let [quoted? (li/quoted? pat)]
    (op/set-list-attribute
     [attr-name
      (if quoted?
        (compile-quoted-list ctx (second pat))
        (mapv #(compile-pattern ctx %) pat))
      quoted?])))

(defn- compile-vector [ctx pat]
  (if (li/registered-macro? (first pat))
    (compile-special-form ctx pat)
    (compile-list-literal ctx nil pat)))

(defn- compile-literal [_ pat]
  (emit-load-literal pat))

(defn- compile-fncall-expression [_ pat]
  (op/call-function (compound-expr-as-fn pat)))

(defn- maybe-dissoc-meta [pat]
  (if (map? pat)
    (dissoc pat :meta :meta?)
    pat))

(defn compile-pattern [ctx pat]
  (let [pat (maybe-dissoc-meta pat)]
    (if-let [c (cond
                 (li/pathname? pat) compile-pathname
                 (map? pat) compile-map
                 (vector? pat) compile-vector
                 (i/const-value? pat) compile-literal
                 (seqable? pat) compile-fncall-expression)]
      (let [code (c ctx pat)]
        (package-opcode code pat))
      (u/throw-ex (str "cannot compile invalid pattern - " pat)))))

(defn- maybe-mark-conditional-df [ctx evt-pattern]
  (when (li/name? evt-pattern)
    (when (cn/conditional-event? evt-pattern)
      (ctx/bind-variable! ctx i/conditional-dataflow-tag true)))
  ctx)

(defn- error-pattern-as-string [pat]
  (with-out-str (pp/pprint pat)))

(def ^:private error-marker (keyword "-^--- ERROR in pattern"))

(defn- report-compiler-error [all-patterns pattern-index ex]
  (loop [pats all-patterns, n pattern-index,
         marker-set false, result []]
    (if-let [p (first pats)]
      (let [f (neg? n)]
        (recur (rest pats)
               (dec n)
               f (if f
                   (conj result error-marker p)
                   (conj result p))))
      (let [err-pat (if marker-set result (conj result error-marker))]
        (log/error (str "error in expression " ex))
        (log/error (error-pattern-as-string err-pat))
        (throw ex)))))

(defn- compile-with-error-report [all-patterns compile-fn
                                  pattern-to-compile n]
  (try
    (compile-fn pattern-to-compile)
    #?(:clj
       (catch Exception e
         (report-compiler-error all-patterns n e))
       :cljs
       (catch js/Error e
         (report-compiler-error all-patterns n e)))))

(def ^:private newname li/unq-name)

(declare preproc-relspec-helper)

(defn- flatten-preproc-patterns [pats]
  (vec (apply concat (mapv :patterns pats))))

(defn- find-preproc-alias [pats]
  (:alias (first (filter :alias pats))))

(defn- maybe-preproc-parent-pat [pat]
  (if (keyword? pat)
    [{:alias pat}]
    (if-let [relspec (li/rel-tag pat)]
      (preproc-relspec-helper pat relspec)
      (let [alias (or (:as pat) (newname))]
        [{:patterns [(assoc pat :as alias)] :alias alias}]))))

(defn maybe-append-path-identity-pattern [path path-ident]
  (if (s/ends-with? path "%")
    (s/replace path "%" (str path-ident))
    path))

(defn- maybe-lift-id-pattern [recname attrs]
  (if-let [pid (cn/path-identity-attribute-name recname)]
    (let [pid (li/name-as-query-pattern pid)
          ks (keys attrs)]
      (if (some #{pid} ks)
        [(dissoc attrs pid) (get attrs pid)]
        [attrs nil]))
    [attrs nil]))

(defn- preproc-contains-spec-by-path [recname pat pat-alias relpat pathpat]
  (let [recattrs (li/record-attributes pat)
        newpat {recname
                (assoc recattrs (if (li/query-pattern? relpat)
                                  li/path-attr?
                                  li/path-attr)
                       pathpat)
                :as pat-alias}]
    {:patterns [newpat] :alias pat-alias}))

(defn- preproc-contains-spec [pat pat-alias relpat nodepat idpat]
  (let [pk (li/record-name pat)
        recname (li/normalize-name pk)
        recversion (li/record-version pat pk)
        relname (li/normalize-name relpat)]
    (when-not (first (filter #(= relname (first %)) (cn/containing-parents recname recversion)))
      (u/throw-ex (str "not a valid contains relationship for " recname " - " relname)))
    (if (= :_ idpat)
      (preproc-contains-spec-by-path recname pat pat-alias relpat nodepat)
      (let [v (newname)
            pp (maybe-preproc-parent-pat nodepat)
            pp-alias (find-preproc-alias pp)
            is-rel-q (li/query-pattern? relpat)
            is-pat-q (li/query-instance-pattern? pat)
            attrs0 (li/record-attributes pat)
            [attrs1 idpat] (if (and (not idpat) (and is-rel-q is-pat-q))
                             (maybe-lift-id-pattern pk attrs0)
                             [attrs0 idpat])
            attrs (assoc attrs1
                         (if is-rel-q
                           li/path-attr?
                           li/path-attr)
                         (if (and is-rel-q (not idpat))
                           [:like v]
                           v))
            pat (assoc pat pk attrs)
            rec-s (li/name-str recname)
            rel-s (li/name-str relname)
            pid-n (cn/path-identity-attribute-name recname recversion)
            maybe-can-fix-path (and (not idpat) (not (and is-rel-q is-pat-q)))
            pat-with-fixed-path
            (when maybe-can-fix-path
              (when-let [pid-v (pid-n attrs)]
                (assoc pat pk
                       (assoc attrs li/path-attr
                              `(agentlang.compiler/maybe-append-path-identity-pattern
                                ~v ~(cn/path-identity-attribute-name recname recversion))))))
            pats [[:eval
                   (if idpat
                     `(agentlang.component/full-path-from-references ~pp-alias ~rel-s ~idpat ~rec-s ~recversion)
                     `(agentlang.component/full-path-from-references ~pp-alias ~rel-s ~rec-s ~recversion))
                   :as v]
                  (assoc (or pat-with-fixed-path pat) :as pat-alias)]
            post-pats (when (and maybe-can-fix-path (not pat-with-fixed-path))
                        (let [ident (cn/identity-attribute-name recname recversion)]
                          [{recname
                            {(li/name-as-query-pattern ident)
                             (li/make-ref pat-alias ident)
                             li/path-attr `(agentlang.compiler/maybe-append-path-identity-pattern
                                            ~v ~(cn/path-identity-attribute-name recname recversion))}
                            :as pat-alias}]))]
        {:patterns (vec (concat (flatten-preproc-patterns pp) pats post-pats))
         :alias pat-alias}))))

(defn- add-between-refs [relattrs relmeta [from-recname from-alias] [to-recname to-alias]]
  (let [[a1 a2] (li/between-nodenames from-recname to-recname relmeta)
        ids (name li/id-attr)
        f #(keyword (str (name %2) "." (name (cn/identity-attribute-name %1))))]
    (assoc relattrs a1 (f from-recname from-alias) a2 (f to-recname to-alias))))

(defn- preproc-between-spec [pat pat-alias relpat nodepat idpat]
  (when-not (li/query-instance-pattern? relpat)
    (let [relattrs (li/record-attributes relpat)
          relname (li/record-name relpat)
          rmeta (cn/fetch-meta relname)
          relmeta (cn/relationship-meta rmeta)
          pn (li/normalize-name (li/record-name pat))
          nodepat (if (keyword? nodepat)
                    [(cn/other-relationship-node relname pn) nodepat]
                    nodepat)]
      (if (vector? nodepat)
        (let [[nodetype alias] nodepat]
          {:patterns [{relname (add-between-refs
                                relattrs relmeta
                                [pn pat-alias]
                                [nodetype alias])}]})
        (let [pp (maybe-preproc-parent-pat nodepat)
              alias (find-preproc-alias pp)]
          (when-not (cn/has-between-relationship? pn relname)
            (u/throw-ex (str relname " is not in the between-relationship " relname)))
          {:patterns (vec (concat (flatten-preproc-patterns pp)
                                  [{relname (add-between-refs
                                             relattrs relmeta
                                             [pn pat-alias]
                                             [(li/normalize-name (li/record-name nodepat)) alias])}]))})))))

(defn- contains-relationship-pattern [pat]
  (cond
    (keyword? pat) pat
    (map? pat)
    (when-let [n (li/record-name pat)]
      (when (cn/contains-relationship? n) n))
    :else nil))

(defn- preproc-relspec-entry [pat pat-alias [relpat nodepat idpat]]
  (if-let [relpat (contains-relationship-pattern relpat)]
    (preproc-contains-spec pat pat-alias relpat nodepat idpat)
    (preproc-between-spec pat pat-alias relpat nodepat idpat)))

(defn- preproc-relspec-helper [pat relspec]
  (let [pat-alias (or (:as pat) (newname))
        new-pats (filter identity (mapv (partial preproc-relspec-entry pat pat-alias) relspec))
        has-contains (some #(contains-relationship-pattern (first %)) relspec)
        inst-pat (when-not has-contains [{:patterns [(assoc pat :as pat-alias)]}])]
    (concat inst-pat new-pats [{:patterns [pat-alias]}])))

(defn- preproc-relspec [pat relspec]
  (flatten-preproc-patterns (preproc-relspec-helper pat relspec)))

(defn- ensure-no-parent-refs! [parent-names v]
  (let [parts (li/path-parts v)]
    (if-let [path (:path parts)]
      (when (some #{path} parent-names)
        (u/throw-ex (str "cannot refer to " path " from embedded instance attribute - " v)))
      (let [cn [(:component parts) (:record parts)]
            pns (mapv li/split-path parent-names)]
        (when (some #{cn} pns)
          (u/throw-ex (str "cannot refer to " cn " from embedded instance attribute - " v)))))))

(defn- lift-embedded-instances-from-pattern
  ([pat]
   (lift-embedded-instances-from-pattern
    nil (or (:as pat) (newname)) pat
    (li/instance-pattern-name pat)
    (li/instance-pattern-attrs pat)))
  ([parent-names alias orig-pat recname attrs]
   (let [inter (mapv (fn [[k v]]
                       (cond
                         (keyword? v)
                         (do (ensure-no-parent-refs! parent-names v)
                             [nil [k v]])

                         (li/instance-pattern? v)
                         (let [v-alias (or (:as v) (newname))
                               new-pats (lift-embedded-instances-from-pattern
                                         (conj parent-names recname alias) v-alias
                                         v (li/instance-pattern-name v)
                                         (li/instance-pattern-attrs v))]
                           [new-pats [k v-alias]])

                         (and (vector? v) (every? li/instance-pattern? v))
                         (let [r (mapv (fn [v]
                                         (let [v-alias (or (:as v) (newname))
                                               new-pats (lift-embedded-instances-from-pattern
                                                         (conj parent-names recname alias) v-alias
                                                         v (li/instance-pattern-name v)
                                                         (li/instance-pattern-attrs v))]
                                           [new-pats v-alias]))
                                       v)]
                           [(apply concat (filter identity (mapv first r))) [k (mapv second r)]])

                         :else [nil [k v]]))
                     attrs)
         pre-pats (seq (filter identity (mapv first inter)))
         new-attrs (into {} (mapv second inter))]
     (flatten (concat pre-pats [(assoc orig-pat recname new-attrs :as alias)])))))

(defn- lift-embedded-instances [dfpats]
  (loop [pats dfpats, final-pats []]
    (if-let [p (first pats)]
      (if (li/instance-pattern? p)
        (recur (rest pats) (concat final-pats (lift-embedded-instances-from-pattern p)))
        (recur (rest pats) (concat final-pats [p])))
      final-pats)))

(defn- preproc-patterns [dfpats]
  (try
    (loop [pats (lift-embedded-instances dfpats), final-pats []]
      (if-let [p (first pats)]
        (if-let [relspec (and (map? p) (li/rel-tag p))]
          (recur (rest pats) (vec (concat final-pats (preproc-relspec p relspec))))
          (recur (rest pats) (conj final-pats p)))
        (if (seq final-pats) final-pats dfpats)))
    (catch #?(:clj Exception :cljs :default) ex
      (log/exception ex)
      #?(:clj (throw (Exception. "Error in dataflow, pre-processing failed"))))))

(defn- fetch-throw [pat]
  (if (= :throws (first pat))
    (u/throw-ex (str ":throws cannot be a standalone expression - " pat))
    (seq (drop-while #(not= :throws %) pat))))

(defn- remove-throw [pat throw]
  (let [a (take-while #(not= :throws %) pat)
        b (nthrest throw 2)]
    (vec (concat a b))))

(defn- lift-throw [pat]
  (w/prewalk
   (fn [pat]
     (if-let [handlers (and (map? pat) (:throws pat))]
       `[:throws
         ~(dissoc pat :throws)
         ~@(us/flatten-map handlers)]
       (if-let [throw (and (vector? pat) (fetch-throw pat))]
         (let [handlers (second throw)]
           `[:throws
             ~(remove-throw pat throw)
             ~@(us/flatten-map handlers)])
         pat)))
   pat))

(def counter (atom 0))

(defn- maybe-inject-exec-graph-instrumentation [event-name patterns]
  (if (s/starts-with? (name (second (li/split-path event-name))) "ExecGraphNode")
    patterns
    (if (gs/exec-graph-enabled?)
      (mapv (fn [p]
              (let [_ (swap! counter inc)
                    evt-name (keyword (str "Agentlang.Kernel.Eval/ExecGraphNode" @counter))]; (s/replace (u/uuid-string) "-" "")))]
                (cn/intern-event evt-name {:Pattern :Agentlang.Kernel.Lang/String})
                (cn/register-dataflow evt-name [p])
                {evt-name {:Pattern (pr-str p)}}))
            patterns)
      patterns)))

(defn- compile-dataflow [ctx evt-pattern df-patterns]
  (let [ename (if (li/name? evt-pattern)
                evt-pattern
                (first (keys evt-pattern)))
        df-patterns (maybe-inject-exec-graph-instrumentation ename df-patterns)
        c (partial
           compile-pattern
           (maybe-mark-conditional-df ctx evt-pattern))
        ec (c evt-pattern)
        df-patterns (preproc-patterns (mapv lift-throw df-patterns))
        safe-compile (partial compile-with-error-report df-patterns c)
        result [ec (mapv safe-compile df-patterns (range (count df-patterns)))]]
    #?(:clj (log/dev-debug (str "compile-dataflow (" evt-pattern " " df-patterns ") => " result)))
    result))

(defn maybe-compile-dataflow
  ([compile-query-fn with-types df]
   (when-not (cn/dataflow-opcode df with-types)
     (let [ctx (make-context with-types)]
       (ctx/bind-compile-query-fn! ctx compile-query-fn)
       (cn/set-dataflow-opcode!
        df (compile-dataflow
            ctx (cn/dataflow-event-pattern df)
            (cn/dataflow-patterns df))
        with-types)))
   df)
  ([compile-query-fn df]
   (maybe-compile-dataflow compile-query-fn cn/with-default-types df)))

(defn compile-dataflows-for-event [compile-query-fn event]
  (binding [active-event-name (li/split-path (cn/instance-type event))]
    (let [evt (dissoc event ctx/with-types-tag)
          wt (get event ctx/with-types-tag cn/with-default-types)]
      (mapv (partial maybe-compile-dataflow compile-query-fn wt)
            (cn/dataflows-for-event evt)))))

(defn compile-standalone-pattern
  ([compile-query-fn with-types pattern]
   (let [ctx (make-context with-types)]
     (ctx/bind-variable! ctx i/conditional-dataflow-tag true)
     (ctx/bind-compile-query-fn! ctx compile-query-fn)
     (compile-pattern ctx pattern)))
  ([compile-query-fn pattern]
   (compile-standalone-pattern compile-query-fn cn/with-default-types pattern)))

(defn- reference-attributes [attrs refrec]
  (when-let [result (cn/all-reference-paths attrs)]
    (let [[attr-name path] (first result)
          {refs :refs} (li/path-parts (:ref path))]
      [attr-name (first refs)])))

(defn- assert-unique! [entity-name attr-name ref-attr]
  (let [scm (cn/entity-schema entity-name)]
    (when-not (cn/unique-attribute? scm attr-name)
      (u/throw-ex (str "Reference not valid - " ref-attr " - " [entity-name attr-name]
                       " is not unique")))))

(defn- extract-identity-attribute-value [root-component parent-recname child-inst]
  (if-let [path (and (map? child-inst) (li/path-attr child-inst))]
    (let [parts (paths/parent-info-from-path root-component (pi/as-partial-path (pi/as-wildcard-path path)))
          id (loop [ps parts]
               (when-let [p (first ps)]
                 (if (= parent-recname p)
                   (first (rest ps))
                   (recur (rest ps)))))]
      (when-not id
        (u/throw-ex (str "Unable to find the guid of " parent-recname " from " path)))
      id)
    (u/throw-ex (str "Path not found in child instance - " child-inst))))

(defn translate-ref-via-relationship [rec-name rec-inst rel-name refs]
  (when-not (cn/in-relationship? rec-name rel-name)
    (u/throw-ex (str rec-name " not in relationship " rel-name)))
  (let [[root-component _] (li/split-path rel-name)]
    (loop [rec-name rec-name, rec-inst rec-inst, rel-name rel-name, all-refs refs, pats []]
      (if-let [refs (seq (take 2 all-refs))]
        (let [rec-name (li/make-path rec-name)
              is-contains (cn/contains-relationship? rel-name)
              is-child (and is-contains (= rec-name (cn/contained-child rel-name)))
              has-child-ref (= 2 (count refs))]
          (when (and has-child-ref (not (or (and is-contains is-child) (cn/one-to-one-relationship? rel-name))))
            (u/throw-ex (str "Reference to attribute not accessible via this relationship - " [rel-name refs])))
          (let [ent (cn/other-relationship-node rel-name rec-name)
                refname (and has-child-ref (second refs))
                idattr (cn/identity-attribute-name rec-name)
                idref (if (keyword? rec-inst)
                        (li/make-ref rec-inst idattr)
                        (idattr rec-inst))
                p0 (if is-contains
                     (if is-child
                       {ent {(li/name-as-query-pattern
                              (cn/identity-attribute-name ent))
                             (extract-identity-attribute-value root-component ent rec-inst)}}
                       {(li/name-as-query-pattern ent)
                        {:where [:like li/path-attr (cn/full-path-from-references rec-inst rel-name ent)]}})
                     {(li/name-as-query-pattern ent) {}
                      li/rel-tag
                      [[{rel-name {(li/name-as-query-pattern
                                    (first (cn/find-between-keys rel-name rec-name)))
                                   idref}}]]})
                alias (and has-child-ref (li/unq-name))
                p1 (if alias (assoc p0 :as [alias]) p0)
                remrefs (drop 2 all-refs)]
            (if (and (not is-contains) (seq remrefs))
              (let [relname (if (li/partial-name? refname)
                              (li/make-path root-component refname)
                              refname)]
                (recur ent alias relname remrefs (conj pats p1)))
              (let [pt0 (conj pats p1)
                    pt1 (if alias (conj pt0 (li/make-ref alias refname)) pt0)]
                (vec pt1)))))
        (vec pats)))))

(defn- translate-expr-arg
  ([rec-name attrs attr-names aname arg]
   (cond
     (li/literal? arg)
     arg

     (= aname arg)
     (u/throw-ex (str "self-reference in attribute expression - " [rec-name aname]))

     (some #{arg} attr-names)
     `(~arg ~current-instance-var)

     :else
     (if-let [[refattr attr] (li/split-ref arg)]
       (if-let [refpath (:ref (get attrs refattr))]
         (let [{component :component rec :record refs :refs} (li/path-parts refpath)
               ukattr (first refs)]
           (assert-unique! [component rec] ukattr refattr)
           ;; The referenced instance is auto-loaded into the environment by the
           ;; evaluator, before the following code executes.
           ;; See evaluator/root/do-load-references
           `(~attr
             (first
              (agentlang.env/lookup-instances-by-attributes
               ~runtime-env-var
               ~[component rec] [[~ukattr (~refattr ~current-instance-var)]]))))
         (u/throw-ex (str "not a reference attribute " [rec-name aname arg])))
       (u/throw-ex (str "invalid reference attribute " [rec-name aname arg])))))
  ([rec-name aname arg]
   (if-let [scm (cn/fetch-schema rec-name)]
     (translate-expr-arg rec-name scm (cn/attribute-names scm) aname arg)
     (u/throw-ex (str "failed to find schema for "  rec-name)))))

(defn- maybe-expr? [x]
  (and (seqable? x)
       (not (vector? x))
       (not (string? x))))

(defn fix-names-in-arg-pattern [arg-lookup pat]
  (if (keyword? pat)
    (or (arg-lookup pat) pat)
    (w/prewalk
     (fn [pat]
       (if (map? pat)
         (if-let [attrs (li/record-attributes pat)]
           (assoc pat (li/record-name pat)
                  (into {} (mapv (fn [[k v]] [k (if (keyword? v)
                                                  (or (arg-lookup v) v)
                                                  v)])
                                 attrs)))
           pat)
         pat))
     pat)))

(defn evaluate-pattern-in-expr [env [c _ :as recname] attr-name generated-pattern]
  (try
    (when-not (env/compound-patterns-blocked? env)
      ((es/get-safe-eval-patterns) c generated-pattern))
    (catch #?(:clj Exception :cljs :default) ex
      (log/debug ex) nil)))

(defn parse-expr [[c _ :as recname] arg-lookup attr-name exp]
  (let [arg-lookup (or arg-lookup #(translate-expr-arg recname attr-name %))]
    (cond
      (keyword? exp)
      (try
        (arg-lookup exp)
        (catch #?(:clj Exception :cljs :default) ex
          (if-let [[r _ :as ref-parts] (li/split-ref exp)]
            `(if (cn/relationship? ~r)
               (evaluate-pattern-in-expr
                ~runtime-env-var ~recname ~attr-name
                (translate-ref-via-relationship ~recname ~current-instance-var ~r ~(vec (rest ref-parts))))
               (u/throw-ex (str "invalid relationship reference - " ~exp)))
            (throw ex))))

      (maybe-expr? exp)
      `(~(first exp) ~@(map #(parse-expr recname arg-lookup attr-name %) (rest exp)))

      (li/patterns-arg? exp)
      (let [safe-arg-lookup (fn [x]
                              (try
                                (arg-lookup x)
                                (catch #?(:clj Exception :cljs :default) ex
                                  nil)))
            pats (mapv (partial fix-names-in-arg-pattern safe-arg-lookup) (rest exp))]
        `(try
           (when-not (agentlang.env/compound-patterns-blocked? ~runtime-env-var)
             (agentlang.evaluator/safe-eval-patterns ~c ~pats))
           (catch #?(:clj Exception :cljs :default) ex#
             (log/debug ex#) nil)))

      :else exp)))

(defn- cleanup-quotes [exp]
  (if (and (seqable? exp) (= 'quote (first exp)))
    (first (rest exp))
    exp))

(defn compile-attribute-expression [rec-name attrs aname aval]
  #?(:clj
     (do (when-not aval
           (u/throw-ex (str "attribute expression cannot be nil - " [rec-name aname])))
         (let [arg-lookup (partial translate-expr-arg rec-name attrs (keys attrs) aname)
               parse-exp (partial parse-expr (li/split-path rec-name) arg-lookup aname)
               aval (cleanup-quotes aval)
               exp `(fn [~runtime-env-var ~current-instance-var] ~(parse-exp aval))]
           (li/evaluate exp)))
     :cljs (concat ['quote] [aval])))

(defn- extract-node-info [tree]
  (let [ks (keys tree)]
    [(first (filter cn/entity? ks))
     (first (filter cn/contains-relationship? ks))
     (seq (filter cn/between-relationship? ks))]))

(defn- instances-from-between-obj [obj]
  (cond
    (li/instance-pattern? obj) [obj]
    (and (vector? obj) (every? li/instance-pattern? obj)) obj))

(defn- between-with-aliased-instance [rel-name rel-alias from to inst]
  (let [inst-alias (newname) inst (assoc inst :as inst-alias)]
    [inst {rel-name {from (li/id-ref rel-alias) to (li/id-ref inst-alias)}}]))

(defn- process-between-rel-node [tree parent-name alias n]
  (let [obj (n tree) [from to] (cn/between-attribute-names n parent-name)]
    (if-let [insts (instances-from-between-obj obj)]
      (vec (apply concat (mapv (partial between-with-aliased-instance n alias from to) insts)))
      [{n (assoc obj from (li/id-ref alias))}])))

(defn parse-relationship-tree
  ([parent-link path-attr tree]
   (let [[root-entity contains-rel between-rels] (extract-node-info tree)]
     (when (and contains-rel (not= root-entity (first (cn/contains-entities contains-rel))))
       (u/throw-ex (str root-entity " not parent in " contains-rel)))
     (when (and between-rels (not (every? (partial cn/has-between-relationship? root-entity) between-rels)))
       (u/throw-ex (str root-entity " does not belong to one of " between-rels)))
     (let [alias (newname)]
       (flatten
        `[~(merge {root-entity (merge (root-entity tree) path-attr) :as alias}
                  (when parent-link {:-> [parent-link]}))
          ~@(when contains-rel
              (mapv (partial parse-relationship-tree [contains-rel alias] nil) (contains-rel tree)))
          ~@(when between-rels
              (mapv (partial process-between-rel-node tree root-entity alias) between-rels))
          ~alias]))))
  ([path-attr tree] (parse-relationship-tree nil path-attr tree))
  ([tree] (parse-relationship-tree nil nil tree)))
