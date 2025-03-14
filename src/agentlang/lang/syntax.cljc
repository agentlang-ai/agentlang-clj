(ns agentlang.lang.syntax
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.datafmt.json :as json]
            [agentlang.datafmt.transit :as t]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn format-error [pattern msg] (str msg " -- " (u/pretty-str pattern)))
(defn raise-syntax-error [pattern msg] (log/warn (format-error pattern msg)))
(defn throw-ex [pattern msg] (u/throw-ex (format-error pattern msg)))

(defn- not-kw [kw x] (not= kw x))
(def ^:private not-as (partial not-kw :as))
(def ^:private not-not-found (partial not-kw :not-found))
(def ^:private not-error (partial not-kw :error))
(def ^:private not-case (partial not-kw li/except-tag))
(def ^:private not-check (partial not-kw :check))

(def syntax-type :type)
(def record-name :record)
(def attributes :attributes)
(def relationships :rels)
(def query-pattern :query)

(defn empty-pattern
  ([of-type]
   {syntax-type of-type :empty-pattern? true})
  ([] (empty-pattern nil)))

(defn empty-pattern? [x] (and (map? x) (:empty-pattern? x)))

(defn conditional? [pat]
  (and (seqable? pat) (li/match-operator? (first pat))))

(defn- maybe-lang-def-name? [n]
  (= 2 (count (li/split-path n))))

(declare literal?)

(defn- normal-map? [x]
  (and (map? x)
       (and (nil? (seq (select-keys x li/instance-meta-keys)))
            (some #(or (literal? %)
                       (if-let [n (and (keyword? %) (li/normalize-name %))]
                         (not (maybe-lang-def-name? n))
                         true))
                  (keys x)))))

(defn literal? [x]
  (or (number? x) (string? x) (boolean? x)
      (normal-map? x) (nil? x) (li/sealed? x)
      (and (vector? x) (literal? (first x)))))

(defn- validate-attributes [pat recname attrs]
  (if-let [all (cn/all-attribute-names recname)]
    (doseq [n (keys attrs)]
      (when-not (some #{(li/normalize-name n)} all)
        (raise-syntax-error pat (str n " is not a valid attribute of " recname))))
    (log/warn (str "Schema not found, failed to validate attributes for " recname)))
  attrs)

(defn maybe-extract-condition-handlers [pat]
  (or
   (cond
     (map? pat)
     (when-let [cases (li/except-tag pat)]
       [cases (dissoc pat li/except-tag)])

     (and (seqable? pat) (= (first pat) :delete))
     (when-let [cases (first (rest (drop-while not-case pat)))]
       [cases (let [p0 (take-while not-case pat)]
                (vec (concat p0 (rest (drop-while not-case (rest p0))))))]))
   [nil pat]))

(defn extract-alias-from-expression [pat]
  (let [[h t] (split-with not-as pat)]
    (if (seq t)
      (let [t (rest t)]
        (when-not (seq t)
          (u/throw-ex (str "Alias not specified after `:as` in " pat)))
        (when (> (count t) 1)
          (u/throw-ex (str "Alias must appear last in " pat)))
        [(vec h) (first t)])
      [(vec h) nil])))

(defn extract-body-patterns [sentries pat]
  (take-while #(not (some #{%} sentries)) pat))

(defn parse-try [pat]
  (let [body (extract-body-patterns #{:as :not-found :error} pat)]
    [body
     (second (drop-while not-as pat))
     {:not-found (second (drop-while not-not-found pat))
      :error (second (drop-while not-error pat))}]))

(def alias-tag :as)
(def check-tag :check)
(def into-tag :into)
(def case-tag li/except-tag)
(def not-found-tag :not-found)
(def error-tag :error)

(defn alias-from-pattern [pat]
  (cond
    (map? pat) (:as pat)
    (seqable? pat) (second (drop-while not-as pat))
    :else nil))

(defn check-from-pattern [pat]
  (when-let [chk (when (seqable? pat)
                   (second (drop-while not-check pat)))]
    (when-not (or (keyword? chk) (fn? chk))
      (raise-syntax-error pat "Invalid check specification, must be a keyword or a function"))
    chk))

(defn case-from-pattern [pat]
  (cond
    (map? pat) (li/except-tag pat)
    (seqable? pat) (second (drop-while not-case pat))
    :else nil))

(def ^:private query-attribute? (fn [[k _]] (li/query-pattern? k)))
(def ^:private update-attribute? (complement query-attribute?))

(declare introspect raw)

(def case-keys #{:not-found :error})
(def reserved-words (set (concat case-keys [:as])))

(defn- as-reserved-word [a]
  (if (keyword? a)
    (some #{a} reserved-words)
    (first (map as-reserved-word a))))

(defn- introspect-alias [a]
  (when a
    (when-not (or (keyword? a) (and (vector? a) (every? keyword? a)))
      (raise-syntax-error a "Not a valid alias"))
    (when-let [a (as-reserved-word a)] (throw-ex a "Invalid alias"))
    a))

(def reference? keyword?)

(defn reference [x]
  (if (keyword? x)
    x
    (u/throw-ex (str "Reference " x " must be a keyword"))))

(def raw-alias identity)

(defn- introspect-into [into]
  (when-not (map? into)
    (raise-syntax-error into "Not a valid into-specification, must be a map"))
  into)

(def raw-into identity)

(defn- call-on-map-values [f m] (into  {} (mapv (fn [[k v]] [k (f v)]) m)))

(defn- extract-main-record-name [pat]
  (let [ks (keys (li/normalize-instance-pattern pat))]
    (first (filter #(let [n (cn/canonical-type-name (li/normalize-name %))]
                      (or (cn/entity? n)
                          (cn/event? n)
                          (cn/rec? n)))
                   ks))))

(defn- extract-possible-record-name [pat]
  (let [ks (keys (li/normalize-instance-pattern pat))] (first (filter maybe-lang-def-name? ks))))

(defn- extract-relationship-names [recname pat]
  (let [ks (keys (li/normalize-instance-pattern pat))
        rn (cn/canonical-type-name (li/normalize-name recname))]
    (filter #(let [n (cn/canonical-type-name (li/normalize-name %))]
               (when (not= rn n)
                 (or (cn/relationship? n)
                     (and (maybe-lang-def-name? n) (not= n rn)))))
            ks)))

(defn- introspect-map-values [m] (call-on-map-values introspect m))
(defn- raw-map-values [m] (call-on-map-values raw m))

(defn- introspect-case [c]
  (when c
    (when-not (map? c)
      (throw-ex c "Not a valid case-specification, must be a map"))
    (when-not (= case-keys (set (set/union (keys c) case-keys)))
      (throw-ex c (str "Allowed keys are - " case-keys)))
    (introspect-map-values c)))

(def raw-case raw-map-values)

(defn with-alias [alias r]
  (assoc r :as alias))

(defn with-into [into r]
  (assoc r :into into))

(defn with-case [case r]
  (assoc r li/except-tag case))

(defn _with-meta [meta r]
  (assoc r :meta meta))

(def distinct-tag :distinct)

(defn with-distinct [d r]
  (assoc r distinct-tag d))

(defn- introspect-meta [obj]
  (when obj
    (when-not (map? obj)
      (u/throw-ex (str ":meta must be a map, instead found " obj)))
    obj))

(def raw-meta introspect-meta)

(defn- introspect-optional-keys [pat]
  {:as (introspect-alias (:as pat))
   :into (when-let [into (:into pat)] (introspect-into into))
   :meta (introspect-meta (:meta pat))
   li/except-tag (when-let [c (li/except-tag pat)] (introspect-case c))})

(defn- raw-optional-keys [r]
  (merge
   (when-let [a (:as r)] {:as (raw-alias a)})
   (when-let [into (:into r)] {:into (raw-into into)})
   (when-let [meta (:meta r)] {:meta (raw-meta meta)})
   (when-let [c (li/except-tag r)] {li/except-tag (raw-case c)})))

(def except-tag li/except-tag)
(def meta-tag :meta)

(defn- introspect-query-upsert [recname pat]
  (let [attrs (validate-attributes pat (li/normalize-name recname) (get pat recname))
        upsattrs (filter update-attribute? attrs)
        rels (extract-relationship-names recname pat)
        rels-spec (mapv (fn [r] [r (introspect (get pat r))]) rels)]
    (merge
     {syntax-type (if (seq upsattrs)
                    (if (seq (filter query-attribute? attrs))
                      :query-upsert
                      :upsert)
                    :query)
      record-name recname
      attributes attrs
      distinct-tag (distinct-tag pat)
      relationships rels-spec}
     (introspect-optional-keys pat))))

(defn- query-upsert-helper
  ([tag recname attrs rels]
   {syntax-type tag
    record-name recname
    attributes attrs
    relationships rels})
  ([tag recname attrs] (query-upsert-helper tag recname attrs nil))
  ([tag] (empty-pattern tag)))

(def upsert (partial query-upsert-helper :upsert))
(def query (partial query-upsert-helper :query))
(def query-upsert (partial query-upsert-helper :query-upsert))

(def raw-relationships raw-map-values)

(defn- raw-query [r]
  (merge
   {(record-name r) (attributes r)}
   (when-let [rels (relationships r)] (raw-relationships rels))
   (when-let [d (distinct-tag r)] {distinct-tag d})
   (raw-optional-keys r)))

(def ^:private raw-upsert raw-query)

(def ^:private introspect-create introspect-query-upsert)

(defn- introspect-query-pattern [pat]
  (when-not (map? pat)
    (raise-syntax-error pat "Query must be a map"))
  (when-not (:where pat)
    (raise-syntax-error pat "No :where clause in query"))
  pat)

(def ^:private raw-query-pattern identity)

(defn- introspect-query-object [recname pat]
  (merge
   {syntax-type :query-object
    record-name recname
    query-pattern (introspect-query-pattern (:? (li/record-attributes pat)))}
   (introspect-optional-keys pat)))

(defn query-object
  ([recname qpat]
   {syntax-type :query-object
    record-name recname
    query-pattern (introspect-query-pattern qpat)})
  ([] (empty-pattern :query-object)))

(defn- raw-query-object [r]
  (merge {(record-name r) {:? (raw-query-pattern (query-pattern r))}}
         (raw-optional-keys r)))

(def function-expression :fn)

(defn- introspect-call [pat]
  {syntax-type :call
   function-expression (let [exp (first pat)]
                         (if (list? exp)
                           exp
                           (raise-syntax-error pat "Not a valid fn-call expression")))
   :as (introspect-alias (alias-from-pattern pat))
   li/except-tag (introspect-case (case-from-pattern pat))
   :check (check-from-pattern pat)})

(defn call
  ([fnexpr check]
   (merge
    {syntax-type :call
     function-expression fnexpr}
    (when check
      {:check check})))
  ([fnexpr] (call fnexpr nil))
  ([] (empty-pattern :call)))

(defn- maybe-add-optional-raw-tags [r pat]
  (let [a (when-let [a (:as r)] (raw-alias a))
        c (when-let [c (li/except-tag r)]
            (raw-case c))
        p0 (if a (concat pat [:as a]) pat)
        p1 (if c (concat p0 [li/except-tag c]) p0)]
    (vec p1)))

(defn- raw-call [r]
  (let [pat [:call (function-expression r)]]
    (maybe-add-optional-raw-tags
     r
     (if-let [c (:check r)]
       (concat pat [:check c])
       pat))))

(defn- introspect-delete [pat]
  (let [q (first pat)]
    {syntax-type :delete
     query-pattern (if (keyword? q)
                     (do (when-not (cn/entity? q)
                           (raise-syntax-error pat (str q " is not an entity")))
                         q)
                     (introspect q))
     :as (introspect-alias (alias-from-pattern pat))
     :purge? (some #{:purge} pat)
     li/except-tag (introspect-case (case-from-pattern pat))}))

(defn- raw-delete [r]
  (let [pat [:delete (raw (query-pattern r))]]
    (maybe-add-optional-raw-tags
     r
     (if (:purge? r)
       (concat pat [:purge])
       pat))))

(defn delete
  ([q purge?]
   {syntax-type :delete
    query-pattern q
    :purge? purge?})
  ([q] (delete q false))
  ([] (empty-pattern :delete)))

(def quote-value :value)

(defn- introspect-quote [pat]
  {syntax-type :quote
   quote-value (second pat)})

(defn- raw-quote [r]
  [:q# (quote-value r)])

(def sealed-value :value)

(defn- introspect-sealed [pat]
  {syntax-type :sealed
   sealed-value (second pat)})

(defn- raw-sealed [r]
  [:s# (sealed-value r)])

(def try-body :body)

(defn- introspect-try [pat]
  (let [[body alias handlers] (parse-try pat)]
    {syntax-type :try
     try-body (mapv introspect body)
     :as (introspect-alias alias)
     li/except-tag (when handlers (introspect-case handlers))}))

(defn- with-raw-try-cases [pat cases]
  (loop [cs cases, pat pat]
    (if-let [[k v] (first cs)]
      (recur (rest cs) (conj pat k (raw v)))
      pat)))

(defn- raw-try [r]
  (let [pat0 `[:try ~@(mapv raw (try-body r))]
        pat (if-let [c (li/except-tag r)]
              (vec (with-raw-try-cases pat0 c))
              pat0)]
    (maybe-add-optional-raw-tags (dissoc r li/except-tag) pat)))

(defn _try
  ([body cases]
   (when-not (vector? body)
     (u/throw-ex "Try body must be a vector"))
   {syntax-type :try
    try-body body
    li/except-tag cases})
  ([] (empty-pattern :try)))

(def for-each-value :src)
(def for-each-body :body)

(defn- introspect-for-each [pat]
  (let [src (introspect (first pat))
        body (extract-body-patterns #{:as} (rest pat))
        alias (alias-from-pattern pat)]
    {syntax-type :for-each
     for-each-value src
     for-each-body (mapv introspect body)
     :as (introspect-alias alias)
     li/except-tag (introspect-case (case-from-pattern pat))}))

(defn- raw-for-each [r]
  (let [pat `[:for-each ~(raw (for-each-value r)) ~@(mapv raw (for-each-body r))]]
    (maybe-add-optional-raw-tags r pat)))

(defn for-each
  ([src body]
   {syntax-type :for-each
    for-each-value src
    for-each-body body})
  ([] (empty-pattern :for-each)))

(def match-value :value)
(def match-body :body)

(defn- introspect-match [pat]
  (let [fpat (first pat)
        has-value? (not (conditional? fpat))
        body (extract-body-patterns #{:as} (if has-value? (rest pat) pat))
        alias (alias-from-pattern pat)]
    {syntax-type :match
     match-value (when has-value?
                   (if (keyword? fpat)
                     fpat
                     (introspect (first pat))))
     match-body (loop [body body, result []]
                  (if (seq body)
                    (let [condition (first body)
                          c (second body)
                          conseq (if (nil? c) condition c)]
                      (if-not c
                        (conj result (introspect conseq))
                        (recur (rest (rest body)) (conj result [condition (introspect conseq)]))))
                    (vec result)))
     :as (introspect-alias alias)
     li/except-tag (introspect-case (case-from-pattern pat))}))

(defn- collect-match-clauses [clauses]
  (loop [cls clauses, result []]
    (if-let [c (first cls)]
      (recur
       (rest cls)
       (if (vector? c)
         (conj result (first c) (second c))
         (conj result c)))
      result)))

(defn raw-match-body [body]
  (collect-match-clauses
   (mapv (fn [v] (if (vector? v)
                   [(first v) (raw (second v))]
                   (raw v)))
         body)))

(defn- raw-match [r]
  (let [body (raw-match-body (match-body r))
        pat (if-let [v (match-value r)]
              `[:match ~(raw v) ~@body]
              `[:match ~@body])]
    (maybe-add-optional-raw-tags r pat)))

(defn match
  ([value body]
   {syntax-type :match
    match-value value
    match-body body})
  ([body] (match nil body))
  ([] (empty-pattern :match)))

(defn- introspect-filter [pat]
  )

(def literal-value :value)

(defn- introspect-literal [pat]
  {syntax-type :literal
   literal-value pat})

(def ^:private raw-literal literal-value)

(defn- introspect-command [pat]
  (when-let [f
        (case (first pat)
          :call introspect-call
          :delete introspect-delete
          :q# introspect-quote
          :s# introspect-sealed
          :try introspect-try
          :for-each introspect-for-each
          :match introspect-match
          :filter introspect-filter
          (do (raise-syntax-error pat "Not a valid expression") nil))]
    (f (rest pat))))

(defn- introspect-map [pat]
  (let [main-recname (or (extract-main-record-name pat)
                         (extract-possible-record-name pat))]
    (if main-recname
      (let [attrs (get pat main-recname)]
        (cond
          (:? attrs) (introspect-query-object main-recname pat)

          (or (li/query-pattern? main-recname)
              (some li/query-pattern? (keys attrs)))
          (introspect-query-upsert main-recname pat)

          :else (introspect-create main-recname pat)))
      (raise-syntax-error pat (str "No schema definition found for " main-recname)))))

(defn introspect [pat]
  (when-let [f (cond
                 (map? pat) (if (seq pat) introspect-map identity)
                 (vector? pat) introspect-command
                 (or (literal? pat) (keyword? pat)) introspect-literal
                 :else (raise-syntax-error pat "Invalid object"))]
    (f pat)))

(defn raw [r]
  (when-let [f (case (syntax-type r)
                 :query raw-query
                 :upsert raw-upsert
                 :query-upsert raw-upsert
                 :query-object raw-query-object
                 :for-each raw-for-each
                 :match raw-match
                 :delete raw-delete
                 :try raw-try
                 :call raw-call
                 :quote raw-quote
                 :sealed raw-sealed
                 :literal raw-literal
                 identity)]
    (f r)))

(defn syntax-type? [t r] (= t (syntax-type r)))

(def query? (partial syntax-type? :query))
(def upsert? (partial syntax-type? :upsert))
(def query-upsert? (partial syntax-type? :query-upsert))
(def query-object? (partial syntax-type? :query-object))
(def for-each? (partial syntax-type? :for-each))
(def match? (partial syntax-type? :match))
(def delete? (partial syntax-type? :delete))
(def try? (partial syntax-type? :try))
(def quote? (partial syntax-type? :quote))
(def sealed? (partial syntax-type? :sealed))
(def literal-object? (partial syntax-type? :literal))
(def call? (partial syntax-type? :call))

(defn- skip-root-component [n]
  (let [parts (s/split (name n) #"\.")]
    (if (> (count parts) 1)
      (keyword (s/join "." (rest parts)))
      n)))

(defn unqualified-name [x]
  (cond
    (li/name? x)
    (let [[c n] (li/split-path x)]
      (or n (skip-root-component c)))

    (li/parsed-path? x)
    (second x)))

(defn fully-qualified?
  ([n]
   (if (second (li/split-path n))
     true
     false))
  ([model-name n]
   (cond
     (= model-name n) false
     (s/starts-with? (str n) (str model-name)) true
     :else false)))
