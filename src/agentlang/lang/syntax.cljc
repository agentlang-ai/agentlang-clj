(ns agentlang.lang.syntax
  (:require [clojure.walk :as w]
            [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang.internal :as li]
            [agentlang.datafmt.json :as json]
            [agentlang.datafmt.transit :as t]))

(defn- get-spec-val [k spec]
  (let [v (get spec k :not-found)]
    (if (= v :not-found)
      (u/throw-ex (str "required key " k " not found"))
      v)))

(def syntax-object-tag :-*-syntax-*-)
(def type-tag :type)
(def exp-fn-tag :fn)
(def exp-args-tag :args)
(def exp-tag :exp)
(def record-tag :record)
(def attrs-tag :attrs)
(def query-tag :query)
(def alias-tag :as)
(def value-tag :value)
(def cases-tag :cases)
(def body-tag :body)
(def check-tag :check)
(def path-tag :path)
(def name-tag :name)
(def meta-tag :meta)
(def throws-tag :throws)
(def error-tag :error)
(def not-found-tag :not-found)

(def rel-tag nil)
(def timeout-ms-tag li/timeout-ms-tag)

(def attributes attrs-tag)
(def query-pattern query-tag)

(def ^:private $fn (partial get-spec-val exp-fn-tag))
(def ^:private $args (partial get-spec-val exp-args-tag))
(def ^:private $exp (partial get-spec-val exp-tag))
(def ^:private $type (partial get-spec-val type-tag))
(def ^:private $record (partial get-spec-val record-tag))
(def ^:private $attrs (partial get-spec-val attrs-tag))
(def ^:private $value (partial get-spec-val value-tag))
(def ^:private $cases (partial get-spec-val cases-tag))
(def ^:private $body (partial get-spec-val body-tag))
(def ^:private $query (partial get-spec-val query-tag))

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

(defn as-syntax-object [t obj]
  (assoc obj type-tag t syntax-object-tag true))

(defn syntax-object? [obj]
  (when (map? obj)
    (syntax-object-tag obj)))

(defn has-type? [t obj]
  (and (syntax-object? obj)
       (= t (type-tag obj))))

(defn- valid-arg? [x]
  (or (li/name? x) (li/literal? x)))

(defn- invalid-arg [x]
  (when-not (valid-arg? x)
    x))

(defn- maybe-alias? [x]
  (if-not x
    true
    (or (li/name? x) (every? li/name? x))))

(defn- validate-alias! [x]
  (when-not (maybe-alias? x)
    (u/throw-ex (str "invalid alias - " x))))

(defn- mark [x]
  ['--> x '<--])

(defn- mark-exp-error [fnname args mark-at]
  (loop [exp `(~fnname ~@args), result []]
    (if-let [x (first exp)]
      (if (= x mark-at)
        (concat result [(mark x)] (rest exp))
        (recur (rest exp) (conj result x)))
      (seq result))))

(defn fn-name? [x]
  (or (symbol? x)
      (some #{x} li/oprs)))

(defn exp
  "Return the intermediate representation (ir)
  for a compound expression - required keys are -
   :fn - function name, a symbol like '+
   :args - function arguments, a vector of attribute names,
           constant values etc"  
  ([spec]
   (when (not= 2 (count spec))
     (u/throw-ex (str "invalid compound-expression spec " spec)))
   (exp ($fn spec) ($args spec)))
  ([fnname args]
   (when-not (fn-name? fnname)
     (u/throw-ex
      (str
       "fn-name must be a symbol - "
       (mark-exp-error fnname args fnname))))
   (when-let [invarg (some invalid-arg args)]
     (u/throw-ex
      (str
       "invalid argument in "
       (mark-exp-error fnname args fnname))))
   (as-syntax-object
    :exp
    {exp-fn-tag fnname
     exp-args-tag args})))

(def exp? (partial has-type? :exp))

(defn- raw-exp [ir]
  `'(~(exp-fn-tag ir) ~@(exp-args-tag ir)))

(declare raw)

(defn- raw-walk [obj]
  (w/postwalk raw obj))

(defn- introspect-exp [pattern]
  (let [p (if (= 'quote (first pattern))
            (second pattern)
            pattern)]
    (exp (first p) (vec (rest p)))))

(defn- valid-attr-spec? [[k v]]
  (and (li/name? k)
       (or (li/name? v)
           (li/literal? v)
           (exp? v))))

(defn- query-attr-name [n]
  (when (li/query-pattern? n)
    n))

(defn- mark-attr-name [attrs name-to-mark]
  (let [rs (mapv (fn [[k v]]
                   [(if (= k name-to-mark)
                      (mark k)
                      k)
                    v])
                 attrs)]
    (into {} rs)))

(declare introspect introspect-attrs)

(defn- introspect-rels [r]
  (mapv introspect r))

(defn- introspect-relationship [r]
  (if (vector? (first r))
    (mapv introspect-rels r)
    (u/throw-ex (str "invalid relationship object, expected vector - " (first r)))))

(defn- introspect-throws [throws]
  (when throws
    (when-not (map? throws)
      (u/throw-ex (str "Invalid throws, must be a map - " throws)))
    (into {} (mapv (fn [[k v]]
                     [k (introspect v)])
                   throws))))

(defn upsert [spec]
  (let [recname ($record spec)
        rec-attrs ($attrs spec)
        rec-alias (alias-tag spec)
        meta (meta-tag spec)
        rel (rel-tag spec)
        throws (throws-tag spec)]
    (when-not (li/name? recname)
      (u/throw-ex (str "A valid record name is required - " spec)))
    (when (li/query-pattern? recname)
      (u/throw-ex (str "looks like a query-only pattern - " recname)))
    (when-not (and (map? rec-attrs) (every? valid-attr-spec? rec-attrs))
      (u/throw-ex (str "invalid attribute spec - " rec-attrs)))
    (when (and meta (not (map? meta)))
      (u/throw-ex (str "meta must be a map - " meta)))
    (when-let [aname (some query-attr-name (keys rec-attrs))]
      (u/throw-ex
       (str "query attributes cannot be specified in upsert - "
            (mark-attr-name rec-attrs aname))))
    (validate-alias! rec-alias)
    (as-syntax-object
     :upsert
     (merge
      {record-tag recname
       attrs-tag (introspect-attrs rec-attrs)}
      (when meta {meta-tag meta})
      (when rel {rel-tag (introspect-relationship rel)})
      (when rec-alias {alias-tag rec-alias})
      (when throws {throws-tag (introspect-throws throws)})))))

(def upsert? (partial has-type? :upsert))

(declare raw-relationship)

(defn- raw-throws [ir]
  (into {} (mapv (fn [[k v]]
                   [k (raw-walk v)])
                 ir)))

(defn- raw-upsert [ir]
  (merge
   {($record ir)
    (raw-walk ($attrs ir))}
   (when-let [meta (meta-tag ir)] {meta-tag meta})
   (when-let [rel (rel-tag ir)] {rel-tag (raw-relationship rel)})
   (when-let [als (alias-tag ir)] {alias-tag als})
   (when-let [throws (throws-tag ir)] {throws-tag (raw-throws throws)})))

;; TODO: temporary placeholder. fix once path syntax is finalized.
(def ^:private proper-path? vector?)

(defn- query-attrs? [attrs]
  (or (proper-path? attrs) (some li/query-pattern? (keys attrs))))

(defn- query-record-name [recname]
  (if (li/query-pattern? recname)
    recname
    (li/name-as-query-pattern recname)))

(defn query-upsert [spec]
  (let [recname ($record spec)
        attrs (attributes spec)
        meta (meta-tag spec)
        rec-alias (alias-tag spec)
        throws (throws-tag spec)
        rel (rel-tag spec)]
    (when-not ($record spec)
      (u/throw-ex (str "Record name in required: " spec)))
    (when-not (attributes spec)
      (u/throw-ex (str "Attributes are required: " spec)))
    (when-not (or (li/query-pattern? recname)
                  (query-attrs? attrs))
      (u/throw-ex
       (str "not a valid query-upsert pattern - " {recname attrs})))
   (when (and meta (not (map? meta)))
     (u/throw-ex (str "meta must be a map - " meta)))
   (validate-alias! rec-alias)
   (as-syntax-object
    :query-upsert
    (merge
     {record-tag recname}
     (if (map? attrs)
       {attrs-tag (introspect-attrs attrs)}
       {path-tag attrs})
     (when meta {meta-tag meta})
     (when rel {rel-tag (introspect-relationship rel)})
     (when rec-alias {alias-tag rec-alias})
     (when throws {throws-tag (introspect-throws throws)})))))

(def query-upsert? (partial has-type? :query-upsert))

(defn- raw-query-upsert [ir]
  (let [path (path-tag ir)
        obj (when-not path (attributes ir))]
    (when-not (or obj path)
      (u/throw-ex (str "expected query-upsert attributes or path not found - " ir)))
    (merge
     {($record ir)
      (or path (raw-walk obj))}
     (when-let [meta (meta-tag ir)] {meta-tag meta})
     (when-let [rel (rel-tag ir)] {rel-tag (raw-relationship rel)})
     (when-let [als (alias-tag ir)] {alias-tag als})
     (when-let [t (throws-tag ir)] {throws-tag (raw-throws t)}))))

(def raw-relationship raw-walk)

(defn- maybe-assoc-relationship [obj pattern]
  (if-let [r (rel-tag pattern)]
    (assoc obj rel-tag (introspect-relationship r))
    obj))

(defn- dissoc-tags [pat]
  (dissoc pat meta-tag throws-tag alias-tag))

(defn- recname-from-map [pat]
  (first (keys (dissoc-tags pat))))

(defn- introspect-query-upsert [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (recname-from-map pat)
        attrs (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (or (map? attrs) (proper-path? attrs))
      (u/throw-ex (str "expected a map or a path query - " attrs)))
    (let [attr-names (and (map? attrs ) (seq (keys attrs)))
          qpat (if attr-names
                 (some li/query-pattern? attr-names)
                 (li/query-pattern? recname))]
      (maybe-assoc-relationship
       ((if qpat query-upsert upsert)
        {record-tag recname
         attrs-tag attrs
         alias-tag (alias-tag pattern)
         meta-tag (meta-tag pattern)
         throws-tag (introspect-throws (throws-tag pattern))})
       pattern))))

(defn query-object [spec]
  (let [recname (record-tag spec)
        query-pat (query-pattern spec)
        rec-alias (alias-tag spec)
        meta (meta-tag spec)
        throws (throws-tag spec)]
    (when-not recname
      (u/throw-ex (str "Name is required - " spec)))
    (when (and query-pat (not (:where query-pat)))
      (u/throw-ex
       (str "not a valid query pattern - " {recname query-pat})))
    (validate-alias! rec-alias)
    (as-syntax-object
     :query-object
     (merge
      {record-tag recname}
      (when meta {meta-tag meta})
      (when query-pat {query-tag query-pat})
      (when rec-alias {alias-tag rec-alias})
      (when throws {throws-tag (introspect-throws throws)})))))

(def query-object? (partial has-type? :query-object))

(defn- raw-query-object [ir]
  (if-let [obj (query-pattern ir)]
    (merge
     {($record ir)
      (raw-walk obj)}
     (when-let [meta (meta-tag ir)] {meta-tag meta})
     (when-let [als (alias-tag ir)] {alias-tag als})
     (when-let [throws (throws-tag ir)] {throws-tag (raw-throws throws)}))
    (let [n ($record ir)]
      (if (li/query-pattern? n)
        n
        (li/name-as-query-pattern n)))))

(defn- introspect-query-object [pattern]
  (let [pat (li/normalize-upsert-pattern pattern)
        recname (recname-from-map pat)
        qpat (recname pat)]
    (when-not (li/name? recname)
      (u/throw-ex (str "invalid record name - " recname)))
    (when-not (:where qpat)
      (u/throw-ex (str "invalid query pattern - " qpat)))
    (query-object
     {record-tag recname
      query-tag qpat
      alias-tag (alias-tag pattern)
      meta-tag (meta-tag pattern)
      throws-tag (throws-tag pattern)})))

(def relationship-object rel-tag)

(defn- verify-cases! [cs]
  (loop [cs cs]
    (when-let [[k v :as c] (first cs)]
      (when (and (nil? v) (seq (rest cs)))
        (u/throw-ex (str "default case must be the last entry - " k)))
      (when (and v (not (or (li/name? k) (li/literal? k))))
        (u/throw-ex (str "invalid key " k " in " c)))
      (recur (rest cs)))))

(defn- introspect-case-vals [cs]
  (mapv (fn [[k v]]
          (if (nil? v)
            [(introspect k)]
            [k (introspect v)]))
        cs))

(defn match
  ([spec]
   (match ($value spec) ($cases spec) (alias-tag spec)))
  ([valpat cases match-alias]
   (when-not (or (li/name? valpat)
                 (li/literal? valpat))
     (u/throw-ex (str "invalid match value - " valpat)))
   (validate-alias! match-alias)
   (verify-cases! cases)
   (as-syntax-object
    :match
    {value-tag (introspect valpat)
     cases-tag (introspect-case-vals cases)
     alias-tag match-alias})))

(def match? (partial has-type? :match))

(def ^:private not-as #(not= alias-tag %))
(def ^:private not-throws #(not= throws-tag %))

(defn- upto-alias [exps]
  (take-while not-as exps))

(defn- special-form-alias [obj]
  (second (drop-while not-as obj)))

(defn- special-form-throws [obj]
  (second (drop-while not-throws obj)))

(defn- raw-special-form [ir cmd-vec]
  (vec
   (concat
    cmd-vec
    (when-let [a (alias-tag ir)]
      [alias-tag a])
    (when-let [t (throws-tag ir)]
      [throws-tag (raw-throws t)]))))

(defn- extract-match-cases [obj]
  (let [cs (take-while not-as obj)]
    (partition-all 2 cs)))

(defn- introspect-match [obj]
  (let [body (nthrest obj 2)]
    (match (second obj)
           (extract-match-cases body)
           (special-form-alias body))))

(defn- raw-case [[k v]]
  (if v
    [(raw k) (raw-walk v)]
    [(raw-walk k)]))

(defn- raw-match [ir]
  (raw-special-form
   ir
   `[:match ~(raw ($value ir))
     ~@(vec (flatten (mapv raw-case ($cases ir))))]))

(defn- name-or-map? [x]
  (or (map? x) (li/name? x)))

(defn- macro-call? [x]
  (and (vector? x)
       (li/registered-macro? (first x))))

(defn- dataflow-pattern? [x]
  (or (name-or-map? x)
      (macro-call? x)))

(defn for-each
  ([spec]
   (for-each ($value spec) ($body spec) (alias-tag spec)))
  ([valpat body val-alias]
   (validate-alias! val-alias)
   (when-not (name-or-map? valpat)
     (u/throw-ex (str "invalid value pattern in for-each - " valpat)))
   (when-not (every? dataflow-pattern? body)
     (u/throw-ex (str "invalid for-each body - " body)))
   (as-syntax-object
    :for-each
    {value-tag (introspect valpat)
     body-tag (mapv introspect body)
     alias-tag val-alias})))

(def for-each? (partial has-type? :for-each))

(def ^:private for-each-body upto-alias)
(def ^:private for-each-alias special-form-alias)

(defn- introspect-for-each [obj]
  (let [valpat (second obj)
        exps (rest (rest obj))
        body (for-each-body exps)
        als (for-each-alias exps)]
    (for-each valpat body als)))

(defn- raw-for-each [ir]
  (raw-special-form
   ir
   `[:for-each
     ~(raw-walk ($value ir))
     ~@(mapv raw-walk ($body ir))]))

(defn- introspect-try-cases [cases]
  (mapv (fn [[k v]]
          (when-not (or (keyword? k)
                        (every? keyword? k))
            (u/throw-ex (str "invalid case in try - " k)))
          [k (introspect v)])
        cases))

(defn _try
  ([spec]
   (_try ($body spec) ($cases spec) (alias-tag spec)))
  ([body cases val-alias]
   (when-not (dataflow-pattern? body)
     (u/throw-ex (str "invalid body for try - " body)))
   (validate-alias! val-alias)
   (as-syntax-object
    :try
    {body-tag (introspect body)
     cases-tag (introspect-try-cases cases)
     alias-tag val-alias})))

(def try? (partial has-type? :try))

(defn- introspect-try [obj]
  (let [exps (rest obj)]
    (_try (first exps)
          (partition-all 2 (upto-alias (rest exps)))
          (special-form-alias (rest exps)))))

(defn- raw-try [ir]
  (raw-special-form
   ir
   `[:try
     ~(raw-walk ($body ir))
     ~@(apply concat (mapv (fn [[k v]] [k (raw-walk v)]) ($cases ir)))]))

(defn- relspec-for-delete? [obj]
  (and (vector? obj) (= rel-tag (first obj))))

(defn delete
  ([spec]
   (delete
    ($record spec)
    (attrs-tag spec)
    (alias-tag spec)
    (throws-tag spec)))
  ([recname attrs result-alias throws]
   (let [amap (map? attrs)]
     (when-not (li/name? recname)
       (u/throw-ex (str "invalid record-name in delete - " recname)))
     (if amap
       (when-not (every? valid-attr-spec? attrs)
         (u/throw-ex (str "invalid attribute spec in delete - " attrs)))
       (when-not (or (= :purge attrs) (= :* attrs))
         (u/throw-ex (str "invalid delete option - " attrs))))
     (validate-alias! result-alias)
     (as-syntax-object
      :delete
      (merge
       {record-tag recname}
       {attrs-tag (if amap (introspect-attrs attrs) attrs)}
       (when result-alias {alias-tag result-alias})
       (when throws {throws-tag (introspect-throws throws)}))))))

(def delete? (partial has-type? :delete))

(defn- introspect-delete [obj]
  (delete (second obj)
          (nth obj 2)
          (special-form-alias obj)
          (special-form-throws obj)))

(defn- raw-delete [ir]
  (raw-special-form
   ir
   [:delete ($record ir)
    (let [attrs (attrs-tag ir)]
      (if (or (= :purge attrs) (= :* attrs))
        attrs
        (raw-walk attrs)))]))

(defn query
  ([spec]
   (if (attrs-tag spec)
     (assoc (query-upsert spec) type-tag query-tag)
     (query ($query spec) (alias-tag spec) (throws-tag spec))))
  ([query-pat result-alias throws]
   (when-not (or (li/name? query-pat) (map? query-pat))
     (u/throw-ex (str "invalid query - " query-pat)))
   (validate-alias! result-alias)
   (as-syntax-object
    :query
    (merge
     {query-tag
      (cond
        (query-object? query-pat) query-pat
        (query-upsert? query-pat) query-pat
        (map? query-pat)
        (let [recname (recname-from-map query-pat)]
          (query-object {record-tag recname query-tag (recname query-pat)
                         meta-tag (meta-tag query-pat) throws-tag (throws-tag query-pat)}))
        :else query-pat)
      alias-tag result-alias}
     (when throws {throws-tag (introspect-throws throws)})))))

(def query? (partial has-type? :query))

(defn- introspect-query [obj]
  (query (second obj) (special-form-alias obj) (special-form-throws obj)))

(defn- raw-query [ir]
  (if (query-tag ir)
    (raw-special-form
     ir
     [:query
      (raw-walk ($query ir))])
    (raw-query-upsert ir)))

(defn _eval
  ([spec]
   (_eval ($exp spec) (check-tag spec) (alias-tag spec) (throws-tag spec)))
  ([exp check result-alias throws]
   (when (and check (not (li/name? check)))
     (u/throw-ex (str "invalid value for check - " check)))
   (validate-alias! result-alias)
   (as-syntax-object
    :eval
    (merge
     {exp-tag (introspect exp)}
     (when check {check-tag check})
     (when result-alias {alias-tag result-alias})
     (when throws {throws-tag (introspect-throws throws)})))))

(def eval? (partial has-type? :eval))

(def ^:private not-check #(not= :check %))

(defn- introspect-eval [obj]
  (_eval (second obj)
         (second (drop-while not-check obj))
         (special-form-alias obj)
         (special-form-throws obj)))

(defn- raw-eval [ir]
  (raw-special-form
   ir
   (vec
    (concat
     [:eval (raw ($exp ir))]
     (when-let [c (check-tag ir)]
       [:check c])))))

(defn _await [spec]
  )

(defn- introspect-await [obj]
  )

(defn entity [spec]
  )

(defn- introspect-entity [obj]
  )

(def special-form-introspectors
  {:match introspect-match
   :try introspect-try
   :for-each introspect-for-each
   :query introspect-query
   :delete introspect-delete
   :await introspect-await
   :eval introspect-eval
   :entity introspect-entity})

(defn- introspect-attrs [attrs]
  (let [rs (mapv (fn [[k v]]
                   [k (introspect v true)])
                 attrs)]
    (into {} rs)))

(defn- introspect-special-form [pat]
  (if-let [h (special-form-introspectors (first pat))]
    (h pat)
    pat))

(defn- pure-query-map? [obj]
  (let [pat (li/normalize-upsert-pattern obj)
        attrs ((recname-from-map pat) pat)]
    (and (map? attrs) (:where attrs))))

(defn- introspect-name [pattern]
  (if (li/query-pattern? pattern)
    (as-syntax-object :query-object {record-tag (li/normalize-name pattern)})
    (as-syntax-object :reference {name-tag pattern})))

(defn reference [n]
  (if (keyword? n)
    (as-syntax-object :reference {name-tag n})
    (u/throw-ex (str "cannot make reference, not a name - " n))))

(def ^:private raw-reference name-tag)

(def reference? (partial has-type? :reference))

(defn- maybe-query [obj]
  (let [ks (seq (keys (attrs-tag obj)))]
    (cond
      (and ks (every? li/query-pattern? ks))
      (assoc obj type-tag query-tag)

      (and (not ks) (li/query-pattern? (record-tag obj)))
      (assoc obj type-tag query-tag)

      :else
      obj)))

(defn- maybe-map-literal? [obj]
  ;; Do a best attempt to distinguish an attribute value
  ;; that is a literal map vs an instance pattern.
  (let [ks (keys obj)]
    (or (some (complement keyword?) ks)
        (not (some #(= 2 (count (li/split-path %))) ks)))))

(defn introspect
  ([pattern is-attr-val]
   (cond
     (syntax-object? pattern) pattern

     (seqable? pattern)
     (cond
       (or (not (seq pattern)) (li/quoted? pattern))
       pattern

       (or (list? pattern) (= 'quote (first pattern)))
       (introspect-exp pattern)

       (map? pattern)
       (cond
         (and is-attr-val (maybe-map-literal? pattern)) pattern
         (pure-query-map? pattern) (introspect-query-object pattern)
         :else (maybe-query (introspect-query-upsert pattern)))

       (vector? pattern)
       (introspect-special-form pattern)

       :else pattern)

     (li/name? pattern)
     (introspect-name pattern)

     :else pattern))
  ([pattern] (introspect pattern false)))

(def introspect-json (comp introspect json/decode))
(def introspect-transit (comp introspect t/decode))

(def ^:private raw-handler
  {:exp raw-exp
   :upsert raw-upsert
   :query-upsert raw-query-upsert
   :query-object raw-query-object
   :match raw-match
   :for-each raw-for-each
   :try raw-try
   :query raw-query
   :delete raw-delete
   :reference raw-reference
   :eval raw-eval})

(defn raw
  "Consume an intermediate representation object,
  return raw agentlang syntax"
  [ir]
  (if (syntax-object? ir)
    (if-let [h (raw-handler ($type ir))]
      (h ir)
      (u/throw-ex (str "invalid syntax-object tag - " (type-tag ir))))
    ir))

(def raw-json (comp json/encode raw))
(def raw-transit (comp t/encode raw))

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

(defn- ref-path-name-info [model-name n]
  (let [root-parts (li/split-ref n)
        parts-count (count root-parts)
        recname (when (= parts-count 1) (first root-parts))
        model-name-parts (when model-name (li/split-ref model-name))
        model-parts (when (> parts-count 1)
                      (if model-name-parts
                        (take (count model-name-parts) root-parts)
                        [(first root-parts)]))
        comp-parts (when (> parts-count 1)
                     (if model-parts
                       (drop (count model-parts) root-parts)
                       root-parts))
        cname (when comp-parts (li/make-ref comp-parts))
        mname (when model-parts (li/make-ref model-parts))]
    (if (and model-name (not= model-name mname))
      nil
      {:model mname :component cname :record recname})))

;; TODO: temporary placeholders. fix once path syntax is finalized.
(def ^:private ref-path-name? (constantly true))
(def ^:private full-path-name? (constantly true))

(defn- full-path-name-info [model-name n]
  (let [{c :component r :record} (li/path-parts n)]
    (if (ref-path-name? c)
      (when-let [info (ref-path-name-info model-name c)]
        (merge info {:record r}))
      {:component c :record r})))

(defn name-info
  ([model-name n]
   (cond
     (full-path-name? n) (full-path-name-info model-name n)
     (ref-path-name? n) (ref-path-name-info model-name n)
     (li/name? n) {:record n}
     :else nil))
  ([n] (name-info nil n)))
