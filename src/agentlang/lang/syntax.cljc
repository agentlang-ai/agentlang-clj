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

(def ^:private load-mode (atom false))

(def ^:private syntax-error-count (u/make-cell 0))
(def ^:private syntax-error-limit 5)

(defn raise-syntax-error [pattern msg]
  (let [err (str msg " -- " (u/pretty-str pattern))]
    (if @load-mode
      (do
        (log/error err)
        #?(:clj (println err))
        (if (> @syntax-error-count syntax-error-limit)
          (u/throw-ex "There are syntax errors in the model, see the log for details.")
          (u/safe-set syntax-error-count (inc @syntax-error-count))))
      (u/throw-ex err))))

(defn force-spit-errors! []
  (when (pos? @syntax-error-count)
    (u/throw-ex "There are syntax errors in the model, see the log for details."))
  (u/safe-set syntax-error-count 0))

(defn set-load-mode! [] (force-spit-errors!) (reset! load-mode true))
(defn unset-load-mode! [] (force-spit-errors!) (reset! load-mode false))

(defn- not-kw [kw x] (not= kw x))
(def ^:private not-as (partial not-kw :as))
(def ^:private not-not-found (partial not-kw :not-found))
(def ^:private not-error (partial not-kw :error))
(def ^:private not-case (partial not-kw li/except-tag))
(def ^:private not-check (partial not-kw :check))

(defn conditional? [pat]
  (and (seqable? pat) (li/match-operator? (first pat))))

(declare literal?)

(defn- normal-map? [x]
  (and (map? x)
       (and (nil? (seq (select-keys x li/instance-meta-keys)))
            (some #(or (literal? %)
                       (if-let [n (and (keyword? %) (li/normalize-name %))]
                         (not (or (cn/entity? n)
                                  (cn/event? n)
                                  (cn/relationship? n)
                                  (cn/rec? n)))
                         true))
                  (keys x)))))

(defn literal? [x]
  (or (number? x) (string? x) (boolean? x)
      (normal-map? x) (nil? x) (li/sealed? x)
      (and (vector? x) (literal? (first x)))))

(defn- validate-attributes [pat recname attrs]
  (let [all (cn/all-attribute-names recname)]
    (doseq [n (keys attrs)]
      (when-not (some #{(li/normalize-name n)} all)
        (raise-syntax-error pat (str n " is not a valid attribute of " recname)))))
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

(defn- introspect-alias [a]
  (when a
    (when-not (or (keyword? a) (and (vector? a) (every? keyword? a)))
      (raise-syntax-error a "Not a valid alias"))
    a))

(def ^:private raw-alias identity)

(defn- introspect-into [into]
  (when-not (map? into)
    (raise-syntax-error into "Not a valid into-specification, must be a map"))
  into)

(def ^:private raw-into identity)

(def case-keys #{:not-found :error})

(defn- call-on-map-values [f m] (into  {} (mapv (fn [[k v]] [k (f v)]) m)))

(defn- extract-main-record-name [pat]
  (let [ks (keys pat)]
    (first (filter #(let [n (li/normalize-name %)]
                      (or (cn/entity? n)
                          (cn/event? n)
                          (cn/rec? n)))
                   ks))))

(defn- extract-relationship-names [pat]
  (let [ks (keys pat)]
    (filter #(let [n (li/normalize-name %)]
               (cn/relationship? n))
            ks)))

(defn- introspect-map-values [m] (call-on-map-values introspect m))
(defn- raw-map-values [m] (call-on-map-values raw m))

(defn- introspect-case [c]
  (when c
    (when-not (map? c)
      (raise-syntax-error c "Not a valid case-specification, must be a map"))
    (when-not (= case-keys (set (set/union (keys c) case-keys)))
      (raise-syntax-error c (str "Allowed keys are - " case-keys)))
    (introspect-map-values c)))

(def ^:private raw-case raw-map-values)

(defn with-alias [alias r]
  (assoc r :as alias))

(defn with-into [into r]
  (assoc r :into into))

(defn with-case [case r]
  (assoc r li/except-tag case))

(defn with-distinct [d r]
  (assoc r :distinct d))

(defn- introspect-optional-keys [pat]
  {:as (introspect-alias (:as pat))
   :into (when-let [into (:into pat)] (introspect-into into))
   li/except-tag (when-let [c (li/except-tag pat)] (introspect-case c))})

(defn- raw-optional-keys [r]
  (merge
   (when-let [a (:as r)] {:as (raw-alias a)})
   (when-let [into (:into r)] {:into (raw-into into)})
   (when-let [c (li/except-tag r)] {li/except-tag (raw-case c)})))

(defn- introspect-query-upsert [recname pat]
  (let [attrs (validate-attributes pat (li/normalize-name recname) (get pat recname))
        upsattrs (filter update-attribute? attrs)
        rels (extract-relationship-names pat)
        rels-spec (mapv (fn [r] [r (introspect (get pat r))]) rels)]
    (merge
     {:type (if (seq upsattrs)
              (if (seq (filter query-attribute? attrs))
                :query-upsert
                :upsert)
              :query)
      :record recname
      :attributes attrs
      :distinct (:distinct pat)
      :rels rels-spec}
     (introspect-optional-keys pat))))

(defn- query-upsert
  ([tag recname attributes rels]
   {:type tag
    :record recname
    :attributes attributes
    :rels rels})
  ([tag recname attributes] (query-upsert tag recname attributes nil)))

(def upsert (partial query-upsert :upsert))
(def query (partial query-upsert :query))

(defn- raw-query [r]
  (merge
   {(:record r)
    (:attributes r)}
   (when-let [rels (:rels r)] (raw-map-values rels))
   (when-let [d (:distinct r)] {:distinct d})
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
   {:type :query-object
    :record recname
    :query (introspect-query-pattern (:? (li/record-attributes pat)))}
   (introspect-optional-keys pat)))

(defn- raw-query-object [r]
  (merge {(:record r) {:? (raw-query-pattern (:query r))}}
         (raw-optional-keys r)))

(defn- introspect-call [pat]
  {:type :call
   :fn (let [exp (second pat)]
         (if (list? exp)
           exp
           (raise-syntax-error pat "Not a valid fn-call expression")))
   :as (introspect-alias (alias-from-pattern pat))
   li/except-tag (introspect-case (case-from-pattern pat))
   :check (check-from-pattern pat)})

(defn- maybe-add-optional-raw-tags [r pat]
  (let [a (when-let [a (:as r)] (raw-alias a))
        c (when-let [c (li/except-tag r)]
            (raw-case c))
        p0 (if a (concat pat [:as a]) pat)
        p1 (if c (concat p0 [li/except-tag c]) p0)]
    (vec p1)))

(defn- raw-call [r]
  (let [pat [:call (:fn r)]]
    (maybe-add-optional-raw-tags
     r
     (if-let [c (:check r)]
       (concat pat [:check c])
       pat))))

(defn- introspect-delete [pat]
  (let [q (first pat)]
    {:type :delete
     :query (if (keyword? q)
              (do (when-not (cn/entity? q)
                    (raise-syntax-error pat (str q " is not an entity")))
                  q)
              (introspect q))
     :as (introspect-alias (alias-from-pattern pat))
     :purge? (some #{:purge} pat)
     li/except-tag (introspect-case (case-from-pattern pat))}))

(defn- raw-delete [r]
  (let [pat [:delete (raw (:query r))]]
    (maybe-add-optional-raw-tags
     r
     (if (:purge? r)
       (concat pat [:purge])
       pat))))

(defn delete
  ([q purge?]
   {:type :delete
    :query q
    :purge? purge?})
  ([q] (delete q false)))

(defn- introspect-quote [pat]
  {:type :quote
   :value (second pat)})

(defn- raw-quote [r]
  [:q# (:value r)])

(defn- introspect-sealed [pat]
  {:type :sealed
   :value (second pat)})

(defn- raw-sealed [r]
  [:s# (:value r)])

(defn- introspect-try [pat]
  (let [[body alias handlers] (parse-try pat)]
    {:type :try
     :body (mapv introspect body)
     :as (introspect-alias alias)
     li/except-tag (when handlers (introspect-case handlers))}))

(defn- with-raw-try-cases [pat cases]
  (loop [cs cases, pat pat]
    (if-let [[k v] (first cs)]
      (recur (rest cs) (conj pat k (raw v)))
      pat)))

(defn- raw-try [r]
  (let [pat0 `[:try ~@(mapv raw (:body r))]
        pat (if-let [c (li/except-tag r)]
              (vec (with-raw-try-cases pat0 c))
              pat0)]
    (maybe-add-optional-raw-tags (dissoc r li/except-tag) pat)))

(defn _try [body cases]
  (when-not (vector? body)
    (u/throw-ex "Try body must be a vector"))
  {:type :try
   :body body
   li/except-tag cases})

(defn- introspect-for-each [pat]
  (let [src (introspect (first pat))
        body (extract-body-patterns #{:as} (rest pat))
        alias (alias-from-pattern pat)]
    {:type :for-each
     :src src
     :body (mapv introspect body)
     :as (introspect-alias alias)
     li/except-tag (introspect-case (case-from-pattern pat))}))

(defn- raw-for-each [r]
  (let [pat `[:for-each ~(raw (:src r)) ~@(mapv raw (:body r))]]
    (maybe-add-optional-raw-tags r pat)))

(defn for-each [src body]
  {:type :for-each
   :src src
   :body body})

(defn- introspect-match [pat]
  (let [fpat (first pat)
        has-value? (not (conditional? fpat))
        body (extract-body-patterns #{:as} (if has-value? (rest pat) pat))
        alias (alias-from-pattern pat)]
    {:type :match
     :value (when has-value?
              (if (keyword? fpat)
                fpat
                (introspect (first pat))))
     :body (loop [body body, result []]
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

(defn- raw-match [r]
  (let [body
        (collect-match-clauses
         (mapv (fn [v] (if (vector? v)
                         [(first v) (raw (second v))]
                         (raw v)))
               (:body r)))
        pat (if-let [v (:value r)]
              `[:match ~(raw v) ~@body]
              `[:match ~@body])]
    (maybe-add-optional-raw-tags r pat)))

(defn match
  ([value body]
   {:type :match
    :value value
    :body body})
  ([body] (match nil body)))

(defn- introspect-filter [pat]
  )

(defn- introspect-literal [pat]
  {:type :literal
   :value pat})

(def ^:private raw-literal :value)

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
  (let [main-recname (extract-main-record-name pat)]
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
                 (literal? pat) introspect-literal
                 :else (raise-syntax-error pat "Invalid object"))]
    (f pat)))

(defn raw [r]
  (when-let [f (case (:type r)
                 :query raw-query
                 :upsert raw-upsert
                 :query-upsert raw-upsert
                 :query-object raw-query-object
                 :for-each raw-for-each
                 :match raw-match
                 :delete raw-delete
                 :try raw-try
                 :quote raw-quote
                 :sealed raw-sealed
                 :literal raw-literal
                 identity)]
    (f r)))

(defn synatx-type? [t r] (= t (:type r)))

(def query? (partial synatx-type? :query))
(def upsert? (partial synatx-type? :upsert))
(def query-upsert? (partial synatx-type? :query-upsert))
(def query-object? (partial synatx-type? :query-object))
(def for-each? (partial synatx-type? :for-each))
(def match? (partial synatx-type? :match))
(def delete? (partial synatx-type? :delete))
(def try? (partial synatx-type? :try))
(def quote? (partial synatx-type? :quote))
(def sealed? (partial synatx-type? :sealed))
(def liuteral? (partial synatx-type? :literal))

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
