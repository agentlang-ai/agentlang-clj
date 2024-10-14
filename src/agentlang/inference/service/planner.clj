(ns agentlang.inference.service.planner
  (:require [agentlang.util :as u]
            [agentlang.lang.internal :as li]))

(defn- validate-record-expr [[n attrs :as expr] alias]
  (when-not (li/name? n)
    (u/throw-ex (str "Invalid record name in " expr)))
  (when-not (map? attrs)
    (u/throw-ex (str "Attributes must be a map: " expr)))
  (when alias
    (if (vector? alias)
      (when-not (every? keyword? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))
      (when-not (li/name? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))))
  n)

(defn- maybe-alias [x]
  (when (symbol? x) (keyword x)))

(defn- parse-ref-or-expr [v]
  (cond
    (list? v)
    (cond
      (some #{(first v)} '(= < > <= >= and or)) (parse-ref-or-expr (vec v))
      (keyword? (first v)) (li/make-ref (u/symbol-as-keyword (second v)) (first v)) ; TODO: handle references more than one level deep
      :else `(~(first v) ~@(reverse (into '() (mapv parse-ref-or-expr (rest v))))))
    (vector? v) [(u/symbol-as-keyword (first v))
                 (or (maybe-alias (second v)) (parse-ref-or-expr (second v)))
                 (or (maybe-alias (last v)) (parse-ref-or-expr (last v)))]
    (symbol? v) (keyword v)
    :else v))

(defn- parse-value-refs-and-exprs
  ([keyfmt attrs]
   (into
    {}
    (mapv (fn [[k v]] [(keyfmt k) (parse-ref-or-expr v)]) attrs)))
  ([attrs] (parse-value-refs-and-exprs identity attrs)))

(defn- parse-make [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    (merge {n (parse-value-refs-and-exprs attrs)}
           (when alias {:as alias}))))

(defn- parse-lookup [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    (merge
     (if (seq attrs)
       {n (parse-value-refs-and-exprs li/name-as-query-pattern attrs)}
       {(li/name-as-query-pattern n) {}})
     (when alias {:as alias}))))

(defn- parse-lookup-one [expr alias]
  (parse-lookup expr (when alias [alias])))

(declare expression-to-pattern)

(def ^:private parse-lookup-many parse-lookup)

(defn- parse-cond [expr alias]
  (loop [expr expr, pats []]
    (let [[condition consequent] expr]
      (if (and condition consequent)
        (recur
         (nthrest expr 2)
         (conj
          pats
          (if (= :else condition)
            [(expression-to-pattern consequent)]
            [(parse-ref-or-expr condition) (expression-to-pattern consequent)])))
        (let [result (apply concat [:match] pats)]
          (vec (if alias
                 (concat result [:as alias])
                 result)))))))

(defn- parse-update [[n attrs new-attrs] alias]
  (let [qexpr (parse-lookup-one [n attrs] nil)
        qattrs (li/record-attributes qexpr)]
    {n (merge qattrs (parse-value-refs-and-exprs new-attrs)
              (when alias {:as alias}))}))

(defn- parse-delete [[n attrs] alias]
  (let [pat [:delete n attrs]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- parse-fn-call [expr alias]
  (let [pat [:eval `'(~(first expr) ~@(mapv parse-ref-or-expr (rest expr)))]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- parse-binding [expr alias]
  ((case (first expr)
     make parse-make
     cond parse-cond
     lookup-one parse-lookup-one
     lookup-many parse-lookup-many
     update parse-update
     delete parse-delete
     parse-fn-call)
   (rest expr) alias))

(declare expressions-to-patterns expression-to-pattern)

(defn- parse-for-each [[n expr] alias]
  (let [pat [:for-each (parse-ref-or-expr n) (expression-to-pattern expr)]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- const-expr? [expr]
  (or (string? expr) (number? expr)))

(defn expression-to-pattern [expr]
  (cond
    (const-expr? expr) expr
    (symbol? expr) (u/symbol-as-keyword expr)
    :else
    (if (seqable? expr)
      (case (first expr)
        def (parse-binding (nth expr 2) (u/symbol-as-keyword (second expr)))
        cond (parse-cond (rest expr) nil)
        for-each (parse-for-each (rest expr) nil)
        do (expressions-to-patterns expr)
        (parse-binding expr nil))
      (u/throw-ex (str "Invalid expression: " expr)))))

(defn maybe-an-expression? [expr]
  (and (seqable? expr) (some #{(first expr)} '(def cond for-each))))

(defn maybe-expressions? [exprs]
  (when-not (string? exprs)
    (and (seqable? exprs) (= 'do (first exprs)))))

(defn expressions-to-patterns [exprs]
  (when (maybe-expressions? exprs)
    (mapv expression-to-pattern (rest exprs))))
