(ns agentlang.lang.name-util
  "Namespace for fully-qualified name utilities"
  (:require [clojure.string :as string]
            [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.lang.internal :as li]))

(defn- normalize-path [path]
  (let [s (str path)]
    (if (string/ends-with? s "?")
      (keyword (subs s 1 (dec (count s))))
      path)))

(def ^:dynamic fq-name nil)

(defn- make-fq-name
  "Return a function that returns the fully-qualified (component-name/n) name of `n`."
  [declared-names]
  (let [component-name (:component declared-names)
        recs (:records declared-names)]
    (fn [n]
      (if (li/name? n)
        (let [path (:path (li/path-parts n))]
          (if (and path (some #{(normalize-path path)} recs))
            (li/make-path component-name n)
            n))
        n))))

(declare map-with-fq-names fq-generic)

(defn- sanitized? [v]
  (or (keyword? v)
      (symbol? v)
      (map? v)
      (string? v)
      (number? v)
      (boolean? v)
      (nil? v)))

(defn- vals-sanitizer [attrs]
  (into
   {}
   (for [[k v] attrs]
     (if (sanitized? v)
       {k v}
       (if (= 'quote (first v))
         {k v}
         (let [qv (str "(quote " v ")")]
           {k #?(:clj (eval (read-string qv))
                 :cljs (cljs.reader/read-string qv))}))))))

(defn- maybe-merge-non-attrs [fq-inst orig-inst]
  (let [rels (when-let [rels (li/rel-tag orig-inst)]
               (fq-generic rels false))
        alias-def (:as orig-inst)
        with-types (when-let [tps (li/with-types-tag orig-inst)]
                     (fq-generic tps false))
        toms (li/timeout-ms-tag orig-inst)]
    (merge
     fq-inst
     (when rels
       {li/rel-tag rels})
     (when alias-def
       {:as alias-def})
     (when with-types
       {li/with-types-tag with-types})
     (when toms
       {li/timeout-ms-tag toms}))))

(defn- fq-inst-pat
  "Update the keys and values in an instance pattern with
   component-qualified names."
  [inst is-recdef]
  (let [x (li/normalize-instance-pattern inst)
        n (first (keys x))
        attrs (first (vals x))
        mvs (vals-sanitizer attrs)]
    (maybe-merge-non-attrs
     {(fq-name n) (map-with-fq-names mvs is-recdef)}
     inst)))

(defn- fq-map
  "Update the keys and values in a map literal with
   component-qualified names."
  [x is-recdef]
  (let [y (mapv (fn [[k v]]
                  [(fq-name k) (fq-generic v is-recdef)])
                x)]
    (into {} y)))

(defn- fq-generic
  "Update a data-literal in a component with fully-qualified names."
  [v is-recdef]
  (if (and (keyword? v) (string/starts-with? (name v) ":%"))
    v
    (if (li/quoted? v)
      (w/prewalk
        #(if (li/unquoted? %)
           (fq-generic % is-recdef)
           %)
        v)
      (cond
        (li/name? v) (fq-name v)
        (map? v) (if (li/instance-pattern? v)
                   (fq-inst-pat v is-recdef)
                   (fq-map v is-recdef))
        (su/list-or-cons? v) (if-not is-recdef
                               (doall (reverse (into '() (mapv #(fq-generic % is-recdef) v))))
                               v)
        (vector? v) (mapv #(fq-generic % is-recdef) v)
        :else v))))

(defn- fq-map-entry [[k v] is-recdef]
  [k (fq-generic v (if (= :meta k) false is-recdef))])

(defn- map-with-fq-names [m is-recdef]
  (into {} (doall (map #(fq-map-entry % is-recdef) m))))

(defn- fq-preproc-attribute-def
  "Preprocess an attribute definition to add fully-qualified names."
  [exp]
  `(~(symbol "attribute") ~(fq-name (second exp))
    ~(let [scm (su/third exp)]
       (if (map? scm)
         (map-with-fq-names scm false)
         (fq-name scm)))))

(defn- fq-preproc-record-def
  "Preprocess a record, entity or event definition to add fully-qualified names."
  [exp]
  (let [scm (second exp)]
    (if (map? scm)
      `(~(symbol (name (first exp)))
        ~(fq-inst-pat scm true))
      `(~(symbol (name (first exp)))
        ~(fq-name scm)
        ~(map-with-fq-names (su/third exp) true)))))

(defn- fq-named-df-pat [pat]
  (let [k (fq-name (first (keys pat)))
        vs (first (vals pat))
        mvs (vals-sanitizer vs)
        mergvs (merge vs mvs)]
    {k mergvs}))

(defn- preproc-crud? [pat]
  (let [f (first pat)]
    (or (= :after f) (= :before f))))

(defn- fq-preproc-dataflow-def
  "Preprocess a dataflow to add fully-qualified names."
  [exp]
  (let [pat (second exp)
        body (nthrest exp 2)
        proc-pat (cond
                   (map? pat)
                   (fq-named-df-pat pat)

                   (keyword? pat)
                   (fq-name pat)

                   (vector? pat)
                   (if (preproc-crud? pat)
                     (assoc pat 2 (fq-name (nth pat 2)))
                     (assoc pat 1 (fq-name (first pat)))))
        proc-body (mapv #(fq-generic % false) body)]
    `(~(symbol "dataflow") ~proc-pat ~@proc-body)))

(defn- fq-preproc-rule-def [exp]
  `(~(symbol "rule") ~(fq-name (second exp)) ~@(mapv #(fq-generic % false) (nthrest exp 2))))

(defn- fq-preproc-pattern-def [pat-exp]
  (let [orig-exp (second pat-exp)
        exp (fq-generic orig-exp false)]
    `(~(symbol "pattern") ~exp)))

;; Preprocssors to add fully-qualified names to each type
;; of expression.
(def ^:private fq-preproc-defs
  {'attribute fq-preproc-attribute-def
   'record fq-preproc-record-def
   'entity fq-preproc-record-def
   'view fq-preproc-record-def
   'event fq-preproc-record-def
   'relationship fq-preproc-record-def
   'inference fq-preproc-record-def
   'rule fq-preproc-rule-def
   'dataflow fq-preproc-dataflow-def
   'resolver fq-preproc-record-def
   'pattern fq-preproc-pattern-def})

(defn- assert-valid-alias! [rec-names pat]
  (when (or (map? pat) (vector? pat))
    (when-let [alias (li/extract-alias-from-pattern pat)]
      (when (some #{alias} rec-names)
        (u/throw-ex (str "Alias " alias " overwrites a record-name. Please rename the alias in " pat)))))
  pat)

(defn fully-qualified-names [declared-names exp]
  (w/prewalk (partial assert-valid-alias! (:records declared-names)) exp)
  (binding [fq-name (make-fq-name declared-names)]
    (if (seqable? exp)
      ((get fq-preproc-defs (first exp) identity) exp)
      exp)))

(defn generic-fully-qualified-names [declared-names exp]
  (binding [fq-name (make-fq-name declared-names)]
    (fq-generic exp false)))
