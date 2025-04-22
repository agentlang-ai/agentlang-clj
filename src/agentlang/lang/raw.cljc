(ns agentlang.lang.raw
  "Raw (edn) representation of all interned component-elements"
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang.internal :as li]
            #?(:clj [agentlang.lang.pub-schema :as pubs])))

(def ^:private raw-store (u/make-cell {}))

(defn- publish-schema? [record-name]
  #?(:clj
     (and (pubs/publish-schema?)
          (not (s/starts-with?
                (s/lower-case (subs (str (first (li/split-path record-name))) 1))
                "agentlang")))
     :cljs false))

(defn- maybe-publish-add-definition [tag record-name attrs]
  #?(:clj
     (when (publish-schema? record-name)
       (pubs/publish-event {:operation :add :tag tag :type record-name :schema attrs}))))

(defn- maybe-publish-remove-definition [tag record-name]
  #?(:clj
     (when (publish-schema? record-name)
       (pubs/publish-event {:operation :delete :tag tag :type record-name}))))

(defn- process-component-spec [spec]
  (if-let [clj-imps (:clj-import spec)]
    (if-not (= 'quote (first clj-imps))
      (assoc spec :clj-import `(~'quote ~clj-imps))
      spec)
    spec))

(def ^:private active-component
  #?(:clj (ThreadLocal.)
     :cljs (atom nil)))

(defn set-active-component! [component-name]
  #?(:clj (.set active-component component-name)
     :cljs (reset! active-component component-name)))

(defn get-active-component []
  #?(:clj (.get active-component)
     :cljs @active-component))

(defn component [component-name spec]
  (let [cdef (get @raw-store component-name '())
        cspec (concat `(~'component ~component-name) (when spec [(process-component-spec spec)]))
        new-cdef (conj (rest cdef) cspec)]
    (u/safe-set raw-store (assoc @raw-store component-name new-cdef))
    (maybe-publish-add-definition 'component component-name spec)
    (set-active-component! component-name)
    component-name))

(defn intern-component [component-name defs]
  (u/safe-set raw-store (assoc @raw-store component-name (seq defs)))
  component-name)

(defn update-component-defs [component-name f]
  (u/call-and-set
   raw-store
   #(let [rs @raw-store]
      (if-let [cdef (get rs component-name)]
        (assoc rs component-name (f cdef))
        rs)))
  component-name)

(defn- count-defs [tag component-name]
  (let [cdef (get @raw-store component-name)]
    (count (filter #(= tag (first %)) cdef))))

(defn append-to-component [component-name definition]
  (update-component-defs component-name #(concat % [definition])))

(defn remove-from-component [component-name predic]
  (update-component-defs component-name #(filter (complement predic) %)))

(defn- replace-in-component-by-tag [component-name tag index newdef]
  (u/call-and-set
   raw-store
   #(let [rs @raw-store]
      (loop [cdef (get rs component-name), i index, fs []]
        (if (neg? i)
          rs
          (if-let [def (first cdef)]
            (if (seqable? def)
              (let [has-tag (= tag (first def))]
                (if (and has-tag (zero? i))
                  (assoc rs component-name (concat fs (when newdef [newdef]) (rest cdef)))
                  (recur (rest cdef) (if has-tag (dec i) i) (conj fs def))))
              (recur (rest cdef) i (conj fs def)))
            rs)))))
  component-name)

(defn- remove-from-component-by-tag [component-name tag index]
  (replace-in-component-by-tag component-name tag index nil))

(defn find-in-component [component-name predic]
  (when-let [cdef (get @raw-store component-name)]
    (filter predic cdef)))

(defn update-in-component [component-name predic definition]
  (u/call-and-set
   raw-store
   #(let [rs @raw-store
          new-cdef
          (loop [cdef (get rs component-name), new-cdef []]
            (if-let [d (first cdef)]
              (if (predic d)
                (concat (conj new-cdef definition) (rest cdef))
                (recur (rest cdef) (conj new-cdef d)))
              new-cdef))]
      (assoc rs component-name new-cdef)))
  component-name)

(defn- tagged-expr?
  ([tag n x]
   (and (seqable? x) (= (first x) tag) (= (second x) n)))
  ([tag x]
   (and (seqable? x) (= (first x) tag))))

(def ^:private clj-defn? (partial tagged-expr? 'defn))
(def ^:private clj-def? (partial tagged-expr? 'def))

(defn- upsert-function
  [component-name function-name params-vector upsert-fn]
  (when-not (symbol? function-name)
    (u/throw-ex (str "invalid function name: " function-name)))
  (when-not (vector? params-vector)
    (u/throw-ex (str "not a vector: " params-vector)))
  (upsert-fn))

(defn create-function
  ([component-name function-name params-vector body]
   (create-function component-name function-name nil params-vector body))
  ([component-name function-name docstring params-vector body]
   (upsert-function
    component-name function-name params-vector
    #(append-to-component
      component-name
      (if docstring
        `(~'defn ~function-name ~docstring ~params-vector ~body)
        `(~'defn ~function-name ~params-vector ~body))))))

(defn update-function
  ([component-name function-name params-vector body]
   (update-function component-name function-name nil params-vector body))
  ([component-name function-name docstring params-vector body]
   (upsert-function
    component-name function-name params-vector
    #(update-in-component
      component-name (partial clj-defn? function-name)
      (if docstring
        `(~'defn ~function-name ~docstring ~params-vector ~body)
        `(~'defn ~function-name ~params-vector ~body))))))

(defn delete-function [component-name function-name]
  (remove-from-component component-name (partial clj-defn? function-name)))

(defn find-defn [component-name function-name]
  (first (find-in-component component-name (partial clj-defn? function-name))))

(defn get-function-params [component-name function-name]
  (nth (find-defn component-name function-name) 2))

(defn get-function-body [component-name function-name]
  (nth (find-defn component-name function-name) 3))

(defn get-function-names [component-name]
  (when-let [cdef (get @raw-store component-name)]
    (mapv second (filter clj-defn? cdef))))

(defn create-definition [component-name varname expr]
  (when-not (symbol? varname)
    (u/throw-ex (str "invalid variable name: " varname)))
  (append-to-component component-name `(~'def ~varname ~expr)))

(defn update-definition [component-name varname expr]
  (when-not (symbol? varname)
    (u/throw-ex (str "invalid variable name: " varname)))
  (update-in-component component-name (partial clj-def? varname) `(~'def ~varname ~expr)))

(defn delete-definition [component-name varname]
  (remove-from-component component-name (partial clj-def? varname)))

(defn- find-def-expr [component-name varname]
  (first (find-in-component component-name (partial clj-def? varname))))

(defn get-definition-expr [component-name varname]
  (nth (find-def-expr component-name varname) 2))

(defn update-component-spec! [component-name spec-key spec]
  (when-let [cdef (get @raw-store component-name)]
    (let [cn (first cdef)
          [_ _ cspec] cn
          new-cspec (process-component-spec (assoc cspec spec-key spec))
          new-cdef (conj (rest cdef) `(~'component ~component-name ~new-cspec))]
      (u/safe-set raw-store (assoc @raw-store component-name new-cdef))
      component-name)))

(defn- infer-component-name [defs]
  (when (seqable? defs)
    (when-not (or (map? defs) (string? defs))
      (when-let [d (first (filter #(= 'component (first %)) defs))]
        (second d)))))

(defn maybe-intern-component [defs]
  (when-let [component-name (infer-component-name defs)]
    (intern-component component-name defs)))

(defn prepost-header? [n]
  (some #{(first n)} #{:after :before}))

(defn- extract-def-name [obj]
  (cond
    (keyword? obj) obj
    (vector? obj) (if (prepost-header? obj)
                    obj
                    (first obj))
    :else (li/record-name obj)))

(defn- def? [tag full-name rec-name obj]
  (and (= (first obj) (symbol tag))
       (let [n (extract-def-name (second obj))]
         (or (= n full-name) (= n rec-name)))))

(defn- extract-record-name [n]
  (cond
    (keyword? n) n
    (vector? n) (if (prepost-header? n)
                  (nth n 2)
                  (first n))
    :else (u/throw-ex (str "cannot extract record name from " n))))

(defn- get-all-defs [record-name]
  (let [s @raw-store
        recname (extract-record-name record-name)
        [c n] (li/split-path recname)]
    (when-let [defs (get s c)]
      [defs c n])))

(defn- remove-def [tag full-recname]
  (when-let [[defs c rec-name] (get-all-defs full-recname)]
    [c (filter (complement (partial def? tag full-recname rec-name)) defs)]))

(defn- find-def [tag full-recname]
  (when-let [[defs _ rec-name] (get-all-defs full-recname)]
    (first (filter (partial def? tag full-recname rec-name) defs))))

(defn- quote-fn-calls [spec]
  (cond
    (map? spec)
    (into {} (mapv (fn [[k v]]
                     [k (cond
                          (and (list? v) (not= 'quote (first v)))
                          `(quote ~v)

                          (map? v) (quote-fn-calls v)

                          :else v)])
                   spec))
    (and (vector? spec) (= :eval (first spec)) (not= 'quote (first (second spec))))
    (assoc spec 1 `(quote ~(second spec)))

    :else spec))

(defn- replace-def [tag full-recname new-spec]
  (when-let [[defs c rec-name] (get-all-defs full-recname)]
    (let [tag (symbol tag)
          p? (partial def? tag full-recname rec-name)
          ndef (if (= 'dataflow tag)
                 `(~tag ~full-recname ~@(map quote-fn-calls new-spec))
                 `(~tag ~full-recname ~(quote-fn-calls new-spec)))]
      (loop [defs defs, found false, new-defs []]
        (if-let [d (first defs)]
          (if (p? d)
            (recur (rest defs) true (conj new-defs ndef))
            (recur (rest defs) found (conj new-defs d)))
          [c (seq (if found new-defs (conj new-defs ndef)))])))))

(defn find-definition [tag full-recname]
  (when-let [d (find-def tag full-recname)]
    (let [s (second d)]
      (if (keyword? s)
        (nth d 2)
        (li/record-attributes d)))))

(def find-entity (partial find-definition 'entity))
(def find-relationship (partial find-definition 'relationship))
(def find-record (partial find-definition 'record))
(def find-event (partial find-definition 'event))
(def find-rule (partial find-definition 'rule))
(def find-inference (partial find-definition 'inference))
(def find-resolver (partial find-definition 'resolver))
(def find-schedule (partial find-definition 'schedule))
(def find-ui (partial find-definition 'ui))

(defn find-attribute [n]
  (when-not (li/internal-attribute-name? n)
    (find-definition 'attribute n)))

(defn- change-defs [f]
  (when-let [[c defs] (f)]
    (u/safe-set raw-store (assoc @raw-store c defs))
    defs))

(defn add-definition [tag record-name attrs]
  (when (change-defs #(replace-def tag record-name attrs))
    (maybe-publish-add-definition tag record-name attrs)
    record-name))

(defn remove-definition [tag record-name]
  (case (symbol tag)
    event (remove-definition 'dataflow record-name)
    entity (doseq [evt (li/prepost-event-heads record-name)]
             (remove-definition 'dataflow evt))
    true)
  (when (change-defs #(remove-def tag record-name))
    (maybe-publish-remove-definition tag record-name)
    record-name))

(defn attribute [n spec]
  (if (li/internal-attribute-name? n)
    n
    (add-definition 'attribute n spec)))

(def record (partial add-definition 'record))
(def entity (partial add-definition 'entity))
(def ui (partial add-definition 'ui))
(def relationship (partial add-definition 'relationship))
(def dataflow (partial add-definition 'dataflow))
(def rule (partial add-definition 'rule))
(def inference (partial add-definition 'inference))
(def resolver (partial add-definition 'resolver))
(def schedule (partial add-definition 'schedule))

(def ^:private count-patterns (partial count-defs 'pattern))

(defn pattern [pat]
  (if-let [cn (get-active-component)]
    (let [pns (count-patterns cn)]
      [(append-to-component cn `(~'pattern ~pat)) pns])
    (u/throw-ex (str "no active component to add pattern - " pat))))

(defn remove-pattern [cn pattern-index]
  (remove-from-component-by-tag cn 'pattern pattern-index))

(defn replace-pattern [cn pattern-index new-pattern]
  (replace-in-component-by-tag cn 'pattern pattern-index `(~'pattern ~new-pattern)))

(defn remove-component [cname]
  (u/safe-set raw-store (dissoc @raw-store cname))
  (maybe-publish-remove-definition 'component cname)
  cname)

(defn event [record-name attrs]
  (if (:inferred attrs)
    record-name
    (add-definition 'event record-name attrs)))

(def remove-attribute (partial remove-definition 'attribute))
(def remove-record (partial remove-definition 'record))
(def remove-entity (partial remove-definition 'entity))
(def remove-relationship (partial remove-definition 'relationship))
(def remove-dataflow (partial remove-definition 'dataflow))
(def remove-rule (partial remove-definition 'rule))
(def remove-inference (partial remove-definition 'inference))
(def remove-resolver (partial remove-definition 'resolver))
(def remove-schedule (partial remove-definition 'schedule))
(def remove-ui (partial remove-definition 'ui))

(defn remove-event [event-name]
  (if (vector? event-name) ; pre-post event - e.g: [:after :create :AnEntity]
    (remove-definition 'event event-name)
    (when (remove-dataflow event-name)
      (remove-inference event-name)
      (remove-definition 'event event-name))))

(defn remove-definition-by-tag [kw-tag record-name]
  (case kw-tag
    :entity (do (remove-entity record-name)
                (remove-relationship record-name)
                record-name)
    :event (remove-event record-name)
    :record (remove-record record-name)
    :rule (remove-rule record-name)
    :inference (remove-inference record-name)
    :resolver (remove-resolver record-name)
    :schedule (remove-schedule record-name)
    :attribute (remove-attribute record-name)))

(defn fetch-attributes [tag record-name]
  (when-let [d (find-def tag record-name)]
    (last d)))

(def entity-attributes (partial fetch-attributes 'entity))
(def record-attributes (partial fetch-attributes 'record))
(def relationship-attributes entity-attributes)

(defn fetch-all-defs [tag component-name]
  (filter #(= tag (first %)) (get @raw-store component-name)))

(def fetch-all-dataflows (partial fetch-all-defs 'dataflow))
(def fetch-all-rules (partial fetch-all-defs 'rule))
(def fetch-all-inferences (partial fetch-all-defs 'inference))
(def fetch-all-resolvers (partial fetch-all-defs 'resolver))
(def fetch-all-schedules (partial fetch-all-defs 'schedule))
(def fetch-all-patterns (comp (partial mapv second) (partial fetch-all-defs 'pattern)))

(defn record-attributes-include-inherits [entity-name]
  (let [raw-attrs (or (entity-attributes entity-name)
                      (record-attributes entity-name))
        attrs (apply dissoc raw-attrs li/property-names)]
    (if-let [p (:inherits (:meta raw-attrs))]
      (merge attrs (record-attributes-include-inherits p))
      attrs)))

(defn entity-meta [entity-name]
  (:meta (entity-attributes entity-name)))

(defn- normalize-expressions [defs]
  (mapv (fn [d]
          (if (seqable? d)
            (case (first d)
              rule `(~(symbol "rule") ~(second d) ~@(first (nthrest d 2)))
              pattern (second d)
              d)
            d))
        defs))

(defn as-edn [component-name]
  (when-let [defs (seq (get @raw-store component-name))]
    `(do ~@(normalize-expressions defs))))

(defn raw-store-reset! []
  (u/safe-set raw-store {}))
