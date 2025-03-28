(ns agentlang.component
  "Components of a model."
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.walk :as w]
            [agentlang.lang.datetime :as dt]
            [agentlang.lang.internal :as li]
            [agentlang.lang.raw :as raw]
            [agentlang.global-state :as gs]
            [agentlang.meta :as mt]
            [agentlang.util :as u]
            [agentlang.util.errors :refer [make-error raise-error throw-ex-info]]
            [agentlang.util.hash :as sh]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.seq :as su]))

(def ^:private type-key :-*-type-*-)
(def ^:private dirty-key :-*-dirty-*-)
(def type-tag-key :type-*-tag-*-)
(def id-attr-type :Agentlang.Kernel.Lang/UUID)
(def id-attr li/id-attr)
(def instance->map identity)
(def instance-type-tag type-tag-key)
(def schema-type-tag type-tag-key)
(def instance-type type-key)

(def additional-internal-component-names
  #{:Agentlang.Kernel.Store
    :raw
    :-*-containers-*-
    :Agentlang.Kernel.UserApp
    :Agentlang.Kernel.Repl
    :Agentlang.Kernel.Eval})

(def non-instance-user-attr-keys
  #{type-tag-key id-attr type-key dirty-key})

(def ^:private models (u/make-cell {}))

(defn register-model [model-name spec]
  (u/safe-set models (assoc @models model-name spec))
  model-name)

(defn unregister-model [model-name]
  (u/safe-set models (dissoc @models model-name))
  model-name)

(defn model-names [] (keys @models))
(defn fetch-model [name] (get @models name))
(defn model-property [prop name] (prop (fetch-model name)))
(def model-version (partial model-property :version))
(def model-config-entity (partial model-property :config-entity))

(defn model-for-component [component-name]
  (ffirst (filter (fn [[_ spec]]
                    (some #{component-name} (:components spec)))
                  @models)))

(defn add-component-to-model [model-name component-name]
  (when-let [spec (get @models model-name)]
    (let [cns (vec (set (conj (:components spec) component-name)))]
      (register-model model-name (assoc spec :components cns)))))

(defn get-model-version [component]
  (or (model-version (model-for-component component)) "0.0.1"))

(defn internal-component-names
  "Returns vector of internal component names."
  []
  (vec (set/union additional-internal-component-names
                  (set (:components (fetch-model :Agentlang))))))

(defn remove-internal-components
  [components]
  (vec (set/difference (set components) (set (internal-component-names)))))

(defn internal-component? [c]
  (some #{c} (set (internal-component-names))))

(def components
  "Table that maps component names to their definitions."
  #?(:clj  (ref {})
     :cljs (atom {})))

(def ^:private kernel-userapp-component :Agentlang.Kernel.UserApp)

(def ^:private current-component
  "The name of the active component for the current thread."
  #?(:clj
     (proxy [ThreadLocal] []
       (initialValue [] kernel-userapp-component))
     :cljs
     (atom nil)))

(def ^:private components-inited
  "All components inited, but init-events not yet fired."
  #?(:clj (ref [])
     :cljs (atom [])))

(defn set-current-component [n]
  #?(:clj (dosync (.set current-component n)
                  (ref-set components-inited (conj @components-inited n)))
     :cljs (do (reset! current-component n)
               (reset! components-inited (conj @components-inited n))))
  n)

(defn get-current-component []
  #?(:clj (.get current-component)
     :cljs @current-component))

(defn switch-component [n]
  (let [oldn (get-current-component)]
    (set-current-component n)
    oldn))

(defn fetch-components-inited! []
  #?(:clj
     (dosync
      (let [ms @components-inited]
        (dosync (ref-set components-inited []))
        ms))
     :cljs (let [ms @components-inited]
             (reset! components-inited [])
             ms)))

(def full-name li/make-path)

(defn canonical-type-name
  "Return the fully-qualified type-name."
  ([component typname]
   (if (pos? (.indexOf (str typname) "/"))
     typname
     (full-name (or component (get-current-component)) typname)))
  ([typname] (canonical-type-name nil typname)))

(defn normalize-type-name [^String n]
  (last (li/split-path n)))

(defn component-init-event-name [component]
  (keyword (str (name component) "_Init")))

(defn- upsert-component! [component spec]
  (u/call-and-set
   components
   #(assoc-in @components [component (str (get-model-version component))] spec)))

(declare intern-attribute intern-event entity? between-relationship?)

(defn create-component
  "Create a new component with the given name and references to
  the components in the imports list. If a component already exists with
  the same name, it will be overwritten. Returns the name of the new component."
  [component spec]
  (upsert-component! component spec)
  (intern-attribute
   [component id-attr]
   {:type :Agentlang.Kernel.Lang/UUID
    :unique true
    :immutable true
    li/path-identity true
    :default u/uuid-string})
  (intern-event [component (component-init-event-name component)]
                {:ComponentName :Agentlang.Kernel.Lang/Keyword})
  (set-current-component component)
  component)

(defn remove-component [component]
  (let [r (u/call-and-set components #(dissoc @components component))]
    (raw/remove-component component)))

(defn component-names
  ([]
   (filter li/name? (keys @components)))
  ([prefix]
   (let [s (str prefix)]
     (filter #(s/starts-with? (str %) s) (component-names)))))

(defn component-exists? [component]
  (if (find @components component)
    true
    false))

(defn component-definition [component]
  (when-let [scm (get-in @components [component (get-model-version component)])]
    [component scm]))

(defn component-specification [component]
  (second (component-definition component)))

(defn declared-names [component]
  (when-let [defs (second (component-definition component))]
    (set (keys (dissoc defs :attributes :records :events :entity-relationship)))))

(defn component-clj-imports [component]
  (when-let [imps (seq (:clj-import (component-specification component)))]
    (let [imps (if (= 'quote (first imps)) (second imps) imps)] ; check for quote literal in cljs
      (into {} (mapv (fn [xs] [(first xs) (vec (rest xs))]) imps)))))

(defn set-component-clj-imports! [component spec]
  (when-let [old-spec (component-specification component)]
    (let [clj-spec (vec (mapv (fn [[k v]] `(~k ~@v)) spec))]
      (raw/update-component-spec! component :clj-import clj-spec)
      (upsert-component!
       component
       (assoc old-spec :clj-import clj-spec)))
    component))

(defn component-references [component]
  (:refer (component-specification component)))

(defn set-component-references! [component spec]
  (when-let [old-spec (component-specification component)]
    (raw/update-component-spec! component :refer spec)
    (upsert-component!
     component
     (assoc old-spec :refer spec))
    component))

(defn extract-alias-of-component [component alias-entry]
  (if (component-exists? component)
    (get-in @components [component :alias alias-entry])
    (log/error (str "Component " component " is not present!"))))

(defn instance-type-kw [inst]
  (when-let [t (instance-type inst)]
    (if (keyword? t)
      t
      (li/make-path t))))

(defn- conj-meta-key [path]
  (conj path mt/meta-key))

(declare find-attribute-schema-internal)

(defn fetch-attribute-meta [path]
  (:meta (find-attribute-schema-internal path)))

(defn fetch-meta
  ([path recversion]
   (let [p (if (string? path)
             (keyword path)
             path)
         [c e] (li/split-path p)]
     (if-let [scm (get-in @components [c (or recversion (get-model-version c))
                                       e mt/meta-key])]
       (assoc
        scm
        mt/meta-of-key
        (if (keyword? p)
          p
          (li/make-path p)))
       (fetch-attribute-meta p))))
  ([path]
   (fetch-meta path nil)))

(def meta-of mt/meta-of-key)

(defn- intern-meta [components rec-name meta]
  (let [p rec-name
        c (first (li/split-path p))
        e (last (li/split-path p))]
    (assoc-in components [c (get-model-version c) e mt/meta-key] meta)))

(defn- remove-meta! [rec-name]
  (let [p rec-name
        c (first (li/split-path p))
        e (last (li/split-path p))]
    (u/call-and-set
     components
     #(su/dissoc-in @components [c (get-model-version c) e mt/meta-key]))))

(defn- component-intern
  "Add or replace a component entry.
  `typname` must be in the format - :ComponentName/TypName
  Returns the name of the entry. If the component is non-existing, raise an exception."
  ([typname typdef typtag meta]
   (let [[a b :as cn] (li/split-path typname)
         [component n :as k] (if (and a b) cn [(get-current-component) a])
         intern-k [component (get-model-version component) typtag n]]
     (when-not (component-exists? component)
       (log/info (str "auto-creating component - " component))
       (create-component component nil))
     (when-let [pp (mt/apply-policy-parsers k meta)]
       (log/debug (str "custom parse policies for " typname " - " pp)))
     (u/call-and-set
      components
      #(assoc-in (if meta
                   (intern-meta @components k meta)
                   @components)
                 intern-k typdef))
     typname))
  ([typname typdef typtag]
   (component-intern typname typdef typtag nil)))

(defn- component-remove [typname typtag]
  (let [[component n :as k] (li/split-path typname)
        intern-k [component (get-model-version component) typtag n]]
    (u/call-and-set
     components
     #(su/dissoc-in @components intern-k))
    typname))

(defn- component-find
  ([path]
   (get-in @components path))
  ([typetag recname]
   (let [[c n] (li/split-path recname)]
     (component-find [c (get-model-version c) typetag n])))
  ([typetag recname recversion]
   (let [[c n] (li/split-path recname)]
     (component-find [c (or recversion (get-model-version c)) typetag n]))))

(defn intern-attribute
  "Add or replace an attribute in a component.
  The attribute name must be fully-qualified, as in - `:ComponentName/AttrName`.
  Returns the name of the attribute. If the component is non-existing, raise an exception."
  [attrname attrdef]
  (component-intern attrname attrdef :attributes))

(defn- normalize-recdef [recdef typetag]
  (let [recdef (if (:schema recdef)
                 recdef
                 {:schema recdef})]
    (if (type-tag-key recdef)
      recdef
      (assoc recdef type-tag-key typetag))))

(defn intern-record
  "Add or replace an record in a component.
  The record name must be fully-qualified, as in - `:ComponentName/RecName`
  Returns the name of the record. If the component is non-existing, raise an exception."
  ([typetag recname recdef]
   (let [meta (:meta recdef)
         recdef (dissoc recdef :meta)]
     (component-intern recname (normalize-recdef recdef typetag) :records meta)))
  ([recname recdef]
   (intern-record :record recname recdef)))

(def intern-entity (partial intern-record :entity))
(def intern-event (partial intern-record :event))
(def intern-relationship intern-entity)

(defn- find-attribute-schema-internal
  "Find and return an attribute schema by the given path.
  Path should be in one of the following forms:
   - :ComponentName/AttributeName
   - :ComponentName/RecordName.AttributeName
  If the lookup succeeds, return the attribute schema as a map.
  Return `nil` on lookup failure."
  ([component version aref]
   (let [[recname attrname] (li/split-ref aref)]
     (if attrname
       (when-let [rec (component-find [component (or version (get-model-version component)) :records recname])]
         (let [[c a] (li/split-path (get-in rec [:schema attrname]))]
           (find-attribute-schema-internal c version a)))
       (when-let [scm (component-find [component (or version (get-model-version component)) :attributes aref])]
         (if-let [parent-type (:type scm)]
           (merge
            (let [[c a] (li/split-path parent-type)]
              (find-attribute-schema-internal c version a)) scm)
           scm)))))
  ([component aref]
   (find-attribute-schema-internal component nil aref))
  ([path]
   (let [[component aref] (li/split-path path)]
     (find-attribute-schema-internal component aref))))

(defn find-attribute-schema
  ([component version aref]
   (dissoc (find-attribute-schema-internal component version aref) :meta))
  ([component aref]
   (find-attribute-schema component nil aref))
  ([path]
   (dissoc (find-attribute-schema-internal path) :meta)))

(defn find-attribute-schema-version
  [version path]
  (let [[component aref] (li/split-path path)]
    (find-attribute-schema-internal component version aref)))

(defn all-attributes [component]
  (component-find [component (get-model-version component) :attributes]))

(defn find-record-schema
  ([component version aref]
   (component-find [component (or version (get-model-version component)) :records aref]))
  ([component aref]
   (find-record-schema component nil aref))
  ([path]
   (let [[component aref] (li/split-path path)]
     (find-record-schema component aref))))

(defn find-record-schema-version
  [version path]
  (let [[component aref] (li/split-path path)]
    (find-record-schema component version aref)))

(defn- find-record-schema-by-type-version [typ version path]
  (when-let [scm (find-record-schema-version version path)]
    (when (= (type-tag-key scm) typ)
      scm)))

(defn- find-record-schema-by-type [typ path]
  (find-record-schema-by-type-version typ nil path))

(defn find-entity-schema
  ([path version] (find-record-schema-by-type-version :entity version path))
  ([path] (find-entity-schema path nil)))

(def find-event-schema (partial find-record-schema-by-type :event))

(defn find-schema
  ([path version]
   (let [fn-find-entity-schema (partial find-record-schema-by-type-version :entity version)
         fn-find-event-schema (partial find-record-schema-by-type-version :event version)
         fn-find-record-schema (partial find-record-schema-version version)
         fn-find-attribute-schema (partial find-attribute-schema-version version)]
     (u/first-applied [fn-find-entity-schema :entity
                       fn-find-event-schema :event
                       fn-find-record-schema :record
                       fn-find-attribute-schema :attribute]
                      [path])))
  ([path]
   (find-schema path nil)))

(defn fetch-schema
  ([rec-name version]
   (:schema (second (find-schema rec-name version))))
  ([rec-name]
   (fetch-schema rec-name nil)))

(defn fetch-entity-schema
  ([entity-name version]
   (:schema (find-entity-schema entity-name version)))
  ([entity-name]
   (fetch-entity-schema entity-name nil)))

(defn fetch-event-schema [event-name]
  (dissoc
   (:schema (find-event-schema event-name))
   li/event-context))

(defn fetch-spec [tag n]
  (when-let [meta (fetch-meta n)]
    (tag meta)))

(def fetch-ui-spec (partial fetch-spec :ui))
(def fetch-rbac-spec (partial fetch-spec :rbac))

(defn- maybe-expand-attribute-schema [attr-name]
  (if (s/index-of (name attr-name) "_")
    (find-attribute-schema attr-name)
    attr-name))

(defn find-object-schema
  ([path]
   (or (find-entity-schema path)
       (find-record-schema path)
       (find-event-schema path)))
  ([path version]
   (let [[component aref] (li/split-path path)]
     (or (find-entity-schema path version)
         (find-record-schema component version aref)
         (find-record-schema-by-type-version :entity version path)))))

(defn ensure-type-and-name [inst type-name type-tag]
  (assoc
   (if (type-tag-key inst)
     inst
     (assoc inst type-tag-key type-tag))
   type-key type-name))

(defn parsed-instance-type [rec]
  (li/split-path (type-key rec)))

(defn record-instance? [rec]
  (= :record (instance-type-tag rec)))

(defn entity-instance? [rec]
  (= :entity (instance-type-tag rec)))

(defn event-instance? [rec]
  (= :event (instance-type-tag rec)))

(defn an-instance? [x]
  (and (map? x) (instance-type-tag x)))

(defn an-entity-instance? [x]
  (and (an-instance? x)
       (entity-instance? x)))

(defn inherits? [base-type child-type]
  (if-let [t (:inherits (fetch-meta child-type))]
    (if (= t base-type)
      true
      (inherits? base-type t))
    false))

(defn instance-of?
  "Return true if the fully-qualified name is the same as that of the instance."
  [nm inst]
  (and (an-instance? inst)
       (or (= (li/split-path nm)
              (parsed-instance-type inst))
           (inherits? nm (instance-type inst)))))

(defn instance-attributes
  ([x include-meta]
   (when (an-instance? x)
     (li/normalize-instance-pattern
      (dissoc
       x type-tag-key
       type-key dirty-key
       (when-not include-meta li/meta-attr)))))
  ([x] (instance-attributes x false)))

(defn instance-all-attributes [x]
  (when (an-instance? x)
    x))

(defn instance-user-attributes
  "Returns only the user assigned attributes.
   Excludes id-attr in its return"
  [inst]
  (when (an-instance? inst)
    (apply dissoc inst non-instance-user-attr-keys)))

(defn all-attributes-with-values [inst]
  (let [attrs (instance-attributes inst)]
    (into {} (filter #(not (nil? (second %))) attrs))))

(def set-attribute-value assoc)

(def error? (partial instance-of? :error))
(def async-future? (partial instance-of? :Agentlang.Kernel.Lang/Future))

(defn same-record-type?
  "Return true if both instances have the same name and type."
  [inst-a inst-b]
  (and (= (instance-type inst-a)
          (instance-type inst-b))
       (= (instance-type-tag inst-a)
          (instance-type-tag inst-b))))

(defn make-with-attributes
  "Make a copy of the given instance and set the new attributes."
  [inst newattrs]
  (u/make-record-instance (instance-type-tag inst)
                          (instance-type inst)
                          newattrs))

(defn attribute-names
  "Return names of attributes from schema as a set."
  [schema]
  (when-let [ks (seq (keys (or (:schema schema) schema)))]
    (set ks)))

(def ^:private system-attribute-names #{li/id-attr li/path-attr
                                        li/parent-attr li/meta-attr
                                        li/event-context})

(defn user-attribute-names [schema]
  (set/difference (attribute-names schema) system-attribute-names))

(declare identity-attribute-name)

(defn query-attribute-names [entity-name]
  (let [ident (identity-attribute-name entity-name)
        anames (concat (user-attribute-names (find-entity-schema entity-name)) [li/path-attr li/parent-attr])]
    (if (= ident li/id-attr)
      (concat anames [ident])
      anames)))

(defn user-attributes [schema] (apply dissoc schema system-attribute-names))

(defn entity-attribute-names [entity-name]
  (set/difference
   (attribute-names (fetch-entity-schema entity-name))
   #{li/meta-attr}))

(def attributes :schema)

(defn has-attribute? [schema-or-name attr-name]
  (if (keyword? schema-or-name)
    (has-attribute? (fetch-schema schema-or-name) attr-name)
    (if (attr-name (attribute-names schema-or-name))
      true
      false)))

(defn- filter-attribute-schemas
  ([predic entity-schema version]
   (filter #(let [p (second %)
                  [component aref] (li/split-path p)
                  ascm (find-attribute-schema component version aref)]
              (predic ascm))
           entity-schema))
  ([predic entity-schema]
   (filter-attribute-schemas predic entity-schema nil)))

(defn- filter-attributes
  "Filter attribute names based on the attribute schema check using the predicate."
  ([predic ent version]
   (let [entity-schema (if (keyword? ent) (fetch-entity-schema ent version) ent)]
     (map first (filter-attribute-schemas predic entity-schema version))))
  ([predic ent]
   (filter-attributes predic ent nil)))

(defn- make-attributes-filter [predic]
  (partial filter-attributes predic))

(def user-indexed-attributes
  "Return the names of all attributes marked :indexed."
  (make-attributes-filter :indexed))

(def hashed-attributes (make-attributes-filter :secure-hash))

(def write-only-attributes (make-attributes-filter :write-only))

(def read-only-attributes (make-attributes-filter :read-only))

(def dynamic-attributes (make-attributes-filter :dynamic))

(def ref-attribute-schemas
  "Return the names and schemas of all attributes which has a :ref."
  (partial filter-attribute-schemas :ref))

(def unique-attributes
  "Return the names of all unique attributes."
  (make-attributes-filter :unique))

(def identity-attributes
  "Return the names of all identity attributes in the schema."
  (make-attributes-filter li/path-identity))

(defn indexed-attributes [schema]
  (concat (user-indexed-attributes schema) (identity-attributes schema)))

(def immutable-attributes
  "Return the names of all immutable attributes in the schema."
  (make-attributes-filter :immutable))

(def match-attributes
  "Return the names of all identity attributes in the schema."
  (make-attributes-filter :match))

(defn- maybe-fetch-entity-schema
  ([type-name-or-scm version]
   (if (map? type-name-or-scm)
     type-name-or-scm
     (fetch-entity-schema type-name-or-scm version)))
  ([type-name-or-scm]
   (maybe-fetch-entity-schema type-name-or-scm nil)))

(def path-id-attrs (make-attributes-filter li/path-identity))

(defn path-identity-attribute-name
  ([type-name-or-scm version]
   (first (path-id-attrs (maybe-fetch-entity-schema type-name-or-scm version))))
  ([type-name-or-scm]
   (path-identity-attribute-name type-name-or-scm nil)))

(defn identity-attribute-names
  "Return the name of any one of the identity attributes of the given entity."
  ([type-name-or-scm version]
   (identity-attributes (maybe-fetch-entity-schema type-name-or-scm version) version))
  ([type-name-or-scm]
   (identity-attribute-names type-name-or-scm nil)))

(defn identity-attribute-name
  ([type-name-or-scm version]
   (first (identity-attribute-names (maybe-fetch-entity-schema type-name-or-scm version) version)))
  ([type-name-or-scm]
   (identity-attribute-name type-name-or-scm nil)))

(defn contained-identity [type-name-or-scm]
  (let [scm (maybe-fetch-entity-schema type-name-or-scm)]
    (or (path-identity-attribute-name scm)
        (identity-attribute-name scm))))

(defn ensure-identity-attribute-name [type-name-or-scm]
  (if-let [id (identity-attribute-name type-name-or-scm)]
    id
    (u/throw-ex (str "no identity attribute for - " type-name-or-scm))))

(defn idval [entity-instance]
  ((ensure-identity-attribute-name (instance-type entity-instance))
   entity-instance))

(defn unique-or-identity? [entity-schema attr-name]
  (some #{attr-name} (concat (identity-attributes entity-schema)
                             (unique-attributes entity-schema))))

(defn instance-eq?
  "Return true if both entity instances have the same identity."
  [a b]
  (or (identical? a b)
      (= (li/path-attr a) (li/path-attr b))))

(defn attributes-eq?
  "Return true if both instances have the same attributes."
  [a b]
  (= (instance-attributes a)
     (instance-attributes b)))

(defn same-type? [inst1 inst2]
  (and (= (instance-type-tag inst1)
          (instance-type-tag inst2))
       (= (parsed-instance-type inst1)
          (parsed-instance-type inst2))))

(defn same-instance? [a b]
  (or (identical? a b)
      (= (all-attributes-with-values a)
         (all-attributes-with-values b))))

(defn exception->error [ex]
  #?(:clj
     (make-error (or (.getMessage ex) (str ex)) {})
     :cljs
     (make-error ex)))

(defn- check-attribute-names [recname schema attributes]
  (let [sks (set (keys schema))
        aks (set (keys attributes))]
    (when-let [ks (seq (set/difference aks sks))]
      (log/warn (str "Error in " (when (get schema li/event-context) "event ")
                     recname
                     " Here is the error line: "
                     (when (get schema li/event-context) "check this line in event: ")
                     (conj {} (first schema))))
      (raise-error :invalid-attribute [recname ks]))
    true))

(defn decimal-value? [x]
  #?(:clj
     (or (decimal? x)
         (decimal? (bigdec x)))
     :cljs
     (float? x)))

(defn decimal [x]
  #?(:clj
     (bigdec x)
     :cljs
     (float x)))

(declare apply-attribute-validation maybe-make-instance)

(defn- element-type-check [tpname [tptag tpscm] x]
  (case tptag
    :attribute (apply-attribute-validation tpname tpscm {tpname x})
    (vec :record :entity) (maybe-make-instance tpname x)
    nil))

(defn- merge-attr-schema [parent-scm child-scm]
  (let [scm (merge parent-scm child-scm)]
    (dissoc scm :type)))

(defn- valid-attrval [v]
  (when-not (nil? v) [true v]))

(defn- get-attr-val [attr-schema attributes attr-name]
  (when-let [[f v :as r] (valid-attrval (get attributes attr-name))]
    (if (:var attr-schema)
      [f (deref v)]
      r)))

(defn- check-format [ascm aname aval]
  (if (and (:optional ascm) (nil? aval))
    aval
    (do
      (when-let [p (:check ascm)]
        (when-not (p aval)
          (raise-error :check-failed [aval aname])))
      (when-let [fmt (:format ascm)]
        (when-not (fmt aval)
          (raise-error :format-mismatch [aname])))
      aval)))

(defn- instantiable-map? [x]
  (and (map? x)
       (= 1 (count (keys x)))
       (map? (first (vals x)))))

(defn- instantiable-map-of? [type-name x]
  (or (instance-of? type-name x)
      (and (instantiable-map? x)
           (= type-name (keyword (first (keys x)))))))

(defn- assert-literal-instance [attr-name type-name obj]
  (if (instantiable-map-of? type-name obj)
    obj
    (raise-error :type-mismatch [attr-name type-name])))

(declare valid-attribute-value)

(defn- type-check [attr-name obj ascm]
  (let [tp (:type ascm)]
    (if (find-record-schema tp)
      (assert-literal-instance attr-name tp obj)
      (valid-attribute-value
       attr-name (check-format ascm attr-name obj) (dissoc ascm :type)))))

(defn- attr-type-preprocessor [attr-type]
  (case attr-type
    :Agentlang.Kernel.Lang/Float float
    :Agentlang.Kernel.Lang/Double double
    :Agentlang.Kernel.Lang/Decimal decimal
    false))

(defn valid-attribute-value
  "Check against the attribute schema, if the provided value (v)
  is a valid value for the attribute. If valid, return v. If v is nil,
  try to return the default value for the attribute. Otherwise, raise an
  exception."
  [aname aval ascm]
  (if (:future ascm)
    aval
    (if-not (nil? aval)
      (cond
        (:type ascm)
        (type-check aname aval ascm)

        (:listof ascm)
        (let [tp (:listof ascm)
              p (partial element-type-check tp (find-schema tp))
              aval (if-let [p (attr-type-preprocessor tp)]
                     (mapv p aval)
                     aval)]
          (if (su/all-true? (mapv p aval))
            aval
            (raise-error :invalid-list-element [aname])))

        (:setof ascm)
        (do (when-not (set? aval)
              (raise-error :not-a-set [aname]))
            (let [tp (:setof ascm)
                  p (partial element-type-check tp (find-schema tp))]
              (if (su/all-true? (map p aval))
                aval
                (raise-error :invalid-set-element [aname]))))

        :else (check-format ascm aname aval))
      (let [dval (:default ascm)]
        (when-not (nil? dval)
          (if (fn? dval) (dval) dval))))))

(defn- maybe-parse-attribute-value [aname ascm attributes]
  (if-let [p (:parse ascm)]
    (if-let [v (aname attributes)]
      (assoc attributes aname (p v))
      attributes)
    attributes))

(defn- apply-attribute-validation [aname ascm attributes]
  (maybe-parse-attribute-value
   aname ascm
   (if (:expr ascm)
     attributes
     (if-let [[_ aval] (get-attr-val ascm attributes aname)]
       (do (valid-attribute-value aname aval ascm)
           attributes)
       (let [dval (valid-attribute-value aname nil ascm)]
         (if-not (nil? dval)
           (assoc attributes aname dval)
           (if (:optional ascm)
             attributes
             (raise-error :no-default-value [aname]))))))))

(declare make-instance)

(defn- map-as-instance [obj]
  (when (map? obj)
    (if (an-instance? obj)
      obj
      (make-instance obj))))

(defn- ensure-attribute-is-instance-of [recname attrname attributes]
  (if-let [aval (map-as-instance (get attributes attrname))]
    (if (instance-of? recname aval)
      attributes
      (raise-error :attribute-type-mismatch [attrname recname]))
    (raise-error :no-record-set [attrname])))

(defn- preproc-attribute-value [attributes attrname attr-type]
  (if-let [p (attr-type-preprocessor attr-type)]
    (assoc attributes attrname (p (get attributes attrname)))
    attributes))

(defn- validated-attribute-values [recname recversion schema attributes]
  (let [r (check-attribute-names recname schema attributes)
        icns (set (internal-component-names))]
    (or (error? r)
        (loop [schema schema, attributes attributes]
          (if-let [[aname atype] (first schema)]
            (if-not (li/name? aname)
              (recur (rest schema) attributes)
              (let [typname (li/extract-attribute-name atype)
                    [component aref] (li/split-path typname)]
                (recur
                 (rest schema)
                 (if-let [ascm (find-attribute-schema
                                component (when-not (contains? icns component) recversion)
                                aref)]
                   (try
                     (apply-attribute-validation
                      aname ascm (preproc-attribute-value attributes aname typname))
                     (catch #?(:clj Exception :cljs :default) ex
                       (u/throw-ex (str "Validation failed for " (li/make-path recname) " - "
                                        #?(:clj (.getMessage ex) :cljs ex)))))
                   (ensure-attribute-is-instance-of typname aname attributes)))))
            attributes)))))

(defn validate-attribute-value
  ([attr-name attr-val schema version]
   (if-let [typname (li/extract-attribute-name (get (:schema schema) attr-name))]
     (let [[component aref] (li/split-path typname)]
       (if-let [ascm (find-attribute-schema component version aref)]
         (valid-attribute-value attr-name attr-val ascm)
         (raise-error :schema-not-found [attr-name])))
     (raise-error :attribute-not-in-schema [attr-name])))
  ([attr-name attr-val schema]
   (validate-attribute-value attr-name attr-val schema nil)))

(def inferred-event-schema {:inferred true})

(defn inferred-event-schema? [schema]
  (if-let [scm (:schema schema)]
    (inferred-event-schema? scm)
    (:inferred schema)))

(defn ensure-schema
  ([recname version]
   (if-let [rec (find-record-schema-version version recname)]
     (:schema rec)
     (raise-error :schema-not-found [recname])))
  ([recname]
   (ensure-schema recname nil)))

(defn ensure-entity-schema [recname]
  (if-let [scm (fetch-entity-schema recname)]
    scm
    (raise-error :schema-not-found [recname])))

(def ^:private external-schema-records (atom #{}))

(defn with-external-schema [recname]
  (let [n (li/make-path recname)]
    (swap! external-schema-records conj n)
    n))

(defn- has-external-schema? [recname]
  (let [recname (li/make-path recname)]
    (when (some #{recname} @external-schema-records)
      true)))

(defn inferred-schema? [recname]
  (when-let [schema (fetch-schema recname)]
    (or (:inferred schema) (has-external-schema? recname))))

(defn validate-record-attributes
  ([recname recversion recattrs schema]
   ;; The :inferred key will be added
   ;; only for inferred events. Do no validate
   ;; the schema of inferred events.
   (if (or (:inferred schema) (has-external-schema? recname))
     recattrs
     (validated-attribute-values recname recversion schema recattrs)))
  ([recname recattrs schema]
   (validate-record-attributes recname nil recattrs schema))
  ([recname recattrs]
   (validate-record-attributes recname recattrs (ensure-schema recname))))

(defn- type-tag-of
  ([recname version]
   (let [[component aref] (li/split-path recname)]
     (type-tag-key (find-record-schema component version aref))))
  ([recname]
   (type-tag-of recname nil)))

(defn- serialized-instance? [x]
  (and (type-tag-key x) (type-key x)))

(defn- deserialize-name [x]
  (let [n (type-key x)]
    (cond
      (keyword? n) n
      (string? n) (keyword n)
      (seqable? n) (vec (map #(if (keyword? %) % (keyword %)) n))
      :else (u/throw-ex (str "not a valid name - " n)))))

(defn- deserialize-instance [x]
  (let [tp (keyword (type-tag-key x))
        nm (deserialize-name x)]
    (assoc x type-tag-key tp type-key nm)))

(defn- maybe-instance [x validate?]
  (cond
    (and (instantiable-map? x) (not (instance-type-tag x)))
    (if (find-record-schema (first (keys x)))
      (make-instance (first (keys x))
                     (first (vals x))
                     validate?)
      x)

    (map? x)
    (if (serialized-instance? x)
      (deserialize-instance x)
      x)

    (or (string? x) (li/sealed? x)) x

    (set? x)
    (set (map #(maybe-instance % validate?) x))

    (li/quoted? x)
    (w/prewalk
     #(if (li/unquoted? %)
        (let [obj (second %)]
          (if (map? obj) (maybe-instance obj validate?) obj))
        %)
     x)

    (vector? x)
    (vec (map #(maybe-instance % validate?) x))

    :else x))

(defn unmake-instance [inst]
  (if (an-instance? inst)
    (let [n (instance-type-kw inst)]
      {n (instance-attributes inst)})
    inst))

(defn- maps-to-insts
  "If any of the values in the attributes map itself is the
   map-encoded representation of a record or entity, convert
   that map into a proper instance."
  [attrs validate?]
  (into {} (map (fn [[k v]]
                  [k (maybe-instance v validate?)])
                attrs)))

(defn secure-attributes
  [recname attrs schema]
  (loop [hashed (seq (hashed-attributes schema)), result attrs]
    (if-let [k (first hashed)]
      (let [v (k result)]
        (recur
         (rest hashed)
         (if (sh/crypto-hash? v)
           result
           (assoc result k (sh/crypto-hash v)))))
      result)))

(defn- maybe-complete-path [path id]
  (let [v (li/path-to-vec path)]
    (li/vec-to-path
     (mapv #(if (= li/id-attr-s %) id %) v))))

(def instance-path li/path-attr)

(defn- maybe-assoc-path [recname attrs]
  (if (entity? recname)
    (let [path (li/path-attr attrs)
          id ((identity-attribute-name recname) attrs)]
      (if (li/default-path? path)
        (assoc attrs li/path-attr (str (li/make-path recname) "," id))
        (assoc attrs li/path-attr (maybe-complete-path path id))))
    attrs))

(defn make-instance
  "Initialize an instance of a record from the given map of attributes.
   All attribute values will be validated using the associated value predicates.
   full-record-name must be in the form - :ComponentName/RecordName.
   Return the new record on success, return an :error record on failure."
  ([record-name version attributes validate?]
   (let [record-name (li/split-path record-name)
         schema (ensure-schema record-name version)
         attrs0 (maps-to-insts attributes validate?)
         attrs (maybe-assoc-path
                record-name
                (if validate?
                  (validate-record-attributes record-name attrs0 schema)
                  attrs0))]
     (if (error? attrs)
       attrs
       (u/make-record-instance (type-tag-of record-name version) record-name attrs))))
  ([record-name attributes validate?]
   (make-instance record-name nil attributes validate?))
  ([record-name attributes]
   (make-instance record-name attributes true))
  ([m]
   (if (an-instance? m)
     m
     (make-instance (li/record-name m) (li/record-attributes m)))))

(defn maybe-make-instance [n obj]
  (if (instance-of? n obj)
    obj
    (let [obj (if (instantiable-map-of? n obj)
                (li/record-attributes obj)
                obj)]
      (make-instance n obj))))

(defn- make-X-instance
  "Make a new instance of the record, entity or event with the name `xname`.
   The attributes for the new instance are encoded as a plain map."
  [x? xname attributes]
  (let [inst (make-instance xname attributes true)]
    (and (x? inst) inst)))

(def ^:private entity-event-sep "_")

(defn make-entity-event-name [entity-name event-type trig-type]
  (keyword (str (subs (str entity-name) 1) entity-event-sep
                (name event-type) entity-event-sep
                (name trig-type))))

(defn for-each-entity-event-name
  "For the given entity, generate the corresponding entity-event names.
   Call `f` with each generated name, its event tag and trigger tag as arguments.
   The generated event name will have the format entityName_eventTag_triggerTag.
   The event tag will stand for an upsert or a delete. The trigger tag denotes when the
   event will be fired - i.e either before or after the upsert/delete."
  [entity-name f]
  (let [p (partial make-entity-event-name entity-name)]
    (doseq [et [:OnUpsert :OnDelete]]
      (doseq [tt [:Before :After]]
        (let [event-name (p et tt)]
          (f event-name et tt))))))

(def ^:private entity-event-name-suffixes
  [(str entity-event-sep "After")
   (str entity-event-sep "Before")])

(defn entity-event? [x]
  (let [n (if (keyword? x)
            (str x)
            (str (instance-type x)))]
    (some (partial s/ends-with? n)
          entity-event-name-suffixes)))

(def make-event-instance (partial make-X-instance event-instance?))
(def make-entity-instance (partial make-X-instance entity-instance?))

(defn kernel-crud-event [event-type trig-type inst oldinst]
  (let [evtname (make-entity-event-name
                 (instance-type inst)
                 event-type trig-type)]
    (make-event-instance
     evtname
     (if (= event-type :OnDelete)
       {:Instance inst}
       {:Instance inst
        :OldInstance oldinst}))))

(def kernel-create-event (partial kernel-crud-event :OnUpsert :After))
(def kernel-update-event kernel-create-event)
(def kernel-delete-event (partial kernel-crud-event :OnDelete :After))

(def kernel-create-pending-event (partial kernel-crud-event :OnUpsert :Before))
(def kernel-update-pending-event kernel-create-pending-event)
(def kernel-delete-pending-event (partial kernel-crud-event :OnDelete :Before))

(defn merge-attributes
  "Both and `a` and `b` must be instances of the same record, entity or event.
   A new instance of this type is returned with the attributes of `b` merged into `a`."
  [a b]
  (let [newattrs (validate-record-attributes
                  (instance-type a)
                  (merge (instance-attributes a) (instance-attributes b)))]
    (make-with-attributes a newattrs)))

(defn- event-name [e]
  (cond
    (keyword? e) e
    (event-instance? e) (instance-type e)
    (map? e) (let [n (first (keys e))]
               (if (keyword? n)
                 n
                 (u/throw-ex (str "not a valid event name - " n))))
    :else (u/throw-ex (str "invalid event pattern - " e))))

(defn record-names-by-type
  "Return a list of record-names, of the given type, interned in this component.
  The type argument `tp` could be one of - :record, :event or :entity."
  [tp component]
  (when-let [recs (seq (filter
                        (fn [[_ v]] (= tp (type-tag-key v)))
                        (:records (get (get @components component) (get-model-version component)))))]
    (set (mapv (partial full-name component) (keys recs)))))

(declare contains-relationship? relationship?)

(defn record-names
  ([component exclude-contains]
   (when-let [recnames (record-names-by-type :record component)]
     (when-let [rs (if exclude-contains
                     (seq (filter (complement contains-relationship?) recnames))
                     recnames)]
       (set rs))))
  ([component] (record-names component true)))

(defn entity-names
  ([component exclude-between]
   (when-let [result (record-names-by-type :entity component)]
     (if exclude-between
       (set (filter #(not (:relationship (fetch-meta %))) result))
       result)))
  ([component] (entity-names component true)))

(defn all-entity-names-and-versions []
  (reduce
   (fn [entities c]
     (merge
      entities
      (apply
       merge
       (map #(do {% (get-model-version c)})
            (entity-names c false)))))
   {} (component-names)))

(def event-names (partial record-names-by-type :event))

(defn relationship-names [component]
  (set (concat (filter #(:contains (fetch-meta %)) (record-names component false))
               (filter #(:relationship (fetch-meta %)) (entity-names component false)))))

(defn user-defined-event? [event-name]
  (not (s/index-of (name event-name) "_")))

(defn user-event-names [component]
  (filter user-defined-event? (event-names component)))

(defn verify-dataflow [df]
  ;; TODO: pre-check df syntax
  df)

(defn register-dataflow
  "Attach a dataflow to the event."
  ([event head patterns component]
   (u/call-and-set
    components
    #(let [ms @components
           ename (normalize-type-name (event-name event))
           path [component (get-model-version component) :events ename]
           newpats [(verify-dataflow
                     [event
                      {:head head
                       :event-pattern event
                       :patterns patterns}])]]
       (when (and (user-defined-event? event) (seq (get-in ms path)))
         (log/warn (str "overwriting dataflow for " event)))
       (assoc-in ms path newpats)))
   event)
  ([event head patterns]
   (let [[component _] (li/split-path (event-name event))]
     (register-dataflow event head patterns component)))
  ([event patterns] (register-dataflow event nil patterns)))

(declare event-cond-expr->fncall-expr normalize-event-cond-predic)

(defn- event-attrval
  "Translate an entry in a event condition based on the following rules:
     - an attribute name: replace with the corresponding attribute value from the event instance.
     - a vector: must be a nested condition expression, translate recursively.
     - a list: must be a predicate call, apply translation rules for predicates.
     - any other value, return as is."
  [event-inst k]
  (cond
    (li/name? k)
    (get-in event-inst (li/split-by-delim #"\." (name k)))

    (vector? k)
    (event-cond-expr->fncall-expr event-inst k)

    (list? k)
    (normalize-event-cond-predic event-inst k)

    :else k))

(defn- normalize-event-cond-predic
  "Translate event attribute references in a predicate call to corresponding
  attribute values from the event instance."
  [event-inst predic-call-expr]
  (let [args (map (partial event-attrval event-inst) (rest predic-call-expr))]
    `(~(first predic-call-expr) ~@args)))

(defn- event-cond-expr->fncall-expr
  "Translate an event condition expression to a predicate function call.
  An example event condition is [:= :X 100], when :X is an attribute of the
  incoming event."
  [event-inst cond-expr]
  (let [fpos-expr
        (let [f (first cond-expr)]
          (if (some #{f} #{:= :< :> :<= :>= :and :or})
            (symbol (name f))
            (raise-error :invalid-operator [cond-expr])))]
    `(~fpos-expr ~@(map (partial event-attrval event-inst) (rest cond-expr)))))

(defn- satisfies-event-condition?
  "Return true if the event instance satisfies the condition expression."
  [event-inst cond-expr]
  (eval (event-cond-expr->fncall-expr event-inst cond-expr)))

(defn- satisfies-event-predicate?
  "Return true if the event instance satisfies the predicate."
  [event-inst fn-call-expr]
  (eval (normalize-event-cond-predic event-inst fn-call-expr)))

(defn- cond-pat->map [p]
  (try
    (and (seqable? p) (into {} (first p)))
    (catch #?(:clj Exception :cljs :default) e
      nil)))

(def dataflow-event-pattern first)
(def ^:private dataflow-spec second)

(defn dataflow-patterns [df]
  (:patterns (dataflow-spec df)))

(def with-default-types :default)

(defn dataflow-on-entity [df]
  (get-in (dataflow-spec df) [:head :on-entity-event]))

(defn dataflow-with-condition [df]
  (get-in (dataflow-spec df) [:head :when]))

(defn dataflow-match-entity-name [df]
  (and (entity-event? (dataflow-event-pattern df))
       (when-let [e (dataflow-on-entity df)]
         (if (li/name? e)
           e
           (first (keys e))))))

(defn- satisfies-conditions?
  "Return true if conditions/predicates in the event pattern attached to the dataflow
  all return true"
  [event-inst df]
  (if-let [condition (dataflow-with-condition df)]
    (let [inst (if (dataflow-match-entity-name df)
                 (:Instance event-inst)
                 event-inst)]
      (cond
        (vector? condition) (satisfies-event-condition? inst condition)
        (list? condition) (satisfies-event-predicate? inst condition)
        :else true))
    true))

(defn- filter-by-conditional-events
  "Return only those dataflows whose event conditions and predicates are all true."
  [event-inst dfs]
  (filter (partial satisfies-conditions? event-inst) dfs))

(defn find-dataflows [event-name]
  (seq (component-find :events event-name)))

(defn fetch-dataflow-patterns [event-instance]
  (:patterns (second (first (find-dataflows (instance-type event-instance))))))

(defn dataflows-for-event
  "Return all dataflows attached to the event."
  [event]
  (let [dfs (find-dataflows (event-name event))]
    (filter-by-conditional-events event dfs)))

(defn all-dataflows [component]
  (su/nonils (mapv (comp first seq dataflows-for-event) (event-names component))))

(defn evalable-dataflow [[k dfspec :as df]]
  [k (dataflow-patterns df)])

;; Component querying, useful for the edges.

(defn get-schema [getter recname]
  (:schema (getter recname)))

(def event-schema (partial get-schema find-event-schema))
(def record-schema (partial get-schema find-record-schema))
(def entity-schema (partial get-schema find-entity-schema))

(defn computed-attribute-fns
  "Return the expression or query functions attached to computed attributes
  as a mapping of [[attrname fn], ...]"
  ([prop schema version]
   (let [schema (dissoc (or (:schema schema) schema) :meta)
         exps (mapv (fn [[k v]]
                      (let [[component aref] (li/split-path v)]
                        (when-let [f (prop (find-attribute-schema
                                            component version aref))]
                          [k f])))
                    schema)]
     (seq (su/nonils exps))))
  ([prop schema]
   (computed-attribute-fns prop schema nil)))

(defn future-attrs
  ([record-name rec-version]
   (computed-attribute-fns :future (find-object-schema record-name rec-version)))
  ([record-name]
   (future-attrs record-name nil)))

(def expr-fns (partial computed-attribute-fns :expr))
(def eval-attrs (partial computed-attribute-fns :eval))

(defn- find-longest-dep [deps]
  (loop [deps deps, n 0, r nil]
    (if-let [[k v] (first deps)]
      (let [dn (count v)
            [nn nr] (if (> dn n) [dn k] [n r])]
        (recur (rest deps) nn nr))
      r)))

(def ^:private expr-fns-cache (u/make-cell {}))

(defn- set-expr-fns-cache [recname fns]
  (u/safe-set expr-fns-cache (assoc @expr-fns-cache recname fns))
  fns)

(defn- get-expr-fns-from-cache [recname]
  (get @expr-fns-cache recname))

(defn- build-deps-graph
  ([deps attrs k]
   (if-let [kdeps (k deps)]
     (let [dg
           (loop [kdeps kdeps, dg []]
             (if-let [kd (first kdeps)]
               (recur (rest kdeps) (concat dg (build-deps-graph deps attrs kd)))
               (vec dg)))]
       (when (some #{k} dg)
         (u/throw-ex (str "Circular dependency on " k " in " attrs)))
       (vec (conj dg k)))
     [k]))
  ([deps attrs] (build-deps-graph deps attrs (find-longest-dep deps))))

(defn- order-exprs-by-deps [recname expr-attrs]
  (when (seq expr-attrs)
    (let [attrs (into {} expr-attrs)
          ks (set (keys attrs))
          graph (atom {})]
      (doseq [[k v] expr-attrs]
        (when-let [deps (seq (filter #(and (keyword? %) (some #{%} ks)) v))]
          (swap! graph assoc k (vec (concat (get @graph k []) deps)))))
      (let [g @graph
            dg (when (seq g) (build-deps-graph g attrs))
            missing-ks (set/difference ks (set dg))
            f (fn [d] [d (get attrs d)])]
        (set-expr-fns-cache recname (concat (when (seq missing-ks) (mapv f missing-ks))
                                            (mapv f dg)))))))

(defn all-computed-attribute-fns
  ([record-name rec-version]
   (when-let [scm (find-object-schema record-name rec-version)]
     [(or (get-expr-fns-from-cache record-name)
          (order-exprs-by-deps record-name (expr-fns scm rec-version)))
      (eval-attrs scm rec-version)]))
  ([rec-name]
   (all-computed-attribute-fns rec-name nil)))

(defn mark-dirty [inst]
  (assoc inst dirty-key true))

(defn dirty? [x]
  (dirty-key x))

(defn unmark-dirty [inst]
  (dissoc inst dirty-key))

(defn filter-dirty [insts-map]
  (let [res (map (fn [[nm insts]]
                   [nm (filter #(dirty-key %) insts)])
                 insts-map)]
    (into {} res)))

(defn- computed? [attr-schema]
  (:expr attr-schema))

(defn- pickled [attr-schema attr-val]
  (if-let [p (:writer attr-schema)]
    (p attr-val)
    attr-val))

(defn serializable-attributes [inst]
  (let [attrs (instance-attributes inst true)
        schema (entity-schema (type-key inst))
        new-attrs (map (fn [[k v]]
                         (let [ascm (find-attribute-schema v)]
                           (when-not (computed? ascm)
                             [k (pickled ascm (get attrs k))])))
                       schema)]
    (into {} (su/nonils new-attrs))))

(defn kernel-resolver-name? [n]
  (= :Agentlang.Kernel.Lang/Resolver n))

(defn tag? [k]
  (or (= k type-key)
      (= k type-tag-key)))

(defn attribute-unique-reference-path [[attr-name attr-spec]]
  (when-let [r (:ref attr-spec)]
    (when (:unique (find-attribute-schema r))
      [attr-name r])))

(defn all-reference-paths [attrs]
  (seq (filter attribute-unique-reference-path attrs)))

(defn unique-attribute? [entity-schema attr]
  (:unique (find-attribute-schema (get entity-schema attr))))

(defn entity-attribute-schema [entity-schema-or-name attr]
  (let [entity-schema (if (keyword? entity-schema-or-name)
                        (fetch-entity-schema entity-schema-or-name)
                        entity-schema-or-name)
        ascm (get entity-schema attr)]
    [(find-attribute-schema ascm) ascm]))

(def attribute-property
  (memoize
   (fn [tag strict? entity-schema-or-name attr]
     (when-let [[ascm ascm0] (entity-attribute-schema entity-schema-or-name attr)]
       (if strict?
         (and (map? ascm) (tag ascm))
         (if (map? ascm)
           (or (tag ascm) ascm0)
           ascm))))))

(def attribute-type (partial attribute-property :type false))
(def attribute-match-spec (partial attribute-property :match true))
(def attribute-path-to (partial attribute-property :to true))

(def identity-attribute? li/path-identity)

(defn attribute-is-identity? [entity-schema attr]
  (let [a (get entity-schema attr)]
    (identity-attribute? (find-attribute-schema a))))

(defn type-any? [entity-schema attr]
  (= :Agentlang.Kernel.Lang/Any (attribute-type entity-schema attr)))

(defn type-path? [entity-schema attr]
  (= :Agentlang.Kernel.Lang/Path (attribute-type entity-schema attr)))

(defn find-ref-path [attr-schema-name]
  (:ref (find-attribute-schema attr-schema-name)))

(defn attribute-ref [entity-schema attr]
  (let [a (get entity-schema attr)
        ascm (find-attribute-schema a)]
    (when-let [r (:ref ascm)]
      [r (:cascade-on-delete ascm)])))

(defn keyword-type-attributes [entity-schema attribute-names]
  (let [atype (partial attribute-type entity-schema)]
    (filter
     #(let [t (atype %)]
        (= t :Agentlang.Kernel.Lang/Keyword))
     attribute-names)))

(defn dissoc-write-only [instance]
  (let [schema (ensure-schema (instance-type instance))]
    (if-let [wo-attrs (seq (write-only-attributes schema))]
      (into {} (filter (fn [[k _]]
                         (not (some #{k} wo-attrs)))
                       instance))
      instance)))

(defn make-future [future-obj timeout-ms]
  (make-instance :Agentlang.Kernel.Lang/Future {:Result future-obj
                                                :TimeoutMillis timeout-ms}))

(def future-object? (partial instance-of? :Agentlang.Kernel.Lang/Future))

(defn deref-future-object [obj]
  #?(:clj
     (deref (:Result obj) (:TimeoutMillis obj) nil)
     :cljs
     ;; Concurrency not yet implemented in cljs.
     (:Result obj)))

(defn maybe-deref [obj]
  (if (future-object? obj)
    (or (deref-future-object obj)
        (make-error "Async timeout" obj))
    obj))

(defn- restore-flags [attrs orig-instance]
  (merge
   attrs
   (when (dirty-key orig-instance)
     {dirty-key true})))

(defn validate-instance [inst]
  (let [n (instance-type inst)
        schema (ensure-schema n)
        attrs (validate-record-attributes
               n (instance-attributes inst true) schema)]
    (if (error? attrs)
      (u/throw-ex attrs)
      (restore-flags
       (u/make-record-instance (type-tag-key inst) n attrs)
       inst))))

(defn tag-record [recname attrs]
  (assoc attrs type-key recname type-tag-key :record))

(def ^:private trigger-store
  #?(:clj  (ref {})
     :cljs (atom {})))

(defn install-triggers!
  "Install the predicate for the given records.
  On upsert, the event is triggered if the predicate
  return true for the record instance"
  [record-names event-name predicate where-clause records-to-load]
  (doseq [rn (if (keyword? record-names)
               [record-names]
               record-names)]
    (let [rn (li/split-path rn)
          ts @trigger-store]
      (u/safe-set
       trigger-store
       (let [trigs (get ts rn)
             rs (set (map li/split-path records-to-load))]
         (assoc
          ts rn
          (conj
           trigs
           [predicate event-name [where-clause rs]]))))))
  (u/call-and-set
   trigger-store
   #(assoc
     @trigger-store
     (li/split-path event-name) :conditional-event)))

(defn conditional-event? [n]
  (= :conditional-event (get @trigger-store (li/split-path n))))

(defn conditional-events
  "Return conditional events to fire for the given instance"
  [obj]
  (let [instance obj
        recname (li/split-path (instance-type instance))]
    (seq (get @trigger-store recname))))

(defn fire-event? [event-info instances]
  (let [args (map (fn [inst] [(li/split-path (instance-type inst)) inst]) instances)]
    (when ((first event-info) (into {} args))
      true)))

(defn- replace-referenced-value [loaded-instances [[c n] r :as term]]
  (if-let [inst (first (filter #(= (li/split-path (instance-type %)) [c n]) loaded-instances))]
    (if-let [v (get inst r)]
      v
      (u/throw-ex (str "failed to load reference - " term)))
    term))

(defn- rewrite-term [loaded-instances term]
  (if (li/parsed-path? term)
    (replace-referenced-value loaded-instances term)
    term))

(defn- extract-query-target [rewritten-clause]
  (loop [rcs rewritten-clause, target nil]
    (if-let [r (first rcs)]
      (if (li/parsed-path? r)
        (if target
          (u/throw-ex
           (str "cannot have two targets in the same clause - "
                rewritten-clause))
          (recur (rest rcs) (first r)))
        (recur (rest rcs) target))
      target)))

(defn- normalize-rewritten [rewritten-clause]
  (loop [rcs rewritten-clause, literals [], result []]
    (if-let [r (first rcs)]
      (if (li/parsed-path? r)
        (recur (rest rcs) literals (conj result (second r)))
        (recur (rest rcs) (conj literals r) result))
      (concat result literals))))

(defn parse-where-clause [clause loaded-instances]
  (let [opr (first clause)
        rewritten (map (partial rewrite-term loaded-instances) (rest clause))]
    {:from (extract-query-target rewritten)
     :where (concat [opr] (normalize-rewritten rewritten))}))

(defn entity-schema-predefined? [entity-name]
  ;; TODO: Check if entity belongs to a set of
  ;; entities with manually defined database tables.
  false)

(defn meta-attribute-name? [k]
  (some #{k} [type-key type-tag-key dirty-key mt/meta-key]))

(defn compound-unique-attributes [entity-name]
  (:unique (fetch-meta entity-name)))

(defn- instance-type-str [n]
  (if (keyword? n)
    (name n)
    (str (name (first n)) "/" (name (second n)))))

(defn instance-str [instance]
  (let [n (instance-type instance)]
    (if-let [str-pat (:str (fetch-meta n))]
      (if (keyword? str-pat)
        (str (str-pat instance))
        (apply str (mapv #(if (keyword? %)
                            (% instance)
                            %)
                         str-pat)))
      (instance-type-str n))))

(defn compact-instance [inst]
  {id-attr (id-attr inst)
   :str (instance-str inst)
   type-tag-key (instance-type-tag inst)
   type-key (instance-type inst)})

(defn- displayable-record-names [component-info]
  (let [components
        (cond
          (keyword? component-info)
          [component-info]

          (vector? component-info)
          component-info

          :else
          (u/throw-ex
           (str "invalid component-info - " component-info)))
        names (set
               (apply
                concat
                (mapv
                 #(concat
                   (entity-names %)
                   (event-names %)
                   (record-names %))
                 components)))]
    (filter #(:order (fetch-meta %)) names)))

(defn event? [recname]
  (and (event-schema recname) true))

(defn entity? [recname]
  (and (entity-schema recname) true))

(defn rec? [recname]
  (and (record-schema recname) true))

(defn authentication-event? [rec-name]
  (and (event? rec-name)
       (:authenticate (fetch-meta rec-name))))

(defn display-order [rec-name]
  (:order (fetch-meta rec-name)))

(def hashed-attribute? :secure-hash)

(defn assoc-event-context [event-instance context-obj]
  (assoc event-instance li/event-context context-obj))

(def event-context li/event-context)

(defn event-context-value [k event-instance]
  (get-in event-instance [li/event-context k]))

(defn assoc-event-context-value [k v event-instance]
  (assoc-in event-instance [li/event-context k] v))

(def event-context-user (partial event-context-value :User))
(def assoc-event-context-user (partial assoc-event-context-value :User))

;; Note: event-context-env is used only from the repl.
(def event-context-env (partial event-context-value :-*-env-*-))
(def assoc-event-context-env (partial assoc-event-context-value :-*-env-*-))

(defn assoc-event-context-values [values-map event-instance]
  (let [current-event-context (get event-instance event-context)
        updated-event-context (merge current-event-context values-map)]
    (assoc event-instance event-context updated-event-context)))

(def ^:private meta-suffix "_Meta")
(def ^:private meta-suffix-len (count meta-suffix))

(defn meta-entity-name [n]
  (let [[component entity-name] (if (keyword? n) (li/split-path n) n)]
    (keyword (str (name component) "/" (name entity-name) meta-suffix))))

(defn meta-entity-name? [n]
  (s/ends-with? (name n) meta-suffix))

(def ^:private inst-priv-suffix "_ipa")

(defn inst-priv-entity-name [entity-name]
  (let [[c n :as cn] (li/split-path entity-name)
        [c n] (if-not n [(get-current-component) c] cn)]
    (li/make-path c (str (name n) inst-priv-suffix))))

(defn inst-priv-entity-name? [n]
  (s/ends-with? (name n) inst-priv-suffix))

(defn meta-entity-update-event-name [n]
  (let [[c n] (li/split-path (meta-entity-name n))]
    (keyword (str (name c) "/Update_" (name n)))))

(def meta-entity-id :EntityId)

(defn meta-entity-attributes [component]
  {meta-entity-id {:type :Agentlang.Kernel.Lang/String li/path-identity true}
   :Owner {:type :Agentlang.Kernel.Lang/String
           :immutable true}
   :Created {:type :Agentlang.Kernel.Lang/DateTime
             :default dt/now
             :immutable true}
   :LastUpdated {:type :Agentlang.Kernel.Lang/DateTime
                 :default dt/now}
   :LastUpdatedBy :Agentlang.Kernel.Lang/String
   :UserData {:type :Agentlang.Kernel.Lang/Map :optional true}
   :meta {:system-defined? true}})

(defn system-defined? [schema]
  (:system-defined? (:meta schema)))

(defn meta-entity-for-any? [entity-names ename]
  (let [n (str (if (keyword? ename) ename (li/make-path ename)))]
    (when (s/ends-with? n meta-suffix)
      (let [root-entity-name (keyword (subs (subs n 0 (- (count n) meta-suffix-len)) 1))]
        (some #{root-entity-name} entity-names)))))

(defn make-meta-instance
  ([inst user user-data]
   (let [ename (li/split-path (instance-type inst))
         mname (meta-entity-name ename)
         entity-id (idval inst)]
     [mname
      (make-instance
       mname
       (merge {meta-entity-id (str entity-id)
               :Owner user :LastUpdatedBy user}
              (when user-data
                {:UserData user-data})))]))
  ([inst user]
   (make-meta-instance inst user nil)))

(defn user-entity-names [component]
  (set (filter #(and (not (meta-entity-name? %))
                     (not (relationship? %))
                     (not (inst-priv-entity-name? %)))
               (entity-names component))))

(def lookup-internal-event-prefix :Lookup_Internal)
(def lookup-internal-event-prefix-s (name lookup-internal-event-prefix))

(defn an-internal-event? [event-name]
  (let [event-name (if (keyword? event-name) event-name (second event-name))]
    (s/starts-with? (name event-name) lookup-internal-event-prefix-s)))

(defn instance-meta-lookup-event [entity-name id]
  (let [[component ename] (li/split-path entity-name)]
    (make-instance
     {(keyword (str (name component) "/" lookup-internal-event-prefix-s "_" (name entity-name) meta-suffix))
      {meta-entity-id (str id)}})))

(def instance-meta-owner :Owner)

(defn kernel-inited? []
  (and (:Agentlang.Kernel.Lang @components) true))

(defn append-id
  ([path id-attr]
   (keyword (str (subs (str path) 1) "." (name id-attr))))
  ([path]
   (let [{c :component r :record} (li/path-parts path)
         id (when (and c r) (identity-attribute-name [c r]))]
     (append-id path (or id id-attr)))))

(defn find-relationships
  ([recname recversion]
   (or (component-find :entity-relationship recname recversion) #{}))
  ([recname]
   (find-relationships recname nil)))

(defn find-contained-relationship [recname]
  (let [recname (li/keyword-name recname)]
    (first
     (filter
      #(= recname (second (mt/contains (fetch-meta %))))
      (find-relationships recname)))))

(defn find-relationships-with-rbac-inheritance
  ([not-found tag recname]
   (let [recname (li/keyword-name recname)]
     (filter #(let [mt (fetch-meta %)
                    [_ e2] (mt/contains mt)]
                (when (= e2 recname)
                  (get-in mt [:rbac :inherit tag] not-found)))
             (find-relationships recname))))
  ([tag recname]
   (find-relationships-with-rbac-inheritance nil tag recname)))

(def relationships-with-instance-rbac (partial find-relationships-with-rbac-inheritance true :instance))
(def relationships-with-entity-rbac (partial find-relationships-with-rbac-inheritance nil :entity))

(defn in-relationship? [recname relname]
  (let [n (if (keyword? relname)
            relname
            (li/make-path relname))]
    (some #{n} (find-relationships recname))))

(defn- intern-entity-rel [relationship-name recname]
  (let [rels (find-relationships recname)]
    (component-intern recname (conj rels relationship-name) :entity-relationship)))

(defn- remove-entity-rel [relationship-name recname]
  (let [rels (find-relationships recname)]
    (component-intern recname (remove #(= % relationship-name) rels) :entity-relationship)))

(defn register-relationship [recs-in-relationship relationship-name]
  (mapv (partial intern-entity-rel relationship-name) recs-in-relationship))

(defn deregister-relationship [recs-in-relationship relationship-name]
  (mapv (partial remove-entity-rel relationship-name) recs-in-relationship))

(defn relationship-attribute-names [rec-a rec-b]
  (let [[_ a] (li/split-path rec-a)
        [_ b] (li/split-path rec-b)]
    (if (= a b)
      [(u/keyword-append a 1) (u/keyword-append b 2)]
      [a b])))

(def relmeta-key :-*-relmeta-*-)
(def relationship-meta relmeta-key)

(defn relationship-nodes [relname]
  (when-let [mt (fetch-meta relname)]
    (or (mt/contains mt) (mt/between mt))))

(defn other-relationship-node [relname nodename]
  (when-let [[a b] (relationship-nodes relname)]
    (if (= a (li/make-path nodename)) b a)))

(defn between-relationship-nodes [relname]
  (mt/between (fetch-meta relname)))

(defn attribute-in-relationship [relname entity-name]
  (let [mt (fetch-meta relname)
        entity-name (if (keyword? entity-name) entity-name (li/make-path entity-name))
        [e1 e2] (or (mt/contains mt) (mt/between mt))]
    (if-let [[a1 a2] (:on (relationship-meta mt))]
      (if (= entity-name e1) a1 a2)
      (identity-attribute-name entity-name))))

(defn- contain-rels
  ([as-parent recname recversion]
   (let [recname (li/make-path recname)
         accessors [first second]
         [this that] (if as-parent (reverse accessors) accessors)]
     (when-let [rels (seq (find-relationships recname recversion))]
       (seq
        (su/nonils
         (mapv #(let [meta (fetch-meta % recversion)
                      contains (mt/contains meta)]
                  (when (= recname (this contains))
                    [% :contains (that contains)]))
               rels))))))
  ([as-parent recname]
   (contain-rels as-parent recname nil)))

(def relinfo-name first)
(defn relinfo-to [[_ _ to]] to)
(def relinfo-type second)

(def contained-children (partial contain-rels false))

(defn contained-children-names [parent]
  (mapv last (contained-children parent)))

(defn containing-parents
  ([recname]
   (contain-rels true recname))
  ([recname recversion]
   (contain-rels true recname recversion)))

(defn parent-identity-attribute-type [parent-recname]
  (when-let [a (or (path-identity-attribute-name parent-recname)
                   (identity-attribute-name parent-recname))]
    (let [[ascm ascm0] (entity-attribute-schema parent-recname a)]
      (if (map? ascm)
        (dissoc ascm :unique :id :optional)
        (or ascm ascm0)))))

(defn parent-of? [child parent]
  (let [child (li/make-path child)
        parent (li/make-path parent)]
    (if (some #{parent} (map last (containing-parents child)))
      true
      false)))

(defn parent-via? [relname child parent]
  (let [relname (li/make-path relname)
        child (li/make-path child)
        parent (li/make-path parent)]
    (if (first (filter #(and (= relname (first %))
                             (if parent (= parent (last %)) true))
                       (containing-parents child)))
      true
      false)))

(defn child-in? [relname child] (parent-via? relname child nil))

(defn parent-relationship [parent-name child-name]
  (ffirst (filter #(= parent-name (last %)) (containing-parents child-name))))

(defn check-cascade-delete-children [entity-name]
  (if-let [rels (contained-children entity-name)]
    (and (every? :cascade-on-delete (map #(fetch-meta (relinfo-name %)) rels))
         (every?
          #(let [c (if (= entity-name %)
                     :delete
                     (check-cascade-delete-children %))]
             (or (= c :delete) (= c :ignore)))
          (map relinfo-to rels))
         :delete)
    :ignore))

(defn between-relationships [recname]
  (when-let [rels (seq (find-relationships recname))]
    (su/nonils
     (mapv #(let [meta (fetch-meta %)
                  [a b :as elems] (mt/between meta)]
              (when elems
                (let [that (if (= a recname) b a)]
                  [% :between that])))
           rels))))

(defn has-between-relationship? [recname relname]
  (some #{relname} (map first (between-relationships recname))))

(defn contains-entities [relname]
  (mt/contains (fetch-meta relname)))

(def contains-relationship? contains-entities)

(defn between-relationship? [recname]
  (mt/between (fetch-meta recname)))

(defn relationship? [n]
  (or (contains-relationship? n)
      (between-relationship? n)))

(defn one-to-one-relationship? [relname]
  (and (between-relationship? relname)
       (:one-one (fetch-meta relname))))

(defn containing-parent [relname]
  (first (mt/contains (fetch-meta relname))))

(defn contained-child [relname]
  (second (mt/contains (fetch-meta relname))))

(defn crud-event-name
  ([component-name entity-name evtname]
   (let [parts (li/split-path entity-name)
         component-name (if (>= (count parts) 2)
                          (first parts)
                          component-name)]
     (canonical-type-name
      component-name
      (keyword (str (name evtname) "_" (name entity-name))))))
  ([entity-name evtname]
   (let [[c n] (li/split-path entity-name)]
     (if (and c n)
       (crud-event-name c n evtname)
       (u/throw-ex (str "entity name " entity-name " must be fully-qualified."))))))

(declare prepost-event-name)

(defn all-crud-events [recname]
  (mapv
   (partial crud-event-name recname)
   [:Create :Update :Delete :Lookup :LookupAll]))

(defn- all-prepost-events [recname]
  (mapv
   #(apply prepost-event-name %)
   (li/prepost-event-heads recname)))

(defn- only-internal-attrs [scm]
  (when-not (inferred-event-schema? scm)
    (mapv #(second (li/split-path %))
          (filter li/internal-attribute-name?
                  (vals (dissoc scm id-attr))))))

(defn remove-record [recname]
  (when-let [[tag {scm :schema}] (find-schema recname)]
    (let [comps @components
          [c n] (li/split-path recname)
          c-version (get-model-version c)
          comp-scm (get-in comps [c c-version])
          attrs (:attributes comp-scm)
          new-comp-scm
          (if (= tag :attribute)
            (assoc comp-scm :attribute (dissoc attrs n))
            (let [recs (:records comp-scm)
                  evts (:events comp-scm)
                  new-attrs (apply
                             dissoc attrs
                             (only-internal-attrs scm))
                  new-recs (dissoc recs n)
                  new-evts (if (= tag :event)
                             (dissoc evts n)
                             evts)]
              (assoc
               comp-scm
               :attributes new-attrs
               :records new-recs
               :events new-evts)))
          final-comps (assoc-in comps [c c-version] (dissoc new-comp-scm n))]
      (and (u/safe-set components final-comps)
           (raw/remove-definition tag recname)
           recname))))

(defn maybe-remove-record [recname]
  (remove-record recname)
  recname)

(defn remove-entity [recname]
  (when-let [r (seq (map first (containing-parents recname)))]
    (u/throw-ex (str "cannot remove entity in child-relationships - " r)))
  (when-let [r (seq (map first (contained-children recname)))]
    (u/throw-ex (str "cannot remove entity in parent-relationships - " r)))
  (when-let [r (seq (map first (between-relationships recname)))]
    (u/throw-ex (str "cannot remove entity in between-relationships - " r)))
  (when (su/all-true?
         (mapv (if (relationship? recname)
                 maybe-remove-record
                 remove-record)
               (all-crud-events recname)))
    (and (su/all-true? (mapv maybe-remove-record (all-prepost-events recname)))
         (remove-record recname))))

(defn remove-event [event-name]
  (cond
    (keyword? event-name)
    (when (remove-record event-name)
      (raw/remove-event event-name))

    (vector? event-name) ; pre-post event - e.g: [:after :create :AnEntity]
    (when (remove-record (apply prepost-event-name event-name))
      (raw/remove-event event-name))

    :else (u/throw-ex (str "failed to remove event, invalid event name - " event-name))))

(defn remove-relationship [relname]
  (let [isbet (between-relationship? relname)
        iscont (when-not isbet (contains-relationship? relname))]
    (when (or iscont isbet)
      (deregister-relationship (relationship-nodes relname) relname)
      (raw/remove-relationship relname)
      ((if isbet remove-entity remove-record) relname)
      (remove-meta! (li/split-path relname))
      relname)))

(defn- dissoc-system-attributes [attrs]
  (into
   {}
   (filter
    (fn [[k _]]
      (and (not= li/event-context k)
           (not (s/index-of (name k) "_"))))
    attrs)))

(defn fetch-user-schema [recname]
  (dissoc-system-attributes
   (or (raw/find-entity recname)
       (raw/find-relationship recname)
       (raw/find-event recname)
       (raw/find-record recname))))

(defn fetch-user-meta [recname]
  (:meta (fetch-user-schema recname)))

(defn between-attribute-names
  ([relname from to]
   (let [relmeta (relationship-meta (fetch-meta relname))]
     (li/between-nodenames from to relmeta)))
  ([relname from]
   (let [[a b] (mt/between (fetch-meta relname))
         to (if (= from a) b a)]
     (between-attribute-names relname from to)))
  ([relname]
   (let [[n1 n2] (between-relationship-nodes relname)]
     (between-attribute-names relname n1 n2))))

(defn find-between-keys [relname entity-name]
  (let [entity-name (li/make-path entity-name)
        [node1 node2] (relationship-nodes relname)
        [a1 a2 :as ks] (between-attribute-names relname node1 node2)]
    (if (= entity-name node1 node2)
      ks
      [(cond
         (= node1 entity-name) a1
         (= node2 entity-name) a2
         :else (u/throw-ex (str entity-name " not in relationship - " relname)))])))

(defn other-between-attribute-name [anames aname]
  (first (set/difference (set anames) #{aname})))

(defn maybe-between-node-as-attribute [relname maybe-node-name]
  (when maybe-node-name
    (let [[c n] (li/split-path maybe-node-name)
          n (or n c)]
      (if (some #{n} (between-attribute-names relname))
        n
        (first (find-between-keys relname maybe-node-name))))))

(defn relationship-node-entity
  ([relname node-attr-names node-name]
   (let [[n1 n2] (relationship-nodes relname)
         [a1 a2 :as aliases] (or node-attr-names
                                 [(second (li/split-path n1))
                                  (second (li/split-path n2))])]
     (cond
       (= node-name a1) n1
       (= node-name a2) n2
       :else (u/throw-ex (str node-name " is not in relationship aliases - " aliases " of " relname)))))
  ([relname node-name]
   (relationship-node-entity
    relname (when (between-relationship? relname) (between-attribute-names relname))
    node-name)))

(defn fetch-default-attribute-values [schema]
  (into
   {}
   (mapv
    (fn [[k v]]
      [k
       (when-let [scm (find-attribute-schema v)]
         (when-let [d (:default scm)]
           (if (fn? d) (d) d)))])
    schema)))

(defn owning-node [relname]
  (li/owner (fetch-rbac-spec relname)))

(defn globally-unique-identity? [entity-name]
  (= id-attr-type
     (:type (find-attribute-schema
             ((identity-attribute-name entity-name)
              (fetch-schema entity-name))))))

(defn null-parent-path? [inst]
  (when-let [p (li/path-attr inst)]
    (li/default-path? p)))

(defn find-parent-info [rel-inst]
  (let [tp (instance-type-kw rel-inst)]
    (when (contains-relationship? tp)
      (let [[p _] (relationship-nodes tp)
            pn (second (li/split-path p))]
        [p (pn rel-inst)]))))

(defn parse-attribute-value [entity-name attr v]
  (let [scm (fetch-schema entity-name)
        ascm (find-attribute-schema (get scm attr))]
    (if-let [t (:type ascm)]
      (case t
        :Agentlang.Kernel.Lang/Int (#?(:clj Integer/parseInt :cljs js/parseInt) v)
        :Agentlang.Kernel.Lang/Int64 (#?(:clj Long/parseLong :cljs js/parseInt) v)
        :Agentlang.Kernel.Lang/BigInteger (#?(:clj BigInteger. :cljs js/parseInt) v)
        :Agentlang.Kernel.Lang/Float (#?(:clj Float/parseFloat :cljs js/parseFloat) v)
        :Agentlang.Kernel.Lang/Double (#?(:clj Double/parseDouble :cljs js/parseFloat) v)
        :Agentlang.Kernel.Lang/Decimal (#?(:clj BigDecimal. :cljs js/parseFloat) v)
        :Agentlang.Kernel.Lang/Boolean (if (= "true" v) true false)
        v)
      v)))

(defn- get-meta-attr [inst]
  (when-let [ma (li/meta-attr inst)]
    (if (string? ma)
      (when (seq ma)
        (#?(:clj read-string :cljs cljs.reader/read-string) ma))
      ma)))

(defn between-relationship-instance? [inst]
  (when-let [t (instance-type-kw inst)]
    (between-relationship? t)))

(defn prepost-event-name [selector tag recname]
  (let [[c n] (li/split-path recname)]
    (li/make-path c (keyword
                     (str (case selector :after "After_" :before "Before_")
                          (name tag) "_"
                          (name n))))))

(def post-event-name (partial prepost-event-name :after))
(def pre-event-name (partial prepost-event-name :before))

(defn make-post-event [event-name inst]
  {event-name {:Instance inst}})

(def ^:private post-events-disabled-entities (atom #{}))

(defn disable-post-events [entity-name]
  (let [n (li/make-path entity-name)]
    (swap! post-events-disabled-entities conj n)
    n))

(defn post-events-disabled? [entity-name]
  (let [entity-name (li/make-path entity-name)]
    (when (some #{entity-name} @post-events-disabled-entities)
      true)))

(defn do-fire-prepost-event [selector event-evaluator tag entity-name inst]
  (let [event-name (prepost-event-name selector tag entity-name)]
    (if (find-dataflows event-name)
      (let [rs (:result (event-evaluator (make-post-event event-name inst)))
            final-inst (if (map? rs) rs (first rs))]
        (if (and (an-instance? final-inst) (instance-of? entity-name final-inst))
          final-inst
          inst))
      inst)))

(defn fire-prepost-event [selector tag inst]
  (let [typ (instance-type-kw inst)
        skip (and (= selector :after) (post-events-disabled? typ))]
    (if-not skip
      (do-fire-prepost-event selector gs/evaluate-pattern tag typ inst)
      inst)))

(def fire-post-event (partial fire-prepost-event :after))
(def force-fire-post-event (partial do-fire-prepost-event :after))
(def fire-pre-event (partial fire-prepost-event :before))

(defn encode-expressions-in-schema [scm]
  (let [norm-scm (mapv (fn [[k v]]
                         [k (cond
                              (map? v)
                              (encode-expressions-in-schema v)

                              (vector? v) v

                              (or (fn? v) (seqable? v)) :fn

                              :else
                              (let [[c n] (li/split-path v)]
                                (if (= c :Agentlang.Kernel.Lang)
                                  n
                                  v)))])
                       (dissoc scm li/event-context))]
    (into {} norm-scm)))

(def ^:private rule-registry (u/make-cell {}))

(defn- register-rule-for-entities! [rule-name ent-names]
  (u/call-and-set
   rule-registry
   #(let [rr @rule-registry
          r (mapv (fn [n] [n (set (conj (get rr n) rule-name))]) ent-names)]
      (merge rr (into {} r)))))

(defn- unregister-rule-for-entities! [rule-name]
  (u/call-and-set
   rule-registry
   #(let [rr @rule-registry
          fr0 (mapv (fn [[k rns]] [k (set (filter (partial not= rule-name) rns))]) rr)
          fr (filter (fn [[_ rns]] (seq rns)) fr0)]
      (into {} fr))))

(def rule-is-passive? :passive)
(def rule-category :category)
(def rule-cc :c-cond)
(def rule-condition :cond)
(def rule-consequence :then)
(def rule-priority :priority)
(def rule-name :name)
(def rule-on-delete :on-delete)

(defn register-rule [rule-name spec]
  (when (and (seq (rule-cc spec))
             (seq (rule-consequence spec)))
    (u/call-and-set
     components
     #(let [ms @components
            [component n] (li/split-path rule-name)
            path [component (get-model-version component) :rules n]]
        (register-rule-for-entities! rule-name (mapv first (rule-cc spec)))
        (assoc-in ms path spec))))
  rule-name)

(defn unregister-rule [rule-name]
  (u/call-and-set
   components
   #(let [ms @components
          [component n] (li/split-path rule-name)
          path [component (get-model-version component) :rules n]]
      (unregister-rule-for-entities! rule-name)
      (su/dissoc-in ms path)))
  rule-name)

(defn remove-rule [rule-name]
  (when (unregister-rule rule-name)
    (raw/remove-rule rule-name)))

(defn fetch-rule [rule-name]
  (let [[c n] (li/split-path rule-name)]
    (component-find [c (get-model-version c) :rules n])))

(defn fetch-rules [component-name]
  (component-find [component-name (get-model-version component-name) :rules]))

(def ^:private rule-for-delete-event? rule-on-delete)
(def ^:private rule-for-upsert-event? (complement rule-for-delete-event?))

(defn- filter-rules [predic rule-names]
  (seq (filter predic (map fetch-rule rule-names))))

(def ^:private filter-delete-rules (partial filter-rules rule-for-delete-event?))
(def ^:private filter-upsert-rules (partial filter-rules rule-for-upsert-event?))

(defn rules-for-entity [tag entity-name]
  (when-let [rule-names (seq (get @rule-registry entity-name))]
    (if (= :delete tag)
      (filter-delete-rules rule-names)
      (filter-upsert-rules rule-names))))

(defn rule-compiled-conditions [entity-name rule-spec]
  (when-let [ccs (seq (filter #(= entity-name (first %)) (rule-cc rule-spec)))]
    (mapv second ccs)))

(defn rule-has-least-priority? [rule-spec]
  (= ##-Inf (rule-priority rule-spec)))

(defn- apply-rule-ccs [env inst ccs]
  (loop [ccs ccs, env env]
    (if-let [c (first ccs)]
      (when-let [env (c env inst)]
        (recur (rest ccs) env))
      env)))

(defn- register-llm-construct [tag construct-name spec]
  (u/call-and-set
   components
   #(let [ms @components
          [component n] (li/split-path construct-name)
          path [component (get-model-version component) tag n]]
      (assoc-in ms path spec)))
  construct-name)

(def register-inference (partial register-llm-construct :inferences))

(defn docstring [n]
  (:doc (fetch-meta n)))

(defn view-query [entity-name]
  (:view-query (fetch-meta entity-name)))

(defn view? [entity-name]
  (if (view-query entity-name) true false))

(create-component kernel-userapp-component nil)

(defn audit-trail-entity-name [entity-name]
  (let [[c n] (li/split-path entity-name)]
    (li/make-path c (str (name n) "Audit"))))

(defn audit-required? [entity-name]
  (:audit (fetch-meta entity-name)))

(defn- find-schema-info [fetch-names find-schema]
  (su/nonils
   (mapv (fn [n]
           (when-let [scm (find-schema n)]
             {n (encode-expressions-in-schema scm)}))
         (fetch-names))))

(defn schema-info [component]
  {:records (find-schema-info #(record-names component) raw/find-record)
   :entities (find-schema-info #(entity-names component) raw/find-entity)
   :relationships (find-schema-info #(relationship-names component) raw/find-relationship)})

(defn register-resolver [res-name res-spec]
  (component-intern res-name res-spec :resolvers))

(defn remove-resolver [res-name]
  (and (raw/remove-resolver res-name)
       (component-remove res-name :resolvers)))

(defn find-resolvers [component-name]
  (get-in @components [component-name (get-model-version component-name) :resolvers]))

(defn attribute-type-as-string [attr-name]
  (let [scm (find-attribute-schema attr-name)]
    (if-let [choices (:oneof scm)]
      (str "Either of " (vec choices))
      (str "A value of type " (or (:type scm) "String")))))

(defn schema-as-string [scm]
  (reduce (fn [s [n t]]
            (str s (name n) ": " (attribute-type-as-string t) "\n"))
          "" scm))

(defn system-component-names []
  (when-let [cns (seq (filter #(s/starts-with? (str %) ":Agentlang") (component-names)))]
    (vec cns)))

(def ^:private extension-attributes (u/make-cell {}))

(defn intern-extension-attribute [entity-name attr-name attr-type-name order]
  (u/call-and-set
   extension-attributes
   #(let [exts @extension-attributes
          attr-names (entity-name exts)]
      (assoc exts entity-name (conj attr-names [attr-name attr-type-name order]))))
  attr-type-name)

(defn find-extension-attributes [entity-name]
  (let [entity-name (li/make-path entity-name)]
    (seq (sort-by last (entity-name @extension-attributes)))))

(def extension-attribute-name first)

(defn find-extension-attribute-names [entity-name]
  (when-let [attrs (find-extension-attributes entity-name)]
    (mapv extension-attribute-name attrs)))

(defn extension-attribute-info [[_ attr-type-name]]
  (if-let [meta (fetch-attribute-meta attr-type-name)]
    meta
    (u/throw-ex (str "no meta defined for " attr-type-name))))

(defn extension-attribute? [attr-name-or-scm]
  (if (keyword? attr-name-or-scm)
    (extension-attribute? (find-attribute-schema attr-name-or-scm))
    (and (:relationship attr-name-or-scm) (:extend attr-name-or-scm))))

(defn- sanitize-secrets [obj]
  (let [r (mapv (fn [[k v]]
                  [k (if (sh/crypto-hash? v)
                       "*********"
                       v)])
                obj)]
    (into {} r)))

(defn cleanup-inst [obj]
  (cond
    (an-instance? obj)
    (let [r (instance-attributes (sanitize-secrets obj))]
      (into {} (mapv (fn [[k v]] [k (if (or (map? v) (vector? v))
                                      (cleanup-inst v)
                                      v)])
                     r)))
    (or (map? obj) (string? obj)) obj
    (seqable? obj) (mapv cleanup-inst obj)
    :else obj))

(def dataflow-opcode (constantly []))

(defn full-path-from-references [_ _ _]
  (u/raise-not-implemented 'full-path-from-references))

(defn force-cast-instance-type [new-type inst]
  (assoc inst type-key new-type))

(defn all-attribute-names [recname]
  (when-let [scm (fetch-user-schema recname)]
    (concat (keys scm)
            (when (entity? recname)
              (find-extension-attributes recname)))))

(defn path-attributes [recname]
  (when-let [scm (fetch-entity-schema recname)]
    (when-let [attrs (seq (filter (fn [[k _]] (type-path? scm k)) scm))]
      (set (mapv first attrs)))))

(defn entities-reached-via-path-attributes [recname]
  (when-let [scm (fetch-entity-schema recname)]
    (when-let [entities (seq (su/nonils (mapv (fn [[k _]]
                                                (when-let [n (attribute-path-to scm k)]
                                                  [n k]))
                                              scm)))]
      (into {} entities))))

(defn inference? [event-name]
  (when-let [meta (fetch-meta event-name)]
    (and (= :event (:record-type meta))
         (= :Agentlang.Core/Inference (:inherits meta)))))

(defn entity-action [tag entity-name]
  (tag (:actions (fetch-meta entity-name))))

(def entity-create-action (partial entity-action :create))
(def entity-update-action (partial entity-action :update))
(def entity-delete-action (partial entity-action :delete))
