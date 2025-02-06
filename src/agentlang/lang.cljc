(ns agentlang.lang
  "The core constructs of the modeling language."
  (:require [clojure.set :as set]
            [clojure.string :as s]
            [clojure.walk :as w]
            #?(:clj [clojure.core.async :as async])
            [agentlang.compiler :as c]
            [agentlang.compiler.context :as ctx]
            [agentlang.compiler.rule :as rl]
            [agentlang.component :as cn]
            [agentlang.global-state :as gs]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k]
            [agentlang.lang.raw :as raw]
            [agentlang.lang.rbac :as lr]
            [agentlang.meta :as mt]
            [agentlang.paths.internal :as pi]
            [agentlang.resolvers]
            [agentlang.resolver.registry :as rr]
            [agentlang.resolver.core :as rc]
            [agentlang.subs :as subs]
            [agentlang.rule :as rule]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.seq :as us]))

(defn- normalize-imports [imports]
  (let [imps (rest imports)]
    (if (keyword? (first imps))
      [imps]
      imps)))

(def ^:private component-spec-validators
  {:clj-import li/validate-clj-imports})

(defn- validate-component-spec [spec]
  (into
   {}
   (mapv
    (fn [[k v]]
      (let [vf (or (k component-spec-validators)
                   identity)]
        [k (vf v)]))
    spec)))

(defn model [spec]
  (let [n (:name spec)]
    (when-not n
      (u/throw-ex "model name is required"))
    (when-not (:agentlang-version spec)
      (u/throw-ex "agentlang-version is required in model spec"))
    (let [v (or (:version spec)
                (if (= n :Agentlang) (gs/agentlang-version) "0.0.1"))
          spec (assoc spec :version v)]
      (log/info (str "registering model " (name n) ", " v))
      (cn/register-model n spec))))

(defn component
  "Create and activate a new component with the given name."
  ([n spec]
   (let [ns-name (li/validate-name n)]
     (when-not gs/migration-mode
       (when (cn/component-exists? ns-name)
         (cn/remove-component ns-name)))
     (when-let [model-name (:model spec)]
       (cn/add-component-to-model model-name n))
     (let [r (cn/create-component
              ns-name
              (when spec
                (validate-component-spec spec)))]
       (when-let [imps (:clj-import spec)]
         (li/do-clj-import imps))
       (and (raw/component n spec)
            r))))
  ([n] (component n nil)))

(defn- attribute-type? [nm]
  (or (k/kernel-type? nm)
      (cn/find-attribute-schema nm)
      (cn/find-record-schema nm)))

(defn- rewrite-ref-path [scm]
  (if-let [p (:ref scm)]
    (assoc scm :ref (li/path-parts p))
    scm))

(defn- normalize-attribute-schema [scm]
  (rewrite-ref-path (if (fn? scm)
                      {:check scm}
                      scm)))

(defn- normalize-oneof-values [xs]
  (map #(if (keyword? %)
          (name %)
          %)
       xs))

(defn- assoc-oneof-default [scm]
  (if-let [d (:default scm)]
    (assoc scm :default
           (if (keyword? d)
             (name d)
             d))
    scm))

(defn- oneof-as-check
  "Convert a {:oneof [a b c]} to {:check #(some #{%} [a b c])}."
  [attr-schema]
  (if-let [xs (seq (:oneof attr-schema))]
    (let [values (set (normalize-oneof-values xs))]
      (assoc (assoc-oneof-default attr-schema)
             :check (fn [x] (some #{x} values))))
    attr-schema))

(defn- reference-exists? [path]
  (if-let [scm (cn/find-attribute-schema path)]
    true
    false))

(defn- query-pattern? [a-map]
  (when-not (:eval a-map)
    (let [ks (keys a-map)
          k (first ks)]
      (and (= 1 (count ks))
           (li/name? k)
           (map? (get a-map k))))))

(defn- fn-or-name? [x]
  #?(:clj
     (or (fn? x) (li/name? x))
     ;; expressions not currently validated or used in the browser.
     :cljs true))

(defn- fn-or-list? [x]
  (or (list? x) (fn? x)))

(defn- encryption? [x]
  ;; true/:default means use the default encryption algorithm.
  ;; In future, specific algorithms may be supported
  ;; as an enum of keywords, e.g - :bcrypt, :sha512 etc.
  (or (= true x)
      (= :default x)))

(def ^:private eval-block-keys #{:patterns :refresh-ms :timeout-ms :opcode})

(defn- eval-block? [x]
  (and (map? x)
       (every?
        #(some #{%} eval-block-keys)
        (keys x))))

(defn- listof-spec? [x]
  (cond
    (keyword? x)
    (attribute-type? x)

    (map? x)
    (reference-exists? (:ref x))

    :else (u/throw-ex (str "invalid :listof specification - " x))))

(defn- meta-specs [attrs]
  (let [meta (:meta attrs)
        ui-spec (:ui attrs)
        rbac-spec (:rbac attrs)
        meta-ui (merge (:ui meta) ui-spec)
        meta-rbac ((if (map? rbac-spec) merge concat) (:rbac meta) rbac-spec)]
    [(merge meta (when (seq meta-ui) {:ui meta-ui})
            (when (seq meta-rbac) {:rbac meta-rbac}))
     (dissoc attrs :meta :ui :rbac)]))

(defn- merge-attribute-meta [scm]
  (let [[meta scm] (meta-specs scm)]
    (assoc scm :meta meta)))

(defn- finalize-raw-attribute-schema [scm]
  (doseq [[k v] scm]
    (if (or (= k li/guid) (= k li/path-identity))
      (li/validate-bool k v)
      (case k
        (:unique
         :immutable :optional :indexed
         :write-only :read-only
         :cascade-on-delete :var :secure-hash)
        (li/validate-bool k v)
        :check (li/validate fn? ":check is not a predicate" v)
        :parse (li/validate fn? ":parse is not a function" v)
        :default (when-not (fn? v)
                   (when-let [predic (:check scm)]
                     (li/validate predic "invalid value for :default" v)))
        :type (li/validate attribute-type? "invalid :type" v)
        :expr (li/validate fn-or-list? ":expr has invalid value" v)
        :eval (li/validate eval-block? ":eval has invalid value" v)
        :format (li/validate string? ":format must be a textual pattern" v)
        :listof (li/validate listof-spec? ":listof has invalid type" v)
        :setof (li/validate attribute-type? ":setof has invalid type" v)
        :type-in-store (li/validate string? ":type-in-store must be specified as a string" v)
        :ref (li/validate reference-exists? ":ref is invalid" v)
        :writer (li/validate fn? ":writer must be a function" v)
        :oneof v
        :label (li/validate symbol? ":label must be a symbol" v)
        :relationship (li/validate cn/relationship? "not a valid relationship name" v)
        :extend (li/validate cn/entity? "not a valid entity name" v)
        :order (li/validate int? "order must be an integer" v)
        (:ui :rbac :meta) v
        (u/throw-ex (str "invalid constraint in attribute definition - " k)))))
  (merge-attribute-meta
   (merge
    {:unique false :immutable false}
    (if-let [fmt (:format scm)]
      (assoc scm :format (partial re-matches (re-pattern fmt)) :format-str fmt)
      scm))))

(defn- find-ref-type [path]
  (when-let [scm (cn/find-attribute-schema path)]
    (or (:type scm)
        (when-let [rpath (:ref scm)]
          (find-ref-type rpath)))))

(defn- maybe-assoc-ref-type [attrscm]
  (if (:type attrscm)
    attrscm
    (if-let [rpath (and (not (:listof attrscm)) (:ref attrscm))]
      (if-let [tp (find-ref-type rpath)]
        (assoc attrscm :type tp)
        attrscm)
      attrscm)))

(defn- attr-type-spec [attr-spec]
  (when-let [p (some #{:type :listof :setof} (keys attr-spec))]
    [p (p attr-spec)]))

(defn- normalize-kernel-types [attrs]
  (let [r (mapv (fn [[k v]]
                  [k (cond
                       (keyword? v)
                       (k/normalize-kernel-type v)

                       (map? v)
                       (if-let [[p t] (attr-type-spec v)]
                         (assoc v p (k/normalize-kernel-type t))
                         v)

                       :else v)])
                attrs)]
    (into {} r)))

(defn- validate-attribute-schema-map-keys [scm]
  (let [newscm (maybe-assoc-ref-type
                (finalize-raw-attribute-schema
                 (oneof-as-check
                  (normalize-kernel-types scm))))]
    (cond
      (:unique newscm)
      (assoc newscm :indexed true)

      (li/guid newscm)
      (assoc newscm :indexed true :unique true)

      :else newscm)))

(defn- validate-attribute-schema [n scm]
  (if (fn? scm)
    scm
    (validate-attribute-schema-map-keys
     (li/validate map? (str n " - attribute specification should be a map") scm))))

(defn- validated-canonical-type-name
  ([validate-name n]
   (let [validate-name (or validate-name li/validate-name)
         canon (cn/canonical-type-name n)
         [c n] (li/split-path canon)]
     (when (and (not= c k/kernel-lang-component) (k/plain-kernel-type? n))
       (log/warn (str "redefinition of kernel type "
                      n " will always require the fully-qualified name - "
                      canon)))
     (validate-name canon)))
  ([n] (validated-canonical-type-name nil n)))

(defn- validate-extension-attribute! [attr-name attr-scm extend-entity]
  (let [rel (:relationship attr-scm)
        reltype (get-in attr-scm [:meta :ext-reltype])]
    (when (not= rel reltype)
      (when (not= reltype (cn/other-relationship-node rel extend-entity))
        (u/throw-ex (str "invalid relationship " rel " on " extend-entity " in attribute " attr-name))))))

(defn- maybe-parse-ext-attr-name [attr-name attr-scm]
  (when-let [ent-name (:extend attr-scm)]
    (validate-extension-attribute! attr-name attr-scm ent-name)
    [ent-name (second (li/split-path attr-name))]))

(defn- maybe-handle-extension-attribute [attr-name attr-scm]
  (if-let [[ent-name new-attr-name] (maybe-parse-ext-attr-name attr-name attr-scm)]
    (if-let [scm (cn/fetch-entity-schema ent-name)]
      (and (cn/intern-entity ent-name (assoc scm new-attr-name attr-name))
           (cn/intern-extension-attribute ent-name new-attr-name attr-name (get-in attr-scm [:meta :ext-order])))
      (u/throw-ex (str "attribute with relationship refers to invalid entity - " ent-name)))
    attr-name))

(defn- intern-attribute
  "Add a new attribute definition to the component."
  ([validate-name n scm]
   (let [raw-scm scm
         scm (if (cn/extension-attribute? scm)
               (assoc
                scm
                :optional true
                :type :Agentlang.Kernel.Lang/Any
                :meta (assoc
                       (:meta scm)
                       :ext-reltype (:type scm)
                       :ext-rel (:relationship scm)
                       :ext-order (or (:order scm) 1)))
               scm)
         r (cn/intern-attribute
            (validate-name n)
            (normalize-attribute-schema
             (validate-attribute-schema n scm)))]
     (and (maybe-handle-extension-attribute n scm) (raw/attribute n raw-scm) r)))
  ([n scm]
   (intern-attribute li/validate-name-relaxed n scm)))

(def attribute (partial
                intern-attribute
                (partial validated-canonical-type-name li/validate-name-relaxed)))

(defn- validate-attributes [attrs]
  (doseq [[k v] attrs]
    (li/validate-name k)
    (cond
      (keyword? v) (when-not (attribute-type? v)
                     (u/throw-ex (str "type not defined - " v)))
      (map? v) (validate-attribute-schema-map-keys v)
      (not (list? v)) (u/throw-ex (str "invalid attribute specification - " [k v]))))
    attrs)

(defn- query-eval-fn [recname attrs k v]
  ;; TODO: implement the query->fn compilation phase.
  (u/throw-ex (str k " - inline compilation for :query not implemented")))

(defn- attref? [n]
  (let [[[_ _] a] (li/ref-as-names n)]
    (if a true false)))

(defn- compile-eval-block [recname attrs evblock]
  (let [ctx (ctx/make)]
    (ctx/put-record! ctx (li/split-path recname) attrs)
    (if-let [opcode (mapv (partial c/compile-pattern ctx) (:patterns evblock))]
      opcode
      (u/throw-ex (str recname " - failed to compile eval-block")))))

(defn- normalize-eval-block [evblock]
  (when evblock
    (if (and (map? evblock) (:patterns evblock))
      evblock
      {:patterns [evblock]})))

(defn- normalize-compound-attr [recname attrs nm [k v]]
  (if-let [ev (normalize-eval-block (:eval v))]
    (attribute
     nm
     (assoc
      v
      :eval (assoc ev :opcode (compile-eval-block recname attrs ev))
      :optional true))
    (when-let [expr (:expr v)]
      (cond
        (fn? expr)
        (attribute nm v)

        :else (attribute
               nm
               (merge (if-let [t (:type v)]
                        {:type t}
                        (u/throw-ex (str ":type is required for attribute " k " with compound expression")))
                      {:expr expr}))))))

(defn- normalize-attr [recname attrs fqn [k v]]
  (let [newv
        (cond
          (map? v)
          (let [v (if (:read-only v) (assoc v :optional true) v)
                nm (fqn (li/unq-name))]
            (if (query-pattern? v)
              (attribute nm {:query (query-eval-fn recname attrs k v)})
              (or (normalize-compound-attr recname attrs nm [k v])
                  (attribute nm v))))

          (list? v)
          (attribute
           (fqn (li/unq-name))
           {:expr (c/compile-attribute-expression recname attrs k v)})

          :else
          (let [fulln (fqn v)]
            (if (attref? fulln)
              (attribute
               (fqn (li/unq-name))
               {:ref fulln})
              fulln)))]
    [k newv]))

(defn- required-attribute-names [attrs]
  (map first
       (filter (fn [[a v]]
                 (if (map? v)
                   (not (or (:optional v) (:default v)))
                   (if-let [scm (and (keyword? v) (cn/find-attribute-schema v))]
                     (not (or (:optional scm) (:default scm)))
                     true)))
               attrs)))

(defn- infer-default [attr-name attr-def dict?]
  (let [type-name (if dict? (:type attr-def) attr-def)
        scm (cn/find-attribute-schema type-name)]
    (if scm
      (if-let [d (:default scm)]
        (if dict?
          (assoc attr-def :default d)
          {:type type-name
           :default d})
        (u/throw-ex (str attr-name " - no default defined for " type-name)))
      (u/throw-ex (str attr-name " - undefined type - " type-name)))))

(defn- assoc-defaults [req-attrs [aname adef]]
  (let [optional? (not (some #{aname} req-attrs))
        dict? (map? adef)]
    (when (and (not optional?) dict? (:optional adef))
      (u/throw-ex (str aname " - cannot be marked :optional")))
    [aname (if optional?
             (if (and dict? (:default adef))
               adef
               (if dict?
                 (assoc adef :optional true)
                 {:type adef :optional true}))
             adef)]))

(defn- merge-unique-flags [attrs uq-attr-names]
  (map (fn [[k v]]
         (if (some #{k} uq-attr-names)
           [k (if (keyword? v)
                {:type v
                 :unique true}
                (assoc v :unique true))]
           [k v]))
       attrs))

(defn- fetch-inherited-schema [type-name child-record-type]
  (if-let [scm (case child-record-type
                 (:entity :record)
                 (or (raw/find-entity type-name)
                     (raw/find-record type-name))
                 :event (or (raw/find-event type-name)
                            (raw/find-record type-name)))]
    (let [final-scm (into {} (mapv (fn [[k v]]
                                     (if (keyword? v)
                                       [k (k/normalize-kernel-type v)]
                                       [k v]))
                                   scm))]
      (if-let [inherits (:inherits (:meta scm))]
        (merge final-scm (fetch-inherited-schema inherits child-record-type))
        final-scm))
    (u/throw-ex (str "parent type not found - " type-name))))

(defn- preproc-for-built-in-attrs [attrs]
  (let [attrs (mapv (fn [[k v]]
                      [k (if (keyword? v)
                           (cond
                             (or (= v :Identity)
                                 (= v :Agentlang.Kernel.Lang/Identity))
                             (cn/find-attribute-schema :Agentlang.Kernel.Lang/Identity)

                             (or (= v :Now)
                                 (= v :Agentlang.Kernel.Lang/Now))
                             (cn/find-attribute-schema :Agentlang.Kernel.Lang/Now)

                             :else (let [[c n] (li/split-path v)]
                                     (if (and c n (not= :Agentlang.Kernel.Lang c))
                                       (or (raw/find-attribute v) v)
                                       v)))
                           v)])
                    attrs)]
    (into {} attrs)))

(defn- preproc-attrs [attrs]
  (let [attrs (mapv (fn [[k v]]
                      [k (if-let [expr (and (map? v) (:expr v))]
                           (if (or (keyword? expr) (li/patterns-arg? expr))
                             (let [t (or (:type v) :Any)]
                               (assoc v :type t :expr `(clojure.core/identity ~expr)))
                             v)
                           v)])
                    attrs)]
  (preproc-for-built-in-attrs (into {} attrs))))

(defn- normalized-attributes [rectype recname orig-attrs]
  (let [f (partial cn/canonical-type-name (cn/get-current-component))
        orig-attrs (normalize-kernel-types orig-attrs)
        [meta base-attrs] (meta-specs orig-attrs)
        inherits (:inherits meta)
        inherited-scm (when inherits (fetch-inherited-schema inherits rectype))
        req-inherited-attrs (or (:required-attributes (:meta inherited-scm))
                                (required-attribute-names inherited-scm))
        attrs (if inherited-scm
                (merge (li/only-user-attributes inherited-scm) base-attrs)
                base-attrs)
        req-orig-attrs (or (:required-attributes meta)
                           (required-attribute-names base-attrs))
        req-attrs (concat req-orig-attrs req-inherited-attrs)
        attrs-with-defaults (into {} (map (partial assoc-defaults req-attrs) attrs))
        newattrs (map (partial normalize-attr recname attrs f) attrs-with-defaults)
        final-attrs (into {} (validate-attributes newattrs))]
    (assoc final-attrs :meta (assoc meta :required-attributes req-attrs :record-type rectype))))

(defn- parse-and-define [f schema]
  (let [n (first (keys schema))]
    (f n (get schema n))))

(defn record
  "Add a new record definition to the component."
  ([n attrs]
   (if (map? attrs)
     (let [cn (validated-canonical-type-name n)
           r (cn/intern-record
              cn (normalized-attributes :record cn (preproc-attrs attrs)))]
       (when r
         (and (if-not (:contains (:meta attrs))
                (raw/record cn attrs)
                cn)
              cn)))
     (u/throw-ex (str "Syntax error in record. Check record: " n))))
  ([schema]
   (parse-and-define record schema)))

(defn event-internal
  ([n attrs verify-name?]
   (let [cn (if verify-name?
              (validated-canonical-type-name n)
              (cn/canonical-type-name n))]
     (cn/intern-event
      cn
      (if (cn/inferred-event-schema? attrs)
        attrs
        (normalized-attributes :event cn attrs)))))
  ([n attrs]
   (event-internal n attrs false)))

(defn- ensure-no-reserved-event-attrs! [attrs]
  (when (some #(= li/event-context (first %)) attrs)
    (u/throw-ex "li/event-context is a reserved attribute name")))

(defn event
  "An event record with timestamp and other auto-generated meta fields."
  ([n attrs]
   (ensure-no-reserved-event-attrs! attrs)
   (let [attrs1 (preproc-attrs attrs)
         r (event-internal
            n (assoc attrs1 li/event-context (k/event-context-attribute-name))
            true)]
     (and (raw/event r attrs) r)))
  ([schema]
   (parse-and-define event schema)))

(defn- intern-inferred-event [nm]
  (event nm cn/inferred-event-schema))

(defn ensure-event! [x]
  (if-let [n (li/record-name x)]
    (when-not (cn/find-event-schema n)
      (intern-inferred-event n))
    (u/throw-ex (str "not an event - " x))))

(defn- ensure-dataflow-pattern! [x]
  (cond
    (keyword? x) (li/validate-name-relaxed x)
    (or (map? x) (li/special-form? x) (symbol? x)) x
    :else (u/throw-ex (str "Invalid dataflow pattern. Possible syntax error - " x))))

(defn- prepare-dataflow-patterns [xs]
  (let [xs (if (and (seqable? xs) (string? (first xs))) ; ignore docstring
             (rest xs)
             xs)]
    (doseq [x xs] (ensure-dataflow-pattern! x))
    xs))

(declare normalize-event-pattern)

(defn- normalize-event-pattern-attribute [[k v]]
  (cond
    (map? v)
    [k (normalize-event-pattern v)]

    (symbol? v)
    [k `(quote ~v)]

    :else [k v]))

(defn- normalize-event-pattern [pattern]
  (if (map? pattern)
    (let [attrs (map normalize-event-pattern-attribute (first (vals pattern)))]
      (into {(first (keys pattern)) (into {} attrs)}))
    pattern))

(defn- extract-on-and-where [match-pat]
  (if (= (count match-pat) 7)
    (do
      (when-not (= :on (nth match-pat 3))
        (u/throw-ex (str ":on keyword not found - " match-pat)))
      (when-not (= :where (nth match-pat 5))
        (u/throw-ex (str ("where clause not found - " match-pat))))
      [(li/validate-on-clause (nth match-pat 4))
       (li/validate-where-clause (nth match-pat 6))])
    (u/throw-ex (str ":on and :where clauses expected - " match-pat))))

(defn- install-event-trigger-pattern [match-pat]
  (let [event-name (first match-pat)]
    (when-not (li/name? event-name)
      (u/throw-ex (str "not a valid event name - " event-name)))
    (when-not (= :when (second match-pat))
      (u/throw-ex (str "expected keyword :when not found - " match-pat)))
    (let [pat (nth match-pat 2)
          predic (rl/compile-rule-pattern pat)
          rnames (li/referenced-record-names pat)
          [on where] (when (> (count rnames) 1)
                       (extract-on-and-where match-pat))
          event-attrs (li/references-to-event-attributes rnames)
          evt-name (event event-name event-attrs)]
      (cn/install-triggers!
       (or on rnames)
       event-name predic where rnames)
      evt-name)))

(defn- concat-refs [n refs]
  (keyword (str (subs (str n) 1) "."
                (s/join "." (mapv name refs)))))

(defn- event-self-ref-pattern [event-name]
  (if-let [scm (:schema (cn/find-event-schema event-name))]
    (let [prefix (subs (str event-name) 1)
          attrs (mapv (fn [[k _]]
                        [k (keyword (str prefix "." (name k)))])
                      scm)]
      [{event-name (into {} attrs)}])
    (log/warn (str "cannot auto-generate dataflow patterns, event schema not found - " event-name))))

(defn- prepost-crud-dataflow? [pat]
  (when (vector? pat)
    (let [p (first pat)]
      (when (or (= :after p) (= :before p))
        (if (some #{(second pat)} #{:create :update :delete})
          (if (cn/entity? (nth pat 2))
            true
            (u/throw-ex (str "invalid entity name in " pat)))
          (u/throw-ex (str "invalid crud operation in " pat)))))))

(defn- parse-prepost-patterns [event-name pats]
  (if (seq pats)
    (let [rf-inst (li/make-ref event-name :Instance)
          rf-ctx (li/make-ref event-name li/event-context)]
      (w/postwalk
       #(if (keyword? %)
          (cond
            (= :Instance %) rf-inst

            (= li/event-context %) rf-ctx

            (s/starts-with? (str %) ":Instance.")
            (keyword (subs (s/replace (str %) ":Instance." (str rf-inst ".")) 1))

            (s/starts-with? (str %) ":EventContext.")
            (keyword (subs (s/replace (str %) ":EventContext." (str rf-ctx ".")) 1))

            :else %)
          %)
       pats))
    (parse-prepost-patterns event-name [:Instance])))

(defn- parse-prepost-crud-header [pat]
  (let [event-name (apply cn/prepost-event-name pat)]
    (event-internal event-name {:Instance :Agentlang.Kernel.Lang/Entity
                                li/event-context (k/event-context-attribute-name)})
    event-name))

(defn- preproc-match-pat [match-pat]
  (when (map? match-pat)
    (:preproc match-pat)))

(defn- as-preproc [match-pat]
  (if (and (map? match-pat) (:preproc match-pat))
    match-pat
    {:preproc match-pat}))

(defn- canonical-dataflow-match-pattern [pat]
  (if (and (keyword? pat) (= 1 (count (li/split-path pat))))
    (cn/canonical-type-name pat)
    pat))

(defn dataflow
  "A declarative data transformation pipeline."
  [match-pat & patterns]
  (let [match-pat (canonical-dataflow-match-pattern match-pat)
        r (cond
            (prepost-crud-dataflow? match-pat)
            (let [event-name (parse-prepost-crud-header match-pat)
                  pats (parse-prepost-patterns event-name patterns)]
              (apply dataflow (as-preproc event-name) pats))

            (not (seq patterns))
            (apply dataflow (as-preproc match-pat)
                   (event-self-ref-pattern (preproc-match-pat match-pat)))

            :else
            (let [match-pat (or (preproc-match-pat match-pat) match-pat)
                  patterns (prepare-dataflow-patterns patterns)]
              (if (vector? match-pat)
                (apply
                 dataflow
                 (as-preproc (install-event-trigger-pattern match-pat))
                 patterns)
                (let [event (normalize-event-pattern match-pat)]
                  (do (ensure-event! event)
                      (cn/register-dataflow event nil patterns))))))]
    (when (and r (not (preproc-match-pat match-pat)))
      (raw/dataflow match-pat patterns))
    r))

(defn- maybe-proc-delete-rule [conds]
  (let [delete-tag (some #(and (vector? %) (= :delete (first %))) conds)]
    (if delete-tag
      (do (when (> (count conds) 1)
            (u/throw-ex (str "no extra rules allowed with :delete - " conds)))
          [delete-tag [(second (first conds))]])
      [false conds])))

(defn- rule-compile-conditionals [cond-pats]
  (try
    (rule/compile-conditionals cond-pats)
    (catch #?(:clj Exception :cljs js/Error) e
      (log/error (str "rule/compile-conditionals failed: " e)))))

(defn- parse-rules-args [args]
  (let [not-then (partial not= :then)
        [cond-pats args] (split-with not-then args)
        [delete-tag cond-pats] (maybe-proc-delete-rule cond-pats)
        meta (let [l (last args)] (when (li/rule-meta? l) l))
        priority (li/rule-meta-value meta :priority ##-Inf)
        passive (li/rule-meta-value meta :passive)
        cat (li/rule-meta-value meta :category)
        conseq-pats (if meta (drop-last (rest args)) (rest args))]
    {:cond (when-not (li/rule-meta? (first cond-pats)) cond-pats)
     :c-cond (rule-compile-conditionals cond-pats)
     :then conseq-pats
     :priority priority
     :passive passive
     :category cat
     :on-delete delete-tag}))

(defn- rule-event [rule-name conseq]
  (if conseq
    (let [revnt-name (li/rule-event-name rule-name)]
      (event-internal revnt-name {})
      (cn/register-dataflow revnt-name conseq))
    (do (log/warn (str rule-name " requires a consequent"))
        rule-name)))

(defn rule [rule-name & args]
  (let [s01 (parse-rules-args args)
        spec (assoc s01 :name rule-name)]
    (when (rule-event rule-name (:then spec))
      (and (cn/register-rule rule-name spec)
           (raw/rule rule-name args)
           rule-name))))

(defn- preproc-agent-messages [agent]
  (if-let [messages (:Messages agent)]
    (if (li/quoted? messages)
      agent
      (assoc agent :Messages (li/as-quoted messages)))
    agent))

(defn- preproc-inference-agent [is-query agent-spec]
  (when agent-spec
    (if is-query
      {:Agentlang.Core/Agent {:Name? agent-spec}}
      (if-let [llm-name (:with-llm agent-spec)]
        {:Agentlang.Core/Agent
         (preproc-agent-messages (dissoc agent-spec :with-llm))
         :-> [[{:Agentlang.Core/AgentLLM {}}
               {:Agentlang.Core/LLM {:Name? llm-name}}]]}
        agent-spec))))

(defn instance-assoc [inst & params]
  (loop [inst inst, params params]
    (if-let [k (first params)]
      (recur (assoc inst (keyword k) (second params))
             (rest (rest params)))
      inst)))

(defn register-inference-dataflow [inference-name spec]
  (let [agent-spec (let [aspec (:agent spec)]
                     (if (keyword? aspec)
                       (str aspec)
                       aspec))
        is-agent-query (string? agent-spec)
        agent0 (preproc-inference-agent is-agent-query agent-spec)
        agent1 (dissoc agent0 :->)
        agent-attrs (li/record-attributes agent1)
        agent {(li/record-name agent1)
               (assoc agent-attrs :Context inference-name)}
        p0 (if is-agent-query
             (assoc agent :as [:Agent])
             (assoc agent :as :Agent :-> (:-> agent0)))
        pfn (:with-prompt-fn spec)
        rh (:with-response-handler spec)
        pfns (when (or pfn rh)
               `[:eval (agentlang.lang/instance-assoc :Agent "PromptFn" ~pfn "ResponseHandler" ~rh) :as :Agent])
        p1 `[:eval (agentlang.inference/run-inference-for-event ~inference-name :Agent)]]
    (cn/register-dataflow inference-name nil (if pfns [p0 pfns p1] [p0 p1]))
    inference-name))

(defn- ensure-spec-keys! [tag label expected-keys spec-keys]
  (when-let [invalid-keys (seq (set/difference (set spec-keys) expected-keys))]
    (u/throw-ex (str "invalid keys " invalid-keys " in " tag " " label)))
  spec-keys)

(defn inference [inference-name spec-map]
  (let [inference-name (cn/canonical-type-name inference-name)]
    (ensure-spec-keys! 'inference inference-name
                       #{:instructions :agent
                         :with-prompt-fn :with-response-handler}
                       (keys spec-map))
    (ensure-event! inference-name)
    (and (register-inference-dataflow inference-name spec-map)
         (cn/register-inference inference-name spec-map)
         (raw/inference inference-name spec-map)
         inference-name)))

(def ^:private crud-evname cn/crud-event-name)

(defn- crud-event-attr-accessor
  ([evtname use-name? attr-name]
   (keyword (str (if use-name? (name evtname) (subs (str evtname) 1)) "." (name attr-name))))
  ([evtname attr-name]
   (crud-event-attr-accessor evtname false attr-name)))

(defn- crud-event-subattr-accessor [evtname attr sub-attr]
  (keyword (str (name evtname) (str "." (name attr))
                (when sub-attr
                  (str "." (name sub-attr))))))

(defn- crud-event-inst-accessor
  ([evtname cname inst-attr]
   (let [r (crud-event-subattr-accessor evtname :Instance inst-attr)
         cname (let [[c n] (li/split-path evtname)]
                 (if (and c n) c cname))]
     (if cname
       (if (keyword? cname)
         (cn/canonical-type-name cname r)
         (cn/canonical-type-name r))
       r)))
  ([evtname inst-attr] (crud-event-inst-accessor evtname true inst-attr))
  ([evtname] (crud-event-inst-accessor evtname true nil)))

(defn- direct-id-accessor [evtname id-attr]
  (let [[c n] (li/split-path evtname)
        cname (and n c)]
    (cn/canonical-type-name
     cname
     (keyword (str (name evtname) "." (name id-attr))))))

(defn- identity-attribute-name [recname]
  (or (cn/identity-attribute-name recname)
      cn/id-attr))

(defn- identity-attribute-type [attr-name attrs]
  (if (= attr-name cn/id-attr)
    cn/id-attr-type
    (let [t (attr-name attrs)]
      (if (keyword? t)
        (if-let [ascm (cn/find-attribute-schema t)]
          (:type ascm)
          t)
        (:type t)))))

(defn- crud-event-delete-pattern [evtname entity-name]
  (let [id-attr (identity-attribute-name entity-name)]
    [:delete entity-name
     {id-attr (direct-id-accessor evtname id-attr)}]))

(defn- crud-event-lookup-pattern [evtname entity-name]
  (let [id-attr (identity-attribute-name entity-name)]
    {entity-name
     {(keyword (str (name id-attr) "?"))
      (direct-id-accessor evtname id-attr)}}))

(defn- implicit-entity-event-dfexp
  "Construct a dataflow expressions for an implicit dataflow
  lifted from the :on-entity-event property of an entity
  definition."
  [ename event-spec]
  `(dataflow {:head {:on-entity-event {~ename {cn/id-attr 'id}}
                     :when ~(:when event-spec)}}
             ~@(:do event-spec)))

(defn- lift-implicit-entity-events
  "Pick out the :on-entity-event definitions from the attributes
  into independent dataflows."
  [ename attrs]
  (if-let [especs (:on-entity-event attrs)]
    [(dissoc attrs :on-entity-event)
     (map (partial implicit-entity-event-dfexp ename) especs)]
    [attrs nil]))

(defn- entity-event [entity-name event-name event-type _]
  (let [attrs (if (= event-type :OnDelete)
                {:Instance entity-name}
                {:Instance entity-name
                 :OldInstance entity-name})]
    (event-internal event-name attrs)))

(defn- has-identity-attribute? [attrs]
  (some (fn [[_ v]]
          (cn/identity-attribute?
           (if (keyword? v)
             (cn/find-attribute-schema v)
             v)))
        attrs))

(defn- maybe-assoc-id [entity-name attrs]
  (if (or (cn/entity-schema-predefined? entity-name)
          (has-identity-attribute? attrs))
    attrs
    (let [attrs (assoc
                 attrs cn/id-attr
                 (cn/canonical-type-name cn/id-attr))
          meta (:meta attrs)
          req-attrs (:required-attributes meta)]
      (assoc attrs :meta
             (assoc meta :required-attributes
                    (set (conj req-attrs cn/id-attr)))))))

(defn- load-ref-pattern [evt-name evt-ref entity-name attr-name attr-schema]
  (let [[c _] (li/split-path entity-name)
        r (:ref attr-schema)]
    {(li/make-path (:component r) (:record r))
     {(keyword (str (name (first (:refs r))) "?"))
      (keyword (str (name c) "/" (name evt-name) "." (name evt-ref) "." (name attr-name)))}}))

(defn- serialize-record [f cn attrs raw-attrs]
  (f cn attrs))

(def ^:private intern-rec-fns
  {:entity (partial serialize-record cn/intern-entity)
   :relationship (partial serialize-record cn/intern-relationship)})

(defn- maybe-add-curd-meta [attrs]
  (let [meta (:meta attrs)]
    (if (some #{li/owner-exclusive-crud} (keys meta))
      attrs
      (assoc attrs :meta (assoc meta li/owner-exclusive-crud true)))))

(defn- serializable-record
  ([rectype n attrs raw-attrs]
   (if-let [intern-rec (rectype intern-rec-fns)]
     (if (map? attrs)
       (let [rec-name (validated-canonical-type-name
                       (when (cn/system-defined? attrs) identity)
                       n)
             [attrs dfexps] (lift-implicit-entity-events rec-name attrs)
             attrs (maybe-add-curd-meta (assoc attrs li/meta-attr li/meta-attr-spec))
             result (intern-rec
                     rec-name
                     (maybe-assoc-id
                      rec-name
                      (normalized-attributes
                       rectype rec-name attrs))
                     raw-attrs)]
         (if (:view-query (:meta raw-attrs))
           (do
             (when-let [rbac-spec (:rbac attrs)]
               (lr/rbac rec-name rbac-spec))
             result)
           (let [ev (partial crud-evname rec-name)
                 ctx-aname (k/event-context-attribute-name)
                 id-attr (identity-attribute-name rec-name)
                 id-attr-type (or (identity-attribute-type id-attr attrs)
                                  :Agentlang.Kernel.Lang/Any)
                 id-evattrs {id-attr id-attr-type
                             li/event-context ctx-aname}
                 cr-evattrs {:Instance n li/event-context ctx-aname}
                 up-id-attr id-attr
                 up-evattrs {up-id-attr id-attr-type
                             :Data :Agentlang.Kernel.Lang/Map}
                 ;; Define CRUD events and dataflows:
                 crevt (ev :Create)
                 upevt (ev :Update)
                 delevt (ev :Delete)
                 lookupevt (ev :Lookup)
                 lookupevt-internal (ev cn/lookup-internal-event-prefix)
                 lookupallevt (ev :LookupAll)]
             (cn/for-each-entity-event-name
              rec-name (partial entity-event rec-name))
             (event-internal delevt id-evattrs)
             (cn/register-dataflow delevt [(crud-event-delete-pattern delevt rec-name)])
             (event-internal crevt cr-evattrs)
             (event-internal upevt up-evattrs)
             (event-internal lookupevt-internal id-evattrs)
             (event-internal lookupevt id-evattrs)
             (event-internal lookupallevt {})
             (let [rs (mapv (fn [[k v]]
                              (let [s (cn/find-attribute-schema v)]
                                [(load-ref-pattern crevt :Instance rec-name k s)
                                 (load-ref-pattern upevt :Data rec-name k s)]))
                            (cn/ref-attribute-schemas (cn/fetch-schema rec-name)))
                   cr-ref-pats (mapv first rs)
                   up-ref-pats (mapv second rs)]
               (cn/register-dataflow
                crevt
                `[~@cr-ref-pats
                  ~{rec-name
                    {}
                    :from
                    (crud-event-inst-accessor crevt)}])
               (cn/register-dataflow
                upevt
                (concat [up-ref-pats]
                        [{rec-name
                          {(li/name-as-query-pattern id-attr) (crud-event-attr-accessor upevt (name up-id-attr))}
                          :from (crud-event-attr-accessor upevt "Data")}])))
             (cn/register-dataflow lookupevt-internal [(crud-event-lookup-pattern lookupevt-internal rec-name)])
             (cn/register-dataflow lookupevt [(crud-event-lookup-pattern lookupevt rec-name)])
             (cn/register-dataflow lookupallevt [(li/name-as-query-pattern rec-name)])
             ;; Install dataflows for implicit events.
             (when dfexps (mapv eval dfexps))
             (let [rbac-spec (:rbac attrs)
                   is-rel (:relationship (:meta attrs))]
               (lr/rbac rec-name rbac-spec))
             result)))
       (u/throw-ex (str "Syntax error. Check " (name rectype) ": " n)))
     (u/throw-ex (str "Not a serializable record type: " (name rectype)))))
  ([rectype n attrs]
   (serializable-record rectype n attrs attrs)))

(def serializable-entity (partial serializable-record :entity))

(defn- assoc-parent-spec [attrs]
  (assoc attrs li/parent-attr li/parent-attr-spec li/path-attr pi/path-attr-spec))

(def ^:private audit-entity-attrs
  (preproc-attrs {:InstanceId :String
                  :Action {:oneof ["create" "update" "delete"]}
                  :Timestamp :Int
                  :User :String
                  :SessionToken {:type :String :optional true}}))

(defn- audit-entity [entity-name spec]
  (if (get-in spec [:meta :audit])
    (let [n (cn/audit-trail-entity-name entity-name)]
      (serializable-entity n audit-entity-attrs))
    entity-name))

(defn entity
  "A record that can be persisted with a unique id."
  ([n attrs raw-attrs]
   (let [attrs (if raw-attrs (assoc-parent-spec attrs) attrs)]
     (when-let [r (serializable-entity n (preproc-attrs attrs))]
       (let [result (and (if raw-attrs (raw/entity r raw-attrs) true) r)]
         (and (audit-entity r raw-attrs) result)))))
  ([n attrs]
   (let [raw-attrs attrs
         attrs (if-not (seq attrs)
                 {li/id-attr :Agentlang.Kernel.Lang/Identity}
                 attrs)]
     (entity n attrs raw-attrs)))
  ([schema] (parse-and-define entity schema)))

(defn- validate-view-attrs! [attrs]
  (doseq [[n k] attrs]
    (let [p1 (and (keyword? n) (= 1 (count (li/split-path n))))
          k0 (s/split (name k) #"\.")
          p2 (and (keyword? k) (= 2 (count k0)))]
      (when-not (and p1 p2)
        (u/throw-ex (str "invalid view attribute-specification: " [n k]))))))

(defn- preproc-view-query [q attrs]
  (c/compile-complex-query
   (if (or (:join q) (:left-join q))
     (assoc q :with-attributes attrs)
     (let [clause (assoc (first (vals q)) :with-attributes attrs)]
       {(first (keys q)) clause}))))

(defn- as-view-attributes [attrs]
  (into
   {}
   (mapv (fn [[k _]] [k {:type :Any :optional true}]) attrs)))

(defn view [n spec]
  (if-let [q (:query spec)]
    (let [[c vn] (li/split-path n)
          attrs (dissoc spec :query :rbac :meta)
          ev (partial crud-evname n)]
      (validate-view-attrs! attrs)
      (entity
       n
       (merge
        (as-view-attributes attrs)
        {:meta (merge (:meta spec)
                      {:view-query (preproc-view-query q attrs)})
         :rbac (:rbac spec) :ui (:ui spec)}))
      (dataflow
       (ev :LookupAll)
       {(li/name-as-query-pattern n) {}}))
    (u/throw-ex (str "query is required to create view " n))))

(defn- extract-rel-meta [meta]
  (when-let [props (seq (filter (fn [[k _]]
                                  (some #{k} #{:as :one-one :one-many}))
                                meta))]
    (into {} props)))

(defn- parse-relationship-member-spec [meta spec]
  (let [elems [(first spec) (second spec)]
        relmeta (seq (rest (rest spec)))]
    (if relmeta
      [elems (merge (us/wrap-to-map relmeta)
                    (extract-rel-meta meta))]
      [elems (extract-rel-meta meta)])))

(declare relationship)

(defn- validate-rbac-owner [rbac nodes]
  (when-let [own (li/owner rbac)]
    (when-not (some #{own} nodes)
      (u/throw-ex (str "invalid rbac owner node " own)))))

(defn- user-defined-identity-attribute-name [entity-name]
  (let [ident (cn/identity-attribute-name entity-name)]
    (when (= ident cn/id-attr)
      (u/throw-ex (str "User-defined identity attribute required for " entity-name)))
    ident))

(defn- cleanup-rel-attrs [attrs]
  (dissoc attrs :meta :rbac :ui))

(defn- contains-relationship [relname attrs relmeta [parent child :as elems]]
  (when (seq (keys (cleanup-rel-attrs attrs)))
    (u/throw-ex (str "attributes not allowed for a contains relationship - " relname)))
  (let [raw-attrs attrs
        meta (:meta attrs)
        attrs (assoc attrs :meta
                     (assoc meta cn/relmeta-key
                            relmeta :relationship :contains))]
    (if-let [r (record relname raw-attrs)]
      (if (cn/register-relationship elems relname)
        (and (raw/relationship relname raw-attrs) r)
        (u/throw-ex (str "failed to register relationship - " relname)))
      (u/throw-ex (str "failed to define schema for " relname)))))

(defn- between-node-types [node1 node2]
  (let [id1 (cn/identity-attribute-name node1)
        t1 (cn/attribute-type node1 id1)]
    (if (= node1 node2)
      [t1 t1]
      [t1 (cn/attribute-type node2 (cn/identity-attribute-name node2))])))

(defn- assoc-relnode-attributes [attrs [node1 node2] relmeta]
  (let [[a1 a2 :as attr-names] (li/between-nodenames node1 node2 relmeta)
        [t1 t2] (between-node-types node1 node2)]
    (when-not (and a1 a2)
      (u/throw-ex (str "failed to resolve both node-attributes for between-relationship - " [a1 a2])))
    [attr-names (assoc attrs a1 t1 a2 t2)]))

(defn- between-unique-meta [meta relmeta [node1 node2] [n1 n2] new-attrs]
  (cond
    (:one-many relmeta)
    [(assoc meta :unique [n1 n2]) new-attrs]

    (:one-one relmeta)
    (let [t1 {:type (n1 new-attrs) :unique true},
          t2 {:type (n2 new-attrs) :unique true}]
      [meta (assoc new-attrs n1 t1 n2 t2)])

    :else [meta new-attrs]))

(defn- between-relationship [relname attrs relmeta elems]
  (let [[node-names new-attrs] (assoc-relnode-attributes (preproc-attrs attrs) elems relmeta)
        [meta new-attrs] (between-unique-meta (:meta attrs) relmeta elems node-names new-attrs)
        meta (assoc meta :relationship :between cn/relmeta-key relmeta)
        r (serializable-entity relname (assoc new-attrs :meta meta) attrs)]
    (when (cn/register-relationship elems relname)
      (and (raw/relationship relname attrs) r))))

(defn relationship
  ([relation-name attrs]
   (let [relation-name (cn/canonical-type-name relation-name)
         meta (let [m (:meta attrs)]
                (if (some #{:cascade-on-delete} (keys m))
                  m
                  (assoc m :cascade-on-delete true)))
         contains (mt/contains meta)
         between (when-not contains (mt/between meta))
         [elems relmeta] (parse-relationship-member-spec meta (or contains between))]
     (when-not elems
       (u/throw-ex
        (str "type (contains, between) of relationship is not defined in meta - "
             relation-name)))
     ((if contains contains-relationship between-relationship)
      relation-name (assoc attrs :meta meta) relmeta elems)))
  ([schema]
   (let [r (parse-and-define relationship schema)
         n (li/record-name schema)]
     (and (raw/relationship n (li/record-attributes schema)) r))))

(defn- validate-resolver-name! [rn]
  (let [[c n :as cn] (li/split-path rn)]
    (if (and (li/name? c) (li/name? n))
      rn
      (u/throw-ex (str "Invalid resolver name - " rn ", valid form is :Component/ResolverName")))))
;;
;; The resolver construct has the following syntax:
;; (resolver <name> <specification-map>)
;; The `name` must be a keyword, like :camel-sfdc-resolver
;; The `specification-map` can contains the following keys:
;;  `:require`, `:with-methods`, `:with-subscription`,
;;  `:paths`, `:type`, `:compose?`, `:config`.
;; The value of `:require` must be a map with the key-values:
;; - `:namespaces` - lists clojure namespaces that are required by the resolver.
;; - `:pre-cond` - a no-argument function that'll be executed before the resolver
;;                 is initialized. (optional)
;; `:paths` - the paths to which the resolver is attached (optional).
;; `:with-methods` - If specified must be a map that specifies the resolver methods.
;;                   A new resolver-type is created with these methods and if `:paths` is specified,
;;                   an instance of the resolver will be attached to those paths. (optional)
;; `:with-subscription` - A new subscriber is started in a new thread for the resolver's
;;                        on-change-notification method. (optional).
;; Examples:
;; A new resolver type is registered:
;;;; (resolver
;;;;  :camel-salesforce
;;;;  {:require {:pre-cond subscribe-to-change-events}
;;;;   :with-methods
;;;;   {:create sf-create
;;;;    :query sf-query
;;;;    :on-set-path sf-on-set-path}})
;; A path is registered with the resolver:
;;;; (resolver
;;;;  :local-resolver
;;;;  {:type :camel-salesforce :paths [:Salesforce/Quote]})
;;
(defn resolver [n spec]
  (validate-resolver-name! n)
  #?(:clj
     (let [req (:require spec)]
       (when-let [nss (:namespaces req)]
         (apply require nss))
       (u/set-on-init!
        (fn []
          (let [s0 (dissoc spec :require :with-methods :with-subscription)
                res-spec (assoc s0 :name n)
                maybe-subs #(when-let [subs (:with-subscription spec)]
                              (async/thread
                                (subs/listen
                                 (subs/open-connection subs))))
                rf #(do (if-let [methods (:with-methods spec)]
                          (if-let [paths (:paths spec)]
                            ((if (:compose? spec) rr/compose-resolver rr/override-resolver)
                             paths
                             (rc/make-resolver n methods))
                            (rr/register-resolver-type n (fn [_ _] (rc/make-resolver n methods))))
                          (rr/register-resolver res-spec))
                        (maybe-subs))]
            (if-let [precond (:pre-cond req)]
              (when (precond)
                (rf))
              (rf))
            n)))))
  (and (cn/register-resolver n spec)
       (raw/resolver n spec)))

(def ^:private standalone-pattern-preprocecssors (atom nil))

(defn install-standalone-pattern-preprocessor! [n f]
  (swap! standalone-pattern-preprocecssors assoc n f)
  n)

(defn- standalone-pattern-preprocecssor [n]
  (get @standalone-pattern-preprocecssors n))

(defn preprocess-standalone-pattern [pat]
  (if (map? pat)
    (let [n (li/record-name pat)]
      (if-let [f (standalone-pattern-preprocecssor n)]
        (f pat)
        pat))
    pat))

(defn standalone-pattern-error [error pat]
  (log/warn (u/pretty-str (assoc error :pattern (second pat)))))

(defn- cleanup-standalone-pattern [pat]
  (if (map? pat)
    (first (keys (dissoc pat :as :async :throws)))
    pat))

(defn- create-pattern? [attrs]
  (every? (fn [[k v]] (not (li/query-pattern? k))) attrs))

(defn- maybe-update-old-instance [pat]
  (when (map? pat)
    (when-let [entity-name (when-let [n (li/record-name pat)]
                             (when (cn/entity? n) n))]
      (let [attrs (li/record-attributes pat)
            id-attr-name (cn/identity-attribute-name entity-name)]
        (when (create-pattern? attrs)
          (when-let [id-attr (id-attr-name attrs)]
            (let [raw-pat {entity-name (assoc attrs (li/name-as-query-pattern id-attr-name) id-attr)}]
              `[:try
                ~raw-pat
                :error [:eval (~'quote (~'agentlang.lang/standalone-pattern-error
                                        :Error [:q# ~raw-pat]))]])))))))

(defn pattern [raw-pat]
  (let [pat (preprocess-standalone-pattern raw-pat)
        update-pat (maybe-update-old-instance pat)
        final-pat [:try pat :error
                   (or update-pat
                       [:eval `(~'quote (~'agentlang.lang/standalone-pattern-error
                                         :Error [:q# ~(cleanup-standalone-pattern raw-pat)]))])]]
    (gs/install-init-pattern! final-pat)
    (raw/pattern raw-pat)))
