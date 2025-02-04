(ns agentlang.env
  "The environment of instance and variable bindings,
  used for pattern resolution."
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]))

(def ^:private env-tag :-*-env-*-)
(def ^:private store-tag :-*-store-*-)
(def ^:private resolver-tag :-*-resolver-*-)
(def ^:private dirty-tag :-*-dirty-*-)
(def ^:private objstack-tag :-*-objstack-*-)

(def EMPTY {env-tag true})

(defn make
  [store resolver]
  (assoc EMPTY
         store-tag store
         resolver-tag resolver))

(defn maybe-enrich [env store resolver]
  (let [env0 (if-not (store-tag env) (assoc env store-tag store) env)]
    (if-not (resolver-tag env0) (assoc env0 resolver-tag resolver) env0)))

(defn env? [x]
  (and (map? x)
       (env-tag x)))

(def get-store store-tag)
(def get-resolver resolver-tag)

;; !!NOTE!! This assertion may be removed once the
;; pattern-match algorithm is fully implemented.
;; This check ensures that the compiler never pushes
;; unparsed names of the form :Module/Entity to the runtime
;; environment.
(defn- assert-parsed-rec-name [rec-name]
  (if-not (vector? rec-name)
    (u/throw-ex (str "not a parsed record name - " rec-name))
    rec-name))

(defn get-instances
  "Fetch the instances of the specified record type.
   Ensures that only parsed record-names are passed to the
   runtime environment."
  [env rec-name]
  (get env (assert-parsed-rec-name rec-name)))

(defn bind-instance
  ([env rec-name instance]
   (if (and rec-name instance)
     (let [rec-name (li/split-path rec-name)
           insts (or (get-instances env rec-name) (list))]
       (assoc env rec-name (conj insts instance)))
     (u/throw-ex (str "may not be a valid instance - " instance ", record-name is - " rec-name))))
  ([env instance]
   (bind-instance
    env (li/split-path (cn/instance-type instance))
    instance)))

(defn bind-instances
  ([env rec-name instances]
   (let [env (assoc env rec-name (list))]
     (su/move-all instances env #(bind-instance %1 rec-name %2))))
  ([env instances]
   (if (and (seq instances) (cn/an-instance? (first instances)))
     (let [[c n :as rec-name] (li/split-path (cn/instance-type (first instances)))]
       (if (and c n)
         (bind-instances env rec-name instances)
         (u/throw-ex (str "failed to fetch record-name from " (first instances)))))
     env)))

(defn bind-instance-to-alias [env alias result]
  (if (vector? alias)
    (if (some vector? alias)
      (reduce (fn [env a] (bind-instance-to-alias env a result)) env alias)
      (let [alias-with-indexes (zipmap alias (range))]
        (reduce
         (fn [env [alias-name idx]]
           (cond
             (#{:_ :&} alias-name) env
             (= :& (get alias (dec idx))) (assoc env (get alias idx) (subvec result (dec idx)))
             :else (assoc env alias-name (nth result idx nil))))
         env alias-with-indexes)))
    (assoc env alias result)))

(def bind-to-alias assoc)
(def lookup-by-alias (comp cn/maybe-deref get))

(defn lookup-instance [env rec-name]
  (cn/maybe-deref (peek (get-instances env rec-name))))

(defn maybe-lookup-instance [env path]
  (if (vector? path) ; parsed-path
    (lookup-instance env path)
    (lookup-by-alias env path)))

(defn purge-instance [env rec-name id-attr-name id]
  (let [insts (filter #(not= (id-attr-name %) id) (get-instances env rec-name))]
    (assoc env rec-name insts)))

(defn- find-instance-by-path-parts [env path-parts has-refs]
  (if-let [p (:path path-parts)] ; maybe an alias
    (let [x (get env p)]
      (if has-refs
        (if (map? x) x (first x))
        x))
    (if (= :% (:record path-parts))
      (lookup-by-alias env :%)
      (let [recname [(:component path-parts) (:record path-parts)]]
        (lookup-instance env recname)))))

(def bind-variable assoc)
(def lookup-variable find)

(defn- fetch-attr-ref-val [obj r]
  (if-let [v (find obj r)]
    (second v)
    (when (= r cn/id-attr)
      (when-let [t (cn/instance-type obj)]
        ((cn/identity-attribute-name t) obj)))))

(defn follow-reference [env path-parts]
  (let [refs (:refs path-parts)]
    (if (symbol? refs)
      [(second (lookup-variable env refs)) env]
      (loop [env env, refs refs
             obj (find-instance-by-path-parts env path-parts (seq refs))]
        (if-let [r (first refs)]
          (let [x (fetch-attr-ref-val obj r)]
            (recur (if (cn/an-instance? x)
                     (bind-instance env (cn/parsed-instance-type x) x)
                     env)
                   (rest refs) x))
          [obj env])))))

(defn lookup [env path]
  (let [parts (li/path-parts path)
        p (:path parts)]
    (cond
      p (lookup-by-alias env p)

      (seq (:refs parts))
      (first (follow-reference env parts))

      :else (lookup-instance env [(:component parts) (:record parts)]))))

(defn instance-ref-path
  "Returns a path to the record in the format of [record-name inst-id]
   along with values of any refs"
  [env record-name alias refs]
  (let [inst (if alias
               (lookup-by-alias env alias)
               (lookup-instance env record-name))
        inst-id (cn/id-attr inst)
        path (when inst-id [record-name inst-id])]
    (if path
      (if (seq refs)
        [path (get-in (cn/instance-attributes inst) refs)]
        [path inst])
      [nil (when (map? inst) (get-in inst refs))])))

(defn lookup-instances-by-attributes
  ([env rec-name query-attrs as-str]
   (when-let [insts (seq (get-instances env rec-name))]
     (filter #(every?
               (fn [[k v]]
                 (let [a0 (get % k)
                       a (if as-str (str a0) a0)]
                   (= v a)))
               query-attrs)
             insts)))
  ([env rec-name query-attrs]
   (lookup-instances-by-attributes env rec-name query-attrs false)))

(defn- objstack [env]
  (get env objstack-tag (list)))

(defn push-obj
  "Push a single object or a sequence of objects to the stack"
  ([env rec-name x]
   (let [stack (objstack env)]
     (assoc env objstack-tag (conj stack [rec-name x]))))
  ([env rec-name] (push-obj env rec-name {})))

(defn peek-obj [env]
  (peek (objstack env)))

(defn pop-obj
  "Pop the object stack,
  return [updated-env single-object-flag? [name object]]"
  [env]
  (when-let [s (seq (objstack env))]
    (let [[_ obj :as x] (peek s)]
      [(assoc env objstack-tag (pop s))
       (map? obj) x])))

(defn reset-objstack [env]
  (dissoc env objstack-tag))

(defn can-pop? [env rec-name]
  (when-let [s (seq (objstack env))]
    (when-let [[n _] (peek s)]
      (= (li/make-path n) (li/make-path rec-name)))))

(defn- identity-attribute [inst]
  (or (cn/identity-attribute-name (cn/instance-type inst))
      cn/id-attr))

(defn- dirty-flag-switch
  "Turn on or off the `dirty` flag for the given instances.
  Instances marked dirty will be later flushed to store."
  [flag env insts]
  (loop [insts insts, ds (get env dirty-tag {})]
    (if-let [inst (first insts)]
      (let [id-attr (identity-attribute inst)]
        (if-let [id (id-attr inst)]
          (recur (rest insts) (assoc ds id flag))
          (recur (rest insts) ds)))
      (assoc env dirty-tag ds))))

(def mark-all-dirty (partial dirty-flag-switch true))

(defn any-dirty?
  "Return true if any of the instances are marked dirty, otherwise
  return false."
  [env insts]
  (if (cn/entity-instance? (first insts))
    (if-let [ds (dirty-tag env)]
      (loop [insts insts]
        (if-let [inst (first insts)]
          (let [id-attr (identity-attribute inst)
                f (get ds (id-attr inst))]
            (if (or f (nil? f))
              true
              (recur (rest insts))))
          false)))
    true))

(defn mark-all-mint [env insts]
  (let [env (dirty-flag-switch false env insts)]
    (if-not (any-dirty? env insts)
      (dissoc env dirty-tag)
      env)))

(def as-map identity)

(def ^:private active-event-key :-*-active-event-*-)

(defn assoc-active-event [env event-instance]
  (assoc env active-event-key event-instance))

(def active-event active-event-key)

(def ^:private interceptors-blocked-key :-*-interceptors-blocked-*-)

(defn block-interceptors [env]
  (assoc env interceptors-blocked-key true))

(def interceptors-blocked? interceptors-blocked-key)

(def ^:private compound-patterns-blocked-key :-*-compound-patterns-blocked-*-)

(defn block-compound-patterns [env]
  (assoc env compound-patterns-blocked-key true))

(def compound-patterns-blocked? compound-patterns-blocked-key)

(def with-types :with-types)

(defn bind-with-types [env types]
  (assoc env with-types types))

(defn bind-queried-ids [env entity-name ids]
  (let [k [:queried-ids (li/split-path entity-name)]
        old-ids (get-in env k #{})]
    (assoc-in env k (set (concat old-ids ids)))))

(defn queried-id? [env entity-name id]
  (when-let [ids (get-in env [:queried-ids (li/split-path entity-name)])]
    (some #{id} ids)))

(def active-error-result :-*-active-error-result-*-)

(defn bind-active-error-result [env r]
  (assoc env active-error-result r :Error r))

(def relationship-context :-*-rel-context-*-)

(defn merge-relationship-context [env ctx]
  (assoc env relationship-context ctx))

(def load-between-refs :load-between-refs)

(defn assoc-load-between-refs [env f]
  (assoc env load-between-refs f))

(def post-event-trigger-sources :-*-post-event-trigger-sources-*-)

(defn- remove-trigger-source [trigger-sources predic]
  (loop [tags [:create :update :delete], trigger-sources trigger-sources]
    (if-let [tag (first tags)]
      (if-let [srcs (tag trigger-sources)]
        (if (first (filter predic srcs))
          (assoc trigger-sources tag (remove predic srcs))
          (recur (rest tags) trigger-sources))
        (recur (rest tags) trigger-sources))
      trigger-sources)))

(defn- add-post-event-trigger-source [tag env inst]
  (let [trigger-sources (get env post-event-trigger-sources)
        srcs (get trigger-sources tag [])
        new-trigger-sources (remove-trigger-source trigger-sources (partial cn/instance-eq? inst))]
    (assoc env post-event-trigger-sources
           (assoc new-trigger-sources tag (conj srcs inst)))))

(defn merge-post-event-trigger-sources [src-env target-env]
  (let [src-trigs (post-event-trigger-sources src-env)]
    (loop [tags [:create :update :delete], result-env target-env]
      (if-let [tag (first tags)]
        (if-let [src-insts (seq (tag src-trigs))]
          (recur (rest tags) (reduce (fn [env inst]
                                       (add-post-event-trigger-source
                                        tag env inst))
                                     result-env src-insts))
          (recur (rest tags) result-env))
        result-env))))

(def create-post-event (partial add-post-event-trigger-source :create))
(def update-post-event (partial add-post-event-trigger-source :update))
(def delete-post-event (partial add-post-event-trigger-source :delete))

(defn disable-post-event-triggers [env]
  (dissoc env post-event-trigger-sources))

(def pattern-evaluator :-*-pattern-evaluator-*-)

(defn assoc-pattern-evaluator [env f]
  (assoc env pattern-evaluator f))

(def rule-futures :*-*-rule-futures-*-)

(defn assoc-rule-futures [env fs]
  (let [rfs (rule-futures env)]
    (assoc env rule-futures (concat rfs fs))))

(defn cleanup
  ([env unmake-insts?]
   (let [env (dissoc env dirty-tag store-tag resolver-tag objstack-tag)
         df-vals (filter (fn [[k _]]
                           (if (keyword? k)
                             (not (s/starts-with? (name k) "-*-"))
                             true))
                         env)
         norm-vals (mapv (fn [[k v]]
                           [(if (vector? k)
                              (li/make-path k)
                              k)
                            (cond
                              (map? v) (if unmake-insts? (cn/unmake-instance v) v)
                              (string? v) v
                              (seqable? v) (if unmake-insts? (mapv cn/unmake-instance v) v)
                              :else v)])
                         df-vals)]
     (into {} norm-vals)))
  ([env] (cleanup env true)))
