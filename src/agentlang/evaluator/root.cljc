(ns agentlang.evaluator.root
  "The default evaluator implementation"
  (:require [clojure.walk :as w]
            [clojure.set :as set]
            [clojure.string :as s]
            [agentlang.env :as env]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.hash :as h]
            [agentlang.util.seq :as su]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.store :as store]
            [agentlang.store.util :as stu]
            [agentlang.resolver.core :as r]
            [agentlang.resolver.registry :as rg]
            [agentlang.paths :as paths]
            [agentlang.paths.internal :as pi]
            [agentlang.evaluator.async :as a]
            [agentlang.evaluator.match :as m]
            [agentlang.evaluator.internal :as i]
            [agentlang.evaluator.intercept.core :as interceptors]
            [agentlang.global-state :as gs]
            [agentlang.compiler :as cl]
            [agentlang.compiler.context :as ctx]
            [agentlang.lang :as ln]
            [agentlang.lang.syntax :as ls]
            [agentlang.lang.opcode :as opc]
            [agentlang.lang.internal :as li]
            #?(:clj [clojure.core.async :as async :refer [go <! >! go-loop]])
            #?(:cljs [cljs.core.async :as async :refer [<! >!]]))
  #?(:cljs (:require-macros [cljs.core.async.macros :refer [go go-loop]])))

(defn- make-query-for-ref [env attr-schema ref-val]
  (let [r (:ref attr-schema)
        rec-name [(:component r) (:record r)]]
    [{:from rec-name
      :where [:= (first (:refs r)) ref-val]}
     [rec-name ref-val]]))

(def ^:private create-intercept interceptors/create-intercept)
(def ^:private update-intercept interceptors/update-intercept)
(def ^:private delete-intercept interceptors/delete-intercept)
(def ^:private read-intercept interceptors/read-intercept)

(defn- enrich-environment-with-refs
  "Find all entity instances referenced (via :ref attribute property)
  from this instance. Load them into the local environment so that
  compound attributes of this instance do not see broken reference values."
  [env record-name inst]
  (let [qs (mapv (fn [[k v]]
                   (make-query-for-ref
                    env (cn/find-attribute-schema v) (k inst)))
                 (cn/ref-attribute-schemas
                  (cn/fetch-schema record-name)))
        store (env/get-store env)]
    (loop [qs qs, env env]
      (if-let [q (first qs)]
        (let [c (store/compile-query store (first q))
              [rec-name ref-val] (second q)
              rs (when ref-val (store/do-query store (first c) [ref-val]))]
          (recur (rest qs) (env/bind-instances env (stu/results-as-instances rec-name rs))))
        env))))

(defn- process-eval-result [obj]
  (if (= :ok (:status obj))
    (let [r (:result obj)]
      (if (seqable? r)
        (if (or (map? r) (string? r))
          r
          (if (= 1 (count r))
            (first r)
            r))
        r))
    {:status (:status obj) :result (:result obj)}))

(defn- eval-result-wrapper [evattr eval-fn]
  (let [result (async/chan)
        timeout-ms (get evattr :timeout-ms 500)
        refresh-ms (:refresh-ms evattr)]
    (go
      (let [r (eval-fn)]
        (>! result r)
        (when refresh-ms
          (go-loop []
            (<! (async/timeout refresh-ms))
            (>! result (eval-fn))
            (recur)))))
    (fn [& args]
      (let [cell (atom nil)]
        (go
          (reset! cell (<! result)))
        (or @cell
            #?(:clj (do (Thread/sleep timeout-ms) @cell)
               :cljs (js/setTimeout #((first args) (deref cell)) timeout-ms)))))))

(defn- assoc-evaled-attributes [env obj evattrs eval-opcode]
  (loop [evs evattrs, obj obj]
    (if-let [[k v] (first evs)]
      (recur
       (rest evs)
       (assoc
        obj k
        (eval-result-wrapper
         v #(process-eval-result (eval-opcode env (:opcode v))))))
      obj)))

(defn- assoc-fn-attributes [env raw-obj fns]
  (loop [fns fns, raw-obj raw-obj]
    (if-let [[a f] (first fns)]
      (recur (rest fns) (assoc raw-obj a (f env raw-obj)))
      raw-obj)))

(defn- assoc-futures [record-name rec-version obj]
  (loop [fattrs (cn/future-attrs record-name rec-version), obj obj]
    (if-let [[k v] (first fattrs)]
      (recur (rest fattrs)
             (assoc obj k v))
      obj)))

(defn- assoc-computed-attributes [env record-name rec-version raw-obj eval-opcode]
  (let [env (enrich-environment-with-refs env record-name raw-obj)
        [efns evattrs] (cn/all-computed-attribute-fns record-name rec-version)
        f (partial assoc-fn-attributes env)
        interim-obj (if (seq evattrs)
                      (assoc-evaled-attributes
                       (env/bind-instance
                        env
                        (li/split-path record-name) raw-obj)
                       raw-obj evattrs eval-opcode)
                      raw-obj)]
    (f (assoc-futures record-name rec-version interim-obj) efns)))

(defn- set-obj-attr
  ([env attr-name attr-ref attr-value]
   (if attr-name
     (if-let [xs (env/pop-obj env)]
       (let [[env single? [n x]] xs
             objs (if single? [x] x)
             new-objs (mapv
                       #(let [attr-value (if-not (nil? attr-value)
                                           attr-value
                                           (when attr-ref
                                             (attr-ref %)))]
                          (assoc
                           % attr-name
                           (if (fn? attr-value)
                             (attr-value env %)
                             attr-value)))
                       objs)
             elem (if single? (first new-objs) new-objs)
             env (env/push-obj env n elem)]
         (i/ok elem (env/mark-all-dirty env new-objs)))
       (i/error
        (str
         "cannot set attribute value, invalid object state - "
         [attr-name attr-value])))
     (i/ok attr-value env)))
  ([env attr-name attr-value]
   (set-obj-attr env attr-name nil attr-value)))

(defn- call-function [env f]
  (if-let [xs (env/pop-obj env)]
    (let [[env single? [n x]] xs
          inst (if single? x (first x))]
      (i/ok (f env inst) env))
    (i/ok (f env nil) env)))

(defn- on-inst [f xs]
  (f (if (map? xs) xs (first xs))))

(defn- need-storage? [xs]
  (on-inst cn/entity-instance? xs))

(defn- resolver-for-instance [resolver insts]
  (let [path (on-inst cn/instance-type insts)]
    (rg/resolver-for-path resolver path)))

(def ^:private async-result-key :-*async-result*-)

(defn- merge-async-result [inst async-result]
  (assoc inst async-result-key async-result))

(defn- realize-async-result [eval-opcode env code orig-status result]
  (go
    (let [final-result
          (cond
            (map? result)
            (when-let [a (async-result-key result)]
              [(merge (dissoc result async-result-key) (<! a))])

            (seqable? result)
            (loop [xs result, r []]
              (if-let [x (first xs)]
                (recur
                 (rest xs)
                 (if-let [a (async-result-key x)]
                   (let [ar (<! a)]
                     (conj r (if (i/ok? (first ar))
                               (merge (dissoc x async-result-key) (:result ar))
                               (dissoc x async-result-key))))
                   (conj r x)))))
            :else (<! result))

          updated-env (env/bind-instances env final-result)]
      (let [r (eval-opcode updated-env code)
            s (:status r)]
        (if (= :ok orig-status r)
          (@gs/fire-post-events (:env r))
          (when (= :ok s)
            (@gs/fire-post-events (:env r))))
        r))))

(defn- process-resolver-upsert [resolver method env inst]
  (if-let [result (:result (method resolver env inst))]
    (if (map? result)
      (merge inst result)
      result)
    inst))

(defn- call-resolver-upsert [f env resolver composed? data]
  (let [rs (if composed? resolver [resolver])
        insts (if (map? data) [data] data)]
    (reduce
     (fn [arg r]
       (mapv (partial process-resolver-upsert r f env) arg))
     insts rs)))

(def ^:private call-resolver-create call-resolver-upsert)
(def ^:private call-resolver-update call-resolver-upsert)

(defn- call-resolver-delete [f env resolver composed? inst]
  (let [rs (if composed? resolver [resolver])]
    (reduce
     (fn [arg r]
       (:result (f r env arg)))
     inst rs)))

(defn- async-invoke [timeout-ms f]
  (cn/make-future (a/async-invoke f) timeout-ms))

(def ^:private resolver-update (partial call-resolver-update r/call-resolver-update))
(def ^:private resolver-create (partial call-resolver-create r/call-resolver-create))
(def ^:private resolver-delete (partial call-resolver-delete r/call-resolver-delete))

(defn- call-resolver-eval [resolver composed? env inst]
  (let [rs (if composed? resolver [resolver])]
    (mapv #(r/call-resolver-eval % env inst) rs)))

(declare find-instances)

(defn- load-instances-for-conditional-event [env store where-clause
                                             records-to-load loaded-instances]
  (loop [wcs where-clause, rs records-to-load, env env, ls loaded-instances]
    (if-let [wc (first wcs)]
      (let [p (cn/parse-where-clause wc ls)
            [result env] (find-instances env store (:from p) p)]
        (recur (rest wcs) rs env (conj ls (first result))))
      [ls env])))

(defn- fire-conditional-event [event-evaluator env store event-info instance]
  (let [[_ event-name [where-clause records-to-load]] event-info
        env (env/bind-instance env instance)
        [all-insts env] (load-instances-for-conditional-event
                         env store where-clause
                         records-to-load #{instance})]
    (when (cn/fire-event? event-info all-insts)
      (let [[_ n] (li/split-path (cn/instance-type instance))
            upserted-n (li/upserted-instance-attribute n)
            evt (cn/make-instance
                 event-name
                 {n instance
                  upserted-n instance})]
        (event-evaluator env evt)))))

(defn- fire-all-conditional-events [event-evaluator env store insts]
  (let [f (partial fire-conditional-event event-evaluator env store)]
    (filter
     identity
     (mapv #(seq (mapv (fn [e] (f e %)) (cn/conditional-events %)))
           insts))))

(defn- chained-crud [store-f res resolver-f single-arg-path insts]
  (let [insts (if (or single-arg-path (not (map? insts))) insts [insts])
        resolver (if single-arg-path
                   (rg/resolver-for-path res single-arg-path)
                   (resolver-for-instance res insts))
        composed? (rg/composed? resolver)
        crud? (or (not resolver) composed?)
        resolved-insts (or
                        (when resolver
                          (let [r (resolver-f resolver composed? insts)]
                            (if (map? r)
                              (when (seq r) r)
                              (seq (filter identity r)))))
                        insts)]
    (if (and crud? store-f
             (or single-arg-path (need-storage? resolved-insts)))
      (store-f resolved-insts)
      resolved-insts)))

(defn- cleanup-conditional-results [res]
  (w/prewalk
   #(if (and (map? %) (:status %))
      (dissoc % :env)
      %)
   res))

(defn- format-upsert-result-for-read-intercept [result]
  (filter cn/an-instance? result))

(defn- revert-upsert-result [orig-result insts]
  (loop [rslt orig-result, insts insts, result []]
    (if-let [r (first rslt)]
      (if (map? r)
        (recur (rest rslt)
               (rest insts)
               (conj result (first insts)))
        (recur (rest rslt)
               insts (conj result r)))
      result)))

(defn- intercept-upsert-result [env result]
  (let [r0 (read-intercept
            env interceptors/skip-for-input
            (fn [_] (format-upsert-result-for-read-intercept result)))
        r (seq (revert-upsert-result result r0))
        f (first r)]
    (if (cn/an-instance? f)
      r
      (first r))))

(defn- active-user [env]
  (or (cn/event-context-user (env/active-event env))
      (gs/active-user)))

(defn- maybe-add-owner [env inst]
  (if (cn/owners inst)
    inst
    (if-let [owner (active-user env)]
      (cn/concat-owners inst [owner])
      inst)))

(defn- upsert-post-event-sources [env record-name insts]
  (let [id-attr (cn/identity-attribute-name record-name)
        insts (if (map? insts) [insts] insts)]
    (reduce (fn [env inst]
              ((if (env/queried-id? env record-name (id-attr inst))
                 env/update-post-event
                 env/create-post-event)
               env inst))
            env insts)))

(defn- fire-pre-crud-event [event-evaluator env tag inst]
  (when-let [[event-name r] (cn/fire-pre-event
                             (partial event-evaluator (env/disable-post-event-triggers env))
                             tag inst)]
    (if-let [result (u/safe-ok-result r)]
      result
      (let [msg (if (map? r) (:message r) (:message (first r)))]
        (u/throw-ex (str "event " event-name " failed: " msg))))))

(defn- chained-upsert [env event-evaluator record-name insts]
  (let [store (env/get-store env)
        resolver (env/get-resolver env)]
    (let [is-single (map? insts)
          result
          (if (or is-single (env/any-dirty? env insts))
            (let [insts (mapv (partial maybe-add-owner env) (if is-single [insts] insts))
                  id-attr (cn/identity-attribute-name record-name)]
              (apply
               concat
               (mapv
                (fn [inst]
                  (let [is-queried (env/queried-id? env record-name (id-attr inst))]
                    ((if is-queried update-intercept create-intercept)
                     env inst
                     (fn [inst]
                       (let [store-f (if is-queried store/update-instances store/create-instances)
                             result (fire-pre-crud-event
                                     event-evaluator env
                                     (if is-queried :update :create)
                                     inst)
                             inst (if (and (cn/an-instance? result)
                                           (cn/instance-eq? inst result))
                                    result
                                    inst)
                             resolver-f (if is-queried resolver-update resolver-create)
                             result
                             (chained-crud
                              (when store (partial store-f store record-name))
                              resolver (partial resolver-f env) nil [inst])
                             conditional-event-results
                             (seq
                              (fire-all-conditional-events
                               event-evaluator env store result))]
                         (concat
                          result
                          (when conditional-event-results
                            (cleanup-conditional-results conditional-event-results))))))))
                insts)))
            insts)
          env (upsert-post-event-sources env record-name insts)]
      [env (intercept-upsert-result env result)])))

(defn- delete-by-id [store record-name inst]
  (let [id-attr (cn/identity-attribute-name record-name)]
    [record-name (store/delete-by-id store record-name id-attr (id-attr inst))]))

(declare load-between-refs)

(defn- chained-delete
  ([env record-name instance with-intercept]
   (let [store (env/get-store env)
         resolver (env/get-resolver env)]
     (if with-intercept
       (delete-intercept
        (if (cn/between-relationship? record-name)
          (env/assoc-load-between-refs env (partial load-between-refs env))
          env)
        [record-name instance]
        (fn [[record-name instance]]
          (chained-crud
           (when store (partial delete-by-id store record-name))
           resolver (partial resolver-delete env) record-name instance)))
       (chained-crud
        (when store (partial delete-by-id store record-name))
        resolver (partial resolver-delete env) record-name instance))))
  ([env record-name instance]
   (chained-delete env record-name instance true)))

(defn- delete-all-children [env store record-name purge]
  (let [res (env/get-resolver env)
        delf (fn [child-name purge]
               (let [resolver (rg/resolver-for-path res child-name)
                     is-composed (rg/composed? resolver)
                     is-crud (or (not resolver) is-composed)
                     resolved-result (or
                                      (when resolver
                                        (resolver-delete env resolver is-composed :*))
                                      child-name)]
                 (when is-crud
                   (store/delete-all store child-name purge))))]
    (cn/maybe-delete-all-children delf record-name purge)))

(defn- delete-children [env store record-name inst]
  (let [res (env/get-resolver env)
        delf (fn [child-name path-prefix]
               (let [resolver (rg/resolver-for-path res child-name)
                     is-composed (rg/composed? resolver)
                     is-crud (or (not resolver) is-composed)
                     inst {cn/instance-type child-name
                           li/path-attr path-prefix}
                     resolved-result (or
                                      (when resolver
                                        (resolver-delete env resolver is-composed inst))
                                      inst)]
                 (when is-crud
                   (store/delete-children store child-name path-prefix))))]
    (cn/maybe-delete-children delf record-name inst)))

(defn- purge-all [env instances]
  (loop [env env, insts instances]
    (if-let [inst (first insts)]
      (let [t (cn/instance-type inst)
            id-attr (cn/identity-attribute-name t)]
        (recur (env/purge-instance env t id-attr (id-attr inst))
               (rest insts)))
      env)))

(defn- bind-and-persist [env x]
  (if (cn/an-instance? x)
    (let [n (li/split-path (cn/instance-type x))]
      [(env/bind-instance env n x) nil])
    [env nil]))

(defn- id-attribute [query-attrs]
  (first (filter #(= cn/id-attr (first %)) query-attrs)))

(defn- result-with-env? [x]
  (and (vector? x)
       (= (count x) 2)
       (env/env? (second x))))

(defn- evaluate-id-result [env rs]
  (loop [rs (if (or (not (seqable? rs)) (string? rs))
              [rs]
              rs)
         env env, values []]
    (if-let [r (first rs)]
      (cond
        (fn? r)
        (let [x (r env nil)
              [v new-env]
              (if (result-with-env? x)
                x
                [x env])]
          (recur
           (rest rs)
           new-env
           (conj values v)))

        (vector? r)
        (let [[v new-env] (evaluate-id-result env r)]
          (recur (rest rs) new-env (conj values v)))

        :else
        (recur (rest rs) env (conj values r)))
      [values env])))

(defn- evaluate-id-query [env store query params running-result merge-operator]
  (let [[p env] (evaluate-id-result env params)
        rs (store/do-query store query p)
        all-ids (mapv su/first-val rs)]
    (if (and (seq running-result) (= :and merge-operator))
      [(set/intersection (set running-result) (set all-ids)) env]
      [(concat running-result all-ids) env])))

(defn- evaluate-id-queries
  "Evaluate unique IDs from queries into index tables. Each entry in the sequence id-queries will
   be a map with two possible keys - :result and :query. If there is a :result, that will be
   bound to an ID statically evaluated by the compiler. Otherwise, execute the query and find the ID.
   Return the final sequence of IDs. Merge operator is either :and or :or. This is used to build intersections
   or unions of ids."
  [env store id-queries merge-operator]
  (loop [idqs id-queries, env env, result []]
    (if-let [idq (first idqs)]
      (if-let [r (:result idq)]
        (let [[obj env] (evaluate-id-result env [r])]
          (recur (rest idqs) env (conj result (first obj))))
        (let [query (:query idq)
              [q p] [(first query) (seq (rest query))]
              [rs env] (evaluate-id-query env store q p result merge-operator)]
          (recur (rest idqs) env rs)))
      [result env])))

(defn- normalize-raw-query [env q]
  (let [[wc env] (let [where-clause (:where q)]
                   (if (seqable? where-clause)
                     (evaluate-id-result env where-clause)
                     [where-clause env]))]
    [(assoc q :where wc) env]))

(defn- find-instances-via-composed-resolvers [env entity-name query resolvers]
  (loop [rs resolvers]
    (if-let [r (first rs)]
      (let [result (r/call-resolver-query r env [entity-name query])]
        (if (:result result)
          [result env]
          (recur (rest rs))))
      [nil env])))

(defn- find-instances-via-resolvers [env entity-name full-query]
  (if-let [resolver (rg/resolver-for-path entity-name)]
    (let [[q env] (normalize-raw-query env (stu/raw-query full-query))]
      (if (rg/composed? resolver)
        (find-instances-via-composed-resolvers env entity-name q resolver)
        [(r/call-resolver-query resolver env [entity-name q]) env]))
    [nil env]))

(defn- filter-query-result [rule env result]
  (when (seq result)
    (let [predic #(rule
                   (fn [x]
                     (let [r (% x)]
                       (if (nil? r)
                         (env/lookup-instance (env/bind-instance env %) x)
                         r))))]
      (filter predic result))))

(defn- query-all [env store entity-name entity-version query is-aggregate-query]
  (cond
    (vector? query)
    (let [[params env] (evaluate-id-result env (rest query))
          results (store/do-query store (first query) params)]
      [(if is-aggregate-query
         (stu/normalize-aggregates results)
         (stu/results-as-instances entity-name entity-version nil results))
       env])

    (string? query)
    [(store/query-all store entity-name query) env]

    (map? query)
    (let [f #(first (evaluate-id-result env %))]
      [(store/query-all store entity-name (assoc query :parse-params f)) env])

    :else
    (u/throw-ex (str "invalid query object - " query))))

(defn- find-instances-in-store [env store entity-name full-query]
  (let [q (or (stu/compiled-query full-query)
              (store/compile-query store full-query))]
    (query-all env store entity-name (:version (stu/raw-query full-query)) q (stu/aggregate-query? (stu/raw-query full-query)))))

(defn- maybe-async-channel? [x]
  (and x (not (seqable? x))))

(defn find-instances [env store entity-name full-query]
  (let [[r env] (find-instances-via-resolvers env entity-name full-query)
        x (:result r)
        ch? (maybe-async-channel? x)
        resolver-result (if ch? x (seq x))
        [result env] (if resolver-result
                       [resolver-result env]
                       (find-instances-in-store env store entity-name full-query))]
    (if ch?
      [result env]
      [result (env/bind-instances env entity-name result)])))

(defn- require-validation? [n]
  (if (or (cn/find-entity-schema n)
          (cn/find-event-schema n)
          (cn/find-record-schema n))
    true
    false))

(defn- normalize-partial-path [record-name obj]
  (if-let [p (li/path-attr obj)]
    (if (cn/entity-instance? p)
      (assoc obj li/path-attr (cn/instance-to-partial-path record-name p))
      obj)
    obj))

(defn- validated-instance [record-name rec-version obj]
  (if (cn/an-instance? obj)
    (if-not (cn/entity-instance? obj)
      (cn/validate-instance obj)
      obj)
    (cn/make-instance
     record-name rec-version (normalize-partial-path record-name obj)
     (require-validation? (li/make-path record-name)))))

(defn- pop-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  ([env record-name rec-version eval-opcode validation-required]
   (if (env/can-pop? env record-name)
     (if-let [xs (env/pop-obj env)]
       (let [[env single? [_ x]] xs]
         (if (maybe-async-channel? x)
           [x single? env]
           (let [objs (if single? [x] x)
                 final-objs (mapv #(assoc-computed-attributes env record-name rec-version % eval-opcode) objs)
                 insts (if validation-required
                         (mapv (partial validated-instance record-name rec-version) final-objs)
                         final-objs)
                 bindable (if single? (first insts) insts)]
             [bindable single? env])))
       [nil false env])
     [nil false (env/reset-objstack env)]))
  ([env record-name rec-version eval-opcode]
   (pop-instance env record-name rec-version eval-opcode true)))

(defn- pop-and-intern-instance
  "An instance is built in stages, the partial object is stored in a stack.
   Once an instance is realized, pop it from the stack and bind it to the environment."
  [env record-name alias eval-opcode]
  (let [[bindable single? new-env] (pop-instance env record-name nil eval-opcode)]
    (if bindable
      (let [env (env/bind-instances env record-name (if single? [bindable] bindable))]
        [bindable (if alias (env/bind-instance-to-alias env alias bindable) env)])
      [nil new-env])))

(defn- ok-result [r]
  (when (i/ok? r)
    (:result r)))

(defn- extract-embedded-resolver-result [r]
  (cond
    (map? r)
    (if (cn/an-instance? r) r (or (:result r) r))
    (vector? r) (extract-embedded-resolver-result (first r))
    :else r))

(defn- extract-resolver-result [resolver-results]
  (let [rr (if (map? resolver-results)
             resolver-results
             (first resolver-results))
        res0 (:result rr)]
    (extract-embedded-resolver-result res0)))

(defn- extract-local-result [r]
  (when (i/ok? r)
    (let [res (:result r)]
      (if (and (vector? res) (vector? (first res)))
        (first res)
        res))))

(defn- extract-local-result-as-vec [r]
  (when-let [rs (extract-local-result r)]
    (if (map? rs)
      [rs]
      rs)))

(defn- bind-result-to-alias [result-alias result]
  (if result-alias
    (let [env (:env result)
          r (if (false? (:result result))
              result
              (:result result))
          new-env (env/bind-instance-to-alias env result-alias r)]
      (assoc result :env new-env))
    result))

(defn- eval-opcode-list [evaluator env eval-opcode opcode-list]
  (loop [opcode-list opcode-list, env env result nil]
    (if-let [opcode (first opcode-list)]
      (let [result (eval-opcode evaluator env opcode)]
        (if (ok-result result)
          (recur (rest opcode-list)
                 (:env result)
                 result)
          result))
      result)))

(defn- match-object-to-result? [match-obj result]
  (let [[a b] [(h/crypto-hash? match-obj) (h/crypto-hash? result)]]
    (cond
      (and a b) (= match-obj result)
      a (h/crypto-hash-eq? match-obj result)
      b (h/crypto-hash-eq? result match-obj)
      :else (= match-obj result))))

(defn- eval-cases [evaluator env eval-opcode match-obj cases-code alternative-code result-alias]
  (bind-result-to-alias
   result-alias
   (loop [cases-code cases-code, env env]
     (if-let [[condition consequent] (first cases-code)]
       (let [result (eval-opcode evaluator env condition)
             r (ok-result result)]
         (if (not (nil? r))
           (if (match-object-to-result? match-obj r)
             (eval-opcode-list evaluator (:env result) eval-opcode consequent)
             (recur (rest cases-code) (:env result)))
           result))
       (if (first alternative-code)
         (eval-opcode-list evaluator env eval-opcode alternative-code)
         (i/ok false env))))))

(defn- eval-condition [evaluator env eval-opcode conds alternative result-alias]
  (bind-result-to-alias
   result-alias
   (let [arg (partial env/maybe-lookup-instance env)]
     (loop [main-clauses conds]
       (if-let [[condition body] (first main-clauses)]
         (if (condition arg)
           (eval-opcode evaluator env body)
           (recur (rest main-clauses)))
         (if alternative
           (eval-opcode evaluator env alternative)
           (i/ok false env)))))))

(defn- bind-for-each-element [env element elem-alias]
  (cond
    elem-alias
    (env/bind-to-alias env elem-alias element)

    (cn/an-instance? element)
    (env/bind-to-alias
     (env/bind-instance env (cn/instance-type element) element)
     :% element)

    :else
    (env/bind-to-alias env :% element)))

(defn- eval-for-each-body [evaluator env eval-opcode body-code elem-alias element]
  (let [new-env (bind-for-each-element env element elem-alias)]
    (loop [body-code body-code, env new-env result nil]
      (if-let [opcode (first body-code)]
        (let [result (eval-opcode evaluator env opcode)]
          (if (ok-result result)
            (recur (rest body-code)
                   (:env result)
                   result)
            result))
        result))))

(defn- eval-for-each [evaluator env eval-opcode collection body-code elem-alias result-alias]
  (let [eval-body (partial eval-for-each-body evaluator env eval-opcode body-code elem-alias)
        results (mapv eval-body collection)] 
    (if (every? #(ok-result %) results)
      (let [eval-output (mapv #(ok-result %) results)
            result-insts (if (seq? (first eval-output))
                           (reduce concat eval-output)
                           eval-output)]
        (if result-alias
          (let [new-env (env/bind-to-alias env result-alias result-insts)]
            (i/ok result-insts new-env))
          (i/ok result-insts env)))
      (first (filter #(not (ok-result %)) results)))))

(defn- opcode-data? [x]
  (if (vector? x)
    (opcode-data? (first x))
    (and (map? x) (:opcode x))))

(defn- set-quoted-list [opcode-eval elements-opcode]
  (w/prewalk
   #(if (opcode-data? %)
      (let [result (opcode-eval %)]
        (or (ok-result result)
            (u/throw-ex result)))
      %)
   elements-opcode))

(defn- set-flat-list [opcode-eval elements-opcode]
  (loop [results (mapv opcode-eval elements-opcode), final-list []]
    (if-let [result (first results)]
      (if-let [r (ok-result result)]
        (recur (rest results) (conj final-list r))
        result)
      final-list)))

(defn- get-ex-info [ex]
  (if-let [d (ex-data ex)]
    {:cause (or (ex-cause ex) (ex-message ex))
     :data d}
    (ex-message ex)))

(defn- call-with-exception-as-error [f]
  (try
    (f)
    #?(:clj
       (catch Exception e (i/error (get-ex-info e)))
       :cljs
       (catch js/Error e (i/error e)))))

(defn- ensure-instances [xs]
  (when (and (seq xs) (cn/an-instance? (first xs)))
    xs))

(defn- filter-results-by-rels [entity-name result-insts
                               {result-filter :filter-by
                                evaluator :eval}]
  (if-not (seq result-insts)
    result-insts
    (let [ident (cn/identity-attribute-name entity-name)]
      (vec
       (reduce
        (fn [result-insts {opcodes :opcodes query-attrs :query-attrs}]
          (let [r (evaluator opcodes), rs (extract-local-result-as-vec r)]
            (if-let [rs (ensure-instances rs)]
              (let [ks (set (cn/find-between-keys (cn/instance-type-kw (first rs)) entity-name))
                    ns (if (= query-attrs ks) ks (set/difference ks query-attrs))
                    ids (set (apply concat (mapv (fn [n] (mapv n rs)) ns)))]
                (filter (fn [inst] (some #{(ident inst)} ids)) result-insts))
              (u/throw-ex (str "filter pattern evaluation failed - " rs)))))
        result-insts result-filter)))))

(defn- query-helper
  ([env entity-name queries result-filter]
   (if-let [[insts env]
            (read-intercept
             env entity-name
             (fn [entity-name]
               (let [[insts env :as r] (find-instances env (env/get-store env) entity-name queries)]
                 (if (:filter-by result-filter)
                   [(filter-results-by-rels entity-name insts result-filter) env]
                   r))))] 
     (cond
       (maybe-async-channel? insts)
       (i/ok
        insts
        (env/push-obj env entity-name insts))

       (seq insts)
       (let [version (get-in queries [:raw-query :version])
             id-attr-name (cn/identity-attribute-name entity-name version)
             ids (mapv id-attr-name insts)]
         (i/ok
          insts
          (env/mark-all-mint
           (env/push-obj
            (env/bind-queried-ids env entity-name ids)
            entity-name insts)
           insts)))

       :else
       (i/ok [] env))
     (i/error (str "query failed for " entity-name))))
  ([env entity-name queries] (query-helper env entity-name queries nil)))

(defn- find-reference [env record-name refs]
  (second (env/instance-ref-path env record-name nil refs)))

(defn- attach-full-path [record-name inst path]
  (let [v (str ((cn/path-identity-attribute-name record-name) inst))
        [c n] (li/split-path record-name)]
    (assoc inst li/path-attr (pi/as-fully-qualified-path c (str path "/" (name n) "/" v)))))

(defn- concat-owners [env inst parent-inst]
  (let [user (active-user env)
        owners (if-let [pos (cn/owners parent-inst)]
                 (set (concat pos [user]))
                 (when user #{user}))]
    (if owners
      (cn/concat-owners inst owners)
      inst)))

(defn- maybe-fix-contains-path [env rel-ctx record-name inst]
  (if-let [path (li/path-attr inst)]
    (if-not (pi/proper-path? path)
      (if-let [parent (paths/find-parent-by-path env record-name path)]
        (and (swap! rel-ctx assoc (li/make-path record-name) {:parent parent})
             (attach-full-path record-name (concat-owners env inst parent) path))
        (u/throw-ex (str "failed to find parent by path - " path)))
      (let [i (s/last-index-of path "/")
            p (str (subs path 0 i) "/%")]
        (swap! rel-ctx assoc (li/make-path record-name)
               {:parent #(paths/find-parent-by-path env record-name (pi/as-partial-path p))})
        inst))
    inst))

(defn- ensure-between-refs [env rel-ctx record-name inst]
  (let [[node1 node2] (mapv li/split-path (cn/relationship-nodes record-name))
        [a1 a2] (cn/between-attribute-names record-name node1 node2)
        lookup (partial paths/lookup-ref-inst false)
        l1 #(when % (lookup env node1 % (a1 inst)))
        l2 #(when % (lookup env node2 % (a2 inst)))
        [r1 r2] [(or (l1 (cn/identity-attribute-name node1))
                     (l1 (cn/path-identity-attribute-name node1)))
                 (or (l2 (cn/identity-attribute-name node2))
                     (l2 (cn/path-identity-attribute-name node2)))]]
    (if (and r1 r2)
      (and (swap! rel-ctx assoc (li/make-path record-name) {a1 r1 a2 r2}) inst)
      (u/throw-ex (str "failed to lookup node-references: " record-name)))))

(defn- ensure-relationship-constraints [env rel-ctx record-name inst]
  (cond
    (cn/between-relationship? record-name)
    (ensure-between-refs env rel-ctx record-name inst)

    (cn/entity? record-name)
    (maybe-fix-contains-path env rel-ctx record-name inst)

    :else inst))

(defn- load-between-refs [env inst]
  (let [rel-ctx (atom nil)
        relname (cn/instance-type-kw inst)]
    (ensure-between-refs env rel-ctx relname inst)
    (relname @rel-ctx)))

(defn- extension-attribute-to-pattern [record-name inst-alias extn-attrs attr-name attr-val]
  (if (vector? attr-val)
    (if (li/quoted? attr-val)
      (extension-attribute-to-pattern record-name inst-alias extn-attrs attr-name (second attr-val))
      (apply concat (mapv (partial extension-attribute-to-pattern record-name inst-alias extn-attrs attr-name) attr-val)))
    (let [{reltype :ext-reltype rel :ext-rel}
          (cn/extension-attribute-info (first (filter #(= attr-name (first %)) extn-attrs)))
          is-contains (cn/contains-relationship? rel)]
      (if (map? attr-val)
        (if is-contains
          [(assoc {reltype attr-val} :-> [[rel inst-alias]])]
          (if (= rel reltype)
            (let [betattrs (set (cn/between-attribute-names reltype))
                  node (first (set/difference betattrs (set (keys attr-val))))]
              [{reltype (assoc attr-val node (li/make-ref inst-alias (cn/identity-attribute-name record-name)))}])
            [(assoc {reltype attr-val} :-> [[{rel {}} inst-alias]])]))
        (if-not is-contains
          (let [ident-attr (cn/identity-attribute-name record-name)]
            [{rel {(cn/maybe-between-node-as-attribute rel record-name) (li/make-ref inst-alias ident-attr)
                   (cn/maybe-between-node-as-attribute rel reltype) attr-val}}])
          (u/throw-ex (str "cannot establish contains relationship " rel " by identity value alone: " attr-val)))))))

(defn- maybe-upsert-relationships-from-extensions [env record-name dataflow-eval insts]
  (let [[cn alias] (li/split-path record-name)
        extn-attrs (cn/find-extension-attributes record-name)
        extn-attr-names (mapv cn/extension-attribute-name extn-attrs)]
    (doseq [inst insts]
      (when (some (set extn-attr-names) (keys (su/dissoc-nils inst)))
        (let [env (env/bind-instance-to-alias env alias inst)
              pats (apply concat (su/nonils
                                  (mapv #(when-let [attr-val (get inst %)]
                                           (extension-attribute-to-pattern
                                            record-name alias extn-attrs %
                                            attr-val)) extn-attr-names)))
              event-name (li/make-path [cn (li/unq-name)])]
          (try
            (binding [ctx/dynamic-context (ctx/add-alias (ctx/make) alias)]
              (if (apply ln/dataflow event-name pats)
                (dataflow-eval (env/disable-post-event-triggers env) {event-name {}})
                (u/throw-ex (str "failed to generate dataflow for relationships upsert - " record-name))))
            (finally
              (cn/remove-event event-name))))))))

(defn- intern-instance [self env eval-opcode eval-event-dataflows
                        record-name inst-alias queries validation-required upsert-required]
  
  (let [rec-version (get-in queries [:query :raw-query :version])
        [insts single? env] (pop-instance env record-name rec-version (partial eval-opcode self) validation-required)
        scm (cn/ensure-schema record-name rec-version)
        extn-attrs (cn/find-extension-attribute-names record-name)
        orig-insts insts
        insts (if extn-attrs
                (if single?
                  (apply dissoc insts extn-attrs)
                  (mapv #(apply dissoc % extn-attrs) insts))
                insts)]
    (when validation-required
      (doseq [inst (if single? [insts] insts)]
        (when-let [attrs (cn/instance-attributes inst)]
          (cn/validate-record-attributes record-name rec-version attrs scm))))
    (cond
      (maybe-async-channel? insts)
      (i/ok insts env)

      insts
      (let [rel-ctx (atom {})
            insts (let [r (mapv
                           (partial ensure-relationship-constraints env rel-ctx record-name)
                           (if single? [insts] (vec insts)))]
                    (if single? (first r) r))
            env (env/merge-relationship-context env @rel-ctx)
            dataflow-eval (partial eval-event-dataflows self)
            [env local-result] (if upsert-required
                                 (chained-upsert env dataflow-eval record-name insts)
                                 [env (if single? (seq [insts]) insts)])]
        (when (and extn-attrs upsert-required)
          (maybe-upsert-relationships-from-extensions
           env record-name dataflow-eval
           (if single? [orig-insts] orig-insts)))
        (if-let [bindable (if single? (first local-result) local-result)]
          (let [env-with-inst (env/bind-instances env record-name local-result)
                final-env (if inst-alias
                            (env/bind-instance-to-alias env-with-inst inst-alias bindable)
                            env-with-inst)] 
            (i/ok local-result final-env))
          (i/ok local-result env)))

      :else
      (i/not-found record-name env))))

(defn- dispatch-dynamic-upsert [self env inst-compiler eval-opcode
                                inst-type id-attr-name attrs inst]
  (let [opc (inst-compiler {(if (keyword? inst-type)
                              inst-type
                              (li/make-path inst-type))
                            (merge (cn/instance-attributes inst) attrs)})
        id-val (or (id-attr-name attrs) (id-attr-name inst))]
    (when-not id-val
      (u/throw-ex
       (str
        "dynamic-types can be used only for instance updates, identity is required - "
        [inst-type id-attr-name])))
    (eval-opcode self (env/bind-queried-ids env inst-type [id-val]) opc)))

(defn- normalize-rel-target [obj]
  (if (map? obj)
    obj
    (first obj)))

(defn make-root-vm
  "Make a VM for running compiled opcode. The is given a handle each to,
     - a store implementation
     - a evaluator for dataflows attached to an event
     - an evaluator for standalone opcode, required for constructs like :match"
  [eval-event-dataflows eval-opcode eval-dataflow]
  (reify opc/VM
    (do-match-instance [_ env [pattern instance]]
      (if-let [updated-env (m/match-pattern env pattern instance)]
        (i/ok true updated-env)
        (i/ok false env)))

    (do-load-literal [_ env x]
      (i/ok x env))

    (do-load-instance [_ env [record-name alias]]
      (if-let [inst (if alias
                      (env/lookup-by-alias env alias)
                      (env/lookup-instance env record-name))]
        (i/ok inst env)
        (i/not-found record-name env)))

    (do-load-references [self env [[record-name alias] refs]]
      (if-let [[path v] (env/instance-ref-path env record-name alias refs)]
        (if-not path
          (i/ok v env)
          (if (cn/an-instance? v)
            (let [opcode-eval (partial eval-opcode self)
                  inst (assoc-computed-attributes env (cn/instance-type v) nil v opcode-eval)
                  rel-ctx (atom nil)
                  final-inst (ensure-relationship-constraints env rel-ctx (cn/instance-type inst) inst)
                  env (env/merge-relationship-context env @rel-ctx)
                  [env r]
                  (bind-and-persist env final-inst)]
              (i/ok (if r r final-inst) env))
            (if-let [store (env/get-store env)]
              (if (store/reactive? store)
                (i/ok (store/get-reference store path refs) env)
                (i/ok v env))
              (i/ok v env))))
        (i/not-found record-name env)))

    (do-new-instance [_ env record-name]
      (let [env (env/push-obj env record-name)]
        (i/ok record-name env)))

    (do-query-instances [self env [entity-name queries result-filter]]
      (query-helper
       env entity-name queries
       {:filter-by result-filter
        :eval (partial eval-opcode self env)}))

    (do-evaluate-query [_ env [fetch-query-fn result-alias]]
      (bind-result-to-alias
       result-alias
       (apply
        query-helper
        env (fetch-query-fn env (partial find-reference env)))))

    (do-set-literal-attribute [_ env [attr-name attr-value]]
      (set-obj-attr env attr-name attr-value))

    (do-set-list-attribute [self env [attr-name elements-opcode quoted]]
      (call-with-exception-as-error
       #(let [opcode-eval (partial eval-opcode self env)
              final-list ((if quoted set-quoted-list set-flat-list)
                          opcode-eval elements-opcode)]
          (set-obj-attr env attr-name final-list))))

    (do-set-ref-attribute [_ env [attr-name attr-ref]]
      (let [[obj env] (env/follow-reference env attr-ref)]
        (set-obj-attr env attr-name (:path attr-ref) obj)))

    (do-set-compound-attribute [_ env [attr-name f]]
      (set-obj-attr env attr-name f))

    (do-intern-instance [self env [record-name inst-alias queries validation-required upsert-required]] 
      (intern-instance
        self env eval-opcode eval-event-dataflows
        record-name inst-alias queries validation-required upsert-required))

    (do-intern-event-instance [self env [record-name alias-name with-types timeout-ms]]
      (let [[inst env] (pop-and-intern-instance
                        env record-name
                        nil (partial eval-opcode self))
            resolver (resolver-for-instance (env/get-resolver env) inst)
            composed? (rg/composed? resolver)
            with-types (merge (env/with-types env) with-types)
            active-event (env/active-event env)
            inst (if active-event
                   (cn/assoc-event-context
                    inst (cn/event-context active-event))
                   inst)
            env (env/assoc-active-event env inst)
            df (first
                (cl/compile-dataflows-for-event
                 (partial store/compile-query (env/get-store env))
                 (if with-types
                   (assoc inst li/with-types-tag with-types)
                   inst)))
            [local-result evt-env]
            (when df
              (when (or (not resolver) composed?)
                (let [[_ dc] (cn/dataflow-opcode df (or with-types cn/with-default-types))
                      evt-result (eval-opcode self env dc)
                      local-result (extract-local-result evt-result)]
                  (when-not local-result
                    (log/error (str record-name " - event failed - " (first evt-result))))
                  [local-result (:env evt-result)])))
            resolver-results (when resolver (call-resolver-eval resolver composed? env inst))
            r (cond
                (and local-result resolver-results)
                (let [ls (if (map? local-result) [local-result] local-result)
                      res0 (extract-resolver-result resolver-results)
                      res (if (map? res0) [res0] res0)]
                  (vec (concat ls res)))
                local-result local-result
                resolver-results (extract-resolver-result resolver-results))
            env0 (if alias-name (env/bind-instance-to-alias env alias-name r) env)
            env (if evt-env (env/merge-post-event-trigger-sources evt-env env0) env0)]
        (i/ok r (env/assoc-active-event env active-event))))

    (do-delete-instance [self env [record-name queries]]
      (if-let [store (env/get-store env)]
        (cond
          (or (= queries :*) (= queries :purge))
          (let [purge (= queries :purge)]
            (i/ok [(delete-intercept
                    env [record-name nil]
                    (fn [[record-name _]]
                      (when (delete-all-children env store record-name purge)
                        (store/delete-all store record-name purge))))]
                  env))

          :else
          (if-let [[insts env] (find-instances env store record-name queries)]
            (let [alias (ls/alias-tag queries)
                  env (if alias (env/bind-instance-to-alias env alias insts) env)
                  id-attr (cn/identity-attribute-name record-name)]
              (doseq [inst insts]
                (fire-pre-crud-event (partial eval-event-dataflows self) env :delete inst))
              (i/ok insts (reduce (fn [env instance]
                                    (when (delete-children env store record-name instance)
                                      (chained-delete env record-name instance)
                                      (env/delete-post-event
                                       (env/purge-instance env record-name id-attr (id-attr instance))
                                       instance)))
                                  env insts)))
            (i/not-found record-name env)))
        (i/error (str "no active store, cannot delete " record-name " instance"))))

    (do-call-function [_ env fnobj]
      (call-function env fnobj))

    (do-eval_ [_ env [fnobj return-type result-alias]]
      (let [r (fnobj env nil)
            typ-ok (if return-type
                     (if (fn? return-type)
                       (return-type r)
                       (cn/instance-of? return-type r))
                     true)]
        (if typ-ok
          (let [new-env (if result-alias
                          (env/bind-instance-to-alias env result-alias r)
                          env)]
            (i/ok r (cond
                      (map? r) (env/bind-instance new-env r)
                      (seqable? r)
                      (if (string? r)
                        new-env
                        (env/bind-instances new-env r))
                      :else new-env)))
          (i/error (str ":eval failed - result is not of type " return-type)))))

    (do-match [self env [match-pattern-code cases-code alternative-code result-alias]]
      (if match-pattern-code
        (let [result (eval-opcode self env match-pattern-code)
              r (ok-result result)]
          (if (nil? r)
            result
            (eval-cases self (:env result) eval-opcode r cases-code alternative-code result-alias)))
        (eval-condition self env eval-opcode cases-code alternative-code result-alias)))

    (do-try_ [self env [rethrow? body handlers alias-name]]
      (let [result (call-with-exception-as-error #(eval-opcode self env body))
            s0 (:status result)
            r (:result result)
            no-data (or (nil? r) (and (seqable? r) (not (string? r)) (not (seq r))))
            status (if (and (= :ok s0) no-data) :not-found s0)
            h (status handlers)]
        (bind-result-to-alias
         alias-name
         (if h
           (let [env (env/bind-active-error-result (or (:env result) env) result)
                 handler-result (eval-opcode-list self env eval-opcode (if (map? h) [h] h))]
             (if rethrow?
               (assoc handler-result :status status :message (:message result))
               handler-result))
           result))))

    (do-rethrow-after [self env [handler]]
      (let [result (eval-opcode self env handler)]
        (if (ok-result result)
          (env/active-error-result env)
          result)))

    (do-await_ [self env [body continuation]]
      (do
        (go
          (let [result (call-with-exception-as-error
                        #(eval-opcode self env body))
                status (:status result)
                new-env (:env result)
                h (status continuation)]
            (realize-async-result (partial eval-opcode self) new-env h status (:result result))))
        (i/ok [:await :ok] env)))

    (do-for-each [self env [bind-pattern-code elem-alias body-code result-alias]]
      (let [result (eval-opcode self env bind-pattern-code)]
        (if-let [r (ok-result result)]
          (eval-for-each self (:env result) eval-opcode r body-code elem-alias result-alias)
          result)))

    (do-instance-from [self env [record-name inst-opcode data-opcode inst-alias]]
      (let [[inst-result inst-err new-env]
            (when inst-opcode
              (let [result (eval-opcode self env inst-opcode)
                    r (first (ok-result result))]
                (if (map? r)
                  [r nil (:env result)]
                  [nil result env])))]
        (or inst-err
            (let [env (or new-env env)
                  result (eval-opcode self env data-opcode)
                  r (ok-result result)
                  env (:env result)]
              (if (map? r)
                (let [attrs (if inst-result (merge inst-result r) r)
                      inst (if (cn/an-instance? attrs) attrs (cn/make-instance record-name attrs))
                      upsert-required (cn/fetch-entity-schema record-name)]
                  (intern-instance
                   self (env/push-obj env record-name inst)
                   eval-opcode eval-event-dataflows
                   record-name inst-alias nil true upsert-required))
                result)))))

    (do-dynamic-upsert [self env [path-parts attrs inst-compiler alias-name]]
      (let [rs (if-let [p (:path path-parts)]
                 (env/lookup-by-alias env p)
                 (first (env/follow-reference env path-parts)))
            single? (map? rs)
            inst-type (cn/instance-type (if single? rs (first rs)))
            scm (cn/find-entity-schema inst-type)
            id-attr-name (cn/identity-attribute-name inst-type)]
        (when-not scm
          (u/throw-ex (str path-parts " is not bound to an entity-instance")))
        (let [dispatch (partial dispatch-dynamic-upsert self env inst-compiler
                                eval-opcode inst-type id-attr-name attrs)]
          (if single?
            (dispatch rs)
            (loop [env env, rs rs, result []]
              (if-let [inst (first rs)]
                (let [r (dispatch inst)
                      okr (ok-result r)]
                  (if okr
                    (recur (:env r) (rest rs) (concat result okr))
                    r))
                (i/ok result env)))))))))

(def ^:private default-evaluator (u/make-cell))

(defn get-default-evaluator [eval-event-dataflows eval-opcode eval-dataflow]
  (u/safe-set-once
   default-evaluator
   #(make-root-vm eval-event-dataflows eval-opcode eval-dataflow)))
