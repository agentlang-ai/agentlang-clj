(ns agentlang.evaluator
  "Helper functions for compiling and evaluating patterns."
  (:require [clojure.walk :as w]
            [clojure.core.async :as async]
            [agentlang.component :as cn]
            [agentlang.compiler :as c]
            [agentlang.env :as env]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.seq :as su]
            [agentlang.util.http :as uh]
            [agentlang.util.errors :refer [extract-client-message-from-ex]]
            [agentlang.datafmt.json :as json]
            [agentlang.store :as store]
            [agentlang.resolver.registry :as rr]
            [agentlang.policy.logging :as logging]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.lang.opcode :as opc]
            [agentlang.lang.datetime :as dt]
            ;; load kernel components
            [agentlang.model]
            [agentlang.telemetry :as telemetry]
            [agentlang.global-state :as gs]
            [agentlang.evaluator.state :as es]
            [agentlang.evaluator.internal :as i]
            [agentlang.evaluator.root :as r]
            [agentlang.evaluator.suspend :as sp]
            [agentlang.evaluator.exec-graph :as exg]
            [agentlang.evaluator.intercept.core :as interceptors]))

(declare eval-all-dataflows evaluator-with-env safe-eval-pattern)

(def ^:private suspension-flag #?(:clj (ThreadLocal.)
                                  :cljs (atom false)))

(defn- reset-suspension-flag! []
  #?(:clj (.set suspension-flag false)
     :cljs (reset! suspension-flag false)))

(defn as-suspended [result]
  #?(:clj (.set suspension-flag true)
     :cljs (reset! suspension-flag true))
  result)

(defn- is-suspension-flag-set? []
  #?(:clj (.get suspension-flag)
     :cljs @suspension-flag))

(defn- extract-alias-from-pattern [pat]
  (if (map? pat)
    (:as pat)
    (when (seqable? pat) (second (drop-while #(not= % :as) pat)))))

(defn- suspend-dataflow [result env opcode opcode-count]
  (reset-suspension-flag!)
  (let [result
        (let [event (env/active-event env)]
          (if (zero? opcode-count)
            (i/suspension result nil)
            (if-let [sid (sp/save-suspension
                          eval-all-dataflows event opcode-count
                          (env/cleanup env false)
                          (when-let [pat (:pattern opcode)]
                            (extract-alias-from-pattern pat)))]
              (i/suspension result sid)
              (u/throw-ex (str "failed to suspend dataflow for " (cn/instance-type-kw event))))))]
    result))

(defn- dispatch-an-opcode [evaluator env opcode]
  (((opc/op opcode) i/dispatch-table) evaluator env (opc/arg opcode)))

(defn dispatch [evaluator env {opcode :opcode pat :pattern}]
  (#?(:clj try :cljs do)
   (let [result
         (if (map? opcode)
           (dispatch-an-opcode evaluator env opcode)
           (loop [opcs opcode, env env, result nil]
             (if-let [opc (first opcs)]
               (let [r (dispatch-an-opcode evaluator env opc)
                     env (or (:env r) env)]
                 (recur (rest opcs) env r))
                result)))]
     (exg/add-step! pat result)
     result)
   #?(:clj
      (catch Exception ex
        (exg/add-step! pat {:status :error :result (.getMessage ex)})
        (throw ex)))))

(def ok? i/ok?)
(def dummy-result i/dummy-result)

(defn- dispatch-opcodes [evaluator env opcodes]
  (if (map? opcodes)
    (dispatch evaluator env opcodes)
    (loop [dc opcodes, result (dummy-result env)]
      (if (or (ok? result)
              (cn/future-object? result))
        (if-let [opcode (first dc)]
          (let [r (dispatch evaluator (:env result) opcode)]
            (if (is-suspension-flag-set?)
              (suspend-dataflow r (:env r) opcode (count (rest dc)))
              (recur (rest dc) r)))
          result)
        result))))

(defn- deref-futures [result]
  (w/prewalk
   #(if (cn/future-object? %)
      (cn/deref-future-object %)
      %)
   result))

(def mark-internal i/mark-internal)
(def internal-event? i/internal-event?)

(defn trigger-rules [tag insts]
  (loop [insts insts, env nil, result nil]
    (if-let [inst (first insts)]
      (let [n (cn/instance-type-kw inst)]
        (when-let [rules (cn/rules-for-entity tag n)]
          (let [env (or env (env/make (es/get-active-store) nil))
                rs (cn/run-rules evaluator-with-env [env env/cleanup] n inst rules)]
            (recur (rest insts) env (concat result rs)))))
      result)))

(defn- str-session-info [sinfo]
  (cond
    (string? sinfo) sinfo
    (map? sinfo) (json/encode sinfo)
    :else (str sinfo)))

(defn- maybe-create-audit-trail [env tag insts]
  #?(:clj
     (when (gs/audit-trail-enabled?)
       (when-let [event-context (li/event-context (env/active-event env))]
         (let [action (name tag)]
           (doseq [inst insts]
             (let [entity-name (cn/instance-type inst)]
               (when (cn/audit-required? entity-name)
                 (let [id-val ((cn/identity-attribute-name entity-name) inst)
                       attrs {:InstanceId (str id-val)
                              :Action action
                              :Timestamp (dt/unix-timestamp)
                              :User (or (:User event-context) "anonymous")}
                       trail-data (if-let [sinfo (get-in event-context [:UserDetails :session-info])]
                                    (assoc attrs :SessionToken (str-session-info sinfo))
                                    attrs)
                       trail-entry {(cn/audit-trail-entity-name entity-name) trail-data}]
                   (binding [gs/audit-trail-mode true]
                     (when-not (safe-eval-pattern trail-entry)
                       (log/warn (str "failed to audit " tag " on " inst))))))))))))
  insts)

(def ^:dynamic internal-post-events false)

(defn- fire-post-events-for
  ([tag is-internal insts]
   (binding [internal-post-events is-internal]
     (doseq [inst insts]
       (when-let [[event-name r] (cn/fire-post-event eval-all-dataflows tag inst)]
         (when-not (u/safe-ok-result r)
           (log/warn r)
           (u/throw-ex (str "internal event " event-name " failed.")))))
     insts))
  ([tag insts] (fire-post-events-for tag nil insts)))

(defn fire-post-events
  ([env is-internal]
   (let [srcs (env/post-event-trigger-sources env)]
     (reduce
      (fn [env tag]
        (if-let [insts (seq (tag srcs))]
          (and (fire-post-events-for tag is-internal insts)
               (maybe-create-audit-trail env tag insts)
               (env/assoc-rule-futures env (trigger-rules tag insts)))
          env))
      env [:create :update :delete])))
  ([env] (fire-post-events env nil)))

(reset! gs/fire-post-events fire-post-events)

(defn- fire-post-event-for [tag inst]
  (fire-post-events-for tag [inst]))

(defn- init-exec-state [event-instance]
  (and (exg/init event-instance)
       (sp/init-suspension-id)))

(def eval-after-create (partial fire-post-event-for :create))
(def eval-after-update (partial fire-post-event-for :update))
(def eval-after-delete (partial fire-post-event-for :delete))

(defn- eval-dataflow-in-transaction [evaluator env event-instance df txn]
  (binding [gs/active-event-context (or (li/event-context event-instance)
                                        gs/active-event-context)]
    (let [txn-set (atom false)]
      (when (and txn (not (gs/get-active-txn)))
        (gs/set-active-txn! txn)
        (reset! txn-set true))
      (try
        (let [_ (init-exec-state event-instance)
              {susp-env :env susp-opcc :opcc} sp/suspension-info
              env (if susp-env (merge env susp-env) env)
              is-internal (or (internal-event? event-instance) internal-post-events)
              event-instance0 (if is-internal
                                (dissoc event-instance i/internal-event-key)
                                event-instance)
              event-instance (if-not (li/event-context event-instance0)
                               (assoc event-instance0 li/event-context gs/active-event-context)
                               event-instance0)
              env0 (if is-internal
                     (env/block-interceptors env)
                     (env/assoc-active-event env event-instance))
              continuation (fn [event-instance]
                             (let [env (if event-instance
                                         (env/assoc-active-event
                                          (env/bind-instance
                                           env0 (li/split-path (cn/instance-type event-instance))
                                           event-instance)
                                          event-instance)
                                         env0)
                                   [_ dc] (cn/dataflow-opcode
                                           df (or (env/with-types env)
                                                  cn/with-default-types))
                                   dc (if susp-opcc (take-last susp-opcc dc) dc)
                                   result (deref-futures (let [r (dispatch-opcodes evaluator env dc)]
                                                           (if (and (map? r) (not= :ok (:status r)))
                                                             (throw (ex-info "eval failed" {:eval-result r}))
                                                             r)))
                                   env0 (fire-post-events (:env result) is-internal)]
                               (assoc result :env env0)))]
          (interceptors/eval-intercept env0 event-instance continuation))
        (finally (do (exg/finalize!)
                     (when @txn-set (gs/set-active-txn! nil))))))))

(defn- maybe-init-event [event-obj]
  (if (cn/event-instance? event-obj)
    event-obj
    (let [event-name (first (keys event-obj))]
      (cn/make-instance event-name (event-name event-obj)))))

(defn eval-dataflow
  "Evaluate a compiled dataflow, triggered by event-instance, within the context
   of the provided environment. Each compiled pattern is dispatched to an evaluator,
   where the real evaluation is happening. Return the value produced by the resolver."
  ([evaluator env event-instance df]
   (let [env0 (or (cn/event-context-env event-instance) env)
         env (env/assoc-pattern-evaluator env0 safe-eval-pattern)
         event-instance (maybe-init-event event-instance)
         f (partial eval-dataflow-in-transaction evaluator env event-instance df)]
     (try
       (let [result (if-let [txn (gs/get-active-txn)]
                      (f txn)
                      (if-let [store (env/get-store env)]
                        (store/call-in-transaction store f)
                        (f nil)))]
         (telemetry/log-event event-instance result)
         result)
       (catch #?(:clj Exception :cljs :default) ex
         (let [err (ex-data ex)]
           (store/maybe-rollback-active-txn!)
           (telemetry/log-event event-instance (i/error (or err #?(:clj (.getMessage ex)))))
           (if-let [r (:eval-result err)]
             r
             (throw ex)))))))
  ([evaluator event-instance df]
   (eval-dataflow evaluator env/EMPTY event-instance df)))

(defn- remove-hidden-attributes [hidden-attrs inst]
  (if-let [r (:result inst)]
    (if (vector? r)
      (assoc
       inst
       :result
       (map
        (partial remove-hidden-attributes hidden-attrs)
        r))
      (remove-hidden-attributes hidden-attrs r))
    (loop [hs hidden-attrs, inst inst]
      (if-let [h (first hs)]
        (recur
         (rest hs)
         (if (cn/instance-of? (first h) inst)
           (su/dissoc-in inst (second h))
           inst))
        inst))))

(defn- log-event [hidden-attrs event-instance]
  #?(:clj (log/dev-debug
           (str "evaluating dataflow for event - "
                (remove-hidden-attributes hidden-attrs event-instance)))))

(defn- log-result-object [hidden-attrs event-instance obj]
  #?(:clj (log/dev-debug
           (str "dataflow result for " (cn/instance-type event-instance)
                " - " (remove-hidden-attributes hidden-attrs obj)))))

(defn- eval-dataflow-with-logs [evaluator env event-instance hidden-attrs df]
  (try
    (let [r (eval-dataflow evaluator env event-instance df)]
      (log-result-object hidden-attrs event-instance r)
      r)
    (catch #?(:clj Exception :cljs :default) ex
      (let [msg (str "error in dataflow for "
                     (or (cn/instance-type event-instance)
                         (li/record-name event-instance)
                         event-instance)
                     " - " #?(:clj (str (.getMessage ex)
                                        (ex-data ex))
                              :cljs ex))]
        (log/warn msg)
        (log/exception ex)
        (i/error (or (extract-client-message-from-ex ex) (.getMessage ex)))))))

(defn- run-dataflows
  "Compile and evaluate all dataflows attached to an event. The query-compiler
   and evaluator returned by a previous call to evaluator/make may be passed as
   the first two arguments."
  [compile-query-fn evaluator env event-instance]
  (let [dfs (c/compile-dataflows-for-event compile-query-fn event-instance)
        logging-rules (logging/rules event-instance)
        hidden-attrs (logging/hidden-attributes logging-rules)
        ef (partial eval-dataflow-with-logs evaluator
                    env event-instance hidden-attrs)]
    (log-event hidden-attrs event-instance)
    (mapv ef dfs)))

(defn- make
  "Use the given store to create a query compiler and pattern evaluator.
   Return the vector [compile-query-fn, evaluator]."
  [store]
  (let [cq (when store
             (partial store/compile-query store))]
    [cq (r/get-default-evaluator (partial run-dataflows cq) dispatch-opcodes eval-dataflow)]))

(defn store-from-config
  [store-or-store-config]
  (cond
    (or (nil? store-or-store-config)
        (map? store-or-store-config))
    (store/open-default-store store-or-store-config)

    (and (keyword? store-or-store-config)
         (= store-or-store-config :none))
    nil

    :else
    store-or-store-config))

(defn- resolver-from-config
  [resolver-or-resolver-config]
  (cond
    (nil? resolver-or-resolver-config)
    (rr/root-registry)

    (map? resolver-or-resolver-config)
    (do (rr/register-resolvers resolver-or-resolver-config)
        (rr/root-registry))

    (and (keyword? resolver-or-resolver-config)
         (= resolver-or-resolver-config :none))
    nil

    :else
    (if (rr/registry? resolver-or-resolver-config)
      resolver-or-resolver-config
      (u/throw-ex (str "invalid resolver config " resolver-or-resolver-config)))))

(defn- evaluator
  ([store-or-store-config resolver-or-resolver-config]
   (let [store (store-from-config store-or-store-config)
         resolver (resolver-from-config resolver-or-resolver-config)
         [compile-query-fn evaluator] (make store)
         env (env/make store resolver)
         ef (partial run-dataflows compile-query-fn evaluator env)]
     (es/set-active-state! ef store)
     ef))
  ([] (evaluator (es/get-active-store) nil)))

(defn- evaluator-with-env [env]
  (let [[compile-query-fn evaluator] (make (es/get-active-store))]
    (partial run-dataflows compile-query-fn evaluator env)))

(defn- maybe-enrich-env [env store resolver]
  (when env
    (env/maybe-enrich env store resolver)))

(defn evaluate-pattern
  ([env store-or-store-config resolver-or-resolver-config pattern]
   (let [store (if (nil? store-or-store-config)
                 (or (es/get-active-store)
                     (store-from-config store-or-store-config))
                 (store-from-config store-or-store-config))
         resolver (resolver-from-config resolver-or-resolver-config)
         [compile-query-fn evaluator] (make store)
         env (or (maybe-enrich-env env store resolver) (env/make store resolver))
         opcode (c/compile-standalone-pattern compile-query-fn pattern)]
     (dispatch evaluator env opcode)))
  ([store-or-store-config resolver-or-resolver-config pattern]
   (evaluate-pattern nil store-or-store-config resolver-or-resolver-config pattern))
  ([env pattern] (evaluate-pattern env nil nil pattern))
  ([pattern] (evaluate-pattern nil nil pattern)))

(def ^:private debug-sessions (atom {}))

(defn- save-debug-session [id sess]
  (swap! debug-sessions assoc id sess)
  id)

(defn- remove-debug-session [id]
  (swap! debug-sessions dissoc id)
  id)

(defn- debug-norm-result [df-result]
  (if (map? df-result)
    df-result
    (first df-result)))

(defn- make-debug-result [status result env]
  {:status status :result result :env (env/cleanup env)})

(defn- debug-step-result [r0]
  (let [s (:status r0), ir (:result r0)
        inner-result (when (= :ok s) ir)
        norm-inner-result (if inner-result
                            (if (or (map? inner-result)
                                    (string? inner-result))
                              inner-result
                              (vec inner-result))
                            ir)
        final-result (if (and (vector? norm-inner-result) (= 1 (count norm-inner-result)))
                       (first norm-inner-result)
                       norm-inner-result)]
    (make-debug-result s final-result (:env r0))))

(defn debug-step [id]
  (when-let [{opcode :opcode env :env ev :eval}
             (get @debug-sessions id)]
    (if-let [opc (first opcode)]
      (let [r (debug-norm-result (dispatch ev env opc))
            sess {:opcode (rest opcode) :env (:env r) :eval ev}]
        [(save-debug-session id sess)
         (assoc (debug-step-result r) :pattern (:pattern opc))])
      [(remove-debug-session id) nil])))

(defn debug-continue [id]
  (when-let [{opcode :opcode env :env ev :eval}
             (get @debug-sessions id)]
    (let [r
          (loop [opcode opcode, env env, result []]
            (if-let [opc (first opcode)]
              (let [r0 (debug-norm-result (dispatch ev env opc))
                    s (:status r0), ir (:result r0)]
                (if (= :ok s)
                  (recur
                   (rest opcode)
                   (:env r0)
                   (conj result (make-debug-result s ir (:env r0))))
                  [(make-debug-result s ir (:env r0))]))
              (vec result)))]
      [(remove-debug-session id) r])))

(defn debug-dataflow
  ([event-instance evaluator]
   (let [store (or (es/get-active-store)
                   (store-from-config (:store (gs/get-app-config))))
         [cq ev] (if evaluator
                   [(partial store/compile-query store) evaluator]
                   (make store))
         event-instance (maybe-init-event event-instance)
         df (first (c/compile-dataflows-for-event cq event-instance))
         [_ opcs] (cn/dataflow-opcode df cn/with-default-types)
         env (env/bind-instance
              (env/make store nil)
              (li/split-path (cn/instance-type event-instance))
              event-instance)
         id (u/uuid-string)
         sess {:opcode opcs :env env :eval ev}]
     (save-debug-session id sess)))
  ([event-instance] (debug-dataflow event-instance nil)))

(def debug-cancel remove-debug-session)

(defn eval-all-dataflows
  ([event-obj store-or-store-config resolver-or-resolver-config]
   (let [ef (evaluator store-or-store-config resolver-or-resolver-config)]
     (ef event-obj)))
  ([event-obj]
   (eval-all-dataflows event-obj (es/get-active-store) nil)))

(defn eval-all-dataflows-atomic
  "Evaluate the dataflows in a new transaction"
  [event-obj]
  (let [txn (gs/get-active-txn)]
    (gs/set-active-txn! nil)
    (try
      (eval-all-dataflows event-obj)
      (finally
        (gs/set-active-txn! txn)))))

(defn eval-pure-dataflows
  "Facility to evaluate dataflows without producing any side-effects.
   This is useful for pure tasks like data-format transformation.
   An example: transforming data before being sent to a resolver to
   match the format requirements of the backend"
  [event-obj]
  (eval-all-dataflows event-obj :none :none))

(defn- filter-public-result [xs]
  (if (map? xs)
    (dissoc xs :env)
    (mapv filter-public-result xs)))

(defn public-evaluator [store-or-config]
  (comp filter-public-result (evaluator store-or-config nil)))

(defn internal-evaluator [store-or-config]
  (evaluator store-or-config nil))

(defn query-fn [store]
  (partial r/find-instances env/EMPTY store))

(defn safe-eval
  ([is-atomic event-obj]
   (u/safe-ok-result
    ((if is-atomic eval-all-dataflows-atomic eval-all-dataflows)
     (cn/make-instance event-obj))))
  ([event-obj] (safe-eval false event-obj)))

(defn safe-eval-internal
  ([is-atomic event-obj]
   (u/safe-ok-result
    ((if is-atomic eval-all-dataflows-atomic eval-all-dataflows)
     (mark-internal
      (cn/make-instance event-obj)))))
  ([event-obj] (safe-eval-internal true event-obj)))

(defn eval-internal [event-obj]
  (eval-all-dataflows (mark-internal (cn/make-instance event-obj))))

(defn safe-eval-pattern
  ([pattern]
   (safe-eval-pattern nil pattern))
  ([env pattern]
   (u/safe-ok-result (evaluate-pattern env pattern))))

(defn- eval-patterns-helper [component pats eval-fn]
  (let [event-name (ln/event (li/make-path component (li/unq-name)) {})]
    (when (apply ln/dataflow event-name pats)
      (try
        (eval-fn {event-name {}})
        (finally
          (cn/remove-event event-name))))))

(defn safe-eval-patterns
  ([is-atomic component pats]
   (eval-patterns-helper component pats (partial safe-eval is-atomic)))
  ([component pats] (safe-eval-patterns true component pats)))

(defn evaluate-patterns-in-env [env component patterns]
  (eval-patterns-helper component patterns (partial evaluate-pattern env)))

(es/set-safe-eval-patterns! safe-eval-patterns)
(es/set-safe-eval-atomic! (partial safe-eval true))
(es/set-evaluate-patterns! evaluate-patterns-in-env)

(defn eval-patterns [component pats]
  (let [event-name (ln/event (li/make-path component (li/unq-name)) {})]
    (when (apply ln/dataflow event-name pats)
      (try
        (eval-all-dataflows (cn/make-instance event-name {}))
        (finally
          (cn/remove-event event-name))))))

(defn- maybe-delete-model-config-instance [entity-name]
  (let [evt-name (cn/crud-event-name entity-name :Delete)]
    (safe-eval-internal {evt-name {:Id 1}})))

(defn save-model-config-instance [app-config model-name]
  (when-let [ent (cn/model-config-entity model-name)]
    (when-let [attrs (ent app-config)]
      (maybe-delete-model-config-instance ent)
      (let [evt-name (cn/crud-event-name ent :Create)]
        (safe-eval-internal {evt-name {:Instance {ent attrs}}})))))

(defn save-model-config-instances []
  (when-let [app-config (gs/get-app-config)]
    (mapv (partial save-model-config-instance app-config) (cn/model-names))))

(defn- fetch-model-config-declaration [entity-name]
  (when-let [app-config (gs/get-app-config)]
    (when-let [rec (entity-name app-config)]
      (cn/make-instance entity-name rec))))

(defn fetch-model-config-instance [model-name]
  (let [model-name (if (li/quoted? model-name)
                     (second model-name)
                     model-name)]
    (when-let [ent (cn/model-config-entity model-name)]
      (let [evt-name (cn/crud-event-name ent :LookupAll)]
        (or (first (safe-eval-internal {evt-name {}}))
            (fetch-model-config-declaration ent))))))

#?(:clj
   (defn async-evaluate-pattern [op-code pat result-chan]
     (async/go
       (try
         (let [evaluation-result (case op-code
                                   "eval" (cond
                                            (map? pat) (evaluate-pattern pat)
                                            (list? pat) (eval pat)
                                            :else (println "Cannot evaluate this pattern: " pat))
                                   "add" (eval pat)
                                   (println "Wrong op-code for the pattern - op-code: " op-code))]
           (log/info (str "Evaluation result from async-evaluate-pattern is: " evaluation-result))
           (async/>! result-chan evaluation-result))
         (catch Exception e
           (do
             (log/warn (str "Exception during evaluation on async-evaluate-pattern: " (.getMessage e)))
             (async/>! result-chan
                       (str "Error during evaluation:"
                            (.getMessage e))))))
       (async/close! result-chan))))
