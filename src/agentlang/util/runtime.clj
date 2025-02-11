(ns agentlang.util.runtime
  (:require
    clojure.main
    clojure.test
   [clojure.java.io :as io]
   [clojure.string :as s]
   [agentlang.util :as u]
   [agentlang.util.seq :as su]
   [agentlang.util.logger :as log]
   [agentlang.util.runtime :as ur]
   [agentlang.store :as store]
   [agentlang.store.util :as sfu]
   [agentlang.store :as as]
   [agentlang.store.db-common :as dbc]
   [agentlang.resolver.timer :as timer]
   [agentlang.resolver.registry :as rr]
   [agentlang.compiler :as c]
   [agentlang.component :as cn]
   [agentlang.interpreter :as intrp]
   [agentlang.evaluator :as e]
   [agentlang.evaluator.intercept :as ei]
   [agentlang.global-state :as gs]
   [agentlang.lang :as ln]
   [agentlang.lang.rbac :as lr]
   [agentlang.lang.tools.loader :as loader]
   [agentlang.lang.tools.build :as build]
   [agentlang.auth :as auth]
   [agentlang.rbac.core :as rbac]
   [agentlang.connections.client :as cc]
   [agentlang.inference.embeddings.core :as ec]
   [agentlang.inference.service.core :as isc]
   [fractl-config-secrets-reader.core :as sr]))

(def ^:private repl-mode-key :-*-repl-mode-*-)
(def ^:private repl-mode? repl-mode-key)
(def config-data-key :-*-config-data-*-)
(def resource-cache (atom nil))

(defn complete-model-paths [model current-model-paths config]
  (let [mpkey :model-paths
        mp (or (mpkey model)
               (mpkey config)
               ".")]
    (set
     (concat
      current-model-paths
      (if (vector? mp)
        mp
        [mp])))))

(defn store-from-config [config]
  (or (:store-handle config)
      (e/store-from-config (:store config))))

(defn load-components [components model-root config]
  (loader/load-components components model-root))

(defn load-components-from-model [model model-root config]
  (loader/load-components-from-model
   model model-root
   (:load-model-from-resource config)))

(defn load-model [model model-root model-paths config]
  (loader/load-model
   model model-root
   (complete-model-paths model model-paths config)
   (:load-model-from-resource config)))

(defn log-seq! [prefix xs]
  (loop [xs xs, sep "", s (str prefix " - ")]
    (when-let [c (first xs)]
      (let [s (str s sep c)]
        (if-let [cs (seq (rest xs))]
          (recur cs " " s)
          (log/info s))))))

(defn register-resolvers! [config evaluator]
  (when-let [resolver-specs (:resolvers config)]
    (when-let [rns (rr/register-resolvers resolver-specs)]
      (log-seq! "Resolvers" rns)))
  (when-let [auth-config (:authentication config)]
    (when (auth/setup-resolver auth-config evaluator)
      (log/info "authentication resolver inited"))))

(defn model-name-from-args [args]
  (and (seq (su/nonils args))
       (= (count args) 1)
       (let [f (first args)]
         (and (s/ends-with? f u/model-script-name)
              f))))

(defn maybe-read-model [args]
  (when-let [n (and args (model-name-from-args args))]
    
    (loader/read-model n)))

(defn log-app-init-result! [result]
  (cond
    (map? result)
    (let [f (if (= :ok (:status result))
              #(log/info %)
              #(log/error %))]
      (f (str "app-init: " result)))

    (seqable? result)
    (doseq [r result] (log-app-init-result! r))

    :else (log/error (str "app-init: " result))))

(defn- run-standalone-patterns! [evaluator]
  (when-let [pats (seq @gs/standalone-patterns)]
    (let [event-name :Agentlang.Kernel.Lang/PreAppInit]
      (when (and (cn/intern-event event-name {})
                 (cn/register-dataflow event-name pats))
        (try
          (evaluator {event-name {}})
          (finally
            (gs/uninstall-standalone-patterns!)
            (cn/remove-event event-name)))))))

(defn trigger-appinit-event! [evaluator data]
  (let [result (evaluator
                (cn/make-instance
                 {:Agentlang.Kernel.Lang/AppInit
                  {:Data (or data {})}}))]
    (log-app-init-result! result)))

(defn- run-configuration-patterns! [evaluator config]
  (doseq [[llm-name llm-attrs] (:llms config)]
    (let [r (first (evaluator
                    (cn/make-instance
                     {:Agentlang.Core/Create_LLM
                      {:Instance
                       (ln/preprocess-standalone-pattern
                        {:Agentlang.Core/LLM
                         (merge {:Name llm-name} llm-attrs)})}})))]
      (when (not= :ok (:status r))
        (log/error (str "failed to initialize LLM - " llm-name))))))

(defn- run-pending-timers! []
  (when (:timer-manager (gs/get-app-config))
    (future
      (loop []
        (doseq [timer (seq (timer/restart-all-runnable))]
          (when (= "running" (:Status timer))
            (log/info (str "Started timer " (:Name timer)))))
        (try
          (Thread/sleep 15000)
          (catch InterruptedException _ nil))
        (recur)))))

(defn run-appinit-tasks! [evaluator init-data]
  (e/save-model-config-instances)
  (run-configuration-patterns! evaluator (gs/get-app-config))
  (run-standalone-patterns! evaluator)
  (trigger-appinit-event! evaluator init-data)
  (run-pending-timers!))

(defn merge-resolver-configs [app-config resolver-configs]
  (let [app-resolvers (:resolvers app-config)]
    (mapv
     #(let [n (:name %)]
        (if-let [ac (first
                     (filter
                      (fn [x] (= (:name x) n))
                      app-resolvers))]
          (assoc % :config (merge (:config ac) (:config %)))
          %))
     resolver-configs)))

(defn run-initconfig [app-config evaluator]
  (let [result (evaluator
                (cn/make-instance
                 {:Agentlang.Kernel.Lang/InitConfig {}}))
        configs (first (mapv :Data (:result (first result))))
        resolver-configs (merge-resolver-configs
                          app-config
                          (vec
                           (apply
                            concat
                            (mapv :resolvers configs))))
        other-configs (mapv #(dissoc % :resolvers) configs)]
    (merge
     (assoc
      (apply merge other-configs)
      :resolvers resolver-configs)
     (dissoc app-config :resolvers))))

(def set-on-init! u/set-on-init!)

(defn init-schema? [config]
  (if-let [[_ f] (find config :init-schema?)]
    f
    true))

(def ^:private runtime-inited (atom nil))

(defn- runtime-inited-with [value]
  (reset! runtime-inited value)
  value)

(defn get-runtime-init-result [] @runtime-inited)

(defn init-runtime [model config]
  (let [store (store-from-config config)
        ev (partial intrp/evaluate-dataflow store)
        ins (:interceptors config)
        embeddings-config (:embeddings config)]
    (when embeddings-config (ec/init embeddings-config))
    (when (or (not (init-schema? config)) (store/init-all-schema store))
      (let [resolved-config (run-initconfig config ev)
            has-rbac (some #{:rbac} (keys ins))]
        (if has-rbac
          (lr/finalize-events ev)
          (lr/reset-events!))
        (u/run-init-fns)
        (register-resolvers! config ev)
        (when (seq (:resolvers resolved-config))
          (register-resolvers! resolved-config ev))
        (isc/init)
        (run-appinit-tasks! ev (or (:init-data model)
                                   (:init-data config)))
        (when embeddings-config (isc/setup-agent-documents))
        (when has-rbac
          (when-not (rbac/init (merge (:rbac ins) (:authentication config)))
            (log/error "failed to initialize rbac")))
        (ei/init-interceptors ins)
        [ev store]))))

(defn finalize-config [model config]
  (let [final-config (merge (:config model) config)]
    (gs/merge-app-config! final-config)
    final-config))

(defn make-server-config [app-config]
  (assoc (:service app-config) :authentication
         (:authentication app-config)))

(defn prepare-runtime
  ([args [[model model-root] config :as abc]]
   (or @runtime-inited
       (let [config (finalize-config model config)
             store (e/store-from-config (:store config))
             config (assoc config :store-handle store)
             components (or
                         (if model
                           (load-model model model-root nil config)
                           (load-components args (:component-root config) config))
                         (cn/component-names))]
         (when (and (seq components) (every? keyword? components))
           (log-seq! "Components" components))
         (runtime-inited-with [(init-runtime model config) config]))))
  ([model-info] (prepare-runtime nil model-info)))

(defn prepare-repl-runtime [[[model model-root] config]]
  ;; TODO: Fix duplicate invocation of `prepare-runtime` and set repl-mode-key to `true`.
  (prepare-runtime [[model model-root] (assoc config repl-mode-key false)]))

(defn find-model-to-read [args config]
  (or (seq (su/nonils args))
      [(:full-model-path config)]))

(defn preproc-config [config]
  (if (:rbac-enabled config)
    (let [opt (:service config)
          serv (if-not (find opt :call-post-sign-up-event)
                 (assoc opt :call-post-sign-up-event true)
                 opt)
          auth (or (:authentication config)
                   {:service :cognito
                    :superuser-email (u/getenv "AGENTLANG_SUPERUSER_EMAIL" "superuser@superuser.com")
                    :whitelist? false})
          opt (:interceptors config)
          inter (if-not (:rbac opt)
                  (assoc opt :rbac {:enabled true})
                  opt)]
      (assoc (dissoc config :rbac-enabled)
             :service serv
             :authentication auth
             :interceptors inter))
    config))

(defn load-config [options]
  (preproc-config
   (u/read-config-file (get options :config "config.edn"))))

(defn read-model-and-config
  ([args options]
   (let [config (or (config-data-key options) (load-config options))]
     (when-let [extn (:script-extn config)]
       (u/set-script-extn! extn))
     (let [[model _ :as m] (maybe-read-model (find-model-to-read args config))
           config (merge (:config model) config)]
       (try
         [m (sr/read-secret-config config)]
         (catch Exception e
           (u/throw-ex (str "error reading secret config " e)))))))
  ([options] (read-model-and-config nil options)))

(defn read-model-from-resource [component-root]
  (let [^String s (slurp
                   (io/resource
                    (str "model/" component-root "/" u/model-script-name)))]
    (if-let [model (loader/read-model-expressions (io/input-stream (.getBytes s)))]
      model
      (u/throw-ex (str "failed to load model from " component-root)))))

(defn load-model-from-resource []
  (when-let [cfgres (io/resource "config.edn")]
    (let [config (read-string (slurp cfgres))]
      (when-let [extn (:script-extn config)]
        (u/set-script-extn! extn))
      (if-let [component-root (:component-root config)]
        (let [model (read-model-from-resource component-root)
              config (merge (:config model) config)
              components (load-model
                          model component-root nil
                          (assoc config :load-model-from-resource true))]
          (when (seq components)
            (log-seq! "Components loaded from resources" components)
            (let [r [config model components]]
              (reset! resource-cache r) r)))
        (u/throw-ex "component-root not defined in config")))))

(defn merge-options-with-config [options]
  (let [basic-config (load-config options)]
    [basic-config (assoc options config-data-key basic-config)]))

(def ^:private loaded-models (u/make-cell #{}))

(defn call-after-load-model
  ([model-name f ignore-load-error]
   (gs/in-script-mode!)
   (if (some #{model-name} @loaded-models)
     (f)
     (when (try
             (when (build/load-model model-name)
               (u/safe-set loaded-models (conj @loaded-models model-name)))
             (catch Exception ex
               (if ignore-load-error true (throw ex))))
       (f))))
  ([model-name f]
   (call-after-load-model model-name f false)))

(defn- rename-entity-table [connection entity-name old-version new-version]
  (try
    (let [old-table-name (sfu/entity-table-name entity-name old-version)
          new-table-name (sfu/entity-table-name entity-name new-version)]
      (dbc/rename-db-table! connection new-table-name old-table-name)
      (println "Table renamed: " old-table-name "to" new-table-name))
    (catch Exception e
      (log/error
       (str "Table not renamed - " entity-name " - " old-version " - " new-version))
      (log/error e))))

(defn rename-db-entity-tables [new-entities old-entities old-agentlang-version config]
  (let [no-auto-migration (get config :no-auto-migration #{})
        connection (as/connection-info (store-from-config config))]
    (loop [ne (into [] old-entities)]
      (when (seq ne)
        (let [[k v] (first ne)]
          (when (and (not (contains? no-auto-migration k))
                     (not (contains? no-auto-migration (keyword (namespace k))))
                     (not (contains? no-auto-migration (cn/model-for-component (keyword (namespace k)))))
                     (nil? (rr/resolver-for-path k)))
            
            (when-let [new-version (get new-entities k)]
              (if (contains? (set (cn/internal-component-names)) (keyword (namespace k)))
                (when (and (not= old-agentlang-version "current")
                           (not= old-agentlang-version new-version))
                  (rename-entity-table connection k old-agentlang-version new-version))
                (when (not= v new-version)
                  (rename-entity-table connection k v new-version))))))
        (recur (rest ne))))))

(defn invoke-migrations-event []
  (try
    (let
     [r (e/eval-all-dataflows
         (cn/make-instance
          {:Agentlang.Kernel.Lang/Migrations {}}))]
      (log/info (str "migrations result: " r))
      r)
    (catch Exception ex
      (log/error (str "migrations event failed: " (.getMessage ex)))
      (throw ex))))

(defn call-after-load-model-migrate
  ([model-name type path options ignore-load-error] 
   (binding [gs/migration-mode true]
     (gs/in-script-mode!)
     (try
       (let [[_ config] (read-model-and-config options)
             [model old-entities] (build/load-model-migration model-name type path)]
         (cn/unregister-model (:name model))
         (let [[_ new-entities] (build/load-model-migration model-name nil nil)]
           (rename-db-entity-tables new-entities old-entities (:agentlang-version model) config))
         (init-runtime (:name model) config)
         (invoke-migrations-event))
       (catch Exception ex
         (if ignore-load-error true (throw ex))))))
  ([model-name type path options]
   (call-after-load-model-migrate model-name type path options false)))

(defn force-call-after-load-model [model-name f]
  (try
    (call-after-load-model model-name f)
    (catch Exception ex
      (println (str "ERROR - " (.getMessage ex)))
      (f))))

(defn run-repl-func [options model-fn]
  (fn [args]
    (let [opt (first args)
          with-logs (= opt ":with-logs")
          remaining-args (if with-logs (rest args) (do (log/log-capture! :agentlang) args))
          model-name (first remaining-args)]
      (model-fn model-name options))))
