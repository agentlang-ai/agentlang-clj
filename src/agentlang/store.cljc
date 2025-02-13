(ns agentlang.store
  (:require #?(:clj [agentlang.store.h2 :as h2])
            #?(:clj [agentlang.store.postgres :as postgres])
            #?(:clj [agentlang.store.sqlite :as sqlite])
            [agentlang.lang.internal :as li]
            [agentlang.store.mem.core :as mem]
            [agentlang.component :as cn]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.store.util :as stu]
            [agentlang.store.protocol :as p]
            [agentlang.global-state :as gs]
            [agentlang.util :as u]))

(def ^:private default-store (u/make-cell))

(defn get-default-store []
  @default-store)

(defn- make-default-store-config []
  #?(:clj {:dbname (str "./agentlang.db." (System/currentTimeMillis))}
     :cljs {:dbname (str (gensym "agentlang_db"))}))

(defn- make-default-store [store-config store]
  ;; NOTE: The default db connection, if opened,
  ;; will last the lifetime of the app.
  (if (:always-init store-config)
    (do
      (p/open-connection
       store
       (or store-config
           (make-default-store-config)))
      store)
    (u/safe-set-once
     default-store
     #(do
        (p/open-connection
         store
         (or store-config
             (make-default-store-config)))
        store))))

(def ^:private store-constructors
  (u/make-cell
   #?(:clj
      {:h2 h2/make
       :postgres postgres/make
       :sqlite sqlite/make
       :mem mem/make}
      :cljs
      {:mem mem/make})))

(defn register-store [store-name constructor]
  (u/call-and-set
   store-constructors
   #(assoc @store-constructors store-name constructor)))

(defn- store-cons [config]
  (if-let [t (:type config)]
    (t @store-constructors)
    #?(:clj h2/make
       :cljs mem/make)))

(defn open-default-store
  ([store-config]
   (let [make-store (store-cons store-config)]
     #?(:clj (make-default-store store-config (make-store))
        :cljs (make-default-store store-config (make-store)))))
  ([]
   (open-default-store nil)))

(defn open-mem-store
  ([store-config]
   #?(:clj (u/throw-ex (str "Mem store not supported - " store-config))
      :cljs (make-default-store (assoc store-config :reactive true) (mem/make))))
  ([]
   (open-mem-store nil)))

(defn- merge-non-unique
  "Merge non-unique attributes from inst-b to inst-a.
   The resulting instance is used for updating the store."
  [inst-a inst-b unique-keys]
  (loop [ks (keys inst-a), result inst-a]
    (if-let [k (first ks)]
      (if-not (some #{k} unique-keys)
        (if (contains? inst-b k)
          (recur (rest ks) (assoc result k (get inst-b k)))
          (recur (rest ks) result))
        (recur (rest ks) result))
      result)))

(defn- maybe-remove-id [record-name uq-attrs]
  (if (> (count uq-attrs) 1)
    (let [id-attr (cn/identity-attribute-name record-name)]
      (vec (filter #(not= % id-attr) uq-attrs)))
    uq-attrs))

(defn- cast-attr-types [entity-schema attr-names instance]
  (reduce (fn [inst attr-n]
            (if (= :Agentlang.Kernel.Lang/Any (:type (cn/find-attribute-schema (attr-n entity-schema))))
              (assoc inst attr-n (str (attr-n inst)))
              inst))
          instance attr-names))

(defn upsert-instance
  ([f store record-name instance]
   (let [scm (stu/find-entity-schema record-name)
         instance (cn/secure-attributes record-name instance scm)]
     (f store record-name
        (cn/validate-instance instance))))
  ([store record-name instance]
   (upsert-instance p/upsert-instance store record-name instance)))

(def open-connection p/open-connection)
(def close-connection p/close-connection)
(def connection-info p/connection-info)
(def create-schema p/create-schema)
(def drop-schema p/drop-schema)
(def delete-by-id p/delete-by-id)
(def delete-all p/delete-all)
(def delete-children p/delete-children)
(def execute-migration p/execute-migration)

(defn- empty-result-on-error [f]
  (try
    (f)
    #?(:clj
       (catch Exception e
         (log/error e)
         [])
       :cljs
       (catch js/Error e
         (log/error e)
         []))))

(defn query-by-id [store entity-name query-sql ids]
  (empty-result-on-error
   #(p/query-by-id store entity-name query-sql ids)))

(defn query-by-unique-keys [store entity-name unique-keys unique-values]
  (empty-result-on-error
   #(p/query-by-unique-keys store entity-name unique-keys unique-values)))

(defn query-all [store entity-name query-sql]
  (empty-result-on-error
   #(p/query-all store entity-name query-sql)))

(defn do-query [store query query-params]
  (empty-result-on-error
   #(p/do-query store query query-params)))

(def compile-query p/compile-query)
(def get-reference p/get-reference)
(def call-in-transaction p/call-in-transaction)
(def drop-entity p/drop-entity)

(defn reactive?
  "Checks whether a given store supports reactive references"
  [store]
  (if-let [conn-info (connection-info store)]
    (when (map? conn-info)
      (get conn-info :reactive))
    false))

(defn update-instances [store record-name insts]
  (mapv
   #(upsert-instance
     p/update-instance store record-name
     %)
   insts))

(defn create-instances [store record-name insts]
  (mapv
   #(upsert-instance
     p/create-instance store record-name
     %)
   insts))

(defn create-instance [store inst]
  (upsert-instance p/create-instance store (cn/instance-type-kw inst) inst))

(defn upsert-instances [store record-name insts]
  (mapv
   #(upsert-instance store record-name %)
   insts))

(defn get-default-compile-query []
  (when-let [store @default-store]
    (partial p/compile-query store)))

(defn lookup-by-id
  ([store entity-name id-attr-name id]
   (query-by-unique-keys store entity-name [id-attr-name] {id-attr-name id}))
  ([store entity-name id]
   (lookup-by-id store entity-name cn/id-attr id)))

(def ^:private inited-components (u/make-cell #{}))
(def ^:private store-schema-lock #?(:clj (Object.) :cljs nil))

(defn remove-inited-component [component]
  (u/safe-set inited-components (disj @inited-components component)))

(defn maybe-init-schema [store component-name]
  (when (not (some #{component-name} @inited-components))
    (#?(:clj locking :cljs do)
     store-schema-lock
     (when-not (some #{component-name} @inited-components)
       (u/safe-set
        inited-components
        (do (create-schema store component-name)
            (conj @inited-components component-name))))))
  component-name)

(defn init-all-schema [store]
  (let [cnames (cn/component-names)]
    (doseq [cname cnames]
      (maybe-init-schema store cname))
    cnames))

(defn force-init-schema [store component-name]
  (u/safe-set inited-components (disj @inited-components component-name))
  (maybe-init-schema store component-name))

(defn maybe-rollback-active-txn! []
  #?(:clj (when-let [txn (gs/get-active-txn)]
            (.rollback txn))))

(defn assign-owner [store entity-name instance]
  (let [ownership-entity-name (stu/inst-priv-entity entity-name)
        priv-inst (cn/make-instance
                   ownership-entity-name
                   {:CanRead true
                    :CanUpdate true
                    :CanDelete true
                    :ResourcePath (li/path-attr instance)
                    :Assignee (gs/active-user)})]
    (p/create-instance store ownership-entity-name priv-inst)))
