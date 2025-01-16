(ns agentlang.test.util
  (:require [agentlang.evaluator :as e]
            [agentlang.evaluator.internal :as ei]
            [agentlang.evaluator.intercept :as ec]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            #?(:clj  [clojure.test :refer [is]]
               :cljs [cljs.test :refer-macros [is]])
            [agentlang.util :as u]
            [agentlang.store :as store]
            [agentlang.rbac.core :as rbac]
            [agentlang.lang.rbac :as lr]
            [clojure.spec.gen.alpha :as gen]
            [clojure.spec.alpha :as s]
            [cljc.java-time.local-date-time :as local-date-time]
            [cljc.java-time.local-date :as local-date]
            [cljc.java-time.local-time :as local-time]
            [cljc.java-time.zone-offset :refer [utc]]
            [cljc.java-time.month :as month]))

(defn- report-expected-ex [ex]
  (println (str "Expected exception in test: "
                #?(:clj (.getMessage ex)
                   :cljs ex)))
  ex)

(defn- maybe-result-map [r]
  (cond
    (map? r) r
    (and (seqable? r) (map? (first r))) (first r)
    :else nil))

(defn is-error [f]
  (is (try
        (if-let [r (maybe-result-map (f))]
          (ei/error? r)
          true)
        #?(:clj (catch Exception ex
                  (report-expected-ex ex))
           :cljs (catch js/Error e
                   (report-expected-ex e))))))

(defn- finalize-kernel-components []
  (doseq [cn [:Agentlang.Kernel.Lang
              :Agentlang.Kernel.Identity
              :Agentlang.Kernel.Rbac
              :Agentlang.Kernel.Eval
              :Agentlang.Core]]
    (store/force-init-schema (store/get-default-store) cn)))

(defn finalize-component [component]
  (finalize-kernel-components)
  (store/force-init-schema (store/get-default-store) component))

(defmacro defcomponent [component & body]
  `(do (agentlang.lang/component ~component)
       ~@body
       ~component
       (finalize-component ~component)))

(defn fresult [r]
  (:result (first r)))

(defn ffresult [r]
  (first (fresult r)))

(defn nth-result [r n]
  (:result (nth r n)))

(defn embedded-results [r]
  (fresult (first (second r))))

(defn uuid-string []
  #?(:clj
     (str (java.util.UUID/randomUUID))
     :cljs
     (str (random-uuid))))

(defn maybe-as-map [evt]
  (if (keyword? evt)
    {evt {}}
    evt))

(defn result [evt]
  (fresult
   (e/eval-all-dataflows
    (maybe-as-map evt))))

(def eval-all-dataflows e/eval-all-dataflows)

(defn first-result [evt]
  (let [r (result (maybe-as-map evt))]
    (if (map? r) r (first r))))

(defn sleep [msec f]
  #?(:clj
     (do
       (try
         (Thread/sleep msec)
         (catch Exception ex
           nil))
       (f))
     :cljs
     (js/setTimeout f msec)))

(defn rand-str [len]
  #?(:clj  
     (apply str (take len (repeatedly #(char (+ (rand 26) 97)))))))

(defn rand-email [domain]
  #?(:clj  
     (str (rand-str 12) "@" domain)))

;; To test postgres in CI
;; export POSTGRES_ENABLED=<something> or SQLITE_ENABLED=<something>
;; To turn off
;; unset POSTGRES_ENABLED / unset SQLITE_ENABLED
(store/open-default-store
 #?(:clj (cond
           #?(:clj (System/getenv "SQLITE_ENABLED") :cljs false)
           {:type     :sqlite}
           #?(:clj (System/getenv "POSTGRES_ENABLED") :cljs false)
           {:type     :postgres
            :host     (or (System/getenv "POSTGRES_HOST") "localhost")
            :dbname   (or (System/getenv "POSTGRES_DB") "postgres")
            :username (or (System/getenv "POSTGRES_USER") "postgres")
            :password (System/getenv "POSTGRES_PASSWORD")})
    :cljs {:type :alasql}))

(s/def ::past-and-future-date-time (s/int-in (local-date-time/to-epoch-second (local-date-time/of 2011 month/january 1 0 00 00) utc)
                                             (local-date-time/to-epoch-second (local-date-time/of 2030 month/december 31 23 59 59) utc)))

(s/def ::past-and-future-date (s/int-in (local-date/to-epoch-day (local-date/of 2011 month/january 1))
                                        (local-date/to-epoch-day (local-date/of 2030 month/december 31))))

(s/def ::time (s/int-in (local-time/to-nano-of-day (local-time/of 01 01))
                        (local-time/to-nano-of-day (local-time/of 23 59))))

(comment
  {:Agentlang.Kernel.Lang/UUID (list `s/with-gen string?
                      #(gen/fmap str (s/gen uuid?)))})

(def agentlang-type->spec-clj-type
  {:Agentlang.Kernel.Lang/String string?
   :Agentlang.Kernel.Lang/Keyword keyword?
   :Agentlang.Kernel.Lang/Int int?
   :Agentlang.Kernel.Lang/Int64 int?
   :Agentlang.Kernel.Lang/BigInteger integer?
   :Agentlang.Kernel.Lang/Float float?
   :Agentlang.Kernel.Lang/Double double?
   :Agentlang.Kernel.Lang/Decimal #?(:clj decimal?
                      :cljs float?)
   :Agentlang.Kernel.Lang/Boolean boolean?
   :Agentlang.Kernel.Lang/Any any?
   :Agentlang.Kernel.Lang/Map map?
   :Agentlang.Kernel.Lang/UUID uuid?
   :Agentlang.Kernel.Lang/Path (list `s/or :string (list `s/and string? (complement clojure.string/blank?))
                      :keyword keyword?)
   :Agentlang.Kernel.Lang/Edn (list `s/or :vector vector?
                     :map map?
                     :symbol symbol?
                     :keyword keyword?
                     :string string?
                     :number number?
                     :boolean boolean?
                     :nil nil?
                     :list list?
                     :set set?)
   :Agentlang.Kernel.Lang/Date (list `s/with-gen (partial instance? java.time.LocalDate)
                      #(gen/fmap (fn [ms]
                                   (local-date/of-epoch-day ms))
                                 (s/gen ::past-and-future-date)))
   :Agentlang.Kernel.Lang/Time (list `s/with-gen (partial instance? java.time.LocalTime)
                      #(gen/fmap (fn [ms]
                                   (local-time/of-nano-of-day ms))
                                 (s/gen ::time)))
   :Agentlang.Kernel.Lang/DateTime (list `s/with-gen (partial instance? java.time.LocalDateTime)
                          #(gen/fmap (fn [ms]
                                       (local-date-time/of-epoch-second ms 0 utc))
                                     (s/gen ::past-and-future-date-time)))})

(defn get-spec-namespace [component-name entity-name]
  (keyword (str (name component-name) "/" (name entity-name))))

(defn get-spec-name [component-name entity-name var-name]
  (keyword (str (name component-name) "/" (name entity-name) "." (name var-name))))

(defn get-required-and-optional-keys [component-name entity-name all-keys component-meta]
  (let [required-keys (:required-attributes component-meta)
        optional-keys (clojure.set/difference (set all-keys) (set required-keys))
        required-keys-mapped (map #(get-spec-name component-name entity-name %) required-keys)
        optional-keys-mapped (map #(get-spec-name component-name entity-name %) optional-keys)]
    {:req required-keys-mapped
     :opt optional-keys-mapped}))

(defmulti define-spec :property-type)

;;String spec defn with format option
(comment
  ;;[com.gfredericks/test.chuck "0.2.13"] can be used for predefined regex patterns
  (defcomponent :RefCheck
                #_(entity {:RefCheck/E3 {:AIdId {:type :Agentlang.Kernel.Lang/String
                                               :format "^((19|2[0-9])[0-9]{2})-(0[1-9]|1[012])-(0[1-9]|[12][0-9]|3[01])$"}}}))
  (defmethod define-spec :Agentlang.Kernel.Lang/String [{:keys [spec-entity-name property-details]}]
    (let [spec-fn (if (:format property-details)
                    (list `s/def spec-entity-name (list `s/and string? #(re-matches (re-pattern (:format property-details)) %)))
                    (list `s/def spec-entity-name string?))]
      (eval spec-fn))))

(defmethod define-spec :oneof [{:keys [spec-entity-name property-details]}]
  (let [spec-fn (list `s/def spec-entity-name (:vals property-details))]
    (eval spec-fn)))

(defmethod define-spec :listof [{:keys [spec-entity-name property-details]}]
  (let [spec-fn (list `s/def spec-entity-name (list `s/coll-of (-> :listof-type property-details agentlang-type->spec-clj-type)))]
    (eval spec-fn)))

(defmethod define-spec :default [{:keys [spec-entity-name property-type]}]
  (let [validator (or (agentlang-type->spec-clj-type property-type)
                      property-type)
        spec-fn (list `s/def spec-entity-name validator)]
    (eval spec-fn)))

(defn- define-spec-and-eval [component-name entity-name entity-schema component-meta]
  (doseq [[p-name p-details] entity-schema]
    (let [spec-entity-name (get-spec-name component-name entity-name p-name)]

      (define-spec {:property-type (:type p-details)
                    :property-details p-details
                    :spec-entity-name spec-entity-name})))

  (let [spec-namespace (get-spec-namespace component-name entity-name)
        required-and-optional-keys (get-required-and-optional-keys component-name entity-name (keys entity-schema) component-meta)
        spec-fn (list `s/def spec-namespace (list `s/keys :req (:req required-and-optional-keys)
                                                  :opt (:opt required-and-optional-keys)))]
    (eval spec-fn)))

(defn fill-property-attributes [attr-schema]
  (cond
    (-> attr-schema :oneof seq) {:type :oneof
                                 :vals (-> attr-schema :oneof set)}
    (= (-> attr-schema :type) :Agentlang.Kernel.Lang/String) {:type :Agentlang.Kernel.Lang/String
                                                    :format (-> attr-schema :format-str)}
    (-> attr-schema :listof some?) {:type :listof
                                    :listof-type (:listof attr-schema)}
    :else attr-schema))

(defn resolve-properties [entity-schema component-name]
  (reduce-kv (fn [r k v]
               (let [v-namespace (-> v namespace keyword)
                     attr-schema (cn/find-attribute-schema v)
                     new-v (if (= v-namespace component-name)
                             (fill-property-attributes attr-schema)
                             {:type v})]
                 (assoc r k new-v)))
             {} entity-schema))

(defn get-deep-ref [prop-details component-name]
  (let [prop-type (if (-> prop-details :type (= :listof))
                    (:listof-type prop-details)
                    (:type prop-details))
        construct-deep-ref? (and (keyword? prop-type)
                                 (-> prop-type namespace keyword (= component-name)))]
    (when construct-deep-ref?
      prop-type)))

(defn construct-spec [component]
  (let [[component-name entity-name] (li/split-path component)
        component-meta (cn/fetch-meta component)
        entity-schema (some-> component
                              cn/fetch-schema
                              (resolve-properties component-name))
        _ (doseq [[_ prop-details] entity-schema]
            (when-let [deep-ref-type (get-deep-ref prop-details component-name)]
              (construct-spec deep-ref-type)))]
    (define-spec-and-eval component-name entity-name entity-schema component-meta)))

#?(:clj
   (defn generate-data [component]
     (let [_ (construct-spec component)
           [component-name entity-name] (li/split-path component)
           spec-name-space (get-spec-namespace component-name entity-name)]
       (gen/sample (s/gen spec-name-space)))))

(def append-id cn/append-id)

(def q-id-attr (keyword (str (name cn/id-attr) "?")))

(defn make-path [component-name record-name]
  (li/make-path [component-name record-name]))

(defn not-found? [r]
  (cond
    (map? r) (= :not-found (:status r))
    (vector? r) (not-found? (first r))
    :else false))

(defn sort-by-attr [attr xs]
  (sort #(compare (attr %1) (attr %2)) xs))

(defn type-check [t]
  (partial cn/instance-of? t))

(defn call-with-rbac
  ([f finalize]
   (is (rbac/init))
   (is (= [:rbac] (ec/init-interceptors [:rbac])))
   (try
     (f)
     (finally
       (finalize))))
  ([f] (call-with-rbac f ec/reset-interceptors!)))

(defn finalize-events []
  (lr/finalize-events eval-all-dataflows))

(def reset-events! lr/reset-events!)

(defn with-user [email event]
  (cn/assoc-event-context-user
   email
   (cn/make-instance
    (if (keyword? event)
      {event {}}
      event))))

(def guid li/guid)
(def path-identity li/path-identity)

(defn windows? []
  (= :windows (u/host-os)))

(defn agents-enabled? []
  (if (System/getenv "OPENAI_API_KEY")
    true
    false))
