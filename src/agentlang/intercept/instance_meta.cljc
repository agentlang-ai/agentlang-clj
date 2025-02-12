(ns agentlang.evaluator.intercept.instance-meta
  (:require [agentlang.component :as cn]
            [agentlang.util.seq :as su]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.lang.datetime :as dt]
            [agentlang.env :as env]
            [agentlang.store :as store]
            [agentlang.lang.internal :as li]
            [agentlang.evaluator :as e]
            [agentlang.evaluator.intercept.internal :as ii]))

(defn- upsert-meta-failed-msg [entity-instances]
  (str "failed to upsert meta-data for "
       (cn/instance-type (first entity-instances))))

(defn- upsert-meta [upsert-f env arg]
  (let [user (cn/event-context-user (ii/event arg))
        entity-instances (ii/data-output arg)]
    (cond
      (not entity-instances) arg

      (not (cn/entity-instance? (first entity-instances))) arg

      (ii/attribute-ref? entity-instances) arg

      user
      (try
        (if (upsert-f env user entity-instances)
          arg
          (log/error (upsert-meta-failed-msg entity-instances)))
        #?(:clj
           (catch Exception e
             (log/error (str (upsert-meta-failed-msg entity-instances)
                             " - " (or (ex-data e) (.getMessage e)))))
           :cljs
           (catch js/Error e
             (log/error (str (upsert-meta-failed-msg entity-instances)
                             " - " (or (.-ex-data e) (i/error e)))))))
      :else arg)))

(def ^:private update-meta
  (partial
   upsert-meta
   (fn [_ user entity-instances]
     (let [n (cn/instance-type (first entity-instances))]
       (su/all-true?
        (mapv #(e/safe-eval-internal
                {(cn/meta-entity-update-event-name n)
                 {cn/meta-entity-id (str (cn/idval %))
                  :Data {:LastUpdated (dt/now) :LastUpdatedBy user}}})
              entity-instances))))))

(def ^:private create-meta
  (partial
   upsert-meta
   (fn [env user entity-instances]
     (let [meta-infos (group-by
                       first
                       (mapv #(cn/make-meta-instance % user) entity-instances))
           ks (keys meta-infos)
           rs (flatten
               (mapv (fn [k]
                       (let [rows (mapv second (meta-infos k))]
                         (store/upsert-instances
                          (env/get-store env)
                          (li/split-path k) rows)))
                     ks))]
       (and (su/all-true? rs) (= (count rs) (count entity-instances)))))))

(defn- delete-meta [env arg]
  (if-let [[record-name id] (ii/data-output arg)]
    (try
      (store/delete-by-id
       (env/get-store env)
       (cn/meta-entity-name (li/split-path record-name))
       cn/meta-entity-id (str id))
      #?(:clj
         (catch Exception e
           (log/error (str "delete meta error " [record-name id]
                           " - " (or (ex-data e) (.getMessage e)))))
         :cljs
         (catch js/Error e
           (log/error (str "delete meta error " [record-name id]
                           " - " (or (.-ex-data e) (i/error e)))))))
    arg))

(def ^:private actions
  {:create create-meta
   :update update-meta
   :delete delete-meta})

(defn- run [env opr arg]
  (if-let [f (opr actions)]
    (or (f env arg) arg)
    arg))

(defn make [_] ; config not used
  (ii/make-interceptor :instance-meta run))
