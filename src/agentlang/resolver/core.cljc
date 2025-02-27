(ns agentlang.resolver.core
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.env :as env]
            [agentlang.component :as cn]
            [agentlang.util.seq :as su]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.lang.internal :as li]))

(def ^:private valid-resolver-keys #{:update :create :delete :query :eval
                                     :invoke :on-set-path :on-change-notification})

(defn- preproc-fnmap [fnmap]
  (let [fns (mapv (fn [[k v]]
                    [k (if (fn? v)
                         {:handler v}
                         v)])
                  fnmap)]
    (into {} fns)))

(defn make-resolver
  ([resolver-name fnmap eval-dataflow]
   (let [fnmap (preproc-fnmap fnmap)]
     (when-not (su/all-true? (mapv #(some #{%} valid-resolver-keys) (keys fnmap)))
       (u/throw-ex (str "invalid resolver keys - " (keys fnmap))))
     (doseq [[k v] fnmap]
       (when-not (fn? (:handler v))
         (u/throw-ex (str "error in resolver " resolver-name ", " k " must be mapped to a function"))))
     (assoc fnmap
            :name resolver-name
            :evt-handler eval-dataflow)))
  ([resolver-name fnmap]
   (make-resolver resolver-name fnmap nil)))

(def resolver-name :name)
(def resolver-create :create)
(def resolver-update :update)
(def resolver-delete :delete)
(def resolver-query :query)
(def resolver-eval :eval)
(def resolver-invoke :invoke)

(defn- ok? [r] (= :ok (:status r)))

(defn- ok-fresult [r]
  (when (ok? r)
    (first (:result r))))

(defn- apply-xform
  "Tranformations can be applied as the data moves from the evaluator to
   resolver, and vice versa. Transformations are specified as a vector in
   a `:xform` map in the resolver specification. `:in` represents the
   xforms of data flowing from evaluator to resolver and `:out` represents
   those in the reverse direction.

   xforms can be represented either by a clojure function or a agentlang name.
   With a clojure function, the function is applied to the data (or the result
   of a previous transformation). With a agentlang name, a event corresponding
   to the specified name is triggered and evaluated. The event body has a single
   field called `:Instance`, which carries the data (entity instance)."
  [xform eval-dataflow env arg]
  (cond
    (fn? xform)
    (xform env arg)

    (li/name? xform)
    (when eval-dataflow
      (let [evt-inst (cn/make-instance
                      {xform {:Instance arg}})
            result (eval-dataflow evt-inst)]
        (ok-fresult (first result))))

    :else
    arg))

(defn- apply-xforms
  [xforms eval-dataflow env arg]
  (loop [xforms xforms arg arg]
    (if-let [xf (first xforms)]
      (recur (rest xforms)
             (apply-xform xf eval-dataflow env arg))
      arg)))

(defn- dispatch-f [method resolver env f arg]
  (if (get-in resolver [method :with-context])
    (let [relctx (env/relationship-context env)]
      (f {:-> relctx} arg))
    (f arg)))

(defn- invoke-method
  ([method resolver handler handler-tag env arg]
   (let [f (if (= :invoke method)
             (partial handler handler-tag env)
             handler)]
     (if-let [in-xforms (get-in resolver [method :xform :in])]
       (let [eval-dataflow (:evt-handler resolver)
             final-arg (apply-xforms in-xforms eval-dataflow env arg)
             result (dispatch-f method resolver env f final-arg)]
         (if-let [out-xforms (get-in resolver [method :xform :out])]
           (apply-xforms out-xforms eval-dataflow env result)
           result))
       (let [eval-dataflow (:evt-handler resolver)
             result (dispatch-f method resolver env f arg)]
         (if-let [out-xforms (get-in resolver [method :xform :out])]
           (apply-xforms out-xforms eval-dataflow env result)
           result)))))
  ([method resolver handler env arg]
   (invoke-method method resolver handler nil env arg)))

(defn- wrap-result [method resolver env arg]
  (log/debug (str "Calling method " method " in resolver " (:name resolver) " for " arg))
  (if-let [m (get-in resolver [method :handler])]
    {:resolver (:name resolver)
     :method method
     :result (invoke-method method resolver m env arg)}
    (when-let [m (get-in resolver [:invoke :handler])]
      {:resolver (:name resolver)
       :method method
       :result (invoke-method :invoke resolver m method env arg)})))

(def call-resolver-create (partial wrap-result :create))
(def call-resolver-update (partial wrap-result :update))
(def call-resolver-delete (partial wrap-result :delete))
(def call-resolver-query (partial wrap-result :query))
(def call-resolver-eval (partial wrap-result :eval))
(def call-resolver-on-set-path (partial wrap-result :on-set-path))
(def call-resolver-on-change-notification (partial wrap-result :on-change-notification))

(defn id-to-delete [arg]
  (if (map? arg)
    (cn/id-attr arg)
    (second arg)))

(def query-entity-name :entity-name)

(defn- col-name-to-attr-name [x]
  (if (keyword? x)
    (let [s (name x)]
      (if (s/starts-with? s "_")
        (keyword (subs s 1))
        x))
    x))

(defn query-attributes [query-spec]
  (when-let [qattrs (:query-attributes query-spec)]
    (into
     {}
     (mapv (fn [[k v]]
             [k (if (vector? v)
                  `[~(first v) ~@(mapv col-name-to-attr-name (rest v))]
                  v)])
           qattrs))))

(defn query-all? [query-attrs]
  (and (map? query-attrs)
       (when-let [pq (or (li/path-attr? query-attrs)
                         (li/path-attr query-attrs))]
         (and (vector? pq)
              (= :like (first pq))))))
