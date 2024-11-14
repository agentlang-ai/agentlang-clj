(ns agentlang.compiler.validation
  "Compile-time schema and data validation."
  (:require [clojure.set :as set]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.lang.internal :as li]
            [agentlang.component :as cn]))

(defn find-schema
  ([path orig-name rec-version]
   (if-let [tag-scm (cn/find-schema path rec-version)]
     tag-scm
     (u/throw-ex (str "schema not found - " [path orig-name]))))
  ([path orig-name]
   (find-schema path orig-name nil))
  ([path] (find-schema path path)))

(defn invalid-attributes
  "Return the set of attributes in the pattern, that is not in the
  original schema. Return nil if there is no difference."
  [pattern-attrs schema]
  (when-not (cn/inferred-event-schema? schema)
    (let [orig-attrs (cn/attribute-names schema)
          pattrs (set (map li/normalize-name (keys pattern-attrs)))]
      (seq (set/difference pattrs orig-attrs)))))

(def validate-attribute-value cn/validate-attribute-value)

(defn validate-references [rec-name refs]
  (loop [k rec-name, rs refs]
    (when-let [r (first rs)]
      (let [[_ scm] (find-schema k)
            attrs (cn/attributes scm)]
        (if attrs
          (when-not (cn/inferred-event-schema? scm)
            (if-let [adef (get attrs r)]
              (recur adef (rest rs))
              (u/throw-ex (str "invalid reference - " [rec-name refs]))))
          (log/warn (str "cannot statically verify reference - " [rec-name refs]))))))
  refs)

(def ^:private where-opr? li/operator?)

(defn ensure-where-clause [clause]
  (if (vector? (first clause))
    (mapv ensure-where-clause clause)
    (if (where-opr? (first clause))
      clause
      (u/throw-ex (str "invalid clause in query - " clause)))))
