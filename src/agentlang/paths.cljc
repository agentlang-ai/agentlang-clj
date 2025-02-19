(ns agentlang.paths
  (:require [clojure.string :as s]
            [agentlang.lang.internal :as li]
            [agentlang.paths.internal :as pi]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            #?(:cljs [cljs.reader :refer [read-string]])
            [agentlang.env :as env]
            [agentlang.component :as cn]))

(defn parent-info-from-path [component-name path]
  (let [parts (filter seq (s/split path #"/"))
        at-root (= (count parts) 3)
        ps (if at-root parts (take-last 3 parts))
        nc (partial pi/name-from-path-component component-name)]
    [(nc (first ps)) (second ps) (nc (last ps)) at-root]))

(defn parent-id-from-path [component path type-convert]
  (when-not (pi/null-path? path)
    (let [c (if (keyword? component) component (keyword component))
          parts (pi/parse-any-path component path)
          pt (pi/find-parent-id parts)]
      (when pt
        (if type-convert
          (read-string pt)
          pt)))))

(defn lookup-ref-inst
  ([cast-val env recname id-attr id-val]
   (try
     (or (first (env/lookup-instances-by-attributes
                 env (li/split-path recname) {id-attr id-val} true))
         (first
          ((env/pattern-evaluator env)
           (env/block-compound-patterns (env/block-interceptors env))
           {(li/make-path recname)
            {(li/name-as-query-pattern id-attr) (if cast-val (cn/parse-attribute-value recname id-attr id-val) id-val)}})))
     (catch #?(:clj Exception :cljs js/Error) e
       (do (log/error e) nil))))
  ([env recname id-attr id-val] (lookup-ref-inst true env recname id-attr id-val)))

(defn find-parent-by-path [env record-name path]
  (let [[c n] (li/split-path record-name)
        [parent pid-val relname at-root] (parent-info-from-path c path)
        pid-attr (cn/identity-attribute-name parent)]
    (when-not (cn/parent-via? relname record-name parent)
      (u/throw-ex (str "not in relationship - " [relname record-name parent])))
    (when-let [result (if at-root
                        (lookup-ref-inst env parent pid-attr pid-val)
                        (let [fq (partial pi/as-fully-qualified-path c)
                              path-val (fq (str pi/path-prefix (subs path 0 (s/last-index-of path "/"))))]
                          (or (first (env/lookup-instances-by-attributes
                                      env (li/split-path parent) {li/path-attr path-val}))
                              (first
                               ((env/pattern-evaluator env)
                                (env/block-interceptors env)
                                {(li/make-path parent)
                                 {li/path-attr? path-val}})))))]
      (if (map? result) result (when (seq result) (first result))))))

(defn find-parent-by-full-path [env child-type child-inst]
  (when-let [path (li/path-attr child-inst)]
    (when-not (pi/null-path? path)
      (find-parent-by-path
       env child-type
       (let [p (pi/as-partial-path path)]
         (subs p 0 (s/last-index-of p "/")))))))
