(ns agentlang.resolver.registry
  (:require [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.lang.internal :as li]
            [agentlang.resolver.core :as r]))

(def ^:private type-tag :-*-resolver-registry-*-)
(def ^:private parent-tag :-*-parent-*-)

(defn make
  ([parent]
   (u/make-cell {parent-tag parent type-tag true}))
  ([] (make nil)))

(defn registry? [obj]
  (and (u/cell? obj)
       (type-tag @obj)))

(def ^:private root-resolver-db (make))

(defn resolver-for-path
  ([resolver-db path]
   (when resolver-db
     (let [k (li/split-path path)]
       (loop [db @resolver-db]
         (if-let [r (get db k)]
           r
           (when-let [p (parent-tag db)]
             (recur @p)))))))
  ([path]
   (resolver-for-path root-resolver-db path)))

(defn override-resolver
  ([resolver-db path resolver]
   (if (vector? path)
     (doseq [p path] (override-resolver p resolver))
     (do (u/call-and-set
          resolver-db
          #(assoc
            @resolver-db
            (li/split-path path) resolver))
         (r/call-resolver-on-set-path resolver nil [:override path])))
   path)
  ([path resolver]
   (override-resolver root-resolver-db path resolver)))

(defn compose-resolver
  ([resolver-db path resolver]
   (if (vector? path)
     (doseq [p path] (compose-resolver p resolver))
     (let [path (li/split-path path)
           resolvers (get @resolver-db path [])]
       (do (u/call-and-set
            resolver-db
            #(assoc
              @resolver-db path
              (conj resolvers resolver)))
           (r/call-resolver-on-set-path resolver nil [:compose path])
           path))))
  ([path resolver]
   (compose-resolver root-resolver-db path resolver)))

(def composed? (complement map?))
(def override? map?)

(def constructors (u/make-cell {}))

(defn register-resolver-type [type-name constructor]
  (u/call-and-set
   constructors
   #(assoc @constructors type-name constructor))
  constructor)

(defmacro defmake [type-name constructor]
  `(def ~(symbol "make")
     (register-resolver-type ~type-name ~constructor)))

(defn register-resolver [{n :name t :type
                          compose? :compose?
                          paths :paths config :config}]
  (if-let [c (and t (t @constructors))]
    (let [resolver (c n config)
          rf (if compose? compose-resolver override-resolver)]
      (doseq [p paths] (rf p resolver))
      n)
    (u/throw-ex (str "Invalid resolver type " t " for resolver " n))))

(defn register-resolvers [specs]
  (mapv register-resolver (su/nonils specs)))

(defn root-registry
  []
  root-resolver-db)
