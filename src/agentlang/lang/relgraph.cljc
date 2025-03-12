(ns agentlang.lang.relgraph
  "Traversal of the schema/instance graph as inferred from `contains` relationships"
  (:require [clojure.set :as set]
            [agentlang.component :as cn]
            [agentlang.meta :as mt]
            [agentlang.util :as u]
            [agentlang.util.seq :as su]
            [agentlang.lang.internal :as li]))

(defn- component-name? [obj]
  (and (li/name? obj)
       (cn/component-exists? obj)))

(defn- path? [obj]
  (and (map? obj)
       (let [ks (keys obj)]
         (and (= 1 (count ks))
              (li/name? (first ks))))))

(def ^:private roots-tag :-*-roots-*-)
(def ^:private paths-tag :-*-paths-*-)
(def ^:private back-link-tag :-*-back-link-*-)

(def get-roots roots-tag)

(defn- attach-roots [graph]
  (assoc
   graph
   roots-tag
   (loop [g graph, result (set (keys graph))]
     (if-let [[k vs] (first g)]
       (if (seq vs)
         (recur
          (rest g)
          (set/difference
           result (set (filter (partial not= k) (mapv :to vs)))))
         (recur (rest g) result))
       result))))

(defn- as-node [[rel-name rel-type child-entity]]
  {:type rel-type
   :relationship rel-name
   :to child-entity})

(defn- do-build-graph [entity-names]
  (let [rooted-graph (attach-roots
                      (reduce
                       (fn [graph entity-name]
                         (let [children (mapv as-node (cn/contained-children entity-name))
                               existing-children (entity-name graph)]
                           (assoc graph entity-name (vec (concat existing-children children)))))
                       {} entity-names))]
    (reduce (fn [graph entity-name]
              (let [between-rels (mapv as-node (cn/between-relationships entity-name))
                    existing-children (entity-name graph)]
                (assoc graph entity-name (vec (concat existing-children between-rels)))))
           rooted-graph entity-names)))

(def roots identity)

(defn paths [graph root-node]
  (when-let [ps (root-node graph)]
    {paths-tag ps back-link-tag [root-node graph]}))

(defn- paths-source-graph [paths]
  (second (back-link-tag paths)))

(defn- paths-rep [obj]
  (when-let [ps (paths-tag obj)]
    (set (mapv :relationship ps))))

(defn descend [paths rel-name]
  (when-let [path (first (filter #(= rel-name (:relationship %))
                                 (paths-tag paths)))]
    (assoc (paths-source-graph paths)
           roots-tag (set [(:to path)]))))

(defn node-object [paths rel-name]
  (when-let [ps (paths-tag paths)]
    (first (filter #(= rel-name (:relationship %)) ps))))

(defn rep [obj]
  (if-let [rts (roots-tag obj)]
    rts
    (paths-rep obj)))

(defn build-graph [root]
  (when (component-name? root)
    (let [enames (cn/user-entity-names root)]
      (do-build-graph
       (seq
        (filter
         #(and (not (cn/relationship? %))
               (not (cn/meta-entity-for-any? enames %)))
         enames))))))

(defn- lookup-relations [relname relinsts other-entity-name]
  #_(let [[c pe :as parent-entity] (li/split-path other-entity-name)
        lookupevt-name (keyword (str (name c) "/" cn/lookup-internal-event-prefix-s "_" (name pe)))]
    (mapv
     (fn [relinst]
       (let [p-rel-attr (cn/attribute-in-relationship relname parent-entity)
             p-id-attr (cn/identity-attribute-name parent-entity)
             p-attr (if (= p-rel-attr p-id-attr) pe (cn/relationship-member-identity pe))
             p-id-val (p-attr relinst)]
         (first (ev/safe-eval {lookupevt-name {p-id-attr p-id-val}}))))
     relinsts)))

(defn- find-instance-contains-rels [contains-lookup instance]
  #_(let [[_ e :as inst-type] (li/split-path (cn/instance-type instance))
        entity-name (li/make-path inst-type)]
    (when-let [parent-rels (seq (contains-lookup entity-name))]
      (mapv
       (fn [p]
         (let [relname (cn/relinfo-name p)
               inst-rel-attr (cn/attribute-in-relationship relname entity-name)
               inst-rel-id (inst-rel-attr instance)
               qattr (keyword (str (name e) "?"))]
           [p
            (when-let [relinsts (seq (ev/safe-eval-pattern {relname {qattr inst-rel-id}}))]
              (lookup-relations relname relinsts (cn/relinfo-to p)))]))
       parent-rels))))

(def find-parents (partial find-instance-contains-rels cn/containing-parents))
(def find-children (partial find-instance-contains-rels cn/contained-children))

(defn find-connected-nodes [relname entity-name entity-instance]
  #_(let [meta (cn/fetch-meta relname)
        [e1 e2] (or (mt/contains meta) (mt/between meta))
        other-entity (if (= entity-name e1) e2 e1)
        inst-rel-attr (cn/attribute-in-relationship relname entity-name)
        inst-rel-id (inst-rel-attr entity-instance)
        qattr (keyword (str (name (second (li/split-path entity-name))) "?"))]
    (when-let [relinsts (seq (ev/safe-eval-pattern {relname {qattr inst-rel-id}}))]
      (lookup-relations relname relinsts other-entity))))
