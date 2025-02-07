(ns agentlang.paths.internal
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.lang.internal :as li]))

(def path-prefix "path:/")
(def path-prefix-len (count path-prefix))

(defn proper-path? [x]
  (and (string? x)
       (s/starts-with? x path-prefix)))

(defn path-string [s]
  (if (s/starts-with? s path-prefix)
    (subs s path-prefix-len)
    s))

(defn as-partial-path [path]
  (let [s (path-string path)
        has-child (s/ends-with? s "%")
        idx (s/last-index-of s "/")
        s1 (subs s 0 idx)]
    (if has-child
      (subs s1 0 (s/last-index-of s1 "/"))
      s1)))

(defn maybe-add-path-prefix [s]
  (if (proper-path? s)
    s
    (str path-prefix (if (= "/" (first s)) s (str "/" s)))))

(declare fully-qualified-path-type)

(defn name-from-path-component [component n]
  (let [k (fully-qualified-path-type component n)
        parts (li/split-path k)]
    (if (= 2 (count parts))
      k
      (li/make-path component k))))

(def ^:private default-path "[]")

(def path-attr-spec
  {:type :Agentlang.Kernel.Lang/String
   :default default-path
   :unique true
   :indexed true})

(defn default-path? [s]
  (= s default-path))

(defn null-path? [s]
  (if (seq s)
    (s/starts-with? s default-path)
    true))

(defn encoded-uri-path-part [entity-name]
  (let [[c n] (li/split-path entity-name)]
    (if (and c n)
      (str (name c) "$" (name n))
      (name entity-name))))

(defn decode-uri-path-part [part]
  (keyword (s/replace part "$" "/")))

(defn uri-path-split [path]
  (vec (filter seq (s/split (path-string path) #"/"))))

(defn uri-join-parts [parts]
  (maybe-add-path-prefix (s/join "/" parts)))

(defn fully-qualified-path-type [base-component n]
  (if (s/index-of n "$")
    (keyword (s/replace n "$" "/"))
    (keyword (str (name base-component) "/" n))))

(defn fully-qualified-path-value [base-component n]
  (if (s/starts-with? n ":")
    (fully-qualified-path-type base-component (subs n 1))
    n))

(defn- fully-qualified-path-component [base-component n]
  (if (s/index-of n "$")
    n
    (str (name base-component) "$" n)))

(defn as-fully-qualified-path [base-component path]
  (let [path (if (proper-path? path) (subs path path-prefix-len) path)
        fqt (partial fully-qualified-path-component base-component)]
    (loop [parts (filter seq (s/split path #"/")), n 0, final-path []]
      (if-let [p (first parts)]
        (case n
          (0 2 3) (recur (rest parts)
                         (if (= n 3) 1 (inc n))
                         (conj final-path (fqt p)))
          (recur (rest parts) (inc n) (conj final-path p)))
        (str path-prefix "/" (s/join "/" final-path))))))

(defn- parse-fully-qualified-name [n]
  (let [[c n] (s/split n #"\$")]
    (keyword (str c "/" n))))

(defn- fully-qualified-parts-as-map [parts]
  (when (seq parts)
    (let [[e id rel] parts
          r0 {:entity [(parse-fully-qualified-name e) id]}]
      (if rel
        (assoc
         r0 li/rel-tag
         [(parse-fully-qualified-name rel)
          (fully-qualified-parts-as-map (nthrest parts 3))])
        r0))))

(defn parse-fully-qualified-path [path]
  (when (proper-path? path)
    (let [path (if (s/ends-with? path "%")
                 (subs path 0 (dec (count path)))
                 path)]
      (fully-qualified-parts-as-map (s/split (subs path (inc path-prefix-len)) #"/")))))

(defn flatten-fully-qualified-path [path]
  (when-let [p (parse-fully-qualified-path path)]
    (loop [p p, f []]
      (if p
        (let [e (:entity p), [r p1] (li/rel-tag p)]
          (if p1
            (recur p1 (concat f e (when r [r])))
            (vec (concat f e))))
        (when (seq f) (vec f))))))

(defn full-path-name? [n]
  (s/index-of (str n) "/"))

(defn ref-path-name? [n]
  (s/index-of (str n) "."))

(defn normalize-path [p]
  (s/replace p "//" "/"))

(defn as-wildcard-path [path]
  (if (s/ends-with? path "%")
    path
    (let [idx (s/last-index-of path "/")
          s0 (subs path 0 idx)]
      (str s0 "/%"))))

(defn parse-any-path [base-component path]
  (parse-fully-qualified-path
   (as-fully-qualified-path base-component path)))

(defn find-parent-id [parsed-path]
  (loop [parsed-path parsed-path]
    (let [[e id] (:entity parsed-path)]
      (if-let [[_ r0] (:-> parsed-path)]
        (if (:-> r0)
          (recur r0)
          id)
        id))))
