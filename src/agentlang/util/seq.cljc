(ns agentlang.util.seq
  "Utilities for sequences."
  (:require [clojure.string :as s]
            [agentlang.util :as u]))

(defn truths
  "Return all truth values returned by f when applied to each element of xs."
  [f xs]
  (when (seq xs)
    (if-let [v (f (first xs))]
      (lazy-seq (cons v (truths f (rest xs))))
      (lazy-seq (truths f (rest xs))))))

(defn map-while
  "Apply f to each element in xs.
   Return false, if any of (f x) returns false for predic.
   Otherwise, return the value of (f x'), where x' is the last element of xs."
  [predic f xs]
  (loop [xs xs, r nil]
    (if (seq xs)
      (let [r (f (first xs))]
        (if (predic r)
          (recur (rest xs) r)
          false))
      r)))

(defn third [xs] (nth xs 2))

(defn- pick-by [posf xs]
  (loop [xs xs, result []]
    (if-let [x (posf xs)]
      (recur (nthrest xs 2) (conj result x))
      result)))

(def odds "Return the values at the odd positions in a sequence."
  (partial pick-by first))

(def evens "Return the values at the even positions in a sequence."
  (partial pick-by second))

(defn values [f xs]
  (loop [xs xs, result []]
    (if-let [x (first xs)]
      (if-let [r (f x)]
        (recur (rest xs) (conj result r))
        (recur (rest xs) result))
      result)))

(defn key-vals [m]
  [(keys m) (vals m)])

(defn wrap-to-map [xs]
  (into {} (mapv vec (partition 2 xs))))

(defn aconj [m tag x]
  (assoc m tag (conj (get m tag []) x)))

(defn seqs [xs]
  (filter seq xs))

(defn nonils [xs]
  (filter identity xs))

(defn all-true? [xs]
  (every? identity xs))

(defn conj-if [xs x]
  (if x (conj xs x) xs))

(defn vec-add-first [x vec]
  (apply conj [x] vec))

(defn first-val [m]
  (first (vals m)))

(defn first-truth [f xs]
  (first (nonils (map f xs))))

(defn move-all [xs target f]
  (loop [xs xs, target target]
    (if-let [x (first xs)]
      (recur (rest xs)
             (f target x))
      target)))

(defn map-mirror [m]
  (let [mm (map (fn [[k v]] [v k]) m)]
    (into {} mm)))

(defn dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
  nested structure. keys is a sequence of keys. Any empty maps that result
  will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))

(defn dissoc-nils [a-map]
  (into
   {}
   (filter (fn [[_ v]] (not (nil? v))) a-map)))

(defn contains-any [xs ys]
  "Return the first element from xs that exists also in ys.
   If no element from xs is found in ys, return nil."
  (loop [xs xs]
    (when-let [x (first xs)]
      (if (some #{x} ys)
        x
        (recur (rest xs))))))

(defn maybe-assoc [m k v]
  (if (contains? m k)
    m
    (assoc m k v)))

(defn list-or-cons? [x]
  #?(:clj
     (or (= (type x) clojure.lang.Cons)
         (list? x))
     :cljs
     (list? x)))

(defn join-as-string [xs delim]
  (loop [xs xs, s ""]
    (if-let [x (first xs)]
      (recur (rest xs)
             (str s x (when (seq (rest xs))
                        delim)))
      s)))

(defn index-of [needle haystack]
  (loop [xs haystack, i 0]
    (when-let [x (first xs)]
      (if (= needle x)
        i
        (recur (rest xs) (inc i))))))

(defn- transform-keys [predic cast a-map]
  (let [r (mapv
           (fn [[k v]]
             [(if (predic k)
                (cast k)
                k)
              (if (or (map? v)
                      #?(:clj (instance? java.util.Map v)))
                (transform-keys predic cast v)
                v)])
           a-map)]
    (into {} r)))

(def keys-as-keywords (partial transform-keys string? keyword))
(def keyword-keys-as-strings (partial transform-keys keyword? #(subs (str %) 1)))

(defn case-keys [m & options]
  (loop [opts options]
    (let [k (first opts) f (first (rest opts))]
      (cond
        (and k f)
        (if-let [v (k m)]
          (f v)
          (recur (rest (rest opts))))

        k (k m)

        :else (u/throw-ex "no default specified for case-keys")))))

(defn value-map [m]
  (into
   {}
   (filter (fn [[_ v]] (not (nil? v))) m)))

(defn remove-twins [xs]
  (loop [xs xs, prev nil, result []]
    (if-let [x (first xs)]
      (if (= x prev)
        (recur (rest xs) prev result)
        (recur (rest xs) x (conj result x)))
      result)))

(defn member? [x xs]
  (first (filter (partial = x) xs)))

(defn- char-range [lo hi]
  (range (int lo) (inc (int hi))))

(def alpha-numeric
  (map char (concat (char-range \a \z)
                    (char-range \A \Z)
                    (char-range \0 \9))))

(defn rand-alpha-numeric []
  (rand-nth alpha-numeric))

(defn generate-code [length]
  (apply str
         (take length
               (repeatedly rand-alpha-numeric))))

(defn snake-to-kebab-keys [a-map]
  (let [r (mapv (fn [[k v]]
                  [(keyword (s/replace (name k) #"_" "-")) v])
                a-map)]
    (into {} r)))

(defn flatten-map [obj]
  (if (map? obj)
    (vec (apply concat obj))
    obj))

(defn vec-as-map [xs]
  (cond
    (map? xs)  xs
    (vector? (first xs)) (into {} xs)
    :else (into {} (mapv vec (partition 2 xs)))))

(defn make-mutable-stack [] (atom []))
(defn mutable-stack-peek [s] (peek @s))
(defn mutable-stack-push! [s obj] (reset! s (conj @s obj)))
(defn mutable-stack-pop! [s]
  (let [sv @s]
    (when (seq sv)
      (reset! s (pop sv)))))
