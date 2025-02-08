(ns agentlang.util
  (:require [clojure.string :as string]
            [clojure.set :as set]
            [clojure.pprint :as pp]
            [clojure.walk :as walk]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.datafmt.json :as json])
  #?(:clj
     (:require [net.cgrand.macrovich :as macros])
     :cljs
     (:require-macros [net.cgrand.macrovich :as macros]
                      [agentlang.util :refer [passthru]]))
  #?(:clj
     (:import [java.io File]
              [java.net MalformedURLException]
              [org.apache.commons.io FilenameUtils]
              [org.apache.commons.exec CommandLine Executor DefaultExecutor])))

(def ^:private on-init-fns (atom []))

(defn set-on-init! [f] (swap! on-init-fns conj f))

(defn run-init-fns []
  (when-let [fns @on-init-fns]
    (doseq [f fns]
      (f))
    (reset! on-init-fns nil))
  true)

(def host-runtime #?(:clj :jvm :cljs :js))

(defn host-is-jvm? [] (= host-runtime :jvm))

(def type-key :-*-type-*-)

(def type-tag-key :type-*-tag-*-)

(defn make-record-instance [type-tag full-name attributes]
  (into {} (concat {type-tag-key type-tag
                    type-key full-name} attributes)))

(defn host-os []
  #?(:clj
     (keyword (first (string/split (string/lower-case (System/getProperty "os.name")) #" ")))
     :cljs :js))

(def ^:private script-extn (atom ".al"))

(defn set-script-extn! [extn]
  (reset! script-extn extn))

(defn get-script-extn []
  @script-extn)

(def model-script-name "model.al")

(defn file-extension [s]
  #?(:clj
     (str "." (FilenameUtils/getExtension s))
     :cljs
     (second (re-find #"(\.[a-zA-Z0-9]+)$" s))))

(defn agentlang-script? [f]
  (= @script-extn (file-extension (str f))))

(defn throw-ex
  [msg]
  #?(:clj
     (throw (Exception. msg))
     :cljs
     (let [e (js/Error. msg)]
       (println msg)
       (.log js/console (.-stack e)))))

(defn ok-result
  ([result safe]
   (let [f (if (map? result) result (first result))]
     (if (= :ok (:status f))
       (:result f)
       (let [msg (str "unexpected result: " result)]
         (if safe
           (do (log/warn msg) nil)
           (throw-ex msg))))))
  ([result] (ok-result result false)))

(defn safe-ok-result [result]
  (ok-result result true))

(macros/deftime
  (defmacro passthru
    "If the predicate function returns true for exp1, return exp1, otherwise return
  the value defined for alternative. The alternative, if provided,
  must be of the form `:else exp2`. If the alternative is not provided
  and exp1 fail the predicate test, return nil."
    [predicate exp1 & alternative]
    (when (seq alternative)
      (let [k (first alternative)]
        (when-not (= k :else)
          (throw-ex (str "Expected :else, not " k)))))
    `(let [x# ~exp1]
       (if (~predicate x#)
         x#
         ~@(rest alternative)))))

(defn uuid-string []
  #?(:clj
     (str (java.util.UUID/randomUUID))
     :cljs
     (str (random-uuid))))

(defn uuid-from-string [string]
  (if (uuid? string)
    string
    (try
      (when (seq string)
        #?(:clj
           (java.util.UUID/fromString string)
           :cljs
           (uuid string)))
      (catch #?(:clj Exception :cljs :default) _ nil))))

(defn call-safe [f arg]
  (when f (f arg)))

(defn remove-extension [^String filename]
  (let [i (.lastIndexOf filename ".")]
    (if (>= i 0)
      (.substring filename 0 i)
      filename)))

(defn capitalize
  "Convert the first character in a string to upper-case, leave the other
  characters untouched."
  ([string]
   (str (string/upper-case (.charAt string 0)) (subs string 1)))
  ([re jsep string]
   (string/join jsep (map capitalize (string/split string re)))))

(defn lowercase
  "Convert the first character in a string to lower-case, leave the other
  characters untouched."
  ([string]
   (str (string/lower-case (.charAt string 0)) (subs string 1)))
  ([re jsep string]
   (string/join jsep (map lowercase (string/split string re)))))

(defn first-applied
  "Apply each function (fn) to args, return the first truth valued result
  as [tag result]."
  [fn-tags args]
  (loop [fn-tags fn-tags]
    (let [[fn tag] [(first fn-tags) (second fn-tags)]]
      (when (and fn tag)
        (if-let [r (apply fn args)]
          [tag r]
          (recur (rest (rest fn-tags))))))))

(defn- logical-for-each
  [predics arg all?]
  (loop [predics predics, result true]
    (if-let [p (first predics)]
      (let [r (p arg)]
        (if r
          (if all?
            (recur (rest predics) r)
            r)
          (if all?
            r
            (recur (rest predics) r))))
      result)))

(defn all-true?
  "Apply each predicate to arg.
  Return true if all calls return a truth value.
  If predics is an empty sequence, return true."
  [predics arg]
  (logical-for-each predics arg true))

(defn any-true?
  "Apply each predicate to arg.
  Return true if any of the call returns a truth value.
  If predics is an empty sequence, return false."
  [predics arg]
  (logical-for-each predics arg false))

#?(:clj
   (defn n-cpu
     "Return number of CPU in the system"
     [] (.availableProcessors (Runtime/getRuntime))))

(defn make-cell
  "A common mutable place for both clj and cljs."
  ([obj]
   #?(:clj (ref obj)
      :cljs (atom obj)))
  ([] (make-cell nil)))

(defn cell? [obj]
  #?(:clj (= clojure.lang.Ref (type obj))
     :cljs (= cljs.core/Atom (type obj))))

(defn call-and-set [cell f]
  #?(:clj (dosync
           (ref-set cell (f)))
     :cljs (reset! cell (f))))

(defn safe-set
  ([cell value result]
   #?(:clj (dosync
            (ref-set cell value))
      :cljs (reset! cell value))
   result)
  ([cell value] (safe-set cell value value)))

(defn safe-set-once [cell f]
  (or @cell (safe-set cell (f))))

(defn safe-set-truth [cell f]
  (when-let [r (f)]
    (safe-set cell r)))

(defn safe-set-first [cell f]
  (let [[a b] (f)]
    (safe-set cell a b)))

(defn apply->
  "Apply args to the first function,
  apply the result to the next function and so on.
  Return the last result."
  ([selector fns args]
   (loop [fs fns, result args]
     (if-let [f (first fs)]
       (recur (rest fs)
              (apply f result))
       (selector result))))
  ([fns args] (apply-> identity fns args)))

(defn map-when [f xs]
  (mapv #(when % (f %)) xs))

(defn apply0 [f] (f))

(defn noop [])

#?(:cljs
   (do
     (def sys-env (atom {}))

     (defn setenv [k v]
       (swap! sys-env assoc k v))))

(defn getenv
  ([varname default]
   (let [val #?(:clj (or (System/getenv varname) default)
                :cljs (get @sys-env varname default))]
     (if-not (nil? val)
       val
       (throw-ex (str varname " - environment variable not set")))))
  ([varname]
   (getenv varname nil)))

(defn empty-string?
  "Return true if x is either nil or an empty string"
  [x]
  (let [s (if (string? x)
            (seq (string/trim x))
            x)]
    (or (nil? x) (nil? s))))

(def path-sep
  #?(:clj java.io.File/separator
     :cljs "/"))

(def line-sep
  #?(:clj (System/lineSeparator)
     :cljs "\n"))

(defn concat-lines [s & ss]
  (loop [ss ss, result s]
    (if-let [s (first ss)]
      (recur (rest ss) (str result line-sep s))
      result)))

(defn- x-as-keyword [x? x]
  (cond
    (x? x) (keyword x)
    (keyword? x) x
    :else nil))

(def string-as-keyword (partial x-as-keyword string?))
(def symbol-as-keyword (partial x-as-keyword symbol?))

(defn keyword-as-string [x]
  (if (keyword? x)
    (subs (str x) 1)
    x))

(defn keyword-append [k x]
  (keyword (str (subs (str k) 1) x)))

(defn objects-as-string [xs]
  (mapv #(cond
           (and (seqable? %) (not (string? %)))
           (json/encode %)

           (or (keyword? %) (symbol? %))
           (str %)

           :else %)
        xs))

(def parse-string
  #?(:clj read-string
     :cljs cljs.reader/read-string))

(defn safe-read-string [s]
  (try
    (parse-string s)
    (catch #?(:clj Exception :cljs :default) _ nil)))

(defn safe-close [obj]
  #?(:clj
     (try
       (.close obj)
       true
       (catch Exception ex
         false))
     :cljs true))

(defn pretty-spit
  [file-name edn]
  #?(:clj
     (spit file-name
           (with-out-str
             (pp/write edn :dispatch pp/code-dispatch)))
     :cljs edn))

(defn pretty-str
  ([data]
   (with-out-str
     (pp/pprint
      (walk/postwalk
       (fn [v]
         (cond
           ;; embedding vector?
           (and (vector? v)
                (seq v)
                (every? float? v))
           [(symbol (str "embedding-vector-" (count v)))]
           ;; vector or map
           (and (counted? v)
                (> (count v) 10))
           (-> (empty v)
               (into (take 10 v))
               (into (if (map? v)
                       [['...snipped... '...snipped...]]
                       ['...snipped...])))
           ;; string
           (and (string? v)
                (> (count v) 32767))
           (str (subs v 0 32768) "...snipped...")
           ;; else
           :else
           v))
       data))))
  ([label data]
   (str label " " (pretty-str data))))

(def pprint pp/pprint)

(defn trace
  "Prints `msg` and `x`. Returns `x`."
  ([msg x]
   (println msg (pr-str x))
   x)
  ([x] (trace "" x)))

(defn trace-with-fn
  "Prints `msg`, `x` and the result of `(f x)`. Returns `x`."
  [msg f x]
  (println msg (pr-str x) (pr-str (f x)))
  x)

(defn pretty-trace [x]
  (pprint x)
  x)

#?(:clj
   (do
     (defn exec-in-directory [path cmd]
       (let [^CommandLine cmd-line (CommandLine/parse cmd)
             ^Executor executor (DefaultExecutor.)]
         (.setWorkingDirectory executor (if (string? path) (File. path) path))
         (zero? (.execute executor cmd-line))))

     (defn- read-env-var-helper [x]
       (let [v
             (cond
               (symbol? x)
               (when-let [v (System/getenv (name x))]
                 (let [s (try
                           (read-string v)
                           (catch Exception _e v))]
                   (cond
                     (not= (str s) v) v
                     (symbol? s) (str s)
                     :else s)))

               (vector? x)
               (first (filter identity (mapv read-env-var-helper x)))

               :else x)]
         v))

     (defn read-env-var [x]
       (let [v (read-env-var-helper x)]
         (when (not v)
           (throw-ex (str "Environment variable " x " is not set.")))
         v))

     (defn- env-var-call? [v]
       (and (list? v) (= 'env (first v))))

     (defn- process-env-var-calls [config]
       (walk/prewalk
        #(if (map? %)
           (into {} (mapv (fn [[k v]]
                            [k (if (env-var-call? v)
                                 (let [[n default] (rest v)]
                                   (getenv (name n) default))
                                 v)])
                          %))
           %)
        config))

     (defn read-config-file [config-file]
       (let [f (io/file config-file)]
         (when-not (.exists f)
           (with-open [out (io/writer f)]
             (binding [*out* out]
               (print {:service {:port 8080}})))))
       (process-env-var-calls
        (binding [*data-readers* {'$ read-env-var}]
          (let [env-config (getenv "AGENT_CONFIG" "nil")]
            (log/debug (str "AGENT_CONFIG = " env-config))
            (merge (read-string (slurp config-file))
                   (read-string env-config))))))))

(defn strs
  ([j ss] (string/join j ss))
  ([ss] (string/join "\n" ss)))

(defn call-with-cache
  "Similar to memoize, but only non-nil values are cached."
  [f]
  (let [mem (atom {})]
    (fn [& args]
      (if-let [e (find @mem args)]
        (val e)
        (let [ret (apply f args)]
          (when-not (nil? ret) (swap! mem assoc args ret))
          ret)))))

(def get-app-uuid (memoize (fn [] (getenv "AGENTLANG_APP_UUID" (uuid-string)))))

(defn url?
  "Return true if supplied string is a URL, false otherwise."
  [u]
  #?(:clj
     (try
       (and (io/as-url u)
            true)
       (catch MalformedURLException _
         false))
     :cljs (string? u))) ; TODO: implement format check in cljs.

(defn keys-in-set?
  "Return true if a-map contains keys only from the expected-keys set.
  Not all keys are mandatory, and a-map may be empty."
  [a-map expected-keys]
  (= (set/union expected-keys (keys a-map)) expected-keys))

(defn as-agent-tools [ks]
  (mapv (fn [k] {:name (subs (str k) 1)}) ks))

(defn execute-script
  [path]
  (let [process (.exec (Runtime/getRuntime) path)]
    (io/copy (io/reader (.getInputStream process)) *out*)
    (let [exit-val (.waitFor process)]
      (log/info (str "Exit code: " exit-val)))
    (io/copy (io/reader (.getErrorStream process)) *out*)))
