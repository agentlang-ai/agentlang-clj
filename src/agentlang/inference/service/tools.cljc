(ns agentlang.inference.service.tools
  (:require [clojure.string :as s]
            [agentlang.util :as u]
            [agentlang.util.seq :as us]
            [agentlang.component :as cn]
            [agentlang.datafmt.json :as json]
            [agentlang.lang.raw :as raw]
            [agentlang.lang.internal :as li]
            [agentlang.lang.kernel :as k]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])))

(defn- record-name-as-function-name [rec-name]
  (let [rec-name (li/make-path rec-name)]
    (s/replace (s/replace (subs (str rec-name) 1) "." "__p__") "/" "__")))

(defn- function-name-as-record-name [fname]
  (keyword (s/replace (s/replace fname "__p__" ".") "__" "/")))

(def ^:private string-types #{"uuid" "datetime" "email" "date" "time"})
(def ^:private number-types #{"double" "float" "decimal" "int" "int64" "biginteger"})

(defn- find-root-type [attr-type]
  (let [s
        (case attr-type
          (:Now :Identity) "string"
          :Edn "object"
          (cond
            (k/plain-kernel-type? attr-type)
            (s/lower-case (name attr-type))

            (k/kernel-type? attr-type)
            (s/lower-case (name (second (li/split-path attr-type))))

            (cn/find-schema attr-type) "object"

            :else nil))]
    ;; TODO: check the root-type keyword and return the appropriate type.
    ;; Do not compare strings.
    (cond
      (some #{s} string-types) "string"
      (some #{s} number-types) "number"
      :else s)))

(defn- as-tool-type [attr-type]
  (let [is-map (map? attr-type)
        spec
        (if is-map
          (if-let [xs (:oneof attr-type)]
            (cond
              (or (string? (first xs)) (keyword? xs))
              {:type "string" :enum (mapv name xs)}

              (number? (first xs))
              {:type "number" :enum xs}

              :else (u/throw-ex (str "cannot handle enum type for: " attr-type)))
            (if (:listof attr-type)
              {:type "object"}
              {:type (find-root-type (:type attr-type))}))
          {:type (find-root-type attr-type)})
        required (cond
                   is-map
                   (not (or (:optional attr-type) (:default attr-type) (:read-only attr-type)))

                   (= :Identity attr-type)
                   false

                   :else true)]
    [spec required]))

(defn- attribute-to-property [event-name [attr-name attr-type]]
  (let [[tool-type required] (as-tool-type attr-type)]
    (when-not (:type tool-type)
      (u/throw-ex (str "cannot translate "
                       [attr-name attr-type] " of " event-name
                       " to an appropriate tool-type")))
    [(name attr-name) tool-type required]))

(def ^:private tool-cache (atom nil))

(defn- record-to-tool
  ([find-schema rec-name docstring]
   (or (get @tool-cache rec-name)
       (when-let [tool-spec
                  (if-let [scm (find-schema rec-name)]
                    (let [tool-name (record-name-as-function-name rec-name)
                          props (mapv (partial attribute-to-property rec-name) (dissoc scm :meta))]
                      {:type "function"
                       :function
                       {:name tool-name
                        :description (or docstring (cn/docstring rec-name) tool-name)
                        :parameters
                        {:type "object"
                         :properties (into {} (mapv (comp vec (partial take 2)) props))
                         :required (vec (mapv first (filter last props)))}}})
                    (do
                      (log/warn (str "cannot generate tool, no schema found for - " rec-name))
                      nil))]
         (swap! tool-cache assoc rec-name tool-spec)
         tool-spec)))
  ([find-schema rec-name] (record-to-tool find-schema rec-name nil)))

(def event-to-tool (partial record-to-tool raw/find-event))

(defn entity-to-tool [entity-name]
  (record-to-tool raw/find-entity entity-name (str "Create an instance of " entity-name)))

(defn all-tools-for-component [component]
  (let [component (if (string? component) (keyword component) component)
        event-tools (mapv event-to-tool (cn/event-names component))
        entity-tools (mapv entity-to-tool (cn/entity-names component))]
    (vec (us/nonils (concat event-tools entity-tools)))))

(defn- maybe-dissoc-attributes-with-defaults [recname attrs]
  (if-let [scm (or (raw/find-event recname) (raw/find-entity recname))]
    (if-let [anames (seq (map first (filter (fn [[k v]] (or (= v :Now) (= v :Identity))) scm)))]
      (apply dissoc attrs anames)
      attrs)
    attrs))

(defn tool-call-to-pattern [tool-call]
  (if-let [{fname "name" args "arguments"} (get tool-call "function")]
    (let [recname (function-name-as-record-name fname)
          attrs (maybe-dissoc-attributes-with-defaults recname (json/decode args))]
      {recname attrs})
    (u/throw-ex (str "Invalid tool-call: " tool-call))))

(defn- maybe-merge-parent-attrs [spec]
  (dissoc
   (if-let [parent (get-in spec [:meta :inherits])]
     (merge
      (cond
        (cn/entity? parent) (raw/find-entity parent)
        (cn/event? parent) (raw/find-event parent)
        :else (raw/find-record parent))
      spec)
     spec)
   :meta :rbac :ui))

(defn- normalize-attributes [attrs]
  (let [xs (mapv (fn [[k v]]
                   [k (if (keyword? v)
                        (let [[c n] (li/split-path v)]
                          (if (and c n) ; (= c :Agentlang.Kernel.Lang)
                            n
                            v))
                        v)])
                 attrs)]
    (into {} xs)))

(defn- force-find-spec [n]
  (when-let [[tag obj] (cn/find-schema n)]
    (let [scm (:schema obj)]
      (normalize-attributes
       (if (= :event tag)
         (dissoc scm :EventContext)
         scm)))))

(defn- prop-as-string [k v]
  (if (= k :type)
    (u/keyword-as-string v)
    (pr-str v)))

(defn- attr-spec-as-string [spec]
  (cond
    (keyword? spec) (name spec)
    (map? spec) (s/join ", " (mapv (fn [[k v]] (str (name k) ": " (prop-as-string k v))) spec))
    :else (pr-str spec)))

(defn- raw-tool-as-markdown [tag n spec doc]
  (str "** " tag " " (u/keyword-as-string n) " **"
       (when doc (str "\n*" doc "*"))
       "\n"
       (s/join "\n" (mapv (fn [[k v]] (str "  - *" (name k) "* " (attr-spec-as-string v))) spec))
       "\n"))

(defn- raw-tool-as-expr [tag n spec doc]
  (let [expr (u/pretty-str `(~tag ~n ~spec))]
    (if (seq doc)
      (str ";; " doc "\n" expr)
      expr)))

  (def ^:dynamic documentation-mode nil)
(defn- raw-tool [tag find-spec n]
  (when-let [spec (maybe-merge-parent-attrs (or (find-spec n) (force-find-spec n)))]
    (let [doc (cn/docstring n)]
      ((if (= documentation-mode :markdown)
         raw-tool-as-markdown
         raw-tool-as-expr)
       tag n spec doc))))

(def ^:private raw-event-tool (partial raw-tool 'event raw/find-event))
(def ^:private raw-entity-tool (partial raw-tool 'entity raw/find-entity))
(def ^:private raw-record-tool (partial raw-tool 'record raw/find-record))
(def ^:private raw-relationship-tool (partial raw-tool 'relationship raw/find-relationship))

(defn- raw-tools [raw-tool rec-names]
  (s/join
   "\n"
   (filter seq (mapv raw-tool rec-names))))

(def ^:private raw-event-tools (partial raw-tools raw-event-tool))
(def ^:private raw-entity-tools (partial raw-tools raw-entity-tool))
(def ^:private raw-record-tools (partial raw-tools raw-record-tool))
(def ^:private raw-relationship-tools (partial raw-tools raw-relationship-tool))

(defn raw-components [components]
  (let [tools
        (mapv (fn [component]
                (let [component (if (string? component) (keyword component) component)
                      event-tools (raw-event-tools (cn/event-names component))
                      entity-tools (raw-entity-tools (cn/entity-names component))
                      rel-tools (raw-relationship-tools (cn/relationship-names component))
                      record-tools (raw-record-tools (cn/record-names component))]
                  (str event-tools "\n" entity-tools "\n" rel-tools "\n" record-tools)))
              components)]
    (str (s/join "\n" tools) "\n")))

(defn as-raw-tools [names]
  (let [tools (filter
               seq
               (mapv (fn [n]
                       (cond
                         (cn/relationship? n) (raw-relationship-tool n)
                         (cn/entity? n) (raw-entity-tool n)
                         (cn/event? n) (raw-event-tool n)
                         :else (raw-record-tool n)))
                     names))]
    (str (s/join "\n" tools) "\n")))

(defn documentation [tools tool-components]
  (binding [documentation-mode :markdown]
    (str (if (seq tool-components)
           (raw-components (mapv u/string-as-keyword tool-components))
           "")
         (if (seq tools)
           (as-raw-tools (mapv #(u/string-as-keyword (:name %)) tools))
           ""))))
