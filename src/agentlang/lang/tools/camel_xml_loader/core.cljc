(ns agentlang.lang.tools.camel-xml-loader.core
  "Loader for Apache Camel XML route templates into Agentlang store"
  (:require [clojure.string :as str]
            [clojure.data.xml :as xml]
            [agentlang.lang :as ln]
            #?(:clj [clojure.java.io :as io])
            #?(:clj [agentlang.util.logger :as log])))

#?(:clj
   (do
     (defn- parse-xml-from-string
       "Parse XML directly from a string containing XML content"
       [xml-string]
       (-> xml-string
           (.getBytes)
           (java.io.ByteArrayInputStream.)
           (xml/parse)))

     (defn- parse-xml-from-file
       "Parse XML from a file path or File object"
       [file]
       (with-open [in (io/input-stream (if (string? file)
                                         (io/file file)
                                         file))]
         (xml/parse in)))

     (defn- is-likely-xml-content?
       "Determine if a string is likely XML content rather than a file path"
       [s]
       (let [trimmed (.trim s)]
         (or (.startsWith trimmed "<")
             (.startsWith trimmed "<?xml"))))

     (defn parse-xml
       "Parse XML from a string, file path, File, or InputStream"
       [input]
       (try
        (cond
          (string? input)
          (if (and (try (.exists (io/file input)) (catch Exception _ false))
                   (not (is-likely-xml-content? input)))
            (parse-xml-from-file input)
            (parse-xml-from-string input))

          ;; Handle InputStream
          (instance? java.io.InputStream input)
          (xml/parse input)

          ;; Handle File
          (instance? java.io.File input)
          (parse-xml-from-file input)

          :else
          (throw (IllegalArgumentException. "Input must be a string, file path, File, or InputStream")))
        (catch Exception e
          (throw (ex-info "Failed to parse XML"
                          {:input-type (type input)
                           :cause (.getMessage e)}
                          e))))))

   :cljs
   (defn parse-xml
     "Parse XML from a string"
     [input]
     (try
       (xml/parse-str input)
       (catch :default e
         (throw (ex-info "Failed to parse XML"
                        {:input-type (type input)
                         :cause (.-message e)}
                        e))))))

(defn extract-route-templates
  "Extract route templates and their parameters from parsed XML"
  [parsed-xml]
  (letfn [(tag-name [tag]
            (cond
              (keyword? tag) (name tag)
              (string? tag) tag
              :else (str tag)))

          (tag-contains? [node tag-fragment]
            (when (map? node)
             (let [node-tag (tag-name (:tag node))]
               (str/includes? node-tag tag-fragment))))]

    ;; Find template container or use root if it contains templates directly
    (let [templates (if (tag-contains? parsed-xml "routeTemplates")
                      parsed-xml
                      nil)

          ;; Extract all route template nodes
          template-nodes (if templates
                           (filter #(and (map? %)
                                         (tag-contains? % "routeTemplate"))
                                   (:content templates))
                           [])]

      ;; Process each template node to extract parameters
      (reduce
        (fn [result template-node]
          (let [template-id (get-in template-node [:attrs :id])

                ;; Find parameter nodes
                param-nodes (filter #(and (map? %)
                                          (tag-contains? % "templateParameter"))
                                    (:content template-node))

                ;; Extract parameters
                params (reduce
                         (fn [param-map param-node]
                           (let [param-name (-> (get-in param-node [:attrs :name])
                                               keyword)
                                 default-value (get-in param-node [:attrs :defaultValue])
                                 param-def (if default-value
                                             {:type :String :default default-value}
                                             :String)]
                             (assoc param-map param-name param-def)))
                         {}
                         param-nodes)]

            ;; Add template with its parameters to result map
            (assoc result (keyword template-id) params)))
        {}
        template-nodes))))


(defn- define-event
  "Define an AgentLang event from template name and parameters"
  [event-name template-params]
  (let [ek (if (keyword? event-name)
             event-name
             (keyword event-name))
        component-name (keyword (namespace event-name))
        ep (-> template-params
               (assoc :SleepMillis {:type :Int :default 10000}
                      :BeanValues  {:type :Map :default {}}))]
    #?(:clj (log/debug (str "Defining event" ek "with params" ep))
       :cljs (js/console.debug "Defining event" ek "with params" ep))
    (println "The component name is: " component-name)
    (ln/component component-name)
    (ln/event ek ep)))

(defn register-xml-templates
  "Register all route templates in XML as AgentLang events

   Input can be:
   - XML string containing route templates (clj/cljs)
   - Path to an XML file (clj only)
   - java.io.File or InputStream (clj only)

   Returns a map of {template-id params} for all registered templates"
  [input]
  (try
    (let [parsed-xml (parse-xml input)
          templates (extract-route-templates parsed-xml)]

      ;; Register each template as an event
      (doseq [[template-id params] templates]
        #?(:clj (log/info (str "Registering template as event:" template-id))
           :cljs (js/console.log "Registering template as event:" template-id))
        (define-event template-id params))

      ;; Return all templates
      templates)
    (catch #?(:clj Exception :cljs js/Error) e
      #?(:clj (log/error (str "Failed to parse XML template " e))
         :cljs (js/console.error "Failed to parse XML template:" e))
      (throw (ex-info "Failed to register XML templates"
                      {:cause #?(:clj (.getMessage e) :cljs (.-message e))}
                      e)))))

(defn register-template
  "Register a specific route template as an AgentLang event

   Parameters:
   - xml-input: XML string, file path (clj), File or InputStream (clj)
   - template-id: ID of the template to register

   Returns a map with :success, :template-id, and :params keys or :error on failure"
  [xml-input template-id]
  (try
    (let [parsed-xml (parse-xml xml-input)
          templates (extract-route-templates parsed-xml)
          template-key (if (keyword? template-id)
                         template-id
                         (keyword template-id))
          params (get templates template-key)]

      (if params
        (do
          #?(:clj (log/info (str "Registering template as event:" template-key))
             :cljs (js/console.log "Registering template as event:" template-key))
          (define-event template-key params)
          {:success true :template-id template-key :params params})
        (let [error-msg (str "Template ID not found in XML: " template-key)]
          #?(:clj (log/warn error-msg)
             :cljs (js/console.warn error-msg))
          {:success false :error error-msg})))
    (catch #?(:clj Exception :cljs js/Error) e
      (let [error-msg #?(:clj (.getMessage e) :cljs (.-message e))]
       #?(:clj (log/error (str "Failed to register template " e))
          :cljs (js/console.error "Failed to register template:" e))
       {:success false
        :error error-msg}))))
