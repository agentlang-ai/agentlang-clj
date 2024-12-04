(ns agentlang.lang.tools.build
  "Compile a agentlang-model to a Java binary package"
  (:require [camel-snake-kebab.core :as csk]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as s]
            [clojure.walk :as w]
            [agentlang.global-state :as gs]
            [agentlang.lang.tools.build.client :as cl]
            [agentlang.lang.tools.loader :as loader]
            [agentlang.lang.tools.util :as tu]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [clj-jgit.porcelain :as git]
            [agentlang.util.seq :as su]
            [agentlang.component :as cn])
  (:import (java.io File)
           (org.apache.commons.io FileUtils)
           (org.apache.commons.io.filefilter IOFileFilter WildcardFileFilter)))

(def out-dir "out")
(def ^:private out-file (File. out-dir))

(def ^:private component-id-var "__COMPONENT_ID__")
(def ^:private model-id-var "__MODEL_ID__")

(def ^:private logback-xml
  "<?xml version=\"1.0\"?>
<configuration>
  <appender name=\"ROLLING\" class=\"ch.qos.logback.core.rolling.RollingFileAppender\">
    <file>logs/$app-version.log</file>
    <rollingPolicy class=\"ch.qos.logback.core.rolling.SizeAndTimeBasedRollingPolicy\">
      <fileNamePattern>logs/$app-version-%d{yyyy-MM-dd}.%i.log</fileNamePattern>
      <maxFileSize>20MB</maxFileSize>
      <maxHistory>30</maxHistory>
      <totalSizeCap>1GB</totalSizeCap>
    </rollingPolicy>
    <encoder>
      <pattern>%d{HH:mm:ss.SSS} %-5level %logger{36} - %msg%n</pattern>
    </encoder>
  </appender>
  <root level=\"INFO\">
    <appender-ref ref=\"ROLLING\" />
  </root>
</configuration>")

(defn- make-log-config [model-name model-version]
  (s/replace logback-xml "$app-version" (str model-name "-" model-version)))

(defn- fetch-agentlang-version [model]
  (when-let [v (:agentlang-version model)]
    (if (= v "current")
      (gs/agentlang-version)
      v)))

(defn- model-version [model]
  (or (:version model) "0.0.1"))

(defn- as-path [s]
  (s/replace s #"[\.\-_]" u/path-sep))

(defn- sanitize [s]
  (s/replace s "-" "_"))

(defn project-dir [model-name]
  (str out-dir u/path-sep model-name u/path-sep))

(defn standalone-jar [model-name]
  (try
    (let [^File dir (File. (str (project-dir model-name) u/path-sep "target"))
          ^IOFileFilter ff (WildcardFileFilter. "*standalone*.jar")
          files (FileUtils/iterateFiles dir ff nil)]
      (when-let [rs (first (iterator-seq files))]
        (str rs)))
    (catch Exception ex
      (log/warn (str "standalone-jar - " (.getMessage ex)))
      nil)))

(defn- make-writer
  ([prefix]
   (fn [file-name contents & options]
     (let [f (File. (if prefix (str prefix file-name) file-name))]
       (FileUtils/createParentDirectories f)
       (with-open [w (io/writer f)]
         (cond
           (some #{:spit} options)
           (spit w contents)

           (some #{:write-each} options)
           (doseq [exp contents]
             (pprint/pprint exp w))

           :else
           (pprint/pprint contents w))))))
  ([] (make-writer nil)))

(defn- client-path [model-name]
  (let [path (str (project-dir model-name) "client" u/path-sep)
        f (File. path)]
    (FileUtils/createParentDirectories f)
    (.mkdir f)
    path))

(defn- clj-io [model-name]
  (let [prefix (project-dir model-name)]
    [#(read-string (slurp (str prefix %)))
     (make-writer prefix)]))

(defn- agentlang-deps-as-clj-deps [deps]
  (when (seq deps)
    (su/nonils
     (mapv #(if (vector? %) ; lein dependency of the form [:project-name "version"]
              %
              (when (map? %)
                (let [model-name (get % :name)
                      pkg-name (csk/->snake_case_string model-name)
                      version (get % :version)]
                  [(symbol pkg-name) (or version "0.0.1")])))
           deps))))

(defn- maybe-add-repos [proj-spec model]
  (if-let [repos (:repositories model)]
    (conj proj-spec :repositories repos)
    proj-spec))

(defn- normalize-model-name [model-name]
  (s/replace model-name #"[\-_]" "."))

(defn- model-name-as-string [model-name]
  (cond
    (keyword? model-name)
    (s/lower-case (subs (str model-name) 1))

    (string? model-name)
    (normalize-model-name model-name)

    :else (u/throw-ex (str "invalid model name " model-name ", must be keyword or string"))))

(defn- create-clj-project [model-dir-name model]
  (let [model-name (model-name-as-string (:name model))
        ns-name (symbol (str model-name ".modelmain"))
        deps (vec
              (concat
               [['com.github.agentlang-ai/agentlang (fetch-agentlang-version model)]]
               (agentlang-deps-as-clj-deps (:dependencies model))))
        spec0 `(~'defproject ~(symbol model-name) ~(model-version model)
                :dependencies ~deps
                :aot :all
                :main ~ns-name)
        project-spec (maybe-add-repos spec0 model)
        w (make-writer)]
    (w (str out-file u/path-sep model-dir-name u/path-sep "project.clj") project-spec)
    model-dir-name))

(defn- exec-for-model [model-name cmd]
  (let [f (partial u/exec-in-directory (project-dir model-name))]
    (if (string? cmd)
      (f cmd)
      ;; else, a vector of commands
      (every? f cmd))))

(defn- find-component-declaration [component]
  (let [f (first component)]
    (when (= 'component (first f))
      f)))

(defn- write-component-clj [component-name write component]
  (let [parts (s/split (str component-name) #"\.")
        compname (last parts)
        dirs (butlast parts)
        file-name
        (str
         "src" u/path-sep
         (s/join u/path-sep (concat dirs [(str compname ".cljc")])))]
    (write file-name component :write-each)
    component-name))

(defn- var-name [defexp]
  (first (filter symbol? defexp)))

(defn- rewrite-in-decl [ns-name local-defs full-defs-map decl]
  (w/prewalk
   #(if (and (symbol? %)
             (some #{%} local-defs))
      (get full-defs-map % %)
      %)
   decl))

(def ^:private clj-defs #{'def 'defn 'defn-})
(def ^:private agentlang-defs #{'entity 'dataflow 'event 'record 'relationship
                                'view 'attribute 'rule 'inference 'resolver 'pattern})

(defn- update-local-defs [ns-name component]
  (let [local-defs (set
                    (mapv
                     #(var-name (rest %))
                     (filter #(and (seqable? %)
                                   (some #{(first %)} clj-defs))
                             component)))
        updated-defs (into {} (mapv (fn [d] [d (symbol (str ns-name "/" d))]) local-defs))
        rw (partial rewrite-in-decl ns-name local-defs updated-defs)]
    (mapv
     #(if (and (seqable? %)
               (some #{(first %)} agentlang-defs))
        (rw %)
        %)
     component)))

(def ^:private lang-vars (vec (conj agentlang-defs 'component)))

(defn- model-refs-to-use [model-name refs]
  (let [spec (mapv
              (fn [r]
                (let [ss (s/split (s/lower-case (name r)) #"\.")
                      cid (symbol (str (s/replace (name r) "." "_") "_" component-id-var))]
                  (if (= 1 (count ss))
                    [(symbol (first ss)) :refer [cid]]
                    [(symbol (s/join "." ss)) :refer [cid]])))
              refs)
        deps (if (= "agentlang" model-name)
               [['agentlang.lang :refer lang-vars]]
               [['agentlang.model]
                ['agentlang.inference.service.model]
                ['agentlang.lang :refer lang-vars]])]
    (concat spec deps)))

(defn- merge-use-models [import-spec use-models]
  (loop [spec import-spec, result [], merged false]
    (if merged
      (concat result spec)
      (if-let [s (first spec)]
        (let [m (= :require (first s))]
          (recur (rest spec)
                 (if m
                   (conj result (concat s use-models))
                   (conj result s)) m))
        (conj result `(:require ~@use-models))))))

(defn- normalize-clj-imports [spec]
  (if (= 'quote (first spec))
    (second spec)
    spec))

(defn- verify-component-name [model-name cn]
  (when-not (s/starts-with? (s/lower-case (subs (str cn) 1)) model-name)
    (u/throw-ex (str "component name " cn " must start with the model prefix " model-name)))
  cn)

(defn- copy-component [write model-name component]
  (if-let [component-decl (find-component-declaration component)]
    (let [component-name (verify-component-name model-name (second component-decl))
          component-spec (when (> (count component-decl) 2)
                           (nth component-decl 2))
          ns-name (symbol (s/lower-case (name component-name)))
          use-models (model-refs-to-use model-name (:refer component-spec))
          clj-imports (merge-use-models
                       (normalize-clj-imports (:clj-import component-spec))
                       use-models)
          ns-decl `(~(symbol "ns") ~ns-name ~@clj-imports)
          exps (concat
                [ns-decl]
                (update-local-defs ns-name component)
                [`(def ~(symbol (str (s/replace (name component-name) "." "_") "_" component-id-var)) ~(u/uuid-string))])]
      (if write
        (write-component-clj ns-name write exps)
        (binding [*ns* *ns*] (doseq [exp exps] (eval exp)))))
    (u/throw-ex "no component declaration found")))

(defn- write-model-clj [write component-names model]
  (let [model-name (model-name-as-string (:name model))
        s-model-name (str model-name)
        root-ns-name (symbol (str s-model-name ".model"))
        req-comp (mapv (fn [c] [c :as (symbol (name c))]) component-names)
        ns-decl `(~'ns ~root-ns-name
                  (:require ~@req-comp))
        model (dissoc model :repositories :dependencies)
        model-path (s/join u/path-sep (s/split s-model-name #"\."))]
    (write (str "src" u/path-sep model-path u/path-sep "model.cljc")
           [ns-decl (if (map? model) `(agentlang.lang/model ~model) model)
            `(def ~(symbol (str (s/replace (name model-name) "." "_") "_" model-id-var)) ~(u/uuid-string))]
           :write-each)
    [s-model-name model-path root-ns-name]))

(defn- write-core-clj [write s-model-name model-path model-ns]
  (let [req-comp [['agentlang.core :as 'agentlang]
                  [model-ns :as model-ns]]
        root-ns-name (symbol (str s-model-name ".modelmain"))
        ns-decl `(~'ns ~root-ns-name
                  (:require ~@req-comp)
                  (:gen-class))]
    (write (str "src" u/path-sep model-path u/path-sep "modelmain.cljc")
           [ns-decl
            `(~'defn ~'-main [& ~'args] (apply ~'agentlang/-main ~'args))]
           :write-each)))

(def ^:private config-edn "config.edn")

(defn- write-config-edn [model-root write]
  (let [src-cfg (str model-root u/path-sep config-edn)]
    (when (.exists (File. src-cfg))
      (let [cfg (u/read-config-file src-cfg)]
        (write config-edn cfg :spit)
        cfg))))

(defn- create-client-project [orig-model-name model-name ver agentlang-ver app-config]
  (let [build-type (if (:service (:authentication app-config))
                     'prod
                     'dev)]
    (cl/build-project
     model-name ver agentlang-ver
     (client-path orig-model-name) build-type)))

(defn- build-clj-project [orig-model-name model-root model components]
  (if (create-clj-project orig-model-name model)
    (let [[rd wr] (clj-io orig-model-name)
          ver (model-version model)
          agentlang-ver (fetch-agentlang-version model)
          log-config (make-log-config orig-model-name ver)]
      (wr "logback.xml" log-config :spit)
      (let [model-name (model-name-as-string (:name model))
            cmps (mapv (partial copy-component wr model-name) components)]
        (let [rs (write-model-clj wr cmps model)]
          (apply write-core-clj wr rs))
        (create-client-project orig-model-name model-name ver agentlang-ver (write-config-edn model-root wr))))
    (log/error (str "failed to create clj project for " orig-model-name))))

(defn- normalize-deps-spec [deps]
  (map (fn [elem]
         (mapv (fn [[k v]]
                 {k v}) elem)) deps))

(declare install-model)

(defn- load-clj-project [model-name _ components]
  (let [f (partial copy-component nil model-name)]
    (doseq [c components]
      (f c))
    model-name))

(defn- get-proper-model-name [model-name spec]
  (if spec
    (let [repo-name (get-in spec [:source :repo])]
      (if repo-name
        repo-name
        (csk/->snake_case_string model-name)))
    (csk/->snake_case_string model-name)))

(defn- install-local-dependencies! [model-paths deps]
  (doseq [[model-name type version spec :as d] (normalize-deps-spec deps)]
    (when (= :agentlang-model (get type :type))
      (when spec (tu/maybe-clone-model spec model-paths)))
    (when-not (install-model model-paths (get-proper-model-name model-name spec))
      (u/throw-ex (str "failed to install dependency " d)))))

(defn- clj-project-path [model-paths model-name]
  (first
   (su/truths
    #(let [dir (str % u/path-sep model-name)
           ^File f (File. (str dir u/path-sep "project.clj"))]
       (when (.exists f)
         dir))
    model-paths)))

(defn compiled-model?
  ([model-path model-name]
   (let [model-path (if (= model-path ".")
                      (System/getProperty "user.dir")
                      model-path)]
     (if (nil? model-path)
       (clj-project-path (tu/get-system-model-paths) model-name)
       (let [^File f (File. (str model-path u/path-sep "project.clj"))]
         (when (.exists f)
           model-path))))))

(defn- check-local-dependency? [deps]
  (boolean (some #(= (:type %) :agentlang-model) deps)))

(defn build-model
  ([model-paths model-name model-info]
   (let [{model-paths :paths model :model model-root :root :as rs}
         (loader/load-all-model-info model-paths model-name model-info)
         s-model-name (model-name-as-string (:name model))
         model-name (or model-name s-model-name)
         result [model model-root]
         fvers (fetch-agentlang-version model)
         orig-model-name model-name
         model-name (normalize-model-name model-name)
         projdir (File. (project-dir orig-model-name))]
     (when-not (= model-name s-model-name)
       (u/throw-ex (str "model-name must match directory name - " orig-model-name " <> " (:name model))))
     (when-not (= fvers (gs/agentlang-version))
       (u/throw-ex (str "runtime version mismatch - required " fvers ", found " (gs/agentlang-version))))
     (if-let [path (clj-project-path model-paths orig-model-name)]
       (let [^File f (File. path)]
         (FileUtils/createParentDirectories f)
         (FileUtils/copyDirectory f projdir)
         [model-name path])
       (let [components (loader/read-components-from-model model model-root)
             model-dependencies (:dependencies model)]
         (when (check-local-dependency? model-dependencies)
           (install-local-dependencies! model-paths model-dependencies))
         (if (.exists projdir)
           (FileUtils/deleteDirectory projdir)
           (when-not (.exists out-file)
             (.mkdir out-file)))
         (when (build-clj-project orig-model-name model-root model components)
           [orig-model-name result])))))
  ([model-paths model-name]
   (build-model model-paths model-name nil)))

(defn exec-with-build-model [cmd model-paths model-name]
  (when-let [result (build-model model-paths model-name)]
    (if cmd
      (when (exec-for-model (first result) cmd)
        (second result))
      (first result))))

(def install-model (partial exec-with-build-model "lein install"))
(def standalone-package (partial exec-with-build-model "lein uberjar" nil))

(defn- maybe-copy-kernel [model-name]
  (when (= model-name "agentlang")
    (FileUtils/copyDirectory
     (File. "out/agentlang/src/agentlang/kernel")
     (File. "src/agentlang/kernel")))
  model-name)

(defn compile-model [model-name]
  (maybe-copy-kernel (exec-with-build-model nil nil model-name)))

(defn- load-script [model-root _ f]
  (when (not= :delete (:kind f))
    (try
      (loader/load-script model-root (:file f))
      (catch Exception ex
        (.printStackTrace ex)
        (log/warn (str "failed to load " (:file f) " - " (str ex)))))))

(defn- handle-load-clj-project [model-name model-root model components]
  (load-clj-project model-name model components))

(defn load-model [model-name]
  (log/info (str "loading model " model-name))
  (if-let [r (loader/load-model model-name)]
    r
    (log/error (str "failed to load components from " model-name))))

(defn load-model-migration 
  ([model-name migration-type path-or-branch model-paths]
   (log/info (str "loading model for migration " migration-type " - " type " - " path-or-branch))
   (let [paths (or model-paths (tu/get-system-model-paths))
         model
         (case migration-type
           nil
           (load-model model-name)
           "git"
           (let [r (git/load-repo (or model-name (first paths)))
                 current-branch (git/git-branch-current r)]
             (git/git-checkout r :name path-or-branch)
             (load-model model-name)
             (let [{model :model} (loader/load-all-model-info paths model-name nil)]
               (git/git-checkout r :name current-branch)
               model))
           "local"
           (let [[model model-root] (loader/read-model (loader/verified-model-file-path
                                                        u/model-script-name path-or-branch nil))]
             (loader/load-model-dependencies model paths false)
             (if (loader/load-components-from-model model model-root)
               model
               (log/error (str "failed to load components from " path-or-branch)))))
         entity-names (cn/all-entity-names-and-versions)]
     [model entity-names]))
     ([model-name migration-type path-or-branch]
      (load-model-migration model-name migration-type path-or-branch nil)))

(defn- config-file-path [model-name]
  (str (project-dir model-name) config-edn))

(defn- exec-standalone [model-name cfg]
  (when-let [jar-file (standalone-jar model-name)]
    (let [cmd (str "java -jar " jar-file " -c " cfg)]
      (println cmd)
      (u/exec-in-directory "." cmd))))

(defn run-standalone-package [model-name]
  (let [model-name (or model-name (:name (loader/load-default-model-info)))
        run #(exec-standalone model-name (config-file-path model-name))]
    (or (run) (when (standalone-package model-name) (run)))))

(defn publish-library [model-name target]
  (let [cmd (case target
              :local "lein install"
              :clojars "lein deploy clojars"
              :github "lein deploy github")]
    (exec-with-build-model cmd nil model-name)))

(def ^:private runtime-dir ".runtime")
(def ^:private calib-proj-name "agentlang-calibrated")
(def ^:private calib-src-dir (s/replace calib-proj-name "-" "_"))

(defn- emit-calibration-project [agentlang-version deps]
  (let [final-deps (vec (concat [['com.github.agentlang-ai/agentlang agentlang-version]]
                                deps))
        ns-name (symbol (str calib-proj-name ".core"))
        project-spec `(~'defproject ~(symbol calib-proj-name) ~agentlang-version
                       :dependencies ~final-deps
                       :aot :all
                       :main ~ns-name)
        args 'args
        core-content `((~'ns ~ns-name
                        (:require [~'agentlang.core :as ~'agentlang])
                        (:gen-class))
                       (~'defn ~(symbol "-main") [& ~args] (apply agentlang/-main ~args)))
        core-src (str runtime-dir u/path-sep "src" u/path-sep calib-src-dir u/path-sep "core.clj")
        w (make-writer)]
    (w (str runtime-dir u/path-sep "project.clj") project-spec)
    (FileUtils/createParentDirectories (File. core-src))
    (w core-src core-content :write-each)
    true))

(defn- build-calibrated-runtime []
  (u/exec-in-directory runtime-dir "lein uberjar")
  true)

(defn calibrate-runtime [model-name]
  (let [{model :model} (loader/load-all-model-info nil model-name nil)]
    (when-let [deps (agentlang-deps-as-clj-deps (:dependencies model))]
      (let [rdir (File. runtime-dir)]
        (FileUtils/deleteDirectory rdir)
        (.mkdir rdir)
        (and (emit-calibration-project (fetch-agentlang-version model) (vec deps))
             (build-calibrated-runtime)
             (:name model))))))
