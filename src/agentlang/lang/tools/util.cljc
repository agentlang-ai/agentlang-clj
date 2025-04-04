(ns agentlang.lang.tools.util
  (:require [clojure.string :as s]
            #?(:clj [clojure.java.io :as io])
            [agentlang.util :as u]))

(defn get-system-model-paths []
  #?(:clj
     (if-let [paths (System/getenv "AGENTLANG_MODEL_PATHS")]
       (s/split paths #":")
       ["."])
     :cljs ["."]))

(defn- repo-dir [path n]
  (str path u/path-sep n))

(defn- repo-exists? [paths n]
  #?(:clj
     (some
      #(.isDirectory (io/file (repo-dir % n)))
      paths)))

(defn component-name-as-ns [cn]
  (symbol (s/lower-case (subs (str cn) 1))))

(defn maybe-clone-model [spec paths]
  (when (map? spec)
    #?(:clj
       (when (= (get-in spec [:source :type]) :github)
         (when-let [repo (get-in spec [:source :repo])]
           (let [branch (get-in spec [:source :branch])
                 org (get-in spec [:source :org])]
             (when-not (repo-exists? paths repo)
               (u/exec-in-directory
                 (first paths) (str "git clone git@github.com:" org "/" repo ".git"))
               (when branch
                 (u/exec-in-directory
                   (repo-dir (first paths) repo)
                   (str "git checkout " branch)))))))
       :cljs (u/throw-ex "git clone not supported")))
  spec)
