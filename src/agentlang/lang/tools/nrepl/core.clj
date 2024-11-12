(ns agentlang.lang.tools.nrepl.core
  (:require
    [agentlang.component :as cn]
    [agentlang.lang :as ln]
    [agentlang.lang.tools.repl :as repl]
    [agentlang.lang.tools.replcmds :as replcmds]
    [agentlang.util.runtime :as ur]))

(defonce nrepl-eval-init (atom nil))

(defn initialize-nrepl-environment [model-name store evaluator]
  "Initializes the REPL environment for a given model name."
  (let [model-name (or model-name (repl/infer-model-name))
        current-cn (cn/get-current-component)
        decl-names (cn/declared-names current-cn)]
    (when decl-names
      (repl/set-declared-names! current-cn decl-names))
    (use '[agentlang.lang])
    (use '[agentlang.lang.tools.replcmds])
    (ln/component repl/repl-component)
    (let [cn (if (= model-name :agentlang)
               repl/repl-component
               current-cn)]
      (replcmds/switch cn))
    (partial repl/repl-eval store (atom nil) evaluator)))

(defn init-nrepl-eval-func [model-name options]
  (.start
   (Thread.
    #(loop [rs (ur/get-runtime-init-result)]
       (if-not rs
         (do (try
               (Thread/sleep 1000)
               (catch Exception _ false))
             (recur (ur/get-runtime-init-result)))
         (let [[[ev store] _] rs]
           (reset! nrepl-eval-init (initialize-nrepl-environment model-name store ev))))))))
