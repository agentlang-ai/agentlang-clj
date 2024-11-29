(ns agentlang.inference
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.env :as env]
            [agentlang.evaluator :as ev]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            #?(:clj [agentlang.util.logger :as log]
               :cljs [agentlang.util.jslogger :as log])
            [agentlang.util.http :as uh]
            #?(:clj [agentlang.inference.service.agent-registry :as ar])
            [agentlang.inference.service.model :as model]
            #?(:clj [agentlang.inference.service.core :as inference])))

(defn as-vec [x]
  (if (vector? x)
    x
    [x]))

(defn- can-eval? [r]
  (not (string? r)))

(defn- eval-patterns [agentlang-patterns context-event]
  (if (can-eval? agentlang-patterns)
    (let [env (env/bind-instance env/EMPTY context-event)]
      (loop [pats (as-vec agentlang-patterns), env env, result nil]
        (if-let [p (first pats)]
          (let [r (ev/evaluate-pattern env p)]
            (if (u/safe-ok-result r)
              (recur (rest pats) (:env r) r)
              (do (log/error (str "inferred-pattern " p " failed with result " r))
                  r)))
          (do (log/info (str "inference succeeded with result " result))
              result))))
    agentlang-patterns))

(defn- maybe-feature-instruction [agent-instance]
  (when-let [features (seq (:Features agent-instance))]
    (let [fts (mapv model/get-feature-prompt features)]
      (s/join "\n" fts))))

(defn input-instance [agent ctx]
  (when-let [input-type (:Input agent)]
    (let [attrs (dissoc (cn/instance-attributes ctx) :UserInstruction :EventContext)]
      (when (seq attrs)
        {(u/string-as-keyword input-type) attrs}))))

(defn- agent-context-as-string [ctx]
  (let [avals (mapv (fn [[k v]] (str (name k) ": " v))
                    (dissoc (cn/instance-attributes ctx) :EventContext))]
    (if (seq avals)
      (s/join ", " avals)
      "")))

#?(:clj
   (defn run-inference-for-event [event agent-instance]
     (when-not agent-instance (u/throw-ex (str "Agent not initialized for " event)))
     (log/debug (str "Processing response for inference " (cn/instance-type event) " - " (u/pretty-str agent-instance)))
     (let [updated-instruction (s/trim
                                (str (or (maybe-feature-instruction agent-instance) "")
                                     "\n"
                                     (if (model/planner-agent? agent-instance)
                                       (agent-context-as-string (:Context agent-instance))
                                       (or (get-in agent-instance [:Context :UserInstruction]) ""))))
           _ (model/maybe-init-agent-chat-session agent-instance updated-instruction)
           agent-instance (ar/handle-generic-agent (assoc agent-instance :UserInstruction updated-instruction))

           ;; (when-let [ctx (:Context agent-instance)]
           ;;   (str "\n" (or (:UserInstruction ctx) "")
           ;;        (when-let [input-inst (input-instance agent-instance ctx)]
           ;;          (str "\nFull input object to agent instance:\n"
           ;;               (u/pretty-str (assoc input-inst :as :Input))))))))))
           r0 (or (:Response agent-instance) agent-instance)
           r1 (if (string? r0) (edn/read-string r0) r0)
           r2 (if-let [f (model/agent-response-handler agent-instance)] (f r1) r1)
           result (if (vector? r2) (first r2) r2)
           is-review-mode (get-in event [:EventContext :evaluate-inferred-patterns])]
       (if-let [patterns (:patterns result)]
         (if is-review-mode
           patterns
           (eval-patterns patterns event))
         (if-let [errmsg (:errormsg result)]
           (u/throw-ex errmsg)
           result)))))
