(ns agentlang.inference
  (:require [clojure.edn :as edn]
            [clojure.string :as s]
            [agentlang.lang :as ln]
            [agentlang.lang.internal :as li]
            [agentlang.env :as env]
            [agentlang.global-state :as gs]
            [agentlang.component :as cn]
            [agentlang.util :as u]
            [agentlang.util.logger :as log]
            [agentlang.util.http :as uh]
            [agentlang.inference.service.agent-registry :as ar]
            [agentlang.inference.service.model :as model]
            [agentlang.inference.service.core :as inference]))

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
          (let [r (gs/evaluate-pattern env p)
                rs (:result r)]
            (if rs
              (recur (rest pats) (:env r) rs)
              (do (log/error (str "inferred-pattern " p " failed with result " rs))
                  rs)))
          (do (log/debug (str "inference succeeded with result " result))
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

(defn run-inference-for-event [event agent-instance]
  (when-not agent-instance (u/throw-ex (str "Agent not initialized for " event)))
  (log/debug (str "Processing response for inference " (cn/instance-type event) " - " (u/pretty-str agent-instance)))
  (let [agent-instance (if-not (:Context agent-instance)
                         (assoc agent-instance :Context event)
                         agent-instance)
        updated-instruction (s/trim
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
        r2 (if-let [f (model/agent-response-handler agent-instance)] (f r1) r1)]
    (if (vector? r2) (first r2) r2)))

(gs/set-run-inference-fn! run-inference-for-event)
