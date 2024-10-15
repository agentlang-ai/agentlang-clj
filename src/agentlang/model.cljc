(ns agentlang.model
  (:require
   [agentlang.lang]))

(agentlang.lang/model
 {:name :Agentlang,
  :agentlang-version "current",
  :components
  [:Agentlang.Kernel.Lang
   :Agentlang.Kernel.Identity
   :Agentlang.Kernel.Rbac
   :Agentlang.Kernel.Repl]})

(require
 '[agentlang.kernel.lang :as agentlang.kernel.lang]
 '[agentlang.kernel.identity :as agentlang.kernel.identity]
 '[agentlang.kernel.rbac :as agentlang.kernel.rbac])

(def agentlang___MODEL_ID__ "7b22bf12-6fdb-42dd-8e3a-2ebb5f5d8a9b")
