(ns
 agentlang.model
 (:require
  [agentlang.kernel.lang :as agentlang.kernel.lang]
  [agentlang.kernel.identity :as agentlang.kernel.identity]
  [agentlang.kernel.rbac :as agentlang.kernel.rbac]))
(agentlang.lang/model
 {:name :Agentlang,
  :agentlang-version "current",
  :components
  [:Agentlang.Kernel.Lang
   :Agentlang.Kernel.Identity
   :Agentlang.Kernel.Rbac]})
(def agentlang___MODEL_ID__ "9790b74c-baa7-49f8-98f2-160ea2d97f38")
