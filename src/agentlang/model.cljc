(ns agentlang.model
  (:require
   [agentlang.lang]))

(agentlang.lang/model
 {:name :Agentlang,
  :agentlang-version "current",
  :components
  [:Agentlang.Kernel.Lang
   :Agentlang.Kernel.Identity
   :Agentlang.Kernel.Rbac]})

#?(:clj
   (require
    (quote [agentlang.kernel.lang :as agentlang.kernel.lang])
    (quote [agentlang.kernel.identity :as agentlang.kernel.identity])
    (quote [agentlang.kernel.rbac :as agentlang.kernel.rbac])))