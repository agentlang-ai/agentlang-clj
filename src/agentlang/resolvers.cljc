(ns agentlang.resolvers
  ;; load resolvers required by kernel
  (:require [agentlang.resolver.timer]
            #?(:clj [agentlang.resolver.data-sync])))
