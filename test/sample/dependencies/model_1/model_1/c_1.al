(ns model1.c1
  (:use [agentlang.lang]))

(component :Model1.C1)

(dataflow
 :E1
 {:Model2.C1/E
  {:X '(+ 10 :Model1.C1/E1.X)}})
