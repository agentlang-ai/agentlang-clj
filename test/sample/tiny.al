(component :Sample.Tiny)

(entity
 :A
 {:Id :Identity
  :X :Int})

(defn tiny-f [x] (* 100 x))

(entity
 :B
 {:Id :Identity
  :X :Int
  :Y '(sample.tiny/tiny-f :X)})

(dataflow
 :Agentlang.Kernel.Identity/PostSignUp
 {:A {:X 200}})
