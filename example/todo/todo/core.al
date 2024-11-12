(component :Todo.Core)

;; This example shows how to persist instances of an
;; entity (:TodoEntry) in a custom format.
;; This is achieved via a custom "resolver" that writes
;; and reads :TodoEntry instances to a file in a user-specified
;; format.

(entity :TodoEntry
 {:Content :String
  :DateCreated :Now})

(def buffer (atom nil))
(def todo-file ".todo")

(defn refresh-buffer! []
  (try
    (reset! buffer (read-string (slurp todo-file)))
    (catch Exception _ nil)))

(defn flush-buffer! []
  (spit todo-file @buffer))

(defn upsert [inst]
  (when-not @buffer
    (refresh-buffer!))
  (swap! buffer conj [(:DateCreated inst) (:Content inst)])
  (flush-buffer!)
  inst)

(defn as-todo-entry [[date-created content]]
  {:Content content
   :DateCreated date-created})

(defn lookup-entries [[_ query]]
  (when-not @buffer
    (refresh-buffer!))
  (let [[_ _ value] (:where query)
        entries (filter #(clojure.string/index-of (second %) value) @buffer)]
    (mapv as-todo-entry entries)))

(def todo-resolver
  (agentlang.resolver.core/make-resolver
   :todo.resolver ; a unique name for the resolver.
   {:create upsert
    :query lookup-entries}))

(agentlang.resolver.registry/override-resolver :Todo.Core/TodoEntry todo-resolver)
