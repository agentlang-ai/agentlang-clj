(ns agentlang.lang.tools.schema.diff)

(defn- remove-entry [schema tag]
  (remove #(= tag (first %)) schema))

(defn apply-diff [schema diff]
  (let [has-type? (= :map (first schema))
        schema (if has-type? (rest schema) schema)
        new-schema
        (loop [diff diff, schema schema]
          (if-let [entry (first diff)]
            (recur
             (rest diff)
             (if (= :- (first entry))
               (remove-entry schema (second entry))
               (conj schema entry)))
            schema))]
    (if has-type?
      `[:map ~@new-schema]
      new-schema)))
