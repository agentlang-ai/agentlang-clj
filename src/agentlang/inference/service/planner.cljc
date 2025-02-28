(ns agentlang.inference.service.planner
  (:require [clojure.walk :as w]
            [agentlang.util :as u]
            [agentlang.component :as cn]
            [agentlang.lang.internal :as li]
            [agentlang.inference.service.tools :as tools]))

(defn- validate-record-expr [[n attrs :as expr] alias]
  (when-not (li/name? n)
    (u/throw-ex (str "Invalid record name in " expr)))
  (when-not (map? attrs)
    (u/throw-ex (str "Attributes must be a map: " expr)))
  (when alias
    (if (vector? alias)
      (when-not (every? keyword? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))
      (when-not (li/name? alias)
        (u/throw-ex (str "Invalid alias " alias " for expression " expr)))))
  n)

(defn- maybe-alias [x]
  (when (symbol? x) (keyword x)))

(defn- operator? [x]
  (and (symbol? x)
       (some #{x} #{'= '< '> 'not= '>= '<= 'and 'or})))

(defn- parse-operator [opr]
  (if (= opr 'not=)
    '<>
    opr))

(declare expression-to-pattern)

(defn- parse-ref-or-expr [v]
  (cond
    (list? v)
    (cond
      (some #{(first v)} '(= < > <= >= not= and or)) (parse-ref-or-expr (vec v))
      (keyword? (first v)) (li/make-ref (u/symbol-as-keyword (second v)) (first v)) ; TODO: handle references more than one level deep
      :else `(~(first v) ~@(reverse (into '() (mapv parse-ref-or-expr (rest v))))))
    (vector? v)
    (if (operator? (first v))
      [(u/symbol-as-keyword (parse-operator (first v)))
       (or (maybe-alias (second v)) (parse-ref-or-expr (second v)))
       (or (maybe-alias (last v)) (parse-ref-or-expr (last v)))]
      (mapv expression-to-pattern v))
    (symbol? v) (keyword v)
    :else v))

(defn- parse-value-refs-and-exprs
  ([keyfmt attrs]
   (into
    {}
    (mapv (fn [[k v]] [(keyfmt k) (parse-ref-or-expr v)]) attrs)))
  ([attrs] (parse-value-refs-and-exprs identity attrs)))

(defn- parse-make [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    (let [a0 (parse-value-refs-and-exprs attrs)]
      (merge {n a0}
             (when alias {:as alias})))))

(defn- merge-contains [entity-name contains-rel-name parent-id]
  (if-let [parent-name (first (cn/relationship-nodes contains-rel-name))]
    {contains-rel-name {parent-name {(li/name-as-query-pattern (cn/identity-attribute-name parent-name)) parent-id}}}
    (u/throw-ex (str "failed to fetch parent for " entity-name " via " contains-rel-name))))

(defn- parse-make-child [[n attrs contains-rel parent-id :as expr] alias]
  (when (validate-record-expr expr alias)
    (let [a0 (parse-value-refs-and-exprs attrs)]
      (merge {n a0}
             (when contains-rel (merge-contains n contains-rel parent-id))
             (when alias {:as alias})))))

(defn- parse-lookup [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    (merge
     (if (seq attrs)
       {n (parse-value-refs-and-exprs li/name-as-query-pattern attrs)}
       {(li/name-as-query-pattern n) {}})
     (when alias {:as alias}))))

(defn- parse-lookup-one [expr alias]
  (parse-lookup expr (when alias [alias])))

(def ^:private parse-lookup-many parse-lookup)

(defn- parse-cond [expr alias]
  (loop [expr expr, pats []]
    (let [[condition consequent] expr]
      (if (and condition consequent)
        (recur
         (nthrest expr 2)
         (conj
          pats
          (if (= :else condition)
            [(expression-to-pattern consequent)]
            [(parse-ref-or-expr condition) (expression-to-pattern consequent)])))
        (let [result (apply concat [:match] pats)]
          (vec (if alias
                 (concat result [:as alias])
                 result)))))))

(defn- parse-update [[n attrs new-attrs] alias]
  (let [qexpr (parse-lookup-one [n attrs] nil)
        qattrs (li/record-attributes qexpr)]
    (merge {n (merge qattrs (parse-value-refs-and-exprs new-attrs))}
           (when alias {:as alias}))))

(defn- parse-delete [[n attrs] alias]
  (let [pat [:delete n attrs]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- parse-fn-call [expr alias]
  (let [pat [:eval (list* (first expr) (mapv parse-ref-or-expr (rest expr)))]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- parse-binding [expr alias]
  ((case (first expr)
     make parse-make
     make-child parse-make-child
     cond parse-cond
     lookup-one parse-lookup-one
     lookup-many parse-lookup-many
     update parse-update
     delete parse-delete
     parse-fn-call)
   (rest expr) alias))

(declare expressions-to-patterns)

(defn- parse-for-each [[n expr] alias]
  (let [pat [:for-each
             (if (vector? n)
               (mapv expression-to-pattern n)
               (parse-ref-or-expr n))
             (expression-to-pattern expr)]]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- const-expr? [expr]
  (or (string? expr) (number? expr)))

(defn expression-to-pattern [expr]
  (cond
    (const-expr? expr) expr
    (symbol? expr) (u/symbol-as-keyword expr)
    :else
    (if (seqable? expr)
      (if (vector? expr)
        [:eval `(vector ~@(mapv expression-to-pattern expr))]
        (case (first expr)
          def (parse-binding (nth expr 2) (u/symbol-as-keyword (second expr)))
          cond (parse-cond (rest expr) nil)
          for-each (parse-for-each (rest expr) nil)
          do (expressions-to-patterns expr)
          (parse-binding expr nil)))
      (u/throw-ex (str "Invalid expression: " expr)))))

(defn maybe-an-expression? [expr]
  (and (seqable? expr) (some #{(first expr)} '(def cond for-each))))

(defn maybe-expressions? [exprs]
  (when-not (string? exprs)
    (and (seqable? exprs) (= 'do (first exprs)))))

(defn expressions-to-patterns [exprs]
  (when (maybe-expressions? exprs)
    (mapv expression-to-pattern (rest exprs))))

(def ^:private generic-planner-instructions
  (str "Consider the following entity definitions in a subset of the Clojure programming language:\n"
       (u/pretty-str
        '(entity
          :Acme.Core/Customer
          {:Email {:type :Email :guid true}
           :Name :String
           :Address {:type :String :optional true}
           :LoyaltyPoints {:type :Int :default 50}}))
       "\n\n"
       (u/pretty-str
        '(entity
          :Acme.Core/PlatinumCustomer
          {:Email :Email}))
       "\n\n"
       (u/pretty-str
        '(entity
          :Acme.Core/GoldenCustomer
          {:Email :Email}))
       "\n\nIf the instruction given to you is to construct a customer instance with name `joe` and email `joe@acme.com`,\n"
       "you must return the following clojure expression:\n"
       (u/pretty-str
        '(def customer (make :Acme.Core/Customer {:Email "joe@acme.com" :Name "joe"})))
       "\nThere's no need to fill in attributes marked `:optional true`, :read-only true` or those with a `:default`, unless explicitly instructed.\n"
       "You can also ignore attributes with types `:Now` and `:Identity` - these will be automatically filled-in by the system.\n"
       "For example, if the instruction is to create customer `joe` with email `joe@acme.com` and loyalty points 6700, then you should return\n"
       (u/pretty-str
        '(def customer (make :Acme.Core/Customer {:Email "joe@acme.com" :Name "joe", :LoyaltyPoints 6700})))
       "\nMaking an instance of a customer will save it to a peristent store or database. To query or lookup instances of an entity, "
       "you can generate the following expressions:\n"
       (u/pretty-str
        '(def customer (lookup-one :Acme.Core/Customer {:Email "joe@acme.com"})))
       "\nThe preceding expression will lookup a customer with email `joe@acme.com`. Here's another example lookup, that will return "
       "all customers whose loyalty-points are greater than 1000:\n"
       (u/pretty-str
        '(def customers (lookup-many :Acme.Core/Customer {:LoyaltyPoints [> 1000]})))
       "\nBasically to fetch a single instance, call the `lookup-one` function and to fetch multiple instances, use `lookup-many`. "
       "To fetch all instances of an entity, call `lookup-many` as:\n"
       (u/pretty-str
        '(def all-customers (lookup-many :Acme.Core/Customer {})))
       "\nTo do something for each instance in a query, use the for-each expression. For example, the following example will create "
       "a PlatinumCustomer instance for each customer from the preceding lookup:\n"
       (u/pretty-str
        '(for-each
          customers
          (make :Acme.Core/PlatinumCustomer {:Email (:Email %)})))
       "\nThe special variable `%` will be bound to each element in the sequence, i.e `customers` in this example.\n"
       "DO NOT EXPLICITLY DEFINE `%`, it is automatically defined for for-each.\n"
       "The other two operations you can do on entities are `update` and `delete`. The following example shows how to change "
       "a customer's name and address. The customer is looked-up by email:\n"
       (u/pretty-str
        '(def changed-customer (update :Acme.Core/Customer {:Email "joe@acme.com"} {:Name "Joe Jae" :Address "151/& MZT"})))
       "\nThe following code-snippet shows how to delete a customer instance by email:\n"
       (u/pretty-str
        '(def deleted-customer (delete :Acme.Core/Customer {:Email "joe@acme.com"})))
       "\nNote that you should call `update` or `delete` only if explicitly asked to do so, in all normal cases entities should be "
       "created using `make`."
       "\nYou can also generate patterns that are evaluated against conditions, using the `cond` expression. For example,\n"
       "if the instruction is to create a customer named `joe` with email `joe@acme.com` and then apply the following \n"
       "business rules:\n"
       "1. If the loyalty-points is 50, return the customer instance.\n"
       "2. If the loyalty-points is greater than 50 and less than 1000, mark the customer as golden.\n"
       "3. Otherwise, mark the customer as platinum\n"
       "Given the above instruction, you must return the following dataflow patterns:\n"
       (u/pretty-str
        '(do (def customer (make :Acme.Core/Customer {:Name "joe" :Email "joe@acme.com"}))
             (cond
               (= (:LoyaltyPoints customer) 50) customer
               (and (> (:LoyaltyPoints customer) 50)
                    (< (:LoyaltyPoints customer) 1000))
               (make :Acme.Core/GoldenCustomer {:Email (:Email customer)})
               :else (make :Acme.Core/PlatinumCustomer {:Email (:Email customer)}))))
       "\n\nTwo entities can form relationships between them. For example, consider the following entity that represents a person:\n"
       (u/pretty-str
        '(entity
          :Family.Core/Person
          {:Email {:type :Email :guid true}
           :Name :String
           :Age :String}))
       "\nA possible relationship between two persons is:\n"
       (u/pretty-str
        '(relationship
          :Family.Core/Spouse
          {:meta {:between [:Person :Person :as [:Husband :Wife]]}}))
       "\nGiven the email of a wife, her husband can be queried as:\n"
       (u/pretty-str
        '(do (def spouse (lookup-one :Family.Core/Spouse {:Wife "mary@family.org"}))
             (def husband (lookup-one :Family.Core/Person {:Email (:Husband spouse)}))))
       "\n\nTwo entities may also form a `contains` relationship - where the relationship is hierarchical. For example, "
       "a Company can contain Departments:\n"
       (u/pretty-str
        '(entity
          :Acme.Core/Company
          {:Name {:type :String :guid true}}))
       (u/pretty-str
        '(entity
          :Acme.Core/Department
          {:No {:type :Int :guid true}
           :Name :String}))
       (u/pretty-str
        '(relationship
          :Acme.Core/CompanyDepartment
          {:meta {:contains [:Acme.Core/Company :Acme.Core/Department]}}))
       "\n\nWhen creating a new Department, the name of its parent Company must be specified as follows:\n"
       (u/pretty-str
        '(def dept (make-child :Acme.Core/Department {:No 101 :Name "sales"} :Acme.Core/CompanyDepartment "RK Steels Corp")))
       "\n\nNote that the parent is identified by its guid - in this case the company-name. The parent is assumed to be already existing."
       "You should not lookup or try to create the parent, unless explicitly instructed to do so. Never call `make` and `make-child` for the same "
       "instance - a instance of an entity with a parent should always be created with a call to `make-child`."
       "\n\nIn addition to entities and relationships, you may also have events in a model, as the one shown below:\n"
       (u/pretty-str
        '(event
          :Acme.Core/InvokeSummaryAgent
          {:UserInstruction :String}))
       "\nYou can call `make` on an event, and it will trigger some actions:\n"
       (u/pretty-str
        '(def summary-result (make :Acme.Core/InvokeSummaryAgent {:UserInstruction "a long essay on my trip to the USA...."})))
       "\nNote that an event that invokes an agent will return a string. So you can use the result as it is in the rest of "
       "the program, i.e use `summary-result` as an atomic value and not a composite - so a reference like `summary-result.text` will be invalid, "
       "just say `summary-result`, as shown below:\n"
       (u/pretty-str
        '(cond
           (= summary-result "trip to USA") "YES"
           :else "NO"))
       "\nIf you are asked to return more than one result, pack them into a list. For example, if you are asked to lookup and return both the "
       "husband and wife, you may do the following:\n"
       (u/pretty-str
        '(do (def wife (lookup-one :Family.Core/Spouse {:Wife "mary@family.org"}))
             (def husband (lookup-one :Family.Core/Person {:Email (:Husband spouse)}))
             [husband wife]))
       "\nAlso keep in mind that you can call only `make` on events, `update`, `delete`, `lookup-one` and `lookup-many` are reserved for entities.\n"
       "Note that you are generating code in a subset of Clojure. In your response, you should not use "
       "any feature of the language that's not present in the above examples. "
       "This means, for conditionals you should always return a `cond` expression, and must not return an `if`.\n"
       "A `def` must always bind to the result of `make`, `update`, `delete`, `lookup-one` and `lookup-many` and nothing else.\n"
       "You must not call functions like `map` or invent functional-syntax like `[data in [1 2 3]]`.\n\n"
       "Now consider the entity definitions and user-instructions that follows to generate fresh dataflow patterns. "
       "An important note: do not return any plain text in your response, only return valid clojure expressions. "
       "\nAnother important thing you should keep in mind: your response must not include any objects from the previous "
       "examples. Your response should only make use of the entities and other definitions provided by the user below.\n"
       "Also make sure the expressions you return are all enclosed in a `(do ...)`.\n"))

(defn- agent-tools-as-definitions [instance]
  (str
   (when-let [cns (:ToolComponents instance)]
     (tools/raw-components cns))
   (tools/as-raw-tools
    (mapv #(keyword (:name %)) (:Tools instance)))))

(defn with-instructions [instance]
  (assoc instance :UserInstruction
         (str generic-planner-instructions
              "These are the application specific entity and event definitions shared by the user:\n\n" (agent-tools-as-definitions instance)
              "Additional application specific instructions from the user follows:\n\n" (:UserInstruction instance))))

(defn validate-expressions [exprs]
  (doseq [expr (rest exprs)]
    (when-not (or (seqable? expr) (symbol? expr))
      (u/throw-ex (str "Unexpected expression - " expr)))
    ;; An embedded def could mean mismatched parenthesis.
    (when (seqable? expr)
      (w/postwalk
       #(when (and (seqable? %) (= (first %) 'def))
          (u/throw-ex (str "Maybe there is a parenthesis mismatch in this expression - " expr)))
       (rest expr))))
  exprs)
