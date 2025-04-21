(ns agentlang.inference.service.planner
  (:require [clojure.walk :as w]
            [clojure.set :as set]
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

(defn- make-between-instance [relname attrs alias]
  (let [nns (cn/between-attribute-names relname)
        attrs (parse-value-refs-and-exprs attrs)
        ks (keys attrs)]
    (when (not= (set nns) (set ks))
      (u/throw-ex (str "Invalid attribute(s) for " relname " - " ks)))
    (let [fent (partial cn/relationship-node-entity relname nns)
          qpats (mapv (fn [n]
                        (let [ent (fent n)
                              idattr (cn/identity-attribute-name ent)]
                          {ent {(li/name-as-query-pattern idattr) (n attrs)} :as [n]}))
                      nns)
          new-attrs (into {} (mapv (fn [k] [k (li/make-ref k li/path-attr)]) ks))]
      (vec
       (concat
        qpats
        [(merge {relname new-attrs} (when alias {:as alias}))])))))

(defn- parse-make [[n attrs :as expr] alias]
  (when (validate-record-expr expr alias)
    (let [a0 (parse-value-refs-and-exprs attrs)]
      (if (cn/between-relationship? n)
        (make-between-instance n attrs alias)
        (merge {n a0}
               (when alias {:as alias}))))))

(defn- merge-contains [entity-name contains-rel-name parent-id]
  (if-let [parent-name (first (cn/relationship-nodes contains-rel-name))]
    {contains-rel-name {parent-name {(li/name-as-query-pattern (cn/identity-attribute-name parent-name)) parent-id}}}
    (u/throw-ex (str "failed to fetch parent for " entity-name " via " contains-rel-name))))

(defn- parse-make-child [[n attrs contains-rel parent-id :as expr] alias]
  (when (validate-record-expr expr alias)
    (let [a0 (parse-value-refs-and-exprs attrs)]
      (merge {n a0}
             (when contains-rel (merge-contains n contains-rel (parse-ref-or-expr parent-id)))
             (when alias {:as alias})))))

(defn- parse-lookup-via-between [[relname attrs :as expr] alias]
  (let [nodes (set (cn/between-attribute-names relname))
        attrs (parse-value-refs-and-exprs attrs)
        k (first (keys attrs))]
    (when-not (some #{k} nodes)
      (u/throw-ex (str k " not in nodes " nodes " of " relname)))
    (let [n1 k, n2 (first (set/difference nodes #{k}))
          e1 (cn/relationship-node-entity relname n1)
          e2 (cn/relationship-node-entity relname n2)]
      (merge
       {(li/name-as-query-pattern e2) {}
        (li/name-as-query-pattern relname)
        {e1 {(cn/identity-attribute-name e1) (parse-ref-or-expr (k attrs))}}}
       (when alias {:as alias})))))

(defn- parse-lookup [[n attrs :as expr] alias]
  (if (cn/between-relationship? n)
    (parse-lookup-via-between expr alias)
    (when (validate-record-expr expr alias)
      (merge
       (if (seq attrs)
         {n (parse-value-refs-and-exprs li/name-as-query-pattern attrs)}
         {(li/name-as-query-pattern n) {}})
       (when alias {:as alias})))))

(defn- parse-lookup-one [expr alias]
  (parse-lookup expr (when alias [alias])))

(def ^:private parse-lookup-many parse-lookup)

(defn- parse-lookup-childern [[relname parent-id :as expr] alias]
  (let [parent (cn/containing-parent relname)
        pid-attr (cn/identity-attribute-name parent)
        child (cn/contained-child relname)]
    (merge
     {(li/name-as-query-pattern child) {}
      (li/name-as-query-pattern relname)
      {parent {pid-attr parent-id}}}
     (when alias {:as alias}))))

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
  (let [pat
        (if (seqable? expr)
          (if (or (string? expr) (vector? expr))
            [:call `(~'identity ~expr)]
            [:call `(~(first expr) ~@(mapv parse-ref-or-expr (rest expr)))])
          [:call `(~'identity ~expr)])]
    (if alias
      (vec (concat pat [:as alias]))
      pat)))

(defn- parse-binding [expr alias]
  (if-let [f (case (first expr)
               make parse-make
               make-child parse-make-child
               cond parse-cond
               lookup-one parse-lookup-one
               lookup-many parse-lookup-many
               lookup-children parse-lookup-childern
               update parse-update
               delete parse-delete
               nil)]
    (f (rest expr) alias)
    (parse-fn-call expr alias)))

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
        [:call `(vector ~@(mapv expression-to-pattern expr))]
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
          {:Email {:type :Email :id true}
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
       (u/pretty-str '(def customer (make :Acme.Core/Customer {:Email "joe@acme.com" :Name "joe"})))
       "\nThere's no need to fill in attributes marked `:optional true`, :read-only true` or those with a `:default`, unless explicitly instructed.\n"
       "You can also ignore attributes with types `:Now` and `:Identity` - these will be automatically filled-in by the system.\n"
       "For example, if the instruction is to create customer `joe` with email `joe@acme.com` and loyalty points 6700, then you should return\n"
       (u/pretty-str '(def customer (make :Acme.Core/Customer {:Email "joe@acme.com" :Name "joe", :LoyaltyPoints 6700})))
       "\nMaking an instance of a customer will save it to a peristent store or database. To query or lookup instances of an entity, "
       "you can generate the following expressions:\n"
       (u/pretty-str '(def customer (lookup-one :Acme.Core/Customer {:Email "joe@acme.com"})))
       "\nThe preceding expression will lookup a customer with email `joe@acme.com`. Here's another example lookup, that will return "
       "all customers whose loyalty-points are greater than 1000:\n"
       (u/pretty-str '(def customers (lookup-many :Acme.Core/Customer {:LoyaltyPoints [> 1000]})))
       "\nBasically to fetch a single instance, call the `lookup-one` function and to fetch multiple instances, use `lookup-many`. "
       "To fetch all instances of an entity, call `lookup-many` as:\n"
       (u/pretty-str '(def all-customers (lookup-many :Acme.Core/Customer {})))
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
       (u/pretty-str '(def changed-customer (update :Acme.Core/Customer {:Email "joe@acme.com"} {:Name "Joe Jae" :Address "151/& MZT"})))
       "\nThe following code-snippet shows how to delete a customer instance by email:\n"
       (u/pretty-str '(def deleted-customer (delete :Acme.Core/Customer {:Email "joe@acme.com"})))
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
          :School.Core/Student
          {:Email {:type :Email :id true}
           :Name :String
           :Age :Int}))
       (u/pretty-str
        '(entity
          :School.Core/ExamResult
          {:Id {:type :Int :id true}
           :Subject :String
           :Mark :Int}))
       "\nA relationship can be established between Students and ExamResults as, \n"
       (u/pretty-str
        '(relationship
          :School.Core/StudentExamResult
          {:meta {:between [:Student :ExamResult]}}))
       "\nCreating a new between relationship is similar to creating an instance of an entity, as shown below:\n"
       (u/pretty-str '(def spouse (make :School.Core/StudentExamResult {:Student "sam@school.org" :ExamResult 101})))
       "\nThe between relationship is established on the `id` attributes of the entities. Given the email of a student, "
       "her exam results can be queried as:\n"
       (u/pretty-str
        '(def exam-results (lookup-many :School.Core/StudentExamResult {:Student "mary@school.org"})))
       "\n\nTwo entities may also form a `contains` relationship - where the relationship is hierarchical. For example, "
       "a Company can contain Departments:\n"
       (u/pretty-str
        '(entity
          :Acme.Core/Company
          {:Name {:type :String :id true}}))
       (u/pretty-str
        '(entity
          :Acme.Core/Department
          {:No {:type :Int :id true}
           :Name :String}))
       (u/pretty-str
        '(relationship
          :Acme.Core/CompanyDepartment
          {:meta {:contains [:Acme.Core/Company :Acme.Core/Department]}}))
       "\n\nIn the `:Acme.Core/CompanyDepartment` contains-relationship, `:Acme.Core/Company` is the parent entity and `:Acme.Core/Department` "
       "is the child entity. When creating a new Department, the name of its parent Company must be specified as follows:\n"
       (u/pretty-str
        '(def dept (make-child :Acme.Core/Department {:No 101 :Name "sales"} :Acme.Core/CompanyDepartment "RK Steels Corp")))
       "\n\nNote that the parent is identified by its id - in this case the company-name. The parent is assumed to be already existing."
       "You should not lookup or try to create the parent, unless explicitly instructed to do so. When asked to create a child instance under "
       "a parent, you must call `make-child`.\n"
       "To lookup childern under a parent, call the `lookup-children` function. For example, the following expression will return "
       "all departments in the company named \"RK Steels Corp\":\n"
       (u/pretty-str '(def departments (lookup-children :Acme.Core/CompanyDepartment "RK Steels Corp")))
       "\nLet me repeat again, for queries on a `:between` relationship, always call `lookup-many` and nothing else.\n"
       "For queries via a `:contains` relationship, always call `lookup-children` and nothing else."
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
       "employee and his salary, you may do the following:\n"
       (u/pretty-str
        '(do (def employee (lookup-one :Company/Employee {:Email "joe@company.com"}))
             (def salary (lookup-one :Company/Salary {:EmployeeId (:Id employee)}))
             [employee salary]))
       "\nAlso keep in mind that you can call only `make` on events, `update`, `delete`, `lookup-one`, "
       "`lookup-many` and `lookup-children` are reserved for entities.\n"
       "Note that you are generating code in a subset of Clojure. In your response, you should not use "
       "any feature of the language that's not present in the above examples. "
       "This means, for conditionals you should always return a `cond` expression, and must not return an `if`.\n"
       "A `def` must always bind to the result of `make`, `make-child`, `update`, `delete`, "
       "`lookup-one`, `lookup-many` and `lookup-children` and nothing else.\n"
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

(def ^:private generic-interactive-planner-instructions
  (str "You are provided with the model of an application as entities, relationships and events. Entities define the schema of business objects."
       "Relationships define hierarchical (:contains) and two-way (:between) connections between entities." 
       "Events trigger various business workflows.\nBased on the model, analyse a user request. If the request is valid "
       "in the context of the model, reply \"Please confirm: <complete-valid-instruction>\" and in response, if the user confirms your instructions, "
       "reply: \"OK <complete-valid-instruction>\". Otherwise, state what additional input is required from the user. "
       "Consider the following example model of a School application:\n"
       (u/pretty-str
        '(entity
          :School.Core/School
          {:Name {:type :String :id true}
           :Address :String}))
       "\n"
       (u/pretty-str
        '(entity
          :School.Core/Teacher
          {:FirstName :String
           :LastName :String
           :Sex {:oneof ["male" "female"]}
           :Age {:type :Int :optional true}
           :Email {:type :Email :unique true}
           :Id {:type :Int :id true}}))
       "\n"
       (u/pretty-str
        '(relationship
          :School.Core/SchoolTeacher
          {:meta {:contains {:School.Core/School :School.Core/Teacher}}}))
       "\n"
       (u/pretty-str
        '(entity
          :School.Core/Course
          {:CourseName {:type :String :id true}}))
       "\n"
       (u/pretty-str
        '(relationship
          :School.Core/TeacherCourse
          {:meta {:doc "Assign a teacher to a course."
                  :between {:School.Core/Teacher :School.Core/Course}}}))
       "\n\n"
       "Now for instance, if the user instruction is: \"Create a new teacher named James Thomas, aged 25\", you should reply - "
       "\"To create a teacher, the Id, Sex and Email attributes are mandatory. You provided only the FirstName and LastName attributes.\"\n"
       "As another example, if the user instruction is: \"Create a new course with name `Basics of Computation`\", then you must reply "
       "\"Please confirm: Creating a new course named `Basics of Computation`\", if the user confirms, then reply: "
       "\"OK Creating a new course named `Basics of Computation`\", because all information required to create the course is contained "
       "in the user-instruction and the user confirmed your suggestion.\n"
       "If you get an instruction like \"Assign course `Genetics` to teacher `James Thomas`\", you must reply \"Teacher's email is required "
       "to assign course\". This is because a course assignment is acheieved via the TeacherCourse :between relationship. Creating a :between "
       "relationship requires the `:id` attribute values of both entities involved. In this case, the `:id` values required are :Course.CourseName "
       "and :Teacher.Email. The user might reply, \"The teacher's email is james@school.org\", for this you must reply "
       "\"Please confirm: Assigning course `Genetics` to teacher james@school.org\". If the user confirms your suggestion, then conclude by "
       "replying - \"OK Assigning course `Genetics` to teacher james@school.org\". Otherwise, if the instruction was "
       "\"Assign course `Genetics` to teacher james@abc.com\", then you must reply \"Please confirm: Assigning course `Genetics` to teacher james@abc.com\""
       " and when the user confirms your suggestion, reply \"OK Assigning course `Genetics` to teacher james@abc.com\".\n"
       "For establishing a :contains relationship, the `:id` value of the parent entity and the complete attribute values of the child entity are "
       "required. For example, a valid request to create a new teacher under a school is \"Add teacher Jacob John under the shool `St. Matthews`."
       "The other details of the new teacher are: Id=101, Age=25, Email=jj@stmatthews.edu and Sex=male\".\n"
       "You may also receive requests to lookup, update and delete entities by various attribute-values. In these cases, unlike the `create` scenarios "
       "we discussed above, not all attributes are needed. Return first a `Please confirm` response if the provided attributes look reasonable.\n"
       "Once the user confirms your suggestion, reply with an `OK` response. If the user does not confirm, continue the converstion by gathering "
       "more information from the user until you can make a suggestion that the user finally confirms or approves.\n"
       "For lookup or updates via a relationship, the `:id` attribute value will be required.\n"
       "Based on the above instructions, respond to user-instructions on the application specific model that follows.\n\n"))

(defn with-interactive-instructions [instance]
  (assoc instance :UserInstruction
         (str generic-interactive-planner-instructions
              "These are the application specific entity, event and dataflow definitions shared by the user:\n\n" (agent-tools-as-definitions instance)
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
