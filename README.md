<div align="center">

# AgentLang Programming Language

AgentLang is the **easiest way** to build AI Agents, Chatbots and Apps - build **teams of AI agents** that collaborate (with other AI agents and humans) to handle complex, time-consuming, monotonous tasks. AgentLang is a data-oriented, declarative abstraction for building agents and apps, similar to how Terraform is a declarative abstraction for infrastructure-as-code.

[![AppCI](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml)
[![AgentLang clj CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml)
[![AgentLang cljs CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml)

### **Open | Enterprise-grade | Production-ready**

The AgentLang language specification, its compiler and runtime are open source. AgentLang programs can run anywhere - **avoiding the vendor lock-in** of other AI agent/programming platforms. 

AgentLang runtime has native integration with databases, vector databases, auth stores, etc. AgentLang programs run on the JVM and can make use of any of the millions of existing Java/Clojure and other JVM libraries out there. 

AgentLang comes with all the modern tooling, dependency management and REPL needed to build production-grade agents and apps.

[Website](https://agentlang-ai.github.io/agentlang/) - [Examples](#examples) - [Documentation](/docs/#readme)

</div>

## First-class AI Agents

Agents are a built-in language construct - developers can choose from one of the built-in agent-types, or easily add their own agent-types.

### Example: A Humorous Chatbot

```clojure
(component :Chat)

{:Agentlang.Core/Agent
 {:Name :comedian
  :Input :Chat/Session
  :UserInstruction "You are an AI bot who tell jokes"}}
```

(Save this example in a file named `chat.al`. In a later section, we will show you how to run it)

## Team of AI Agents

AI Agents can delegate tasks to other specialized agents and dramatically increase the efficiency and accuracy of agentic behavior.

### Example: Expense Processor

Analyse scanned images of expense receipts and generate expense records

```clojure
;; file: expense.al

(component :Expense)

(entity
 :Expense
 {:Id :Identity
  :Title :String
  :Amount :Double})

{:Agentlang.Core/Agent
 {:Name :ocr-agent
  :Type :ocr
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")}}

{:Agentlang.Core/Agent
 {:Name :expense-agent
  :Type :planner
  :UserInstruction "Convert an expense report into individual instances of the expense entity."
  :Tools [:Expense/Expense]
  :Input :Expense/SaveExpenses
  :Delegates {:To :ocr-agent :Preprocessor true}}}
```

## Data Modeling

Model any business domain - from simple to complex - with the relationship graph based data modeling approach of AgentLang. Apply RBAC policies, declaratively, to the data model and secure your application data.

### Example: Personal Accounting

Defines the model for a simple accounting application, where the income and expense records of multiple users can be tracked separately.

```clojure
;; file: accounts.al

(component :Accounts)

(entity
 :User
 {:Email {:type :Email :guid true}
  :Name :String
  :Created :Now})

(record
 :Entry
 {:Id :Identity
  :Description :String
  :Date :Now
  :Amount :Double})

(entity :Income {:meta {:inherits :Entry}})
(entity :Expense {:meta {:inherits :Entry}})

(relationship :UserIncome {:meta {:contains [:User :Income]}})

(relationship :UserExpense {:meta {:contains [:User :Expense]}})
```

## Dataflow

A dataflow allows you to express complex business logic simply as purely-declarative [patterns of data operations](https://docs.agentlang.io/docs/concepts/declarative-dataflow). The dataflow defined below creates an income and expense report, given the email of a user:

```clojure
;; file: accounts.al

(defn compute-total [entries]
 (apply + (mapv :Amount entries)))

(record
 :Report
 {:Incomes {:listof :Income}
  :Expenses {:listof :Expense}
  :TotalIncome '(accounts/compute-total :Incomes)
  :TotalExpense '(accounts/compute-total :Expenses)
  :NetIncome '(- :TotalIncome :TotalExpense)})

(dataflow
 :GenerateReport
 {:User {:Email? :GenerateReport.Email} :as [:U]} ; find the user
 ; query the user's incomes and expenses:
 {:Income? {} :-> [[:UserIncome? :U]] :as :Incomes}
 {:Expense? {} :-> [[:UserExpense? :U]] :as :Expenses}
 {:Report {:Incomes :Incomes :Expenses :Expenses}})
```

# Installing AgentLang

### Prerequisites

1. Linux, Mac OSX or a Unix emulator in Windows
2. Download and install the [AgentLang CLI tool](https://github.com/agentlang-ai/agentlang.cli) or use it via Docker
   a. Pre-requisite: [Java SE 21](https://openjdk.org/projects/jdk/21/) or later
   b. Pre-requisite: Git CLI for your OS
3. Set the `OPENAI_API_KEY` environment variable to a valid API key from OpenAI

### Running the examples

```shell
agent /path/to/chat.al
```

Or run it via Docker (assuming the file `chat.al` is in the current directory):

```shell
docker run --rm \
  -v .:/agentlang \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -p 8080:8080 \
  -it agentlang/agentlang.cli:latest \
  agent chat.al
```

Once the agent starts running, send it a message with an HTTP POST,

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Chat/Session": {"UserInstruction": "tell me a joke about AI agents"}}' \
http://localhost:8080/api/Chat/Session
```

You should see a response from the agent with a joke about itself!

Now you can try running the expenses example:

```shell
agent /path/to/expense.al
```

Or run it via Docker (assuming the file `expense.al` is in the current directory):

```shell
docker run --rm \
  -v .:/agentlang \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -p 8080:8080 \
  -it agentlang/agentlang.cli:latest \
  agent expense.al
```

Send a request with a proper URL pointing to the image of a receipt or a bill:

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Expense/SaveExpenses": {"UserInstruction": "https://acme.com/receipts/r01.png"}}' \
http://localhost:8080/api/Expense/SaveExpenses
```

Once the expenses are processed, you can execute the following `GET` request to fetch the individual expense items that were created:

```shell
curl --header "Content-Type: application/json" http://localhost:8080/api/Expense/Expense
```

Next we will try running the account example:

```shell
agent /path/to/accounts.al
```

Or run it via Docker (assuming the file `accounts.al` is in the current directory):

```shell
docker run --rm \
  -v .:/agentlang \
  -e OPENAI_API_KEY="$OPENAI_API_KEY" \
  -p 8080:8080 \
  -it agentlang/agentlang.cli:latest \
  agent accounts.al
```

Create a user:

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Accounts/User": {"Email": "j@acme.com", "Name": "JJ"}}' \
http://localhost:8080/api/Accounts/User
```

Make some account entries for the user:

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Accounts/Income": {"Description": "salary", "Amount": 3450.54}}' \
http://localhost:8080/api/Accounts/User/j@acme.com/UserIncome/Income

curl --header "Content-Type: application/json" \
--request POST \
--data '{"Accounts/Expense": {"Description": "rent", "Amount": 50.0}}' \
http://localhost:8080/api/Accounts/User/j@acme.com/UserExpense/Expense
```

Generate the income and expense report:

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Accounts/GenerateReport": {"Email": "j@acme.com"}}' \
http://localhost:8080/api/Accounts/GenerateReport
```

### Contributing

If you are excited about cutting-edge AI and programming language technology, please consider becoming a contributor to the Agentlang project.

There are two main ways you can contribute:

  1. Try out the language, report bugs and proposals in the project's [issue tracker](https://github.com/agentlang-ai/agentlang/issues).
  2. Actively participate in the development of Agentlang and submit your patches as [pull requests](https://github.com/agentlang-ai/agentlang/pulls).

### License

Copyright 2024 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0
