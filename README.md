<div align="center">

# AgentLang Programming Language

AgentLang is the **easiest way** to build AI Agents, Chatbots and Apps - build **teams of AI agents** that collaborate (with other AI agents and humans) to handle complex, time-consuming, monotonous tasks. AgentLang is a data-oriented, declarative abstraction for building agents and apps, similar to how Terraform is a declarative abstraction for infrastructure-as-code.

[![AppCI](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml)
[![AgentLang clj CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml)
[![AgentLang cljs CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml)

### **Open | Enterprise-grade | Production-ready**

The AgentLang language specification, its compiler and runtime are open source. The code you build in AgentLang can run anywhere - **avoiding the vendor lock-in** of other AI agent/programming platforms. AgentLang programs run on the JVM and can make use of any of the millions of existing Java/Clojure and other JVM libraries out there. AgentLang runtime has native integration with databases, vector dbs, auth systems. AgentLang comes with all the modern tooling, dependency management needed to build production-grade agents and apps.

[Website](https://agentlang-ai.github.io/agentlang/) - [Examples](#examples) - [Documentation](/docs/#readme)

</div>

## First-class AI Agents

Agents are a built-in language construct - developers can choose from one of the built-in agent-types, or easily add their own agent-types.

### Humor Bot

```clojure
(component :Humor)

{:Agentlang.Core/Agent
 {:Name "Comedian"
  :Input :Chat/Session
  :UserInstruction "You are an AI bot who tell jokes"}}
```
## Team of AI Agents

AI Agents can delegate tasks to other specialized agents and dramatically increase the efficiency and accuracy of agentic behavior.

### Expense Processor

Scans expense receipts and generates expense records

```clojure
(component :Expense)

(entity
 :Expense
 {:Id :Identity
  :Vendor :String
  :Address :String
  :Amount :Double
  :ExpenseDate :Date
  :CreatedAt {:default now}})

{:Agentlang.Core/Agent
 {:Name "OCR Agent"
  :Type :planner
  :UserInstruction (str "Analyse the image of a receipt and return only the items and their amounts. "
                        "No need to include sub-totals, totals and other data.")
  :LLM "openai-4o-mini"}}

{:Agentlang.Core/Agent
 {:Name "Expense Handler"
  :Type :planner
  :LLM "openai-4o-mini"
  :UserInstruction "Convert an expense report into individual instances of the expense entity."
  :Tools [:Expense.Workflow/Expense]
  :Input :Expense.Workflow/SaveExpenses
  :Delegates {:To :receipt-ocr-agent :Preprocessor true}}}
```
## Data Modeling

Model any business domain - from simple to complex - with the relationship graph based data modeling approach of AgentLang. Apply RBAC policies, declaratively, to the data model and secure your application data.

## Dataflow

Dataflow allows you to express complex business logic simply as purely-declarative [patterns of data operations](https://docs.agentlang.io/docs/concepts/declarative-dataflow).

# Getting Started

### Prerequisites

1. [Java SE 21](https://openjdk.org/projects/jdk/21/) or later
2. Linux, Mac OSX or a Unix emulator in Windows
3. Download and install the [AgentLang CLI tool](https://github.com/agentlang-ai/agentlang.cli)
4. Set the `OPENAI_API_KEY` environment variable to a valid API key from OpenAI

### Running your example

Save your example into a file named `chat.al`. Now you can run the chat-agent as,

```shell
agent /path/to/chat.al
```

Once the agent starts running, send it a message with an HTTP POST like,

```shell
curl --header "Content-Type: application/json" \
--request POST \
--data '{"Chat/Session": {"UserInstruction": "tell me a joke about AI agents"}}' \
http://localhost:8080/api/Chat/Session
```

You should see a response from the agent with a joke about itself!

### Contributing

If you are excited about cutting-edge AI and programming language technology, please consider becoming a contributor to the Agentlang project.

There are two main ways you can contribute:

  1. Try out the language, report bugs and proposals in the project's [issue tracker](https://github.com/agentlang-ai/agentlang/issues).
  2. Actively participate in the development of Agentlang and submit your patches as [pull requests](https://github.com/agentlang-ai/agentlang/pulls).

### License

Copyright 2024 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0
