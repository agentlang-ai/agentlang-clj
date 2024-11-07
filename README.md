<div align="center">

[![AppCI](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml)
[![AgentLang clj CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml)
[![AgentLang cljs CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml)

# AgentLang Programming Language

AgentLang is the **easiest way** to build AI Agents, Chatbots and Apps - build **teams of AI agents** that collaborate (with other AI agents and humans) to handle complex, time-consuming, monotonous tasks.

### **Open | Enterprise-grade | Production-ready**

</div>

The AgentLang language specification, its compiler and runtime are open source. The code you build in AgentLang can be run anywhere, thereby **avoiding the vendor lock-in** of other AI agent/programming platforms.

## AgentLang is innovative

AgentLang is a data-oriented, declarative abstraction for building agents and apps, similar to how Terraform is a declarative abstraction for infrastructure-as-code. It is feature-rich making programming easy and fun:
* **First-class AI Agents** - interacting AI Agents is a built-in language construct - developers can choose from one of the built-in agent-types, or easily add their own agent-types.
* **Graph-based Data Model** - model real-world relationships elegantly as a hierarchical graph of business entities with relationships. Such [entities and relationships](https://docs.agentlang.io/docs/concepts/data-model) are first-class constructs in AgentLang.
* **RBAC** - tightly control operations on business entities through [declarative access-control](https://docs.agentlang.io/docs/concepts/zero-trust-programming) encoded directly in the model itself.
* **Declarative flows** - express complex business logic simply as purely-declarative [patterns of data operations](https://docs.agentlang.io/docs/concepts/declarative-dataflow).
* **Resolvers** - use a simple, but [powerful mechanism](https://docs.agentlang.io/docs/concepts/resolvers) to interface with external systems.

AgentLang programs run on the JVM and can make use of any of the millions of existing Java/Clojure and other JVM libraries out there. AgentLang runtime has native integration with databases, vector dbs, auth systems.

AgentLang comes with all the modern tooling, dependency management needed to build production-grade agents and apps.

## A Taste of AgentLang

The following code snippet shows a simple agent that can interact with a human user:

```clojure
(component :Chat)

{:Agentlang.Core/Agent
 {:Name :example-agent
  :Input :Chat/Session
  :UserInstruction "You are an AI bot who tell jokes"}}
```

Save this code to a file named `chat.al` and it's ready to be run as a highly-scalable service with auto-generated HTTP APIs for interacting with the agent. But before you can actually run it, you need to install AgentLang. The next section will help you with that.

### Download and Install

#### Prerequisites

1. [Java SE 21](https://openjdk.org/projects/jdk/21/) or later
2. Linux, Mac OSX or a Unix emulator in Windows
3. Download and install the [AgentLang CLI tool](https://github.com/agentlang-ai/agentlang.cli)
4. Set the `OPENAI_API_KEY` environment variable to a valid API key from OpenAI

Now you can run the chat-agent as,

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

If all goes well, the agent will reply with a joke about itself!

### Contributing

If you are excited about cutting-edge AI and programming language technology, please consider becoming a contributor to the Agentlang project.

There are two main ways you can contribute:

  1. Try out the language, report bugs and proposals in the project's [issue tracker](https://github.com/agentlang-ai/agentlang/issues).
  2. Actively participate in the development of Agentlang and submit your patches as [pull requests](https://github.com/agentlang-ai/agentlang/pulls).

### License

Copyright 2024 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0
