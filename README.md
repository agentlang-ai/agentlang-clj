[![AppCI](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/app.yml)
[![AgentLang clj CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-clj.yml)
[![AgentLang cljs CI](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml/badge.svg)](https://github.com/agentlang-ai/agentlang/actions/workflows/agentlang-cljs.yml)

# The AgentLang Programming Language
AgentLang is a very high-level, declarative, open-source programming language for solving complex tasks with the help of interacting AI agents.
An AI agent can be enhanced with tools, knowledge bases and chat prompts. Agents can also form complex graphs of inter-relationships,
which allows them to collaborate together in solving difficult problems.

While most AI programming frameworks limit themselves to LLM based text-processing and generation tasks, AgentLang is designed
as a complete tool for real-world application development. As a language, AgentLang is data-oriented and declarative, with
an abstraction that is closer to natural languages than traditional programming languages. This makes AgentLang a much better
fit for LLM-powered code generation. Users can rapidly build business application in AgentLang from high-level
specifications - typically more than 10x faster than traditional programming languages.

## AgentLang is open
The AgentLang language specification, its compiler and runtime are open source.

The code you build in AgentLang can be run anywhere using the open source compiler and runtime, thereby avoiding the vendor
lock-in of other AI programming platforms.

## AgentLang is innovative
AgentLang introduces a number of innovative concepts to programming:

1. **First-class AI Agents** - interacting AI Agents is a built-in language concept - developers can choose from one of the built-in agent-types, or easily add their own new types.
2. **Graph-based Hierarchical Data Model** - compose the high-level data model of an application as a hierarchical graph of business entities with relationships. Such [entities and relationships](https://docs.agentlang.io/docs/concepts/data-model) are first-class constructs in AgentLang.
3. **Zero-trust Programming** - tightly control operations on business entities through [declarative access-control](https://docs.agentlang.io/docs/concepts/zero-trust-programming) encoded directly in the model itself.
4. **Declarative Dataflow** - express business logic as [purely-declarative patterns of data](https://docs.agentlang.io/docs/concepts/declarative-dataflow).
5. **Resolvers** - use a simple, but [powerful mechanism](https://docs.agentlang.io/docs/concepts/resolvers) to interface with external systems.
6. **Interceptors** - [extend the agentlang runtime](https://docs.agentlang.io/docs/concepts/interceptors) with custom capabilities.
7. **Entity-graph-Database Mapping** - take advantage of an [abstract persistence layer](https://docs.agentlang.io/docs/concepts/entity-db-mapping) for fully-automated storage of entity instances.

## A Taste of AgentLang

The following code snippet shows a simple agent that can interact with a human user:

```clojure
(component :Chat)

{:Agentlang.Core/Agent
 {:Name :Chat/ExampleAgent
  :Input :Chat/Session
  :UserInstruction "You are an AI bot who tell jokes"}}
```

Save this code to a file named `chat.al` and it's ready to be run as a highly-scalable service with ready-to-use
HTTP APIs to interact with the agent. But before you can actually run it, you need to install AgentLang.
The next section will help you with that.

## Download and Install

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

## License

Copyright 2024 Fractl Inc.

Licensed under the Apache License, Version 2.0:
http://www.apache.org/licenses/LICENSE-2.0