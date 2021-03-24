# Distributed Protocol Combinators

[![License](https://img.shields.io/badge/License-BSD%202--Clause-orange.svg)](https://raw.githubusercontent.com/kandersen/dpc/master/LICENSE)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3902686.svg)](https://doi.org/10.5281/zenodo.3902686)

## Overview

This repository contains a (work-in-progress) development of a framework for experimenting with fine-grained concurrent, distributed programming languages and mechanisms for specifying programs. 

The core modules are outlined below:

### DPC.Language

The `MonadDiSeL` typeclass extends a monadic language with `send` and `receive` primitives for message passing; a `par` combinator for running sub-computations in parallel; and a `this` operation for querying the identity of the thread/node/unit of concurrency that the computation is running on. 

With these primitives we can build higher-level operations, such as a blocking receive (allowing for the primitive `receive`  to be non-blocking), and more intricate message passing patterns such as a "synchronous RPC" (our term) where a client essentially invokes a computation on a remote server and waits for the result. See `rpcCall` in `src/DPC/Language.hs`.

Case studies have inspired other combinators like broadcast polling. 

There are two implementation of the language, one into an AST capturing the basic monadic fragment + the distributed language, embedding Haskell's pure computations. Alternatively, the language can be interpreted as proper Haskell threads communicating via message passing. The implementation is decentralized, without a central message passing authority, all communication via channels, leaving execution order to the Haskell runtime.

### DPC.Specifications

This module contains a specification language for distributed programming languages. It tries to be very fine-grained and modular. The notion of composition and interaction is still unclear, but here follows first a sketch of the blocks included, then a brief consideration of what composition and interaction might mean/model in this system.

The framework is parametric in a notion of 'state' (as in state machine, not as in heap), in most type signatures represented by the type parameter `s`. The framework essentially provides the tools for describing state machines, providing some structure for describing transitions.

The framework enriches the state-space with a notion of 'blocking', which indicates that a node is awaiting a particular of responses. This is a pattern that has come up in every case study so far, hence it why it has been lifted into the framework. See the `NodeState` datatype

The framework defines a notion of `protlet`, which is a small protocol involving at least two nodes, to describe their pattern of interaction. These encapsulate patterns found in the case study so far. Every distributed protocol so far investigated can be described by interactions of remote procedure calls, broadcasts and event notifications. The framework lift's out the boilerplate implications of these patterns (i.e. a client initiating an RPC is blocking until it receives the response), so the designer need only specify the ingredients by means of pure Haskell functions.

For example, the pattern _RPC_ is specified by two partial functions - defined when the client is in a state from which it initiates the call, and one defined when the server can receive and respond to the call. Both must describe a continuation and a message payload. The client must supply a continuation parameterised on the response. An example of boilerplate that is factored out is that the server need not specify the NodeID of the client - this is known from the message.

The idea is that this level of abstraction is sufficient to describe an interesting class of distributed protocols. Furthermore, the patterns encoded should allow effective specifications of protocols in the DiSeL framework, which is otherwise quite boiletplate heavy. The idea is that a collection of protlets can be _compiled_ to a DiSeL protocol specification, a number of proof burdens like consistency and well-formedness conditions given by virtue of conforming to the framework.

The rest of the module includes code for 'executing' or simulating the protocols, which can be used to inspect the specification for correctness. Specifically, it gives a pure way of describing the evolution of a network of nodes starting in a given state for each instance of a protlet that node is involved in. Each subsequent network is then obtained by a series of send and receive transitions. All of this machinery is parameterised by a notion of non-determinism (parameter `f` in most functions) to enable back-tracking, which is not at the moment used for anything exciting, but could enable exhaustive exploration of state spaces or the like.

There's a `try` function for each component of protlets, returning an `f` full of all the ways these transitions are possible.

### DPC.Invariant

This module describes a language for writing inductive invariants for state machines expressed using the DPC.Specifications module, the idea being that the protocol can be checked against the invariant.

An invariant is simply a boolean predicate on a network, parameterised by a protlet instance label and some protocol specific `metadata`, i.e. to capture 'roles' or other protocol specific properties.

The module then supplies simple combinators for expressing a property on nodes, messages in the soup etc.

### DPC.GUI

This module contains functionality for stepping a network consisting of a set of nodes and a collection of protlets. The user is presented with a list of all possible transitions, and the network is advanced according to that transition. (The notion of non-determininsm used is that of lists -- i.e. all possible transitions are presented at every step).

An execution can be furthermore given an invariant, and the the application will check whether the invariant holds at every step.

## Areas of Improvement

See the TODO file.

## Case Studies

Found in the `DPC.Examples` module, 

* `BatchCalculator`, from the DiSeL paper
* `Calculator`, a concurrent polynomial calculator server
* `Database`, a read-write lock protected concurrent database
* `DistributedLock`, based on [Martin Kleppmann's blog post](https://martin.kleppmann.com/2016/02/08/how-to-do-distributed-locking.html)
* `Paxos`, a single decreee modular paxos implementation based on paper by Sergey et al
* `SimpleClientServer`
* `TwoPhaseCommit`, a simple 2PC protocol

Ideally, some of these case studies can build on others, as sub-modules, though it's as of yet unclear how this can work in the framework -- e.g. `2PC + Paxos`, or `Database + DistributedLock`.

## Building and Running

See `INSTRUCTIONS.md`.


## Taking Stock

We have:

* A *specification language* of RPCs. It is implemented in `Specifications.hs`. This is the language currently described in the "Overview" section of the paper. It has 'combinators' for RPCs and Broadcast messages, and is at the moment "interactively executable" i.e. simulations can be explored using the GUI. Technically, it is executable in any monad providing a non-deterministic binary choice operator, used to model the interleaving of nodes and to non-deterministic choice in the specification.
* A small library for describing predicates of the states in the aforementioned specification language. It is implemented in `Invariant.hs`. Provided an invariant, the GUI checks the validity of this invariant at every step of the exploration. This is so far not described in the paper, though I have tried outlining where it might fit in the second-to-last paragraph in the "Overview" section.
* A simple monadic *programming language*, a DSL for Message passing, parallel execution and shared memory, and compound operations built from these primitives is found in `Language.hs`. In the paper, it is described in Section 3, "A DiSeL Monad".
* The aforementioned language is given 3 notions of execution. They are in turn described in Section 3 of the paper.
    - a _pure_ interpretation, `Interpretations/Pure.hs` that implements the operational semantics of message passing and parallel execution in a purely functional style. The model is closely related to that of the specification language, though there is no actual code-sharing at the moment.
    - a _shared memory_ interpretation, `Interpretations/SharedMemory.hs` that implements the operational semantics of message passing, parallel execution and fine-grained shared memory operations using Haskell's notion of IO threads. The hierarchy of nodes and their local threads run as an actual hierarchy of Haskell IO threads.
    - a _web socket_ interpretation, `Interpretations/WebSockets.hs` that implement a library for building applications that implement nodes communicating via web-sockets. At the moment it supports just the message passing DSL, but should easily be extended to node-local concurrency.
* A series of examples that use and demonstrate an array of all of the above:

| Example            | Specification | Invariant | DSL Implementation | Web Socket App | 
|--------------------|---------------|-----------|--------------------|----------------| 
| Calculator         | X             |           | M(P)               | X              | 
| BatchCalculator    | X             |           | M                  |                | 
| Database           | X             |           | MSP                |                | 
| DistributedLocking | X             |           |                    |                | 
| Paxos              |               |           | M                  |                | 
| SimpleClientServer | X             |           |                    |                | 
| TwoPhaseCommit     | X             | X         | M                  |                | 

- _X: Implemented_
- _M: Implementation makes use of message passing_
- _S: Implementation makes use of shared memory concurrency_
- _P: Implementation makes use of parallel execution_
- _(P): Parallel version implemented in addition to sequential_
