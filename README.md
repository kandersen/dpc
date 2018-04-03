# Network Simulation

## Overview

This repository contains a (work-in-progress) development of a framework for experimenting with fine-grained concurrent, distributed programming languages and mechanisms for specifying programs. 

The core modules are outlined below:

### NetSim.Language

The `MonadDiSeL` typeclass extends a monadic language with `send` and `receive` primitives for message passing; a `par` combinator for running sub-computations in parallel; and a `this` operation for querying the identity of the thread/node/unit of concurrency that the computation is running on. 

With these primitives we can build higher-level operations, such as a blocking receive (allowing for the primitive `receive`  to be non-blocking), and more intricate message passing patterns such as a "synchronous RPC" (our term) where a client essentially invokes a computation on a remote server and waits for the result. See `rpcCall` in `src/NetSim/Language.hs`.

Case studies have inspired other combinators like broadcast polling. 

## Building and Running

### Setup and build

To build with `stack`, first make sure you have the latest version:
```
stack upgrade
```
The project can be then compiled with
```
stack install
```
### Running simulations

Once built run 
```
stack repl
```

Then run the simulator of the corresponding distributed application as follows:
```
runGUI NetSim.AppName.initNetwork
```
where `AppName` is, e.g., `BatchCalculator` or `DistributedLocking`. Use digits `1-N` to choose the next system move.


