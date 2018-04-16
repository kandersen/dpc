The purpose of this work is to extend the DiSeL framework with concurrency at the node level. The state of DiSeL is such that each node in the network spanning the distributed computated is a single-threaded program, sending and receiving messages while manipulating node-local state.

This is expressive and interesting enough to express algorithms (protocols) from the distributed systems literature; to express interesting specifications and to program adequate implementations.

In reality, one node will in general be participating in the network in a multitude of roles, and these roles can be served concurrently. The current system cannot distinguish this concurrency at the node level, and can at best inadequately model the threads of a concurrent node as several nodes.

By extending the implementation language of DiSeL with concurrency primitives, we can improve the adequacy of modelling networks with nodes serving multiple roles in one or more protocols concurrently.

-- 

The programming language of DiSeL is a core, purely functional programming language enriched with a monad for performing network communication and side effects. We enrich this language with a constructor `par :: [m a] -> ([a] -> m b) -> m b` which takes a sequence of actions to perform in parallel and a continuation awaiting the values produced.

--

The work done to explore case studies in this work has shown a need for a better specification language for DiSeL. In this work we also explore a higher-level specification language for DiSeL.

Much proof and specification effort are spent on essentially writing out boilerplate, so we lift these patterns to a language of specifications that can be translated to the DiSeL framework. The translation can then not only expand definintions into the required boilerplate, but also generate statements and even proofs of cohesion and other well-formedness criteria that specifications in DiSeL must adhere to.

The language specifies a network protocol by essentially enumerating the statespace of the nodes involved in the protocol, and then encodes transitions of the state-machines describing the protocol implicitly through combinators corresponding to common patterns in distributed protocols.

For example, a common pattern is akin to a remote procedure call (RPC): one node passes a message to another that, when it is ready to receive it, computes an appropriate response. All the while the sender is blocking, waiting for the answer.

Technically, this involves 4 protocol transitions: sending, and receiving, the request and response, respectively. The fact that the calling node is blocking must also be encoded in the protocol on an ad-hoc basis.

Alternatively, the general structure of this pattern can be lifted out, and the state space of the protocol enriched with a "blocking" state for each RPC specified. Then, a partial function from node states to message payloads can indicate what states can perform the RPC, and a partial function from payloads and node states to payloads can describe the states in which the server can respond and with what payload.

The 4 transitions for the DiSeL specification of this pattern can then be translated from this description.



This work is also interested in investigating if you we can lift the level of proof to these protocol fragments. 

--

The biggest case study so far is a read-write protected data-base, as follows: a database contains a number of cells. Clients can query and modify the values of the cells. Local to the cells a snapshotting process runs, that will periodically grab a consistent snapshot across all memorycells.

This specification will probably be very nice with a history-related specification, as we will be able to specify that each snapshot is taken out of the history of the database, precisely as in [1].

Histories are a kind of resource mapping abstract time stamps to _changes_ in abstract state. The state is then further tied to concrete representation through more of the specification, but abstractly, all one needs to know is the history formalism. It can be expressed like `t |-> (s_1, s_2)`, suggesting that at time `t` the state was changed from `s_1` to `s_2`. 

Do absences of such resource tell you that there was no change? No probably not, but possibly the resource `t 
-> emp/0/[]/nil` might be consistent with the logic.



[1]: Sergey et al at ESOP15.
<!--stackedit_data:
eyJoaXN0b3J5IjpbMTA5ODM0MjEyOSwtMTMwOTgxNzI4OSwxMj
YxNjg3ODAwXX0=
-->