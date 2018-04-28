# Round 3

The question is whether either of the following two server implementations satisfy the the spec { True }{ False }: i.e., endlessly running server loop that obeys an asynchronous RPC protocol where these implementations play the role of servers. Internally, they use a queue in local shared memory to communicate the incomming requests between a receiver and a handler loop. Ideally, we would like to be able to show the spec of both a sequential implementation and of a parallel, where the receiver is concurrently running alongside one or more handlers.

Compare

```
while(true) {
  spinReceiveInto(q);
  process(q);
}
```

versus

```
   while(true) { spinReceiveInto(q) }
|| while(true) { process(q) }
|| while(true) { process(q) }
```


## Spec

Recall we build local state assertions for node `n` by wrapping a protocol specific state `s` in the constructors

```
data NodeState s = Running s
                 | BlockingOn NodeID Tag ([Int] -> s)
```

This has a pcm structure, assuming we have a pcm structure for `s`:

```
Running s • Running s' = Running (s • s') if s • s' defined
BlockingOn id t k • BlockingOn id t k = BlockingOn id t k
undefined otherwise
```


```
data States = Client NodeID [Int]
            | Server (MultiSet (NodeID, [Int]))

pcm of states:

(Client n args) • (Client n args) = Client n args
(Server ms) • (Server ms') = Server (ms \+ ms') if ms # ms'
_ • _ otherwise is undefined
```

This induces a PCM on the local state assertions. Let the current node be `n`, then

``` 
n |-> Running (Server reqs) <=> n |-> Running (Server reqs1) * Running (Server reqs2) where reqs = reqs1 \+ reqs2 and reqs1 # reqs2
```

```
    Request(args)
     /----->
( C )        ( S )
      <-----/
  Response(f(args))

process = ARPC "Process" clientStep serverReceive serverSend
  where
    clientStep (Client server n) =
      Just $ (server, [n], Client server)

    serverReceive msg (Server reqs) =
      Just $ Server $ reqs \+ (_msgFrom msg, _msgBody msg)

    serverSend _ (Server reqs) = do
      req@(client, args) <- reqs
      pure (Message client "Process__Response" [sum args], Server $ reqs - req)
```

```
receive[ServerReceive(ARPC)] ["Process__Request"] :
  { n |-> Running (Server reqs) }
  { if res = Some msg
    then n |-> Running (Server $ reqs \+ (msg.sender, msg.body))
    else n |-> Running (Server reqs) }

send[ServerRespond(arpc)] "Process__Response" id [sum args] :
  { n |-> Running (Server $ reqs \+ (id, args)) }
  { n |-> Running (Server $ reqs) }
```

from which we should be able to build an interference-relation on states.

```
type Queue a

Queue(q, P) <=> Queue(q, P) * Queue(q, P)

push :: (a : a) -> (q : Queue a) -> M ()
  { P(a) /\ Queue(q, P) }
  { (). Queue(q, P) }
pop :: (q : Queue a) -> M a
  { Queue(q, P) }
  { (). P(ret) /\ Queue(q, P) }

par :: [M a] -> ([a] -> M b) -> M b
  forall i. { P_i } cs !! i { x_i. R_i x }
  forall i. { * R_i (xs !! i) } k xs { r. Q } 
 -------------------------------------------
        { * P_i } par cs k { r. Q }

forever :: M a -> M b
        { I } c { I }
  -------------------------
  { I } forever c { _. False }

QI(client) = exists reqs, args. n |-> Running (Server (reqs \+ (client, args)))

spinReceiveInto :: (q : Queue [(NodeID, [Int])]) -> M ()
spinReceiveInto q = do
    { Queue(q,QI) * n |-> Running (Server reqs) }
  (_, client, args, _) <- spinReceive[ServerReceive(process)] ["Process__Request"]
    { Queue(q,QI) * n |-> Running (Server $ reqs \+ (client, args)) }
    { Queue(q,QI) * n |-> Running (Server reqs) * n |-> Running (Server $ emp \+ (client, args) }
  push (client, args) q
    { (). Queue(q,Q) * n |-> Running (Server reqs) }

process :: (q : Queue[(NodeID, [Int])]) -> M ()
process q = do
    { Queue(q,QI) }
  (client, args) <- pop q
    { Queue(q, QI) * n |-> Running (Server (client, args)) }
  send[ServerResponse(process)] "Process__Response" client [sum args]
    { Queue(q,QI) }

server1 :: Queue[(NodeID, [Int])] -> M a
server1 q = forever $ do
    { Queue(q, QI) * n |-> Running (Server _)) }
  spinReceiveInto q
    { Queue(q, QI) * n |-> Running (Server _)) }
  process q
    { Queue(q, QI) * n |-> Running (Server _)) }
  { False }

server2 :: Queue[(NodeID, [Int])] -> M a
server2 q = 
  { Queue(q, QI) * n |-> Running (Server _) }
  { Queue(q, QI) * n |-> Running (Server _)   -- P1
    * Queue(q, QI)                            -- P2
    * Queue(q, QI) }                          -- P3
  par [ forever (spinReceiveInto q )
      , forever (process q)
      , forever (process q)] 
      undefined
  }
  { _ . False }
```




# DB Spec - Take 2

Basic assertion logic contains points to assertions parameterised by the protocol instance. They point to local state of the node. The basic form of the triple is

C |-^n p : {P}{Q}

Where C is a collection if protocols, n is the name of the current or executing node (the value of `this`, the name at which messages are received and the sender of any messages sent by the program `p`).

Each node has some default state, the shape of the local state assertion is as follows:

n |->_l (inbox, s)

where l is a protocol instance label, inbox is a multiset of messages, s is a (NodeState s) parameterised by an l-protocolwide specificed statespace.

A message is a 5-tuple of `label x nid x tag x [int] x nid` consisting of a protocol instance, a sender, a tag, a payload and a receiver.

DiSeL has as key abstraction in the specification of distributed systems the notion of _transition_. The primitive distributed operations of the programming language, _send_ and _receive_ are instrimunted by these transitions. These instrumentations can be erased - the operational behaviour is not influenced by the transitions of the protocol, only the proof burden is.

> Do they serve as programmer intent annotations? _this_ is where _this_ step of _that_ protocol takes place?

Defining transitions turns out to be somewhat derivative work where a number of distributed algorithms make use of standard patterns of communication - hence, the transitions are formulated according to pattern. One such pattern is the remote procedure call (RPC) where a client sends a message to a server that performs some computation before responding with a result. From a client perspective, if it was to block and await that response, it in essence functions as a procedure call, just running remotely.

Transitions are pairs of _conditions_ and _step_ transformations that describe the precondition for performing a transition and the update to the local state. The condition is a predicate on the on the local protocol state of the involved nodes and the message sent. The step is a partial function that, in effect, is defined when the precondition is met, computing a new local state based on the old one, assuming the precondition is met.

The instrumented messaging primitives can then be given Hoare-style specs based on the transitions they are decorated with, as follows from the diesel paper:
```
C |-_n send[st, l](msg, to) :
          { n |->_l s /\ st.pre(n, to, m, s) }
          { n |->_l s' /\ st.step(n, to, m, s) = s' }
```


A pattern like RPC involves 4 such transitions: the caller sending, the server receiving then responding, and finally the caller receiving the response. 

Part of this work is the observation that these transitions can be generated from simpler descriptions.

An RPC for a a server serving an addition function might look as follows:

```
add = RPC "Add" clientStep serverStep
  where
    clientStep s = case s of
      ClientInit server a b -> Just (server, [a, b], ClientDone . head)
      _ -> Nothing
    serverStep s@(Server _) [a, b] = Just([a + b], s)
```

Where the protocol specific statespace can be enumerated as

```
data State = ClientInit NodeID Int Int
           | ClientDone Int
           | Server [Message]
```

The list of messages is a collection of outstanding requests. We need it to technically build the RPC pattern, but we specify in the RPC pattern that the message should be handled immediately. If we wish to explicitly program and prove with the collection of messages, we can use the _asynchronous_ RPC, where the server does not respond to each request as the _next_ action, but can perform arbitrary work in between.

> Perhaps we can say something interesting relating RPC to ARPCs? Can the "atomicity" be observed by the client? Or maybe explicitly not? Does one refine the other? I think that the ARPC construction is probably what we really want as for a client to enforce "atomicity"the server seems impossible, and not generally useful. Our case studies will tell as we move on.

This spec says a node in state `ClientInit server a b` can initiate the RPC by sending '[a, b]' to `server`, and then blocking until a response is received from `server`. A node can process that request once it's in state `Server`, where it will respond with `[a + b]` and continue as a server. The client will continue in state `ClientDone n` upon receiving `[n]` from the server.

All these messages are appropriately based on the "Add" name used in the RPC constructor. 

The idea now is that we can derive the 4 transitions mentioned from the RPC description. Perhaps `translation` functions like `ClientCall :: RPC -> Label ->  Transition`, `ServerReturn :: RPC -> Transition`, taking the above `add :: RPC` as the example, recall that it is defined by a `"Add" :: Tag`, `clientStep :: State -> Maybe (NodeID, [Int], [Int] -> State)` and `serverStep :: State -> Maybe ([Int] -> State)`.

```
ClientCall(rpc, l) = 
  \node, to, msg, statelet -> statelet(node, l) = Running s /\ rpc.clientStep(s) = Just(to, msg, k),
  \node, to, msg, statelet ->
    let (Running s) = statelet(node, l) in 
    let Just(to, msg, k) = clientStep(s) in
    statelet[(node, l) |-> BlockingOn(tag, to, k)]#statelet.MS + (l, n, tag, msg, from, )
```

```
add |-_n send[ClientCall(add, l)]([a, b], server) : 
  { n |->_l Running (ClientInit server a b) }
  { n |->_l BlockingOn ("Add__Response", server, ClientDone . head)
    /\ message soup has message to server }

add |-_n receive[ClientReturn(add, l)] : 
  { n |->_l BlockingOn ("Add__Response", server, ClientDone . head) }
  { if res = Some(Add__Response(server, ans) 
    then n |->_l Running (ClientDone . head $ ans)
    else n |->_l BlockingOn ("Add__Response", server, ClientDone . head) } 

add |-_n receive[ServerCall(add, l)] :
  { n |->_l Running (Server reqs) }
  { if res = Some(Add__Request(n, msg, client))
    then n |-> Running (Server (reqs \+ (msg, client)))
    else n |-> Running (Server reqs) }

add |-_n send[ServerReturn(add, l)](msg, to) :
  { n |->_l Running (Server reqs) /\ reqs = reqs' \+ ([a, b], client) }
  { n |-> Running (Server reqs)
    /\ message soup has message to client }
```

With these in hand, we can build even higher-level combinators for e.g. client side of the RPC as follows:
```
add@l |->^n rpcCall[add,l](server, [a, b]) : 
  { n |->_l Running (ClientInit server a b) }
  { n |->_l Running (ClientDone (a + b)) }
```

> Probably we want some kind of logical resource to protect the send and receive primitives, essentially guards on the protocol instance transitions. How they should look and how they should work I am not sure yet. Perhaps the send consumes a transition and receive produces one? This way the protocol becomes kind of transactional. 

> Does this idea mix cleanly with the Fine-grained caper-like stuff? 

# DB Spec - Take 1

```   
data DBState m = DBState {
  _locks :: Map Label (RWLock m),
  _cells :: Map Label (Ref m Int)
}

WellFormedDB(Labels, DBState locks cells) := Dom(locks) == Labels == Dom(cells)
```

```
type RWLock m = Ref m Int

region RWLock(r, x) {
  guards |LOCK|;
  interpretation {
    0 : x |-> 0 * r@LOCK|-1|;
    1 : x |-> k * k > 0 * r@LOCK|-1-k|;
    2 : x |-> -1;
  }
  actions {
             : 0 ~> 2;
    LOCK|-1| : 2 ~> 0;
             : 0 ~> 1;
    LOCK|-1| : 1 ~> 0;
             : n ~> n;
  }
}

mkRWLock :: MonadDiSeL m => m (RWLock m)
mkRWLock() :
  { true }
  { RWLock(r, res, 0) }

readerEnter l :: MonadDiSeL m => RWLock m -> m ()
  requires RWLock(r, l, _);
  ensures RWLock(r, l, 1) * r@LOCK|1|;
readerExit :: MonadDiSeL m => RWLock m -> m ()
  requires RWLock(r, l, 1) * r@LOCK|1|;
  ensures RWLock(r, l, _);

writerEnter :: MonadDiSeL m => RWLock m -> m ()
  requires RWLock(r, l, __);
  ensures RWLock(r, l, 2) * r@LOCK|-1|;
writerExit :: MonadDiSeL m => RWLock m -> m ()
  requires RWLock(r, l, 2) * r@LOCK|-1|;
  ensures RWLock(r, l, __);
```

```
mkDB labels :: MonadDiSeL m => Map Label Int -> m (DBState m)
  requires ;
  ensures WellFormedDB(labels, ret);

readDB db label :: MonadDiSeL m => DBState m -> Label -> m Int
  requires WFDB(labels, db) /\ label \in labels
  ensures ...;

writeDB :: MonadDiSeL m => DBState m -> Label -> Int -> m ()
  requires 
```

```
dbServer :: MonadDiSeL m => DBState m -> m a
snapshotter' :: MonadDiSeL m => Label -> DBState m -> m a
compositeServer :: MonadDiSeL m => [Label] -> Label -> m a 
```

```
rpcCall :: Label -> String -> [Int] -> NodeID -> m [Int]
rpcCall[l](tag, msg, server) :
  { RPC[l](tag, CStep, _) * n |-> state * CStep(state) = Just (msg, server, k) }
  { n |-> k res }
rpcCall label protlet body to = do
  send label (protlet ++ "__Request") body to
  (_, _, resp, _) <- spinReceive label [protlet ++ "__Response"]
  return resp

ClientSendTransition(RPC[l], tag, CStep, SStep)) = ...

send[request(RPC[l], tag, CStep, SStep), l](msg, to) : 
  { RPC[l](tag, CStep, _) * n |-> state * CStep(state) = Just(msg, server, k) }
  { RPC[l](tag, CStep, _) * n |-> BlockingOn(tag, k) }

receive[response(RPC[l],tag,CStep,SStep)] : 
  { RPC[l](tag, CStep, SStep) * n |-> BlockingOn(tag, k) } 
  { RPC[l](tag, CStep, SStep) * 
    if res = Some(from, (tag, args))
    then n |-> k args /\ (from, n, •, (tag, args)) \in MS_l
    else n |-> BlockingOn(tag, k) }

spinReceive[response(RPC[l], tag, CStep, SStep)] :
  { RPC[l](tag, CStep, SStep) * n |-> BlockingOn(tag, k) }
  { RPC[l](tag, CStep, SStep) * 
    n |-> k res }
spinReceive label tags = do
    mmsg <- receive label tags
    case mmsg of
      Nothing -> spinReceive label tags
      Just msg -> return msg
```

>Hmm, we probably need something akin to the `interpretation ` section of Caper to the tie the States of the protlet statespace to concrete states in the proofs. E.g. `ClientWrite Int` is symbolically the intent to write an integer, but that can be realized in different ways in actual code. 
