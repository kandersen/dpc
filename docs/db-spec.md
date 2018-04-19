# DB Spec

Basic assertion logic contains points to assertions parameterised by the protocol instance. They point to local state of the node. THe basic form of the triple is

C |-^n p : {P}{Q}

Where C is a collection if protocols, n is the name of the current or executing node (the value of `this`, the name at which messages are received and the sender of any messages sent by the program `p`).

Each node has some default state, the local state assertion is as follows:

n |->_l (inbox, s)

where l is a protocol instance label, inbox is a multiset of messages, s is a (NodeState s) parameterised by an l-protocolwide specificed statespace.

A message is a 5-tuple of `label x nid x tag x [int] x nid` consisting of a protocol instance, a sender, a tag, a payload and a receiver.

DiSeL has as key abstraction in the specification of distributed systems the notion of _transition_. The primitive distributed operations of the programming language, _send_ and _receive_ are instrimunted by these transitions. These instrumentations can be erased - the operational behaviour is not influenced by the transitions of the protocol, only the proof burden.

> Do they serve as programmer intent annotations? _this_ is where _this_ step of _that_ protocol takes place?

Defining transitions turns out to be somewhat derivative work where a number of distributed algorithms make use of standard patterns of communication - hence, the transitions are formulated according to pattern. One such pattern is the remote procedure call (RPC) where a client sends a message to a server that performs some computation before responding with a result. From a client perspective, if it was to block and await that response, it in essence functions as a procedure call, just running remotely.

A transitions are pairs of _conditions_ and _step_ transformations that describe the precondition for performing a transition and the update to the local state.

A pattern like RPC involves 4 such transitions: the caller sending, the server receiving then responding, and finally the caller receiving the response. 

Part of this work is the observation that these transitions can be generated from simpler descriptions.

An RPC for an addition function might look as follows:

```
add = RPC "Add" clientStep serverStep
  where
    clientStep s = case s of
      ClientInit server a b -> Just (server, [a, b], ClientDone . head)
      _ -> Nothing
    serverStep Server [a, b] = Just([a + b], Server)
```

This spec says a node in state `ClientInit server a b` can initiate the RPC by sending '[a, b]' to `server`, and then blocking until a response is received from `server`. A node can process that request once it's in state `Server`, where it will respond with `[a + b]` and continue as a server. The client will continue in state `ClientDone n` upon receiving `[n]` from the server.

All these messages are appropriately based on the "Add" name used in the RPC constructor. 

The instrumented messaging primitives can then be given Hoare-style specs based on the transitions they are decorated with, as follows from the diesel paper:
```
C |-_n send[st, l](msg, to) :
          { n |->_l s /\ st.pre(n, to, m, s) }
          { n |->_l s' /\ st.step(n, to, m, s) = s' }
```

The idea now is that we can derive transitions from the RPC description. Perhaps `translation` functions like `ClientCall :: RPC -> Label ->  Transition`, `ServerReturn :: RPC -> Transition`, taking the above `add :: RPC` as the example, recall that it is defined by a `"Add" :: Tag`, `clientStep :: State -> Maybe (NodeID, [Int], [Int] -> State)` and `serverStep :: State -> Maybe ([Int] -> State)`.

```
ClientCall(rpc, l) = 
  \node, to, msg, statelet -> statelet(node, l) = Running s /\ rpc.clientStep(s) = Just(to, msg, k),
  \node, to, msg, statelet ->
    let (Running s) = statelet(node, l) in 
    let Just(to, msg, k) = clientStep(s) in
    statelet[(node, l) |-> BlockingOn(tag, to, k)]#statelet.MS + (l, n, tag, msg, from, )
```

```
send[ClientCall(add, l)]([a, b], server) : 
  { n |->_l Running (ClientInit server a b) }
  { n |->_l BlockingOn ("Add__Response", server, ClientDone . head) }

receive[ClientReturn(add, l)] : 
  { n |->_l BlockingOn ("Add__Response", server, ClientDone . head) }
  { if res = Some(Add__Response(server, ans) 
    then n |->_l Running (ClientDone . head $ ans)
    else n |->_l BlockingOn ("Add__Response", server, ClientDone . head) } 
```
`receive[ServerCall(add)] : {} {}`
`send[ServerReturn(add)](msg, to) : {} {}`
`receive[ClientReturn(add)] : {} {}`

With these in hand, we can build even higher-level combinators for the client and server side of the RPC as follows:

`rpcCall[add](server, args) : {} {}`
`handleRPC[add](handler) : {} {}`

perhaps built with other higher-level operations like

`spinReceive[T, L] : {}{}`





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

Hmm, we probably need something akin to the `interpretation ` section of Caper to the tie the States of the protlet statespace to concrete states in the proofs. E.g. `ClientWrite Int` is symbolically the intent to write an integer, but that can be realized in different ways in actual code. 