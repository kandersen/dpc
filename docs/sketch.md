# Protocol

```
data State = Client NodeID [Int]
           | ServerIdle 
           | ServerRun NodeID [Int]

mapInc :: Protlet State
mapInc = ARPC "mapInc" clientStep serverRec serverSend
  where
    clientStep = \case
      Client server ns -> 
        Just (server, ns, Client server)
      _ -> 
        Nothing
    
    serverIdle Msg{..} = \case
      ServerIdle ->
        Just (ServerRun _msgFrom _msgBody)
      _ ->
        Nothing

    serverSend = \case
      ServerRun client ns ->
        Just (client, (+1) <$> ns, ServerIdle)
      _ ->
        Nothing
```

# IMPLEMENTATION

## Single threaded

```
server1 :: Label -> M a
server1 l = do
  mmsg <- receive[ServerReceive(mapInc)] l 
  case mmsg of
    Nothing ->
      return ()
    Just (_, client, ns, _) ->
      send[ServerResponse(mapInc)] l client ((+1) <$> ns)
  server1 l
```

## 2-threaded

```
type Job = (NodeID, [Int])
type OPB a = Ref (Maybe a) -- One Place Buffer

put :: a -> OPB a -> M ()
put a r = do
  b <- CAS r Nothing (Just a)
  if b
    then return ()
    else put a r

take :: OPB a -> M a
take r = do
  ma <- read r
  case ma of
    Nothing ->
      take r
    Just a -> do
      b <- CAS r (Just a) Nothing
      if b
        then return a
        else take r

server2 :: Label -> M a
server2 l = do
  r <- newRef Nothing
  par [listener l r, handler l r] undefined

listener :: Label -> OPB Job -> M a
listener l r = do
  mmsg <- receive[ServerReceive(mapInc)] l 
  case mmsg of
    Nothing ->
      return ()
    Just (_, client, ns, _) ->
    put (client, ns) r
  listener l r

handler :: Label -> OPB Job -> M a
handler l r = do
  (client, ns) <- take r
  send[ServerResponse(mapInc)] l client ((+1) <$> ns)
  handler l r
```

Desiderate:
  - the _send_ of the handler is only performed _if_ the listener has received a request. We would like a spec like:
   
    ```
      send[ServerResponse(p)] l nid msg : 
        { CanRespond(p.serverSend) /\ 
          n |->_l Running s /\
          p.serverSend(s) = Just(nid, msg, s') }
        { ret. ret = () /\ n |->_l Running s' }                         
    ```

  - the request is transferred via the One place buffer, and the _knowledge_ of the state s of the nodes involved in the protocol should be transferred with it.

## Proof

### First Attempt!

  We enrich the OPB with an invariant, and a predicate to connect this, where `n` is the local node identifier: 
   ```
    (r :: OPB a I) iff r |->_n ma /\ I ma
   ```

   this yields the following spec for the `put` and `take` operations:

   ```
     put x r : { r :: OPB a I /\ I (Just x) }
               { ret. ret = () /\ r :: OPB a I }
     take r : { r :: OPB a I }
              { ret. ret = a /\ I (Just x) }
   ```

   we instantiate the invariant as follows:

     I ma iff (ma = Nothing /\ n |->_l Running ServerIdle) \/
              (exists c ns, ma = Just (c, ns) /\ n |->_l Running (ServerJob c ns)) 

  this way we can CAS the reference from `Nothing` to `Just ...` and vice versa supplying appropriate evidence to perform the swap.

  However, this means the _send_ must take place at the same time as the CAS, as we learn from the CAS that the server is processing, and has the permission to respond to the client. This is the resource needed to guard the send. The send yields evidence we're in the ServerIdle state, and this needs to be propagated back to the handler. This requires information to flow backwards from the `handler` to the `listener`. This can only happen by communication via the shared memory. But what we want to communicate is that something has taken place in the network.

  Some possibilities:
  1. We introduce atomic operations that perform both network and shared-memory operations. 
  2. Perhaps the "node n is running in state s" is not the right abstraction for resources guarding protocol actions. 
    - Perhaps we should focus on the `CanRespond` view. Thinking ahead to the two phase commit, we meet new complications to this scheme. Entering the "MakeDecision" state of the client can either respond "Yes" or "No". Either transition is permissible, so if these resources are presented as seperable resources to the current thread, then one can be invalidated by another thread performing the other action.

### Second Attempt

  The first attempt fails because there is no way for the handler to communicate back to the listener that the send is complete. It can only signal that the OPB has been emptied. 

  We could let the `listener` _learn_ that it is in the _ServerIdle_ state by trying to receive the request, thereby gainining the knowledge that the rest of the network "expects" us to be in a given state.

  >This feels somewhat unsatisfactory. 

  I.e. we have a receive spec like

  ```
  receive[ReceiveARPC(p)] l :
    { emp }
    { ret. exists s. 
        if ret == Nothing 
          then emp
          else ret = Just msg /\
               p.serverReceive(msg, s) = Just s' /\
               n |-> Running s' /\
               "Gain resources enabling transitions from s'" }
  ```

instead of something _ensuring_ we are in an appropriate state for the protocol.

  ```
  receive[ReceiveARPC(p)] l :
    { n |-> Running s }
    { ret. 
        if ret == Nothing 
          then n |-> Running s
          else ret = Just msg /\
               p.serverReceive(msg, s) = Just s' /\
               n |-> Running s' /\
               "Gain resources enabling transitions from s'" }
  ```

The point of a 'weakening' like this is to let the `listener` part of the server implementation _try_ to receive a message without needing to wait for the `handler` to complete the `send`. 

The "emerging" spec from this will ensure "protocol fidelity" but it's not immeadiately clear how to make that precise.

