# Semantics

# States

A global state speaks of the entire netowrk state of a specific protocol instance.

A local state 


## Session Fidelity


Actions 
```
Msg = sender x label x tag x body x receiver

NetworkActions na
na ::= Send Msg
     | Receive Msg
     | Skip
```

Local Actions
```
la ::= Network na
     | Read(nid,X,t,v)
     | Write(nid,X,t,v)
     | Alloc(nid,X,t,v)
     | CAS(nid, X,v,v',b)
```

We can project out the network actions from a trace of local actions by the projection/erasure:
```
erase (Network na) = na
erase _            = Skip
```


Program 
```
Commands c
c ::= send :: Label -> String -> [Int] -> NodeID -> m ()
      receive :: Label -> [String] -> m (Maybe Packet)
      this :: m NodeID
      par :: [m a] -> ([a] -> m c) -> m c
      allocRef :: a -> m (Ref m a)
      readRef :: Ref m a -> m a
      writeRef :: Ref m a -> a -> m ()
      casRef :: Eq a => Ref m a -> a -> a -> m Bool
```

Trace
```
[[c]]_net \in NetworkActions^*
[[c]]_loc \in LocalActions^*
[[c]]_net <= [[c]]_loc
```

## Example

```
-- A Type of One-Place Queues
type OPQ a = Ref (Maybe a)

mkOPQ :: M (OPQ a)
mkOPQ = alloc Nothing

trace = Alloc(res, Maybe a, Nothing)

put :: a -> OPQ a -> M ()
put a q = do
  res <- casRef q Nothing (Just a)
  if res
    then return ()
    else put a q

traces:
CAS(q,Just v0,Just a,False)
CAS(q,Just v1,Just a,False)
...
CAS(q,Nothing,Just a,True)

take :: OPQ a -> M a
take q = do
  ma <- readRef q
  case ma of
    Nothing -> take q
    Just a -> do
      res <- casRef q (Just a) Nothing
      if res
        then return a
        else take q

traces:
Read(q, Nothing)
Read(q, Nothing)
Read(q, Just v0)
CAS(q, Just v1, Nothing, False)
Read(q, Just v1)
CAS(q, Nothing, Nothing, False)
Read(q, Nothing)
Read(q, Just v2)
CAS(q, Just v2, Nothing, True)

put a q : { P(a) /\ OPQ(q, P) }{ OPQ(q, P) }
take q : { OPQ(q, P) }{ OPQ(q, P) /\ P(res) }
```

```
spinReceiveInto :: (q : OPQ [(NodeID, [Int])]) -> M ()
spinReceiveInto q = do
  (_, client, args, _) <- spinReceive[ServerReceive(process)] ["Process__Request"]
  put (client, args) q


```

Receive(l)



process :: (q : Queue[(NodeID, [Int])]) -> M ()
process q = do
    { Queue(q,QI) }
  (client, args) <- pop q
    { Queue(q, QI) * n |-> Running (Server (client, args)) }
  send[ServerResponse(process)] "Process__Response" client [sum args]
    { Queue(q,QI) }