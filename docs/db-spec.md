# DB Spec
    
data DBState m = DBState {
  _locks :: Map Label (RWLock m),
  _cells :: Map Label (Ref m Int)
}

WellFormedDB(Labels, DBState locks cells) := Dom(locks) == Labels == Dom(cells)

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

mkDB labels :: MonadDiSeL m => Map Label Int -> m (DBState m)
  requires ;
  ensures WellFormedDB(labels, ret);

readDB db label :: MonadDiSeL m => DBState m -> Label -> m Int
  requires WFDB(labels, db) /\ label \in labels
  ensures ...;

writeDB :: MonadDiSeL m => DBState m -> Label -> Int -> m ()
  requires 
dbServer :: MonadDiSeL m => DBState m -> m a
snapshotter' :: MonadDiSeL m => Label -> DBState m -> m a
compositeServer :: MonadDiSeL m => [Label] -> Label -> m a 


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
pinReceive label tags = do
    mmsg <- receive label tags
    case mmsg of
      Nothing -> spinReceive label tags
      Just msg -> return msg
