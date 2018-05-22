# CSL Style locks

We eschew the fine-grained concurrent presentation in favor of a simpler CSL style locking with second-class locks.

The multi-threaded server could be expressed as follows, where we use a global lock for every reference. Hence, we can use a 'using' block to take that lock. We no longer have atomic memory references, but ensure exclusive access with the locking.

```
Ref : (a : Type) -> (a -> Assertion) -> Type

type OPB a = Ref (Maybe a)
                 (\ma. case ma of { Nothing -> True; Just a -> SendGuard } )

server2 :: Label -> M a
server2 l = do
  r <- newRef Nothing
  par [listener l r, handler l r] undefined

listener :: Label -> OPB Job I -> M a
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

where `put` and `take` lock `r` suitably:

```
put :: a -> OPB a I -> M ()
put { I (Just a) }
    { _. True }
put a r = do
  b <- using r $ do
    ma <- readRef r
    case ma of
      Nothing -> do
        writeRef r (Just a)
        return True
      _ -> 
        return False
  if b
    then return ()
    else put a r

take :: Eq a => OPB a -> M a
take { True }
     { I (Just a)  }
take r = do
  res <- using r $ do
    ma <- readRef r
    case ma of
      Nothing -> return Nothing
      Just a -> do
        writeRef r Nothing
        return (Just a)
  case res of
    Nothing -> 
      take r
    Just a
      return a
```