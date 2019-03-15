{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
module DPC.Examples.Database where

import           Control.Concurrent
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.Map                            (Map, fromList)
import qualified Data.Map                            as Map

import DPC.Types
import DPC.Specifications
import DPC.Interpretations.SharedMemory
import DPC.Language

-- | Specification


data S = ClientRead NodeID
       | ClientWrite NodeID Int
       | ClientIdle NodeID
       | Server Int
       | SnapshotServer [Int]
       | SnapshotClient NodeID
       | SnapshotClientDone [Int]

clientRead :: Protlet f S
clientRead = RPC "Read" clientStep serverStep
  where
    clientStep = \case
      ClientRead serverID -> Just (serverID, [], clientReceive serverID)
      _ -> Nothing
    clientReceive serverID [_] = ClientIdle serverID
    serverStep [] (Server n) = Just ([n], Server n)


clientWrite :: Protlet f S
clientWrite = RPC "Write" clientStep serverStep
  where
    clientStep :: ClientStep S
    clientStep = \case
      ClientWrite serverID n -> Just (serverID, [n], clientReceive serverID)
      _ -> Nothing
    clientReceive serverID [1] = ClientIdle serverID
    serverStep [v] (Server _) = Just ([1], Server v)

requestSnapShot :: Protlet f S
requestSnapShot = RPC "Snap" clientStep serverStep
  where
    clientStep = \case
      SnapshotClient serverID -> Just (serverID, [], SnapshotClientDone)
    serverStep [] (SnapshotServer snap) = Just (snap, SnapshotServer snap)

-- | Implementation
type RWLock m = Ref m Int

mkRWLock :: (SharedMemory m) => m (RWLock m)
mkRWLock = allocRef 0

readerEnter :: SharedMemory m => RWLock m -> m ()
readerEnter rwlock = do
  n <- readRef rwlock
  if n >= 0
    then do
      succeeded <- casRef rwlock n (n + 1)
      unless succeeded $ readerEnter rwlock
    else readerEnter rwlock

readerExit :: SharedMemory m => RWLock m -> m ()
readerExit rwlock = do
    n <- readRef rwlock
    succeeded <- casRef rwlock n (n - 1)
    unless succeeded $
      readerExit rwlock

writerEnter :: SharedMemory m => RWLock m -> m ()
writerEnter rwlock = do
    succeeded <- casRef rwlock 0 (-1)
    unless succeeded $
      writerEnter rwlock

writerExit :: SharedMemory m => RWLock m -> m ()
writerExit rwlock = writeRef rwlock 0

data DBState m = DBState {
    _locks :: Map Label (RWLock m),
    _cells :: Map Label (Ref m Int)
}

mkDB :: SharedMemory m => Map Label Int -> m (DBState m)
mkDB initMap = do
  locks <- forM (Map.assocs initMap) $ \(k, _) -> do
    l <- mkRWLock
    return (k, l)
  cells <- forM (Map.assocs initMap) $ \(k, v) -> do
    c <- allocRef v
    return (k, c)
  return $ DBState (fromList locks) (fromList cells)

readDB :: SharedMemory m => DBState m -> Label -> m Int
readDB db label = do
  readerEnter lock
  v <- readRef cell
  readerExit lock
  return v
  where
    lock = (Map.! label) . _locks $ db
    cell = (Map.! label ). _cells $ db


writeDB :: SharedMemory m => DBState m -> Label -> Int -> m ()
writeDB db label val = do
  writerEnter lock
  writeRef cell val
  writerExit lock
  where
    lock = (Map.! label) . _locks $ db
    cell = (Map.! label ). _cells $ db

dbServer :: (Par m, SharedMemory m, MessagePassing m) => DBState m -> m a
dbServer db = par (oneCell <$> (Map.keys . _cells) db ) undefined
  where
    oneCell label = do
      Message client tag msg _ _ <- spinReceive [(label,"Read__Request"), (label, "Write__Request")]
      case (tag, msg) of
        ("Read__Request", []) -> do
            val <- readDB db label
            send client label "Read__Response" [val]
        ("Write__Request", [v]) -> do
            writeDB db label v
            send client label "Write__Response" [1]
      oneCell label

seconds :: Int -> Int
seconds = (* 1000000)

snapshotter :: DBState Runner -> Runner a
snapshotter db = do
    liftIO $ threadDelay (seconds 8)
    forM_ (_locks db) readerEnter
    vs <- forM (_cells db) readRef
    liftIO $ print vs
    forM_ (_locks db) readerExit
    snapshotter db

takeSnap :: SharedMemory m => DBState m -> m [Int]
takeSnap db = do
  forM_ (Map.elems . _locks $ db) readerEnter
  vs <- Map.elems <$> forM (_cells db) readRef
  forM_ (Map.elems . _locks $ db) readerExit
  return vs

snapshotter' :: (Par m, SharedMemory m, MessagePassing m) => Label -> DBState m -> m a
snapshotter' label db = do
    firstSnap <- takeSnap db
    snapLoc <- allocRef firstSnap
    par [lookForChanges snapLoc, messenger snapLoc] undefined
  where
    lookForChanges loc = do
      oldSnap <- readRef loc
      go oldSnap $ Map.keys . _locks $ db
      where
        go _ [] = lookForChanges loc
        go (s:ss) (l:ls) = do
          v <- readDB db l
          if v == s
            then go ss ls
            else do
              takeSnap db >>= writeRef loc
              lookForChanges loc

    messenger loc = do
      Message client _ _ _ _ <- spinReceive [(label, "Snap__Request")]
      ans <- readRef loc
      send client label "Snap__Response" ans
      messenger loc

compositeServer :: (MessagePassing m, Par m, SharedMemory m) => [Label] -> Label -> m a
compositeServer labels snapLabel = do
    db <- mkDB $ fromList (zip labels (repeat 0))
    par [snapshotter' snapLabel db, dbServer db] undefined

clientIO :: (MessagePassing m, MonadIO m) => Label -> NodeID -> m a
clientIO lbl server = do
    [v] <- rpcCall lbl "Read" [] server
    liftIO $ putStr $ concat ["Cell[", show lbl, "] has value ", show v, "\nValue to write: " ]
    x <- liftIO $ read <$> getLine
    [1] <- rpcCall lbl "Write" [x] server
    clientIO lbl server

clientPredeterminedVals :: (MessagePassing m, MonadIO m) => Label -> NodeID -> [Int] -> m ()
clientPredeterminedVals _ _ [] = liftIO $ putStrLn "Done!"
clientPredeterminedVals lbl server (x:xs) = do
    [v] <- rpcCall lbl "Read" [] server
    liftIO $ putStrLn $ concat ["Cell[", show lbl, "] has value ", show v, "\nValue to write: ", show x]
    [1] <- rpcCall lbl "Write" [x] server
    clientPredeterminedVals lbl server xs

initConf :: ImplNetwork Runner ()
initConf = initializeImplNetwork [
      (1, clientIO 3 serverID),
      (serverID, compositeServer instances 47)
    ]
  where
    serverID = 0 :: Label
    instances = [0..5] :: [Label]

  