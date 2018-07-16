{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module NetSim.Interpretations.SharedMemory where
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent
import NetSim.Core
import NetSim.Language
--
-- IO Implementation with real threads!
--
type Runner = ReaderT (NodeID, Chan Packet, Map NodeID (Chan Packet)) IO

instance MonadDiSeL Runner where
  type Ref Runner = MVar
  allocRef a = liftIO $ newMVar a
  readRef v = liftIO $ takeMVar v
  writeRef v a = liftIO $ putMVar v a
  casRef v a' b = liftIO $ modifyMVar v (\a -> return (if a == a' then b else a, a == a'))
  send (label, tag, body, to) = do
    (nodeID, _, channels) <- ask
    lift $ writeChan (channels Map.! to) (label, tag, body, nodeID)
  receive label tags = do
    (_, inbox, _) <- ask
    pkt@(label', tag, _, _) <- lift $ readChan inbox
    if tag `elem` tags && label' == label
      then return $ Just pkt
      else do
        lift $ writeChan inbox pkt
        return Nothing
  this = (\(nodeID, _, _) -> nodeID) <$> ask
  par mas k = do
    env <- ask
    vars <- forkThreads env mas
    as <-  awaitThreads vars
    k as
    where
      forkThreads _ [] = return []
      forkThreads env (ma:mas') = do
        var <- liftIO newEmptyMVar
        liftIO . void . forkIO $ (runReaderT ma env >>= putMVar var)
        (var:) <$> forkThreads env mas'
      awaitThreads [] = return []
      awaitThreads (v:vs) = do
        a <- liftIO $ takeMVar v
        (a:) <$> awaitThreads vs

runNetworkIO :: Configuration Runner a -> IO [(NodeID, a)]
runNetworkIO conf = do
  let network = Map.toList $ _confNodeStates conf
  envs <- sequence $ do
    (nodeID, code) <- network
    return $ do
      inbox <- newChan
      return (nodeID, inbox, code)
  let mapping = Map.fromList [(nodeID, inbox) | (nodeID, inbox, _) <- envs]
  output <- newChan
  sequence_ . flip fmap network  $ \(nodeID, code) ->
    forkIO . void $ runReaderT (code >>= epilogue output . (nodeID,)) (nodeID, mapping Map.! nodeID, mapping)
  getChanContents output

  where
    epilogue :: Chan a -> a -> Runner ()
    epilogue output a = liftIO $ writeChan output a
