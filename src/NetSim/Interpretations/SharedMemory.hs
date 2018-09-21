{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
module NetSim.Interpretations.SharedMemory where

import           Control.Concurrent
import           Control.Monad.Reader
import           Data.Map             (Map)
import qualified Data.Map             as Map

import           NetSim.Types
import           NetSim.Language
--
-- IO Implementation with real threads!
--
newtype RunnerT m a = RunnerT { runRunnerT :: ReaderT (NodeID, Chan Message, Map NodeID (Chan Message)) m a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadTrans,
            MonadIO,
            MonadReader (NodeID, Chan Message, Map NodeID (Chan Message))
           )

type Runner = RunnerT IO

instance NetworkNode Runner where
  this = (\(nodeID, _, _) -> nodeID) <$> ask

instance MessagePassing Runner where
  send to label tag body = do
    (nodeID, _, channels) <- ask
    lift $ writeChan (channels Map.! to) (Message nodeID tag body to label)
  receive candidates = do
    (_, inbox, _) <- ask
    msg <- lift $ readChan inbox
    if isReceivable msg candidates      
      then return $ Just msg
      else do
        lift $ writeChan inbox msg
        return Nothing

instance SharedMemory Runner where
  type Ref Runner = MVar
  allocRef a = liftIO $ newMVar a
  readRef v = liftIO $ takeMVar v
  writeRef v a = liftIO $ putMVar v a
  casRef v a' b = liftIO $ modifyMVar v (\a -> return (if a == a' then b else a, a == a'))

instance Par Runner where
  par mas k = do
    env <- ask
    vars <- forkThreads env mas
    as <-  awaitThreads vars
    k as
    where
      forkThreads _ [] = return []
      forkThreads env (ma:mas') = do
        var <- liftIO newEmptyMVar
        liftIO . void . forkIO $ (runReaderT (runRunnerT ma) env >>= putMVar var)
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
    forkIO . void $ runReaderT (runRunnerT (code >>= epilogue output . (nodeID,))) (nodeID, mapping Map.! nodeID, mapping)
  getChanContents output

  where
    epilogue :: Chan a -> a -> Runner ()
    epilogue output a = RunnerT $ liftIO $ writeChan output a
