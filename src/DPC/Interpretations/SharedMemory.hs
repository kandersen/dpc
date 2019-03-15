{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module DPC.Interpretations.SharedMemory where

import           UnliftIO
import           UnliftIO.Concurrent
import           Control.Monad.Reader
import           Data.Map             (Map)
import qualified Data.Map             as Map


import           DPC.Types
import           DPC.Language
--
-- IO Implementation with real threads!
--
type SharedMemoryContext = (NodeID, Chan Message, Map NodeID (Chan Message))

newtype RunnerT m a = RunnerT { runRunnerT :: ReaderT SharedMemoryContext m a }
  deriving (Functor,
            Applicative,
            Monad,
            MonadTrans,
            MonadIO,
            MonadReader (NodeID, Chan Message, Map NodeID (Chan Message))
           )

instance Monad m => NetworkNode (RunnerT m) where
  this = (\(nodeID, _, _) -> nodeID) <$> ask

instance MonadIO m => MessagePassing (RunnerT m) where
  send to label tag body = do
    (nodeID, _, channels) <- ask
    liftIO $ writeChan (channels Map.! to) (Message nodeID tag body to label)
  receive candidates = do
    (_, inbox, _) <- ask
    msg <- liftIO $ readChan inbox
    if isReceivable msg candidates      
      then return $ Just msg
      else do
        liftIO $ writeChan inbox msg
        return Nothing  

instance MonadIO m => SharedMemory (RunnerT m) where
  type Ref (RunnerT m) = MVar
  allocRef a = liftIO $ newMVar a
  readRef v = liftIO $ takeMVar v
  writeRef v a = liftIO $ putMVar v a
  casRef v a' b = liftIO $ modifyMVar v (\a -> return (if a == a' then b else a, a == a'))

instance MonadUnliftIO m => Par (RunnerT m) where
  par mas k = do
    env <- ask
    vars <- forkThreads env mas
    as <-  awaitThreads vars
    k as
    where
      forkThreads :: SharedMemoryContext -> [RunnerT m a] -> RunnerT m [MVar a]
      forkThreads _ [] = return []
      forkThreads env (ma:mas') = do
        var <- liftIO newEmptyMVar
        lift $ withRunInIO $ \runInner -> 
          void . liftIO . forkIO $ runInner (runReaderT (runRunnerT ma) env) >>= liftIO . putMVar var
        (var:) <$> forkThreads env mas'
      awaitThreads [] = return []
      awaitThreads (v:vs) = do
        a <- liftIO $ takeMVar v
        (a:) <$> awaitThreads vs

runNetwork :: MonadUnliftIO m => ImplNetwork (RunnerT m) a -> m [(NodeID, a)]
runNetwork conf = do
  let network = Map.toList $ _localStates conf
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
    epilogue output a = RunnerT $ liftIO $ writeChan output a