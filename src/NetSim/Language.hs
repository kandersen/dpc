{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
module NetSim.Language where

import           Data.Foldable
import           Data.Maybe    (isJust)
import           Data.Map as Map
import           Control.Monad.State

import NetSim.Types
import NetSim.Specifications

--
-- Language primitives
--
class Monad m => NetworkNode m where
  this :: m NodeID

class Monad m => MessagePassing m where
  send :: NodeID -> Label -> String -> [Int] -> m ()
  receive :: [(Label, String)] -> m (Maybe Message)

class ProtletAnnotations s m where
  enactingServer :: Protlet [] s -> m a -> m a
  enactingClient :: Protlet [] s -> m a -> m a

class Monad m => Par m where
  par :: [m a] -> ([a] -> m c) -> m c

class Monad m => SharedMemory m where
  type Ref m :: (* -> *)
  allocRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  casRef :: Eq a => Ref m a -> a -> a -> m Bool

--
-- Implementation Utilities
--
isReceivable :: Message -> [(Label, String)] -> Bool
isReceivable Message{..} = isJust . find (\(lbl,t) -> _msgLabel == lbl && _msgTag == t)

-- Instances
instance MessagePassing m => MessagePassing (StateT s m) where
  send n l t m = lift $ send n l t m
  receive = lift . receive

instance ProtletAnnotations a m => ProtletAnnotations a (StateT s m) where
  enactingClient p m = StateT (enactingClient p . runStateT m)
  enactingServer p m = StateT (enactingServer p . runStateT m)

--
-- Compound operations
--
spinReceive :: MessagePassing m => [(Label, String)] -> m Message
spinReceive candidates = do
    mmsg <- receive candidates
    case mmsg of
      Nothing  -> spinReceive candidates
      Just msg -> return msg

rpcCall :: MessagePassing m =>
  Label -> String -> [Int] -> NodeID -> m [Int]
rpcCall label protlet body to = do
  send to label (protlet ++ "__Request") body
  Message{..} <- spinReceive [(label, protlet ++ "__Response")]
  return _msgBody

broadcastQuorom :: (MessagePassing m) =>
  Rational -> Label -> String -> [Int] -> [NodeID] -> m [Message]
broadcastQuorom responsesNeeded label protlet body receivers = do
  traverse_ (\to -> send to label (protlet ++ "__Broadcast") body) receivers
  spinForResponses []
  where
    spinForResponses resps
      | fromIntegral (length resps) >= responsesNeeded =
          return resps
      | otherwise = do
          resp <- spinReceive [(label, protlet ++ "__Response")]
          spinForResponses (resp:resps)

broadcast :: (MessagePassing m) =>
  Label -> String -> [Int] -> [NodeID] -> m [Message]
broadcast label protlet body receivers = 
  broadcastQuorom (fromIntegral $ length receivers) label protlet body receivers

---
-- Network description
--

type ImplNetwork m a = NetworkState [Message] (m a)

initializeImplNetwork :: [(NodeID, m a)] -> ImplNetwork m a
initializeImplNetwork = NetworkState [] . Map.fromList
