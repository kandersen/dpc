{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}
module NetSim.Language where

import           Data.Foldable
import           Data.Map      (Map)
import           NetSim.Core

--
-- Language primitives
--
class Monad m => MessagePassing m where
  send :: NodeID -> Label -> String -> [Int] -> m ()
  receive :: [Label] -> [String] -> m (Maybe Message)
  this :: m NodeID

class Monad m => Par m where
  par :: [m a] -> ([a] -> m c) -> m c

class Monad m => SharedMemory m where
  type Ref m :: (* -> *)
  allocRef :: a -> m (Ref m a)
  readRef :: Ref m a -> m a
  writeRef :: Ref m a -> a -> m ()
  casRef :: Eq a => Ref m a -> a -> a -> m Bool
--
-- Compound operations
--
spinReceive :: MessagePassing m => [Label] -> [String] -> m Message
spinReceive labels tags = do
    mmsg <- receive labels tags
    case mmsg of
      Nothing  -> spinReceive labels tags
      Just msg -> return msg

rpcCall :: MessagePassing m =>
  Label -> String -> [Int] -> NodeID -> m [Int]
rpcCall label protlet body to = do
  send to label (protlet ++ "__Request") body
  Message{..} <- spinReceive [label] [protlet ++ "__Response"]
  return _msgBody

broadcastQuorom :: (MessagePassing m, Ord fraction, Fractional fraction) =>
  fraction -> Label -> String -> [Int] -> [NodeID] -> m [Message]
broadcastQuorom fraction label protlet body receivers = do
  traverse_ (\to -> send to label (protlet ++ "__Broadcast") body) receivers
  spinForResponses []
  where
    spinForResponses resps
      | fromIntegral (length resps) >= fraction * fromIntegral (length receivers) =
         return resps
      | otherwise = do
          resp <- spinReceive [label] [protlet ++ "__Response"]
          spinForResponses (resp:resps)

broadcast :: (MessagePassing m) =>
  Label -> String -> [Int] -> [NodeID] -> m [Message]
broadcast = broadcastQuorom (1 :: Double)

---
-- Network description
--
data Configuration m a = Configuration {
  _confNodes      :: [NodeID],
  _confNodeStates :: Map NodeID (m a),
  _confSoup       :: [Message]
  }
  deriving Show
