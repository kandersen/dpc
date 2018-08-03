{-# LANGUAGE TypeFamilies #-}
module NetSim.Language where

import NetSim.Core
import Data.Map (Map)
import Data.Foldable

{-
Packet p = (l, t, p, s)
  where
    l = protocol instance label
    t = tag
    p = payload
    s = sender
-}    
type Packet = (Label, String, [Int], NodeID)

--
-- Language primitives
--
class Monad m => MessagePassing m where
  send :: Packet -> m ()
  receive :: Label -> [String] -> m (Maybe Packet)
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
spinReceive :: MessagePassing m => Label -> [String] -> m Packet
spinReceive label tags = do
    mmsg <- receive label tags
    case mmsg of
      Nothing -> spinReceive label tags
      Just msg -> return msg

rpcCall :: MessagePassing m =>
  Label -> String -> [Int] -> NodeID -> m [Int]
rpcCall label protlet body to = do
  send (label, protlet ++ "__Request", body, to)
  (_, _, resp, _) <- spinReceive label [protlet ++ "__Response"]
  return resp

broadcastQuorom :: (MessagePassing m, Ord fraction, Fractional fraction) =>
  fraction -> Label -> String -> [Int] -> [NodeID] -> m [Packet]
broadcastQuorom fraction label protlet body receivers = do
  traverse_ (\to -> send (label, protlet ++ "__Broadcast", body, to)) receivers
  spinForResponses []
  where    
    spinForResponses resps 
      | fromIntegral (length resps) >= fraction * fromIntegral (length receivers) =
         return resps
      | otherwise = do
          resp <- spinReceive label [protlet ++ "__Response"]
          spinForResponses (resp:resps)

broadcast :: (MessagePassing m) =>
  Label -> String -> [Int] -> [NodeID] -> m [Packet]
broadcast = broadcastQuorom (1 :: Double)

--
-- Network description
--
data Configuration m a = Configuration {
  _confNodes :: [NodeID],
  _confNodeStates :: Map NodeID (m a),
  _confSoup :: [Message]
  }
  deriving Show
