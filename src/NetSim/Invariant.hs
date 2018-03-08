{-# LANGUAGE Rank2Types #-}
module NetSim.Invariant where

import NetSim.Core
import qualified Data.Map as Map

type Invariant m s a = forall f. (m, Network f s) -> a

(<||>) :: Invariant m s Bool -> Invariant m s Bool -> Invariant m s Bool
l <||> r = (||) <$> l <*> r

(<&&>) :: Invariant m s Bool -> Invariant m s Bool -> Invariant m s Bool
l <&&> r = (&&) <$> l <*> r

forNode :: NodeID -> ((NodeID, NodeState s, [Message]) -> Invariant m s Bool) -> Invariant m s Bool
forNode nodeID p (meta, network) = p node (meta, network)
  where
    node = (nodeID, _states network Map.! nodeID, _inboxes network Map.! nodeID)

forNodes :: [NodeID] -> (NodeID -> Invariant m s Bool) -> Invariant m s Bool
forNodes nodes p = and . sequence (p <$> nodes)

noOutstandingMessagesBetween :: NodeID -> NodeID -> Invariant m s Bool
noOutstandingMessagesBetween a b = 
  noMessagesAtFrom a b <&&> noMessagesAtFrom b a

noMessagesAtFrom :: NodeID -> NodeID -> Invariant m s Bool
noMessagesAtFrom at from = forNode at nothingFrom
  where
    nothingFrom (_, _, inbox) = pure . not . any ((==from) . _msgFrom) $ inbox

messageAt :: NodeID -> String -> [Int] -> NodeID -> Invariant m s Bool
messageAt at tag body from = undefined

runningInState :: Eq s => s -> NodeID -> Invariant m s Bool
runningInState s node = forNode node inState 
  where
    inState (_, Running s', _) = pure $ s == s'
    inState _ = pure False