{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module NetSim.Invariant where

import NetSim.Core
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (isJust)

type Invariant m s a = forall f. (m, Label, Network f s) -> a

(<||>) :: ((m, Label, Network f s) -> Bool) -> ((m, Label, Network f s) -> Bool) -> ((m, Label, Network f s) -> Bool)
l <||> r = (||) <$> l <*> r

foldOr :: [(m, Label, Network f s) -> Bool] -> (m, Label, Network f s) -> Bool
foldOr = foldr1 (<||>)

(<&&>) :: Invariant m s Bool -> Invariant m s Bool -> Invariant m s Bool
l <&&> r = (&&) <$> l <*> r

noInvariant :: Invariant m s Bool
noInvariant = const True

forNode :: NodeID -> ((NodeState s, [Message]) -> Invariant m s Bool) -> Invariant m s Bool
forNode nodeID p (meta, label, network) = p node (meta, label, network)
  where
    node = ((_states network Map.! nodeID) Map.! label, _inboxes network Map.! nodeID)

forNodes :: [NodeID] -> (NodeID -> Invariant m s Bool) -> Invariant m s Bool
forNodes nodes p = and . sequence (p <$> nodes)

noOutstandingMessagesBetween :: NodeID -> NodeID -> Invariant m s Bool
noOutstandingMessagesBetween a b = 
  noMessageFromTo a b <&&> noMessageFromTo b a

noMessageFromTo :: NodeID -> NodeID -> Invariant m s Bool
noMessageFromTo from to = forNode to nothingFrom
  where
    nothingFrom (_, inbox) = pure . not . any ((==from) . _msgFrom) $ inbox

messageAt :: NodeID -> String -> [Int] -> NodeID -> Invariant m s Bool
messageAt at tag body from = forNode at check
  where
    isRight Message{..} = _msgTag == tag && _msgFrom == from && _msgBody == body
    check (_, inbox) = pure . isJust $ find isRight inbox

runningInState :: Eq s => s -> NodeID -> Invariant m s Bool
runningInState s node = forNode node inState 
  where
    inState (Running s', _) = pure $ s == s'
    inState _ = pure False

blockingOn :: String -> NodeID -> ([Message] -> Invariant m s Bool) -> Invariant m s Bool
blockingOn tag node responseHandler = forNode node inState
  where
    inState (BlockingOn t waitingFor _, inbox) = pure (t == tag) <&&> 
      responseHandler (filter (\m -> _msgFrom m `elem` waitingFor && _msgTag m == t) inbox)
    inState _ = pure False
    