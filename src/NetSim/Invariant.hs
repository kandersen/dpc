{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module NetSim.Invariant where

import NetSim.Core
import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (isJust)

type Invariant m s a = forall f. (m, Network f s) -> a

(<||>) :: ((m, Network f s) -> Bool) -> ((m, Network f s) -> Bool) -> ((m, Network f s) -> Bool)
l <||> r = (||) <$> l <*> r

(<&&>) :: Invariant m s Bool -> Invariant m s Bool -> Invariant m s Bool
l <&&> r = (&&) <$> l <*> r

noInvariant :: Invariant m s Bool
noInvariant = const True

forNode :: Label -> NodeID -> ((NodeState s, [Message]) -> Invariant m s Bool) -> Invariant m s Bool
forNode label nodeID p (meta, network) = p node (meta, network)
  where
    node = ((_states network Map.! nodeID) Map.! label, _inboxes network Map.! nodeID)

forNodes :: [NodeID] -> (NodeID -> Invariant m s Bool) -> Invariant m s Bool
forNodes nodes p = and . sequence (p <$> nodes)

noOutstandingMessagesBetween :: Label -> NodeID -> NodeID -> Invariant m s Bool
noOutstandingMessagesBetween label a b = 
  noMessageFromTo label a b <&&> noMessageFromTo label b a

noMessageFromTo :: Label -> NodeID -> NodeID -> Invariant m s Bool
noMessageFromTo label from to = forNode label to nothingFrom
  where
    nothingFrom (_, inbox) = pure . not . any ((==from) . _msgFrom) $ inbox

messageAt :: Label -> NodeID -> String -> [Int] -> NodeID -> Invariant m s Bool
messageAt label at tag body from = forNode label at check
  where
    isRight Message{..} = _msgTag == tag && _msgFrom == from && _msgBody == body
    check (_, inbox) = pure . isJust $ find isRight inbox

runningInState :: Eq s => Label -> s -> NodeID -> Invariant m s Bool
runningInState label s node = forNode label node inState 
  where
    inState (Running s', _) = pure $ s == s'
    inState _ = pure False

blockingOn :: Label -> String -> NodeID -> ([Message] -> Invariant m s Bool) -> Invariant m s Bool
blockingOn label tag node responseHandler = forNode label node inState
  where
    inState (BlockingOn t waitingFor _, inbox) = (pure $ t == tag) <&&> 
      (responseHandler $ filter (\m -> _msgFrom m `elem` waitingFor && _msgTag m == t) inbox)
    inState _ = pure False