{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
module DPC.Invariant where

import DPC.Specifications
import DPC.Types

import qualified Data.Map as Map
import Data.List (find)
import Data.Maybe (isJust)

type Invariant m s a = forall f. (m, Label, SpecNetwork f s) -> a

(<||>) :: ((m, Label, SpecNetwork f s) -> Bool) -> ((m, Label, SpecNetwork f s) -> Bool) -> ((m, Label, SpecNetwork f s) -> Bool)
l <||> r = (||) <$> l <*> r

foldOr :: [(m, Label, SpecNetwork f s) -> Bool] -> (m, Label, SpecNetwork f s) -> Bool
foldOr = foldr1 (<||>)

(<&&>) :: Invariant m s Bool -> Invariant m s Bool -> Invariant m s Bool
l <&&> r = (&&) <$> l <*> r

noInvariant :: Invariant m s Bool
noInvariant = const True

forNode :: NodeID -> ((NodeState s, [Message]) -> Invariant m s Bool) -> Invariant m s Bool
forNode nodeID p (meta, label, network) = p (instanceState, mailbox) (meta, label, network)
  where
    (nodeStates, mailbox, _) = _localStates network Map.! nodeID
    instanceState = nodeStates Map.! label

forNodes :: [NodeID] -> (NodeID -> Invariant m s Bool) -> Invariant m s Bool
forNodes nodeIDs p = and . sequence (p <$> nodeIDs)

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
    inState (BlockingOn _ t _ waitingFor _, inbox) = pure (t == tag) <&&> 
      responseHandler (filter (\m -> _msgFrom m `elem` waitingFor && _msgTag m == t) inbox)
    inState _ = pure False
    
-- |Bounded invariant checking
-- simulateNetworkCollecting :: Network [] s -> [[Network [] s]]
applyInvariantForInstance :: Invariant m s a -> m -> SpecNetwork f s -> Label -> a
applyInvariantForInstance inv m n l = inv (m, l, n) 

applyInvariant :: Invariant m s a -> m -> SpecNetwork f s -> [a]
applyInvariant inv m n = applyInvariantForInstance inv m n <$> Map.keys (_globalState n)

-- |Will return `Just n` if the `n`th state was found in violation of the invariant.
-- Otherwise `Nothing`
checkTrace :: Invariant m s Bool -> m -> [SpecNetwork f s] -> Maybe Int
checkTrace inv m = go 0
  where
    go _ [] = Nothing
    go n (t:ts) = 
      if and (applyInvariant inv m t)
        then go (n + 1) ts
        else Just n

-- |Returns Just (n, m) if the nth trace is found in violation of the invariant after
-- m steps. 
checkTraces :: Invariant m s Bool -> m -> [[SpecNetwork [] s]] -> Maybe (Int, Int)
checkTraces inv m = go 0
  where
    go _ [] = Nothing
    go n (t:ts) = 
      case checkTrace inv m t of
        Nothing -> go (n + 1) ts
        Just k -> Just (n, k)

exhaustiveInvariantCheck :: Invariant m s Bool -> m -> SpecNetwork [] s -> Maybe (Int, Int)
exhaustiveInvariantCheck inv m net = checkTraces inv m (simulateNetworkTraces net)

boundedInvariantCheck :: Invariant m s Bool -> m -> SpecNetwork [] s -> Int -> Maybe (Int, Int)
boundedInvariantCheck inv m net bound = checkTraces inv m (take bound <$> simulateNetworkTraces net)