{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module NetSim.Specifications (
  module NetSim.Specifications,
  module Control.Applicative
  ) where

import           Control.Applicative
import           Control.Monad       (guard)
import           Data.Map            (Map)
import qualified Data.Map            as Map
import           Data.Foldable
import           Lens.Micro
import           NetSim.Util

import           Text.Show.Functions ()

import NetSim.Types

data NodeState s = Running s
                 | BlockingOn s String Rational [NodeID] ([(NodeID, [Int])] -> s)

instance Show s => Show (NodeState s) where
  show (Running s) =
    "Running " ++ show s
  show (BlockingOn s rpc f from _) =
    unwords ["Blocking in state", show s, "expecting", show f, "of the responses from", rpc, show from, "<Continuation>"]

data Protlet f s = 
--  Protlet      Name            Initiator      Recipient(s)  
    RPC          String          (ClientStep s) (ServerStep s)
  | ARPC         String          (ClientStep s) (Receive s) (Send f s)
  | Notification String          (Send f s)     (Receive s)
  | Broadcast    String          (Broadcast s)  (Receive s) (Send f s) 
  | Quorum       String Rational (Broadcast s)  (Receive s) (Send f s)
  | OneOf        [Protlet f s]
  deriving (Show)

type ClientStep s = s -> Maybe (NodeID, [Int], [Int] -> s)
type ServerStep s = [Int] -> s -> Maybe ([Int], s)

type Receive   s = Message -> s -> Maybe s
type Send    f s = NodeID  -> s -> f (Message, s)

type Broadcast s = s -> Maybe ([(NodeID, [Int])], [(NodeID, [Int])] -> s)

type SpecNetwork f s = NetworkState (Map Label [Protlet f s]) (Map Label (NodeState s), [Message], Bool)

protletInstances :: SpecNetwork f s -> [Label]
protletInstances = Map.keys . _globalState

-- |Initialize a network with an association of nodes to protlet instance states, and 
-- an association of protlet instsnaces to protlets. 
initializeNetwork :: [(NodeID,[(Label, s)])] -> [(Label, [Protlet f s])] -> SpecNetwork f s
initializeNetwork ns protlets = NetworkState {
  _localStates = Map.fromList [ (n, (nodestates states, [], True)) | (n, states) <- ns ],
  _globalState = Map.fromList protlets
  }
  where
    nodestates :: [(Label, s)] -> Map Label (NodeState s)
    nodestates xs = Map.fromList [ (label, Running s) | (label, s) <- xs ]

--
--  Transitions
--
data Transition s = ReceivedMessages Label NodeID [Message] (NodeState s) [Message]
                  | SentMessages     Label NodeID [Message] (NodeState s) [Message]
                  | Crash                  NodeID
                  deriving Show

applyTransition :: Transition s -> (SpecNetwork f s -> SpecNetwork f s)
applyTransition (ReceivedMessages label nodeID _ s' inbox') =
  update label nodeID s' inbox'
applyTransition (SentMessages label nodeID msgs s' inbox') =
  foldr (.) id (deliver <$> msgs) . update label nodeID s' inbox'
applyTransition (Crash nodeID) = \network@NetworkState{..} ->
  network {
    _localStates = Map.adjust (_3 .~ False) nodeID _localStates
  }

update :: Label -> NodeID -> NodeState s -> [Message] -> (SpecNetwork f s -> SpecNetwork f s)
update label nodeID s' inbox' network@NetworkState{..} = network {
    _localStates = Map.adjust (\(ss, _, status) -> (Map.insert label s' ss, inbox', status)) nodeID _localStates
  }

deliver :: Message -> (SpecNetwork f s -> SpecNetwork f s)
deliver msg@Message { .. } network@NetworkState{..} =
  network { _localStates = Map.adjust (\(ss,inbox,status) -> (ss, msg:inbox, status)) _msgTo _localStates }

resolveBlock :: (Monad m, Alternative m) =>
  Label -> String -> Rational -> NodeID -> [Message] -> [NodeID] -> ([(NodeID, [Int])] -> s) -> m (Transition s)
resolveBlock label tag responsesNeeded nodeID inbox responders k = do
  -- Enumerates all the ways of receiving a message from each of a given set of nodes from a given set of messages
  -- Yielding the chosen messages and the residual set of messages.
  (responses, inbox') <- findResponses inbox responders
  -- Then weed the ones without sufficient number of responses
  guard $ fromIntegral (length responses) >= responsesNeeded
  pure $ ReceivedMessages label nodeID responses (Running $ k (arrange <$> responses)) inbox'
  where
    findResponses :: (Monad m, Alternative m) => [Message] -> [NodeID] -> m ([Message], [Message])
    findResponses rest     [] = pure ([], rest)
    findResponses rest (r:rs) = (do
      (response, inbox') <- oneOfP (isResponseFrom r) rest
      (_1 %~ (response:)) <$> findResponses inbox' rs)
      <|> 
      findResponses rest rs

    isResponseFrom from Message{..} = _msgFrom == from && _msgTag == tag && _msgLabel == label

    arrange :: Message -> (NodeID, [Int])
    arrange m = (_msgFrom m, _msgBody m)

tryClientStep :: (Alternative m) =>
  Label -> String -> ClientStep s -> NodeID -> s -> [Message] -> m (Transition s)
tryClientStep label protlet step nodeID state inbox = case step state of
  Just (server, req, k) ->
    pure $ SentMessages label nodeID [buildRequest server req] (BlockingOn state (protlet ++ "__Response") 1 [server] (k . snd . head)) inbox
  _  -> empty
  where
    buildRequest receiver body = Message {
      _msgTag = protlet ++ "__Request",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID,
      _msgLabel = label
      }

tryServerStep :: (Monad m, Alternative m) =>
  Label -> String -> ServerStep s -> NodeID -> s -> [Message] -> m (Transition s)
tryServerStep label protlet step nodeID state inbox = do
  (Message{..}, inbox') <- oneOfP isRequest inbox
  case step _msgBody state of
    Just (ans, state') ->
      pure $ SentMessages label nodeID [buildReply _msgFrom ans] (Running state') inbox'
    _ -> empty
  where
    isRequest Message{..} = _msgTag == protlet ++ "__Request" && _msgLabel == label

    buildReply :: NodeID -> [Int] -> Message
    buildReply receiver body = Message {
      _msgTag = protlet ++ "__Response",
      _msgFrom = nodeID,
      _msgBody = body,
      _msgTo = receiver,
      _msgLabel = label
      }

tryReceive :: (Monad m, Alternative m) =>
  Label -> String -> Receive s -> NodeID -> s -> [Message] -> m (Transition s)
tryReceive label tag receive nodeID state inbox = do
  (m, inbox') <- oneOfP isGood inbox
  case receive m state of
    Just state' ->
      pure $ ReceivedMessages label nodeID [m] (Running state') inbox'
    _ ->
      empty
  where
    isGood :: Message -> Bool
    isGood Message{..} = tag == _msgTag && _msgLabel == label

trySend :: (Monad m, Alternative m) =>
  Label -> Send m s -> NodeID -> s -> [Message] -> m (Transition s)
trySend label send nodeID state inbox = do
  (msg , state') <- send nodeID state
  pure $ SentMessages label nodeID [msg { _msgLabel = label }] (Running state') inbox

tryBroadcast :: (Alternative m) =>
  Label -> String -> Broadcast s -> NodeID -> s -> [Message] -> m (Transition s)
tryBroadcast label name broadcast nodeID state inbox = case broadcast state of
  Just (casts, k) ->
    pure $ SentMessages label nodeID (buildRequest <$> casts) (BlockingOn state (name ++ "__Response") (fromIntegral $ length casts) (fst <$> casts) k) inbox
  _  -> empty
  where
    buildRequest (receiver, body) = Message {
      _msgTag = name ++ "__Broadcast",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID,
      _msgLabel = label
      }

tryQuorum :: (Alternative m) =>
      Label -> String -> Rational -> Broadcast s -> NodeID -> s -> [Message] -> m (Transition s)
tryQuorum label name quorumSize broadcast nodeID state inbox = case broadcast state of
  Just (casts, k) -> 
   pure $ SentMessages label nodeID (buildRequest <$> casts) (BlockingOn state (name ++ "__Response") quorumSize (fst <$> casts) k) inbox
  Nothing -> empty
  where
    buildRequest (receiver, body) = Message {
      _msgTag = name ++ "__Quorum",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID,
      _msgLabel = label
      }
    
stepProtlet :: (Monad m, Alternative m) =>
  NodeID -> s -> [Message] -> Label -> Protlet m s ->  m (Transition s)
stepProtlet nodeID state inbox label protlet = case protlet of
  RPC name cstep sstep ->
    tryClientStep label name cstep nodeID state inbox <|>
    tryServerStep label name sstep nodeID state inbox
  ARPC name cstep sreceive ssend ->
    tryClientStep label name cstep nodeID state inbox <|>
    tryReceive label (name ++ "__Request") sreceive nodeID state inbox <|>
    trySend label ssend nodeID state inbox
  Notification name send receive ->
    trySend label send nodeID state inbox <|>
    tryReceive label (name ++ "__Notification") receive nodeID state inbox
  Broadcast name broadcast receive respond ->
    tryBroadcast label name broadcast nodeID state inbox <|>
    tryReceive label (name ++ "__Broadcast") receive nodeID state inbox <|>
    trySend label respond nodeID state inbox
  Quorum name quorumSize broadcast receive respond ->
    tryQuorum label name quorumSize broadcast nodeID state inbox <|>
    tryReceive label (name ++ "__Quorum") receive nodeID state inbox <|>
    trySend label respond nodeID state inbox
  OneOf protlets -> asum (stepProtlet nodeID state inbox label <$> protlets)

-- Non-deterministically chose a transition.
possibleTransitions :: (Monad f, Alternative f) => SpecNetwork f s -> f (Transition s)
possibleTransitions ns@NetworkState{..} = do
  nodeID <- fst <$> oneOf (nodes ns)
  let (states, inbox, isOnline) = _localStates Map.! nodeID
  if not isOnline
    then empty
    else do
      (label, state) <- fst <$> oneOf (Map.toList states)
      case state of        
        BlockingOn _ tag f nodeIDs k ->
          resolveBlock label tag f nodeID inbox nodeIDs k
        Running s -> do
          (protlet, _) <- oneOf $ _globalState Map.! label
          stepProtlet nodeID s inbox label protlet

possibleCrashes :: (Monad f, Alternative f) => SpecNetwork f s -> f (Transition s)
possibleCrashes ns@NetworkState{..} = do
  nodeID <- fst <$> oneOf (nodes ns)
  let (_, _, isOnline) = _localStates Map.! nodeID
  if isOnline
    then return $ Crash nodeID
    else empty

-- Non-deterministically advance a network, "chosing" amongst possible transitions.
stepNetwork :: (Monad f, Alternative f) => SpecNetwork f s -> f (SpecNetwork f s)
stepNetwork network = applyTransition <$> possibleTransitions network <*> pure network

-- Simulate network execution using Random IO to pick transitions. Yields a trace of network states
simulateNetworkIO :: SpecNetwork [] s -> IO [SpecNetwork [] s]
simulateNetworkIO n = do
  let nextNs = stepNetwork n
  case nextNs of 
    [] -> return [n]
    _ -> (n:) <$> (simulateNetworkIO =<< pickRandom nextNs)

-- Simulate network execution using the list-monad to explore possible states. 
-- Yields a list with the `nth` index containing states after `n` steps of execution.
simulateNetworkTraces :: SpecNetwork [] s -> [[SpecNetwork [] s]]
simulateNetworkTraces = go
  where
    go n = 
      concatMap (n:) <$> (simulateNetworkTraces <$> stepNetwork n)