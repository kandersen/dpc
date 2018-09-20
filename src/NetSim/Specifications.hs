{-# LANGUAGE ApplicativeDo    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
module NetSim.Specifications (
  module NetSim.Specifications,
  module Control.Applicative
  ) where

import           Control.Applicative
import           Data.Map            (Map, (!))
import qualified Data.Map            as Map
import           Data.Foldable
import           Lens.Micro
import           NetSim.Util

import           Text.Show.Functions ()

import NetSim.Types


data NodeState s = Running s
                 | BlockingOn s String [NodeID] ([[Int]] -> s)

instance Show s => Show (NodeState s) where
  show (Running s) =
    "Running " ++ show s
  show (BlockingOn s rpc from _) =
    unwords ["Blocking in state ", show s, " expecting ", rpc, show from, "<Continuation>"]

--                 Protlet      Name   Initiator      Recipient(s)
data Protlet f s = RPC          String (ClientStep s) (ServerStep s)
                 | ARPC         String (ClientStep s) (Receive s) (Send f s)
                 | Notification String (Send f s)     (Receive s)
                 | Broadcast    String (Broadcast s)  (Receive s) (Send f s) 
                 | OneOf        [Protlet f s]
                 deriving (Show)

--                 | BroadcastNotification String (
--                 | Iterated Int (Protlet f s)
--                 | Quorum String Fraction (Broadcast s) (Receive s) (Send f s)

type ClientStep s = s -> Maybe (NodeID, [Int], [Int] -> s)
type ServerStep s = [Int] -> s -> Maybe ([Int], s)

type Receive   s = Message -> s -> Maybe s
type Send    f s = NodeID  -> s -> f (Message, s)

type Broadcast s = s -> Maybe ([(NodeID, [Int])], [[Int]] -> s)

data Network f s = Network {
  _nodes    :: [NodeID],
  _states   :: Map NodeID (Map Label (NodeState s)),
  _inboxes  :: Map NodeID [Message],
  _protlets :: [(Label, Protlet f s)]
  }
  deriving (Show)

data NetworkState global local = NetworkState {
  _nsNodes :: [NodeID],
  _nsLocalStates :: Map NodeID local,
  _nsGlobalState :: global
}

-- type Network = NetworkState [(Label, Protlet f s)] (Map Label (NodeState s), [Message])
-- type Configuration m a = NetworkState [Message] (m a)

-- |Initialize a network with an association of nodes to protlet instance states, and 
-- an association of protlet instsnaces to protlets. 
initializeNetwork :: [(NodeID,[(Label, s)])] -> [(Label, Protlet f s)] -> Network f s
initializeNetwork ns protlets = Network {
  _nodes = fst <$> ns,
  _states = Map.fromList [ (n, nodestates states) | (n, states) <- ns ],
  _inboxes = Map.fromList [ (n, []) | n <- fst <$> ns ],
  _protlets = protlets
  }
  where
    nodestates :: [(Label, s)] -> Map Label (NodeState s)
    nodestates xs = Map.fromList [ (label, Running s) | (label, s) <- xs ]

--
--  Transitions
--
data Transition s = ReceivedMessages Label NodeID [Message] (NodeState s) [Message]
                  | SentMessages     Label NodeID [Message] (NodeState s) [Message]
                  deriving Show

applyTransition :: Transition s -> (Network f s -> Network f s)
applyTransition (ReceivedMessages label nodeID _ s' inbox') =
  update label nodeID s' inbox'
applyTransition (SentMessages label nodeID msgs s' inbox') =
  foldr (.) id (deliver <$> msgs) . update label nodeID s' inbox'

update :: Label -> NodeID -> NodeState s -> [Message] -> (Network f s -> Network f s)
update label nodeID s' inbox' network@Network{..} = network {
    _states = Map.adjust (Map.insert label s') nodeID _states,
    _inboxes = Map.insert nodeID inbox' _inboxes
  }

deliver :: Message -> (Network f s -> Network f s)
deliver msg@Message { .. } network@Network{..} =
  network { _inboxes = Map.adjust (msg:) _msgTo _inboxes }

resolveBlock :: (Monad m, Alternative m) =>
  Label -> String -> NodeID -> [Message] -> [NodeID] -> ([[Int]] -> s) -> m (Transition s)
resolveBlock label tag nodeID inbox responders k = do
  (responses, inbox') <- findAllResponses inbox responders
  pure $ ReceivedMessages label nodeID responses (Running $ k (_msgBody <$> responses)) inbox'
  where
    findAllResponses rest     [] = pure ([], rest)
    findAllResponses rest (r:rs) = do
      (response, inbox') <- oneOfP (isResponseFrom r) rest
      (_1 %~ (response:)) <$> findAllResponses inbox' rs

    isResponseFrom from Message{..} = _msgFrom == from && _msgTag == tag && _msgLabel == label

tryClientStep :: (Alternative m) =>
  Label -> String -> ClientStep s -> NodeID -> s -> [Message] -> m (Transition s)
tryClientStep label protlet step nodeID state inbox = case step state of
  Just (server, req, k) ->
    pure $ SentMessages label nodeID [buildRequest server req] (BlockingOn state (protlet ++ "__Response") [server] (k . head)) inbox
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
    pure $ SentMessages label nodeID (buildRequest <$> casts) (BlockingOn state (name ++ "__Response") (fst <$> casts) k) inbox
  _  -> empty
  where
    buildRequest (receiver, body) = Message {
      _msgTag = name ++ "__Broadcast",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID,
      _msgLabel = label
      }

stepProtlet :: (Monad m, Alternative m) =>
  NodeID -> s -> [Message] -> (Label, Protlet m s) ->  m (Transition s)
stepProtlet nodeID state inbox (label, protlet) = case protlet of
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
  OneOf protlets -> asum ((\p -> stepProtlet nodeID state inbox (label, p)) <$> protlets)

-- Non-deterministically chose a transition.
possibleTransitions :: (Monad f, Alternative f) => Network f s -> f (Transition s)
possibleTransitions Network{..} = do
  nodeID <- fst <$> oneOf _nodes
  let inbox = _inboxes ! nodeID
  (label, state) <- fst <$> oneOf (Map.toList $ _states ! nodeID)
  case state of
    BlockingOn _ tag nodeIDs k ->
      resolveBlock label tag nodeID inbox nodeIDs k
    Running s -> do
      (plabel, protlet) <- fst <$> oneOf _protlets
      if plabel == label
      then stepProtlet nodeID s inbox (plabel, protlet)
      else empty

-- Non-deterministically advance a network, "chosing" amongst possible transitions.
stepNetwork :: (Monad f, Alternative f) => Network f s -> f (Network f s)
stepNetwork network = applyTransition <$> possibleTransitions network <*> pure network


-- Simulate network execution using Random IO to pick transitions. Yields a trace of network states
simulateNetworkIO :: Network [] s -> IO [Network [] s]
simulateNetworkIO n = (n:) <$> (simulateNetworkIO =<< pickRandom (stepNetwork n))

-- Simulate network execution using the list-monad to explore possible states. 
-- Yields a list with the `nth` index containing states after `n` steps of execution.
simulateNetworkTraces :: Network [] s -> [[Network [] s]]
simulateNetworkTraces = go
  where
    go n = 
      concatMap (n:) <$> (simulateNetworkTraces <$> stepNetwork n)