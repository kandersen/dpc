{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}
module NetSim.Core (
    module NetSim.Core
  , module Control.Applicative
  ) where

import NetSim.Util
import Data.List (partition)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative

type NodeID = Int

data Message = Message {
  _msgFrom :: NodeID,
  _msgTag  :: String,
  _msgBody :: [Int],
  _msgTo   :: NodeID
  }
  deriving Show

withTag :: String -> (Message -> Bool)
withTag tag = (tag ==) . _msgTag

data NodeState s = Running s
                 | BlockingOn String [NodeID] ([[Int]] -> s)

instance Show s => Show (NodeState s) where
  show (Running s) =
    "Running " ++ show s
  show (BlockingOn rpc from _) =
    unwords ["BlockingOn", rpc, show from, "<Continuation>"]

--                            Name   Initiateor     Recipient(s)
data Protlet s = RPC          String (ClientStep s) (ServerStep s)
               | ARPC         String (ClientStep s) (Receive s) (Send s)
               | Notification String (Send s)       (Receive s)
               | Broadcast    String (Broadcast s)  (Receive s) (Send s)

type ClientStep s = s -> Maybe (NodeID, [Int], [Int] -> s)
type ServerStep s = [Int] -> s -> Maybe ([Int], s)

type Receive s = Message -> s -> Maybe s
type Send s = NodeID -> s -> Maybe (Message, s)

type Broadcast s = NodeID -> s -> Maybe ([(NodeID, [Int])], [[Int]] -> s)

data Network s = Network {
  _nodes :: [NodeID],
  _states :: Map NodeID (NodeState s),
  _inboxes :: Map NodeID [Message],
  _protlets :: [Protlet s]
  }

findMessage :: (Alternative m) =>
  (Message -> Bool) -> ([Message] -> m a) -> [Message] -> m (a, [Message])
findMessage predicate combine messages = do
  let (candidates, rest) = partition predicate messages
  (,rest) <$> combine candidates

data NodeTransition s = ReceivedMessage NodeID (NodeState s) [Message]
                      | SentMessages    NodeID (NodeState s) [Message] [Message]
                      deriving Show

resolveBlock :: (Monad m, Alternative m) =>
  String -> NodeID -> [Message] -> [NodeID] -> ([[Int]] -> s) -> m (NodeTransition s)
resolveBlock tag nodeID inbox [responder] k = do
  ((response, others), inbox') <- findMessage isResponse oneOf inbox
  pure $ ReceivedMessage nodeID (Running $ k [_msgBody response]) (others ++ inbox')
  where
    isResponse Message{..} = _msgTag == tag && _msgFrom == responder

deliver :: Message -> Network s -> Network s
deliver msg@Message { .. } network@Network{..} =
  network { _inboxes = Map.adjust (msg:) _msgTo _inboxes }

updateNetwork :: NodeTransition s -> (Network s -> Network s)
updateNetwork (ReceivedMessage nodeID s' inbox') network@Network{..} =
  network {
    _states = Map.insert nodeID s' _states,
    _inboxes = Map.insert nodeID inbox' _inboxes
  }
updateNetwork (SentMessages nodeID s' inbox' msgs) network@Network{..} =
  foldr (.) id (deliver <$> msgs) $ network {
  _states = Map.insert nodeID s' _states,
  _inboxes = Map.insert nodeID inbox' _inboxes
 }

tryClientStep :: (Alternative m) =>
  String -> ClientStep s -> NodeID -> s -> [Message] -> m (NodeTransition s)
tryClientStep protlet step nodeID state inbox = case step state of
  Just (server, req, k) ->
    pure $ SentMessages nodeID (BlockingOn (protlet ++ "__Response") [server] (k . head)) inbox [buildRequest server req]
  _  -> empty
  where
    buildRequest receiver body = Message {
      _msgTag = protlet ++ "__Request",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID
      }

tryServerStep :: (Monad m, Alternative m) =>
  String -> ServerStep s -> NodeID -> s -> [Message] -> m (NodeTransition s)
tryServerStep protocol step nodeID state inbox = do
  ((Message{..}, requests'), inbox') <- findMessage isRequest oneOf inbox
  case step _msgBody state of
    Just (ans, state') ->
      pure $ SentMessages nodeID (Running state') (requests' ++ inbox') [buildReply _msgFrom ans]
    _ -> empty
  where
    isRequest Message{..} = _msgTag == protocol ++ "__Request"

    buildReply :: NodeID -> [Int] -> Message
    buildReply receiver body = Message {
      _msgTag = protocol ++ "__Response",
      _msgFrom = nodeID,
      _msgBody = body,
      _msgTo = receiver
      }

stepProtlet :: (Monad m, Alternative m) =>
  NodeID -> s -> [Message] -> Protlet s ->  m (NodeTransition s)
stepProtlet nodeID state inbox protlet = case protlet of
  RPC name cstep sstep ->
    tryClientStep name cstep nodeID state inbox <|>
    tryServerStep name sstep nodeID state inbox
  ARPC name cstep sreceive ssend ->
    tryClientStep name cstep nodeID state inbox <|>
    tryReceive (name ++ "__Request") sreceive nodeID state inbox <|>
    trySend ssend nodeID state inbox
  Notification name send receive ->
    trySend send nodeID state inbox <|>
    tryReceive (name ++ "__Notification") receive nodeID state inbox

tryReceive :: (Monad m, Alternative m) =>
  String -> Receive s -> NodeID -> s -> [Message] -> m (NodeTransition s)
tryReceive tag receive nodeID state inbox = do
  ((m, requests'), rest) <- findMessage (withTag tag) oneOf inbox
  case receive m state of
    Just state' ->
      pure $ ReceivedMessage nodeID (Running state') (requests' ++ rest)
    _ ->
      empty

trySend :: (Alternative m) =>
  Send s -> NodeID -> s -> [Message] -> m (NodeTransition s)
trySend send nodeID state inbox = case send nodeID state of
  Just (msg, state') ->
    pure $ SentMessages nodeID (Running state') inbox [msg]
  _ ->
    empty

possibleTransitions :: (Monad m, Alternative m) => Network s -> m (NodeTransition s)
possibleTransitions Network{..} = do
  nodeID <- fst <$> oneOf _nodes
  let state = _states Map.! nodeID
  let inbox = _inboxes Map.! nodeID
  case state of
    BlockingOn tag nodeIDs k ->
      resolveBlock tag nodeID inbox nodeIDs k
    Running s -> do
      protlet <- fst <$> oneOf _protlets
      stepProtlet nodeID s inbox protlet

applyTransition :: NodeTransition s -> (Network s -> Network s)
applyTransition = updateNetwork

stepNetwork :: (Monad m, Alternative m) => Network s -> m (Network s)
stepNetwork network = updateNetwork <$> possibleTransitions network <*> pure network

initializeNetwork :: [(NodeID,s)] -> [Protlet s] -> Network s
initializeNetwork ns protlets = Network {
  _nodes = fst <$> ns,
  _states = Map.fromList [ (n, Running s) | (n, s) <- ns ],
  _inboxes = Map.fromList [ (n, []) | (n, _) <- ns ],
  _protlets = protlets
  }
