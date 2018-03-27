{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TupleSections #-}
module NetSim.Core (
  module NetSim.Core,
  module Control.Applicative
  ) where

import NetSim.Util
import Data.List (partition)
import qualified Data.Map as Map
import Data.Map (Map)
import Control.Applicative
import Lens.Micro

type NodeID = Int

type Label = Int

data Message = Message {
  _msgFrom :: NodeID,
  _msgTag  :: String,
  _msgBody :: [Int],
  _msgTo   :: NodeID,
  _msgLabel :: Label
  }
  deriving Show

withTag :: Label -> String -> (Message -> Bool)
withTag label tag Message{..} = tag == _msgTag && _msgLabel == label

data NodeState s = Running s
                 | BlockingOn String [NodeID] ([[Int]] -> s)

instance Show s => Show (NodeState s) where
  show (Running s) =
    "Running " ++ show s
  show (BlockingOn rpc from _) =
    unwords ["BlockingOn", rpc, show from, "<Continuation>"]

--                              Name   Initiator     Recipient(s)
data Protlet f s = RPC          String (ClientStep s) (ServerStep s)
                 | ARPC         String (ClientStep s) (Receive s) (Send f s)
                 | Notification String (Send f s)     (Receive s)
                 | Broadcast    String (Broadcast s)  (Receive s) (Send f s)

type ClientStep s = s -> Maybe (NodeID, [Int], [Int] -> s)
type ServerStep s = [Int] -> s -> Maybe ([Int], s)

type Receive   s = Message -> s -> Maybe s
type Send    f s = NodeID  -> s -> f (Message, s)

type Broadcast s = s -> Maybe ([(NodeID, [Int])], [[Int]] -> s)

data Network f s = Network {
  _nodes    :: [NodeID],
  _states   :: Map NodeID (NodeState s),
  _inboxes  :: Map NodeID [Message],
  _protlets :: [(Label, Protlet f s)]
  }

findMessage :: (Alternative m) =>
  (Message -> Bool) -> ([Message] -> m a) -> [Message] -> m (a, [Message])
findMessage predicate combine messages = do
  let (candidates, rest) = partition predicate messages
  (,rest) <$> combine candidates

data Transition s = ReceivedMessage NodeID (NodeState s) [Message]
                  | SentMessages    NodeID (NodeState s) [Message] [Message]
                  deriving Show

resolveBlock :: (Monad m, Alternative m) =>
  String -> NodeID -> [Message] -> [NodeID] -> ([[Int]] -> s) -> m (Transition s)
resolveBlock tag nodeID inbox [responder] k = do
  ((response, others), inbox') <- findMessage isResponse oneOf inbox
  pure $ ReceivedMessage nodeID (Running $ k [_msgBody response]) (others ++ inbox')
  where
    isResponse Message{..} = _msgTag == tag && _msgFrom == responder
resolveBlock tag nodeID inbox responders k = do
  (responses, inbox') <- findAllResponses inbox responders
  pure $ ReceivedMessage nodeID (Running $ k (_msgBody <$> responses)) inbox'
  where
    findAllResponses :: (Monad m, Alternative m) =>
      [Message] -> [NodeID] -> m ([Message], [Message])
    findAllResponses rest [] = pure ([], rest)
    findAllResponses rest (r:rs) = do
      ((response, others), inbox') <- findMessage (isResponse r) oneOf rest
      (_1 %~ (response:)) <$> findAllResponses (inbox' ++ others) rs

    isResponse from Message{..} = _msgFrom == from && _msgTag == tag

deliver :: Message -> Network f s -> Network f s
deliver msg@Message { .. } network@Network{..} =
  network { _inboxes = Map.adjust (msg:) _msgTo _inboxes }

applyTransition :: Transition s -> (Network f s -> Network f s)
applyTransition (ReceivedMessage nodeID s' inbox') network@Network{..} =
  network {
    _states = Map.insert nodeID s' _states,
    _inboxes = Map.insert nodeID inbox' _inboxes
  }
applyTransition (SentMessages nodeID s' inbox' msgs) network@Network{..} =
  foldr (.) id (deliver <$> msgs) $ network {
  _states = Map.insert nodeID s' _states,
  _inboxes = Map.insert nodeID inbox' _inboxes
 }

tryClientStep :: (Alternative m) =>
  Label -> String -> ClientStep s -> NodeID -> s -> [Message] -> m (Transition s)
tryClientStep label protlet step nodeID state inbox = case step state of
  Just (server, req, k) ->
    pure $ SentMessages nodeID (BlockingOn (protlet ++ "__Response") [server] (k . head)) inbox [buildRequest server req]
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
  ((Message{..}, requests'), inbox') <- findMessage isRequest oneOf inbox
  case step _msgBody state of
    Just (ans, state') ->
      pure $ SentMessages nodeID (Running state') (requests' ++ inbox') [buildReply _msgFrom ans]
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
  ((m, requests'), rest) <- findMessage (withTag label tag) oneOf inbox
  case receive m state of
    Just state' ->
      pure $ ReceivedMessage nodeID (Running state') (requests' ++ rest)
    _ ->
      empty

trySend :: (Monad m, Alternative m) =>
  Label -> Send m s -> NodeID -> s -> [Message] -> m (Transition s)
trySend label send nodeID state inbox = do
  (msg , state') <- send nodeID state
  pure $ SentMessages nodeID (Running state') inbox [msg { _msgLabel = label }]

tryBroadcast :: (Alternative m) =>
  Label -> String -> Broadcast s -> NodeID -> s -> [Message] -> m (Transition s)
tryBroadcast label name broadcast nodeID state inbox = case broadcast state of
  Just (casts, k) ->
    pure $ SentMessages nodeID (BlockingOn (name ++ "__Response") (fst <$> casts) k) inbox (buildRequest <$> casts)
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

possibleTransitions :: (Monad f, Alternative f) => Network f s -> f (Transition s)
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

stepNetwork :: (Monad f, Alternative f) => Network f s -> f (Network f s)
stepNetwork network = applyTransition <$> possibleTransitions network <*> pure network

initializeNetwork :: [(NodeID,s)] -> [(Label, Protlet f s)] -> Network f s
initializeNetwork ns protlets = Network {
  _nodes = fst <$> ns,
  _states = Map.fromList [ (n, Running s) | (n, s) <- ns ],
  _inboxes = Map.fromList [ (n, []) | n <- fst <$> ns ],
  _protlets = protlets
  }