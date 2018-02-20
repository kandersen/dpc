{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NetSim.Core where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (partition)
import Control.Applicative
import Control.Monad

type NodeID = Int

data Message = Message {
  _msgFrom :: NodeID,
  _msgTag :: String,
  _msgBody :: [Int],
  _msgTo :: NodeID
  }
  deriving Show

data NodeState s = Running s
                 | BlockingOn String NodeID ([Int] -> NodeState s)

instance Show s => Show (NodeState s) where
  show (Running s) = "Running " ++ show s
  show (BlockingOn rpc from _) = unwords ["BlockingOn", rpc, show from, "<Continuation>"]

data Node s = Node {
  _state :: NodeState s,
  _incommingMsgs :: [Message],
  _outgoingMsgs :: [Message]
} deriving Show

type ClientStep s = Node s -> Maybe (NodeID, [Int], [Int] -> NodeState s)
type ServerStep s = [Int] -> Node s -> Maybe ([Int], NodeState s)


type ServerReceive s = [Int] -> Node s -> Maybe (NodeState s)
type ServerSend s = Node s -> Maybe (NodeID, [Int], NodeState s)

data Protocol s = RPC String (ClientStep s) (ServerStep s)
                | ARPC String (ClientStep s) (ServerReceive s) (ServerSend s)

instance Show s => Show (Protocol s) where
  show (RPC name _ _) = unwords [ "RPC { _name =", name, "}" ]
  show (ARPC name _ _ _) = unwords [ "ARPC { _name =", name, "}" ]

data Network s = Network {
  _nodes :: Map NodeID (Node s),
  _rpcs :: [Protocol s]
  } deriving Show

findResponse :: String -> NodeID -> [Message] -> Maybe (Message, [Message])
findResponse rpc sender messages = do
  let (candidates, rest) = partition rightMessage messages
  case candidates of
    [] -> Nothing
    [m] -> Just (m, rest)
    _ -> error $ "multiple responses to one request shouldn't happen"
  where
    rightMessage Message{..} = _msgTag == (rpc ++ "__Response") && _msgFrom == sender

data NodeTransition s = Received (Node s)
                      | SentMessage (Node s) Message

stepNode :: Node s -> Maybe (NodeTransition s)
stepNode n@Node{..} = case _state of
  (BlockingOn rpcName from k) -> do
    (response, msgs') <- findResponse rpcName from _incommingMsgs
    return $ Received $ n { _state = k (_msgBody response), _incommingMsgs = msgs' }
  (Running s) -> do
    case _outgoingMsgs of
      [] -> Nothing
      (m:ms) -> Just $ SentMessage n{ _outgoingMsgs = ms } m

deliverToNode :: Message -> Node s -> Node s
deliverToNode msg node = node { _incommingMsgs = _incommingMsgs node ++ [msg] }

deliver :: Message -> Network s -> Network s
deliver msg@Message { .. } network =
  network { _nodes = Map.adjust (deliverToNode msg) _msgTo (_nodes network) }

updateNetwork :: NodeID -> NodeTransition s -> (Network s -> Network s)
updateNetwork node (Received s) network@Network{..} =
  network { _nodes = Map.insert node s _nodes }
updateNetwork node (SentMessage s msg) network@Network{..} =
  deliver msg $ network { _nodes = Map.insert node s _nodes }

tryClientStep :: String -> ClientStep s -> NodeID -> Node s -> Maybe (NodeTransition s)
tryClientStep rpc step nodeID node =
  case step node of
    Nothing -> Nothing
    Just (server, req, k) ->
      Just $ SentMessage (buildBlockingNode server k) (buildRequest server req)
  where
    buildBlockingNode server k = node {
      _state = BlockingOn rpc server k
      }
    buildRequest receiver body = Message {
      _msgTag = rpc ++ "__Request",
      _msgTo = receiver,
      _msgBody = body,
      _msgFrom = nodeID
      }

findRequest :: String -> [Message] -> Maybe (Message, [Message])
findRequest rpc messages = do
  let (candidates, rest) = partition rightMessage messages
  case candidates of
    [] -> Nothing
    (m:ms') -> Just (m, ms' ++ rest)
  where
    rightMessage Message{..} = _msgTag == (rpc ++ "__Request")


tryServerStep :: String -> ServerStep s -> NodeID -> Node s -> Maybe (NodeTransition s)
tryServerStep rpc step nodeID node@Node{..} =
  case findRequest rpc _incommingMsgs of
    Nothing -> Nothing
    Just (Message{..},msgs') -> do
      let n' = node { _incommingMsgs = msgs' }
      case step _msgBody n' of
        Nothing -> Nothing
        Just (ans, s') ->
          Just . Received $ n' {
            _state = s',
            _outgoingMsgs = buildReply _msgFrom ans : _outgoingMsgs
          }
  where
    buildReply :: NodeID -> [Int] -> Message
    buildReply receiver body = Message {
      _msgTag = rpc ++ "__Response",
      _msgFrom = nodeID,
      _msgBody = body,
      _msgTo = receiver
      }

applyRPC :: Protocol s -> NodeID -> Node s -> Maybe (NodeTransition s)
applyRPC (RPC name cstep sstep) nodeID node =
  tryClientStep name cstep nodeID node <|> tryServerStep name sstep nodeID node

step :: Network s -> [Network s]
step network@Network{..} = do
  nodeID <- Map.keys _nodes
  let node = _nodes Map.! nodeID
  case stepNode node of
    Nothing -> do
      rpc <- _rpcs
      case applyRPC rpc nodeID node of
        Nothing -> empty
        Just t -> return $ updateNetwork nodeID t network
    Just t -> return $ updateNetwork nodeID t network

initNode :: s -> Node s
initNode s = Node {
  _state = Running s,
  _incommingMsgs = [],
  _outgoingMsgs = []
  }

driveNetwork :: Show s => Network s -> IO ()
driveNetwork = go
  where
    go n = do
      print n
      void $ getLine
      case step n of
        [] -> return ()
        (n':_) -> go n'

runNetwork :: Show s => Int -> Network s -> IO ()
runNetwork stepLimit = go stepLimit
  where
    go 0 _ = putStrLn $ "Hit the step limit"
    go s n = do
      print n
      case step n of
        [] -> return ()
        (n':_) -> go (s - 1) n'

