{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module NetSim.Core where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (partition, intercalate)
import Control.Applicative
import Control.Monad

type NodeID = Int

data Message = Message {
  _msgFrom :: NodeID,
  _msgTag :: String,
  _msgBody :: [Int],
  _msgTo :: NodeID
  }

instance Show Message where
  show Message{..} = concat [
    show _msgFrom, " : ",
    _msgTag, "(", intercalate "," (map show _msgBody), ")" ]

data NodeState s = Running s
                 | BlockingOn String NodeID ([Int] -> NodeState s)

instance Show s => Show (NodeState s) where
  show (Running s) = "Running " ++ show s
  show (BlockingOn rpc from _) = unwords ["BlockingOn", rpc, show from, "<Continuation>"]

data Node s = Node {
  _state :: NodeState s,
  _incommingMsgs :: [Message]
} deriving Show

data Protocol s = RPC String (ClientStep s) (ServerStep s)
                | ARPC String (ClientStep s) (ServerReceive s) (ServerSend s)
                | Notification String (Send s) (Receive s)

type ClientStep s = Node s -> Maybe (NodeID, [Int], [Int] -> NodeState s)
type ServerStep s = [Int] -> Node s -> Maybe ([Int], NodeState s)


type ServerReceive s = Message -> NodeState s -> Maybe (NodeState s)
type ServerSend s = NodeID -> NodeState s -> Maybe (Message, NodeState s)

type Receive s = Message -> s -> Maybe s
type Send s = NodeID -> s -> Maybe (Message, s)


instance Show s => Show (Protocol s) where
  show (RPC name _ _) = unwords [ "RPC { _name =", name, "}" ]
  show (ARPC name _ _ _) = unwords [ "ARPC { _name =", name, "}" ]
  show (Notification name _ _) = unwords [ "Notification { _name =", name, "}" ]

type Network s = NetworkM Protocol s

data NetworkM p s = NetworkM {
  _nodes :: Map NodeID (Node s),
  _rpcs :: [p s]
  } deriving Show

picks :: Alternative m => [a] -> m (a, [a])
picks = go []
  where
    go _ [] = empty
    go l (x:rs) = pure (x, l ++ rs) <|> go (x : l) rs

findMessage :: (Monad m, Alternative m) => (Message -> [Message] -> [Message] -> m a) -> String  -> [Message] -> m a
findMessage combine tag messages = do
  let (candidates, rest) = partition rightMessage messages
  (msg, ms) <- picks candidates
  combine msg ms rest
  where
    rightMessage Message{..} = _msgTag == tag

findResponse :: (Monad m, Alternative m) => String -> NodeID -> [Message] -> m (Message, [Message])
findResponse protocol sender =
  findMessage combine (protocol ++ "__Response")
  where
    combine msg [] rest = if (_msgFrom msg == sender) then  pure (msg, rest) else empty
    combine   _  _    _ = error "multiple responses to one request shouldn't happen"

findRequest :: (Monad m, Alternative m) => String -> [Message] -> m (Message, [Message])
findRequest protocol =
  findMessage combine (protocol ++ "__Request")
  where
    combine msg others rest = pure (msg, others ++ rest)

findNotificiation :: (Monad m, Alternative m) => String -> [Message] -> m (Message, [Message])
findNotificiation protocol =
  findMessage combine (protocol ++ "__Notification")
  where
    combine msg others rest = pure (msg, others ++ rest)

data NodeTransition s = Received (Node s)
                      | SentMessage (Node s) Message
                      deriving Show

stepNode :: Node s -> [(NodeTransition s)]
stepNode n@Node{..} = case _state of
  (BlockingOn rpcName from k) -> do
    (response, msgs') <- findResponse rpcName from _incommingMsgs
    return $ Received $ n { _state = k (_msgBody response), _incommingMsgs = msgs' }
  _ -> []

deliverToNode :: Message -> Node s -> Node s
deliverToNode msg node = node { _incommingMsgs = _incommingMsgs node ++ [msg] }

deliver :: Message -> NetworkM p s -> NetworkM p s
deliver msg@Message { .. } network =
  network { _nodes = Map.adjust (deliverToNode msg) _msgTo (_nodes network) }

updateNetwork :: NodeID -> NodeTransition s -> (NetworkM p s -> NetworkM p s)
updateNetwork node (Received s) network@NetworkM{..} =
  network { _nodes = Map.insert node s _nodes }
updateNetwork node (SentMessage s msg) network@NetworkM{..} =
  deliver msg $ network { _nodes = Map.insert node s _nodes }

tryClientStep :: String -> ClientStep s -> NodeID -> Node s -> [(NodeTransition s)]
tryClientStep rpc step nodeID node =
  case step node of
    Nothing -> []
    Just (server, req, k) ->
      return $ SentMessage (buildBlockingNode server k) (buildRequest server req)
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


tryServerStep :: String -> ServerStep s -> NodeID -> Node s -> [(NodeTransition s)]
tryServerStep rpc step nodeID node@Node{..} = do
  (Message{..}, msgs') <- findRequest rpc _incommingMsgs
  let n' = node { _incommingMsgs = msgs' }
  case step _msgBody n' of
    Nothing -> []
    Just (ans, s') ->
      return $ SentMessage n' {_state = s' } (buildReply _msgFrom ans)
  where
    buildReply :: NodeID -> [Int] -> Message
    buildReply receiver body = Message {
      _msgTag = rpc ++ "__Response",
      _msgFrom = nodeID,
      _msgBody = body,
      _msgTo = receiver
      }

applyRPC :: Protocol s -> NodeID -> Node s -> [(NodeTransition s)]
applyRPC (RPC name cstep sstep) nodeID node =
  tryClientStep name cstep nodeID node <|> tryServerStep name sstep nodeID node
applyRPC (ARPC name cstep sreceive ssend) nodeID node =
   tryClientStep name cstep nodeID node
   <|> tryServerSend ssend nodeID node
   <|> tryServerReceive name sreceive node
applyRPC (Notification name send receive) nodeID node =
  trySend send nodeID node
  <|> tryReceiveNotification name receive node

trySend :: Alternative m => Send s -> NodeID -> Node s -> m (NodeTransition s)
trySend send nodeID node@Node{..} =
  case _state of
    Running state ->
      case send nodeID state of
        Nothing -> empty
        Just (msg, s') ->
          pure $ SentMessage node { _state = Running s' } msg
    _ -> empty

tryReceiveNotification :: (Monad m, Alternative m) => String -> Receive s -> Node s -> m (NodeTransition s)
tryReceiveNotification protocol receive node@Node{..} = do
  (msg, msgs') <- findNotificiation protocol _incommingMsgs
  case _state of
    Running state ->
      case receive msg state of
        Just s' ->
          return . Received $ node { _state = Running s', _incommingMsgs = msgs' }
        Nothing -> empty
    _ -> empty

tryServerSend :: ServerSend s -> NodeID -> Node s -> [(NodeTransition s)]
tryServerSend send nodeID node@Node{..} =
  case send nodeID _state of
    Nothing -> []
    Just (msg, s') ->
      return $ SentMessage node { _state = s' } msg

tryServerReceive :: String -> ServerReceive s -> Node s -> [(NodeTransition s)]
tryServerReceive protocol receive node@Node{..} = do
  (msg, msgs') <- findRequest protocol _incommingMsgs
  case receive msg _state of
    Nothing -> []
    Just s' ->
      return . Received $ node {_state = s', _incommingMsgs = msgs' }


possibleTransitions :: Network s -> [(NodeID, NodeTransition s)]
possibleTransitions NetworkM{..} = do
  nodeID <- Map.keys _nodes
  let node = _nodes Map.! nodeID
  t <- stepNode node <|> do
    rpc <- _rpcs
    applyRPC rpc nodeID node
  return (nodeID, t)

applyTransition :: (NodeID, NodeTransition s) -> (Network s -> Network s)
applyTransition (nodeID, t) = updateNetwork nodeID t

stepNetwork :: Network s -> [Network s]
stepNetwork network = do
  t <- possibleTransitions network
  return $ applyTransition t network

initNode :: s -> Node s
initNode s = Node {
  _state = Running s,
  _incommingMsgs = []
  }

driveNetwork :: Show s => Network s -> IO ()
driveNetwork = go
  where
    go n = do
      print n
      void getLine
      case stepNetwork n of
        [] -> return ()
        (n':_) -> go n'

runNetwork :: Show s => Int -> Network s -> IO ()
runNetwork = go
  where
    go 0 _ = putStrLn "Hit the step limit"
    go s n = do
      print n
      case stepNetwork n of
        [] -> return ()
        (n':_) -> go (s - 1) n'

runNetworkWithUserInput :: Show s => Network s -> IO ()
runNetworkWithUserInput = go
  where
    go n = do
      let possibilities = possibleTransitions n
      case possibilities of
        [] -> return ()
        _ -> do
          print n
          next <- userPick (zip (map show possibilities) possibilities)
          go $ applyTransition next n

userPick :: [(String, a)] -> IO a
userPick [] = error "Shouldn't happen"
userPick cs = do
  forM_ (zip cs [1 :: Int ..]) $ \((l,_), n) ->
    putStrLn $ show n ++ ":\t" ++ l
  n <- getIndex (length cs)
  return . snd $ cs !! n
  where
    getIndex :: Int -> IO Int
    getIndex atMost = do
      mn <- fmap fst . listToMaybe . reads <$> getLine
      case mn of
        Just n | 0 < n && n <= atMost -> return (n - 1)
        _ -> getIndex atMost

    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

