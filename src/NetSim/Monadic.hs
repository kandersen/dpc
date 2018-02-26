{-# LANGUAGE RecordWildCards #-}
module NetSim.Monadic where

import NetSim.Core
import Control.Applicative
import qualified Data.Map as Map
import Control.Monad

data SimulationM l a = Pure a
                     | Choice [(l, SimulationM l a)]

instance Functor (SimulationM l) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Choice cs) = Choice $ fmap (fmap (fmap f)) cs

instance Applicative (SimulationM l) where
  pure = Pure
  mf <*> ma = case mf of
    Pure f -> fmap f ma
    Choice fs -> Choice $
      for fs $ \(d, mf') ->
        (d, mf' <*> ma)

instance Monad (SimulationM l) where
  ma >>= mf = case ma of
    Pure a -> mf a
    Choice mas -> Choice $
      for mas $ \(d, ma') ->
        (d, ma' >>= mf)

instance Monoid l => Alternative (SimulationM l) where
  empty = Choice []
  ma <|> mb = case (ma, mb) of
    (Pure a, Pure b) -> Choice [(mempty, return a), (mempty, return b)]
    (Pure a, Choice cs) -> Choice $ (mempty, return a) : cs
    (Choice cs, Pure b) -> Choice $ cs ++ [(mempty, return b)]
    (Choice cs, Choice cs') -> Choice $ cs ++ cs'

label :: l -> SimulationM l a -> SimulationM l a
label l m = Choice [(l, m)]

choice :: l -> a -> SimulationM l a
choice l a = Choice [(l, pure a)]

for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap


findRequestM :: String -> [Message] -> SimulationM String (Message, [Message])
findRequestM rpc messages = do
  let choices = filter (rightMessage . fst) . picks $ messages
  Choice [ (show msg, return (msg, rest)) | (msg, rest) <- choices ]
  where
    rightMessage Message{..} = _msgTag == (rpc ++ "__Request")

findResponseM :: String -> NodeID -> [Message] -> SimulationM String (Message, [Message])
findResponseM rpc sender messages = do
  let choices = filter (rightMessage . fst) . picks $ messages
  Choice [ (show msg, return (msg, rest)) | (msg, rest) <- choices ]
  where
    rightMessage Message{..} = _msgTag == (rpc ++ "__Response") && _msgFrom == sender


type ServerStepM s = [Int] -> Node s -> SimulationM String ([Int], NodeState s)

tryServerStepM :: String -> ServerStepM s -> NodeID -> Node s -> SimulationM String (NodeTransition s)
tryServerStepM rpc step nodeID node@Node{..} = do
  (Message{..}, msgs') <- label "Choosing message to process on the server" $ findRequestM rpc _incommingMsgs
  let n' = node { _incommingMsgs = msgs' }
  (ans, s') <- step _msgBody n'
  return $ SentMessage n' {_state = s' } (buildReply _msgFrom ans)
  where
    buildReply :: NodeID -> [Int] -> Message
    buildReply receiver body = Message {
      _msgTag = rpc ++ "__Response",
      _msgFrom = nodeID,
      _msgBody = body,
      _msgTo = receiver
      }

type ClientStepM s = Node s -> SimulationM String (NodeID, [Int], [Int] -> NodeState s)


tryClientStepM :: String -> ClientStepM s -> NodeID -> Node s -> SimulationM String (NodeTransition s)
tryClientStepM rpc step nodeID node = do
   (server, req, k) <- step node
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

choices :: Alternative f => [f a] -> f a
choices = foldr (<|>) empty

data ProtocolM s = RPCM String (ClientStepM s) (ServerStepM s)

applyRPCM :: ProtocolM s -> NodeID -> Node s -> SimulationM String (NodeTransition s)
applyRPCM (RPCM name cstep sstep) nodeID node =
  tryClientStepM name cstep nodeID node <|> tryServerStepM name sstep nodeID node

choseOneOf :: Monoid l => [a] -> SimulationM l a
choseOneOf [] = empty
choseOneOf (x:xs) = choice mempty x <|> choseOneOf xs

stepNodeM :: Node s -> SimulationM String (NodeTransition s)
stepNodeM n@Node{..} = case _state of
  (BlockingOn rpcName from k) -> do
    (response, msgs') <- findResponseM rpcName from _incommingMsgs
    return $ Received $ n { _state = k (_msgBody response), _incommingMsgs = msgs' }
  _ -> empty

stepNetworkM :: NetworkM ProtocolM s -> SimulationM String (NetworkM ProtocolM s)
stepNetworkM network@NetworkM{..} = do
  nodeID <- choseOneOf $ Map.keys _nodes
  let node = _nodes Map.! nodeID
  t <- stepNodeM node <|> do
           rpc <- choseOneOf _rpcs
           applyRPCM rpc nodeID node
  return $ updateNetwork nodeID t network

stepSimulationIO :: SimulationM String a -> IO a
stepSimulationIO (Pure a) = return a
stepSimulationIO (Choice []) = error "Impossible step"
stepSimulationIO (Choice [(l, a)]) = putStrLn l >> stepSimulationIO a
stepSimulationIO (Choice cs) = do
  next <- userPick cs
  stepSimulationIO next

simulateNetworkIO :: a -> (a -> SimulationM String a) -> IO a
simulateNetworkIO start step = go start (step start)
  where
    go _ (Pure a) = return a
    go prev (Choice []) = return prev
    go _ m = do
      next <- stepSimulationIO m
      go next (step next)
