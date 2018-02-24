{-# LANGUAGE RecordWildCards #-}
module NetSim.DistributedLocking where

import NetSim.Core
import Data.Map as Map

data AppNodeStates = ClientInit NodeID NodeID Int
                   | ClientAcquired NodeID NodeID Int Int
                   | ClientDone
                   | LockIdle Int
                   | LockHeld Int Int
                   | ResourceIdle NodeID Int
                   | ResourceVerifying NodeID (NodeID, Int, Int) Int
                   | ResourceModifySucceeded NodeID NodeID Int Int
                   | ResourceModifyFailed NodeID NodeID Int
                   deriving Show

acquire :: Protocol AppNodeStates
acquire = RPC "Acquire" client server
  where
    client cnode = case _state cnode of
                     Running (ClientInit lock resource val) -> Just (
                       lock, [],
                       \[token] -> Running . ClientAcquired lock resource val $ token)
                     _ -> Nothing
    server [] snode = case _state snode of
                        Running (LockIdle nextToken) ->
                          Just ([nextToken], Running $ LockHeld (nextToken + 1) nextToken)
                        _ -> Nothing

modifyResource :: Protocol AppNodeStates
modifyResource = ARPC "Modify" clientStep serverReceive serverRespond
  where
    clientStep :: ClientStep AppNodeStates
    clientStep cnode = case _state cnode of
      Running (ClientAcquired lock resource val token) ->
        Just (resource, [val, token], \[ans] -> case ans of
                                                  0 -> Running $ ClientInit lock resource val
                                                  _ -> Running ClientDone)
      _ -> Nothing

    serverReceive :: ServerReceive AppNodeStates
    serverReceive Message{..} snode = case snode of
      Running (ResourceIdle lock val) -> do
        let [val', token] = _msgBody
        return . Running $ ResourceVerifying lock (_msgFrom, val', token) val
      _ -> Nothing

    serverRespond :: ServerSend AppNodeStates
    serverRespond nodeID snode = case snode of
      Running (ResourceModifySucceeded lock _ _ _) -> Nothing
      Running (ResourceModifyFailed lock _ _) -> Nothing
      _ -> Nothing
      where
        buildReply to ans = Message {
          _msgTag = "Modify__Response",
          _msgFrom = nodeID,
          _msgTo = to,
          _msgBody = ans
          }

verifyToken :: Protocol AppNodeStates
verifyToken = RPC "Verify" clientStep serverStep
  where
    clientStep cnode = case _state cnode of
      Running (ResourceVerifying lock (client, write, token) val) ->
        Just (lock, [token], \[ans] ->
                 case ans of
                   0 -> Running $ undefined)
    serverStep [token] snode = case _state snode of
      s@(Running (LockIdle _)) ->
        Just ([0], s)
      s@(Running (LockHeld _ heldBy)) ->
        Just ([if heldBy == token then 1 else 0], s)
      _ -> Nothing


initNetwork :: Network AppNodeStates
initNetwork = NetworkM {
  _nodes = Map.fromList [(0, initNode $ ClientInit 2 3 42),
                         (1, initNode $ ClientInit 2 3 99),
                         (2, initNode $ LockIdle 0),
                         (3, initNode $ ResourceIdle 2 0)],
  _rpcs = [acquire, modifyResource, verifyToken]
  }

main :: IO ()
main = driveNetwork initNetwork

