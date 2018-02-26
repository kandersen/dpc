{-# LANGUAGE RecordWildCards #-}
module NetSim.DistributedLocking where

import NetSim.Core
import Data.Map as Map

data AppNodeState = ClientInit NodeID NodeID Int
                  | ClientAcquired NodeID NodeID Int Int
                  | ClientUpdateDone NodeID Int
                  | ClientDone
                  | LockIdle Int
                  | LockHeld Int Int
                  | ResourceIdle NodeID Int
                  | ResourceVerifying NodeID (NodeID, Int, Int) Int
                  | ResourceModifySucceeded NodeID NodeID Int
                  | ResourceModifyFailed NodeID NodeID Int
                  deriving Show

acquire :: Protocol AppNodeState
acquire = RPC "Acquire" client server
  where
    client :: ClientStep AppNodeState
    client cnode = case _state cnode of
                     Running (ClientInit lock resource val) ->
                       Just (lock, [], clientReceive lock resource val)
                     _ -> Nothing
    clientReceive lock resource val [token] = Running $ ClientAcquired lock resource val token

    server :: ServerStep AppNodeState
    server [] snode = case _state snode of
                        Running (LockIdle nextToken) ->
                          Just ([nextToken], Running $ LockHeld (nextToken + 1) nextToken)
                        _ -> Nothing

release :: Protocol AppNodeState
release = Notification "Release" client server
  where
    client :: Send AppNodeState
    client this state = case state of
      ClientUpdateDone lock token ->
        Just (buildNotification lock token, ClientDone)
      _ -> Nothing
      where
        buildNotification lock token = Message {
          _msgFrom = this,
          _msgBody = [token],
          _msgTag = "Release__Notification",
          _msgTo = lock
          }

    server :: Receive AppNodeState
    server Message{..} state = case state of
      LockIdle n -> Just (LockIdle n)
      LockHeld this held -> Just $ if held == head _msgBody
                                   then LockIdle this
                                   else LockHeld this held
      _ -> Nothing

modifyResource :: Protocol AppNodeState
modifyResource = ARPC "Modify" clientStep serverReceive serverRespond
  where
    clientStep :: ClientStep AppNodeState
    clientStep cnode = case _state cnode of
      Running (ClientAcquired lock resource val token) ->
        Just (resource, [val, token], clientReceive lock resource val token)
      _ -> Nothing

    clientReceive lock resource val token [ans] = case ans of
      0 -> Running $ ClientInit lock resource val
      _ -> Running $ ClientUpdateDone lock token

    serverReceive :: ServerReceive AppNodeState
    serverReceive Message{..} snode = case snode of
      Running (ResourceIdle lock val) -> do
        let [val', token] = _msgBody
        return . Running $ ResourceVerifying lock (_msgFrom, val', token) val
      _ -> Nothing

    serverRespond :: ServerSend AppNodeState
    serverRespond nodeID snode = case snode of
      Running (ResourceModifySucceeded lock client val') ->
        Just (buildReply client [1], Running $ ResourceIdle lock val')
      Running (ResourceModifyFailed lock client val) ->
        Just (buildReply client [0], Running $ ResourceIdle lock val)
      _ -> Nothing
      where
        buildReply to ans = Message {
          _msgTag = "Modify__Response",
          _msgFrom = nodeID,
          _msgTo = to,
          _msgBody = ans
          }

verifyToken :: Protocol AppNodeState
verifyToken = RPC "Verify" clientStep serverStep
  where
    clientStep :: ClientStep AppNodeState
    clientStep cnode = case _state cnode of
      Running (ResourceVerifying lock (client, write, token) val) ->
        Just (lock, [token], clientReceive lock client write val)
      _ -> Nothing
    clientReceive lock client write val [ans] = case ans of
      0 -> Running $ ResourceModifyFailed lock client val
      _ -> Running $ ResourceModifySucceeded lock client write

    serverStep [token] snode = case _state snode of
      s@(Running (LockIdle _)) ->
        Just ([0], s)
      s@(Running (LockHeld _ heldBy)) ->
        Just ([if heldBy == token then 1 else 0], s)
      _ -> Nothing

initNetwork :: Network AppNodeState
initNetwork = NetworkM {
  _nodes = Map.fromList [(0, initNode $ ClientInit 2 3 42),
                         (1, initNode $ ClientInit 2 3 99),
                         (2, initNode $ LockIdle 0),
                         (3, initNode $ ResourceIdle 2 0)],
  _rpcs = [acquire
          , modifyResource
          , verifyToken
          , release
          ]
  }

main :: IO ()
main = driveNetwork initNetwork

