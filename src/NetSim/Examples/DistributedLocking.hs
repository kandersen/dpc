{-# LANGUAGE RecordWildCards #-}
module NetSim.Examples.DistributedLocking where

import NetSim.Core
import Control.Applicative

data State = ClientInit NodeID NodeID Int
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

acquire :: Protlet f State
acquire = RPC "Acquire" client server
  where
    client :: ClientStep State
    client state = case state of
      ClientInit lock resource val ->
        pure (lock, [], clientReceive lock resource val)
      _ -> empty
    clientReceive lock resource val [token] = ClientAcquired lock resource val token

    server :: ServerStep State
    server [] state = case state of
      LockIdle nextToken ->
        pure ([nextToken], LockHeld (nextToken + 1) nextToken)
      _ -> empty

release :: Alternative f => Protlet f State
release = Notification "Release" client server
  where
    client this state = case state of
      ClientUpdateDone lock token ->
        pure (buildNotification lock token, ClientDone)
      _ -> empty
      where
        buildNotification lock token = Message {
          _msgFrom = this,
          _msgBody = [token],
          _msgTag = "Release__Notification",
          _msgTo = lock
          }

    server :: Receive State
    server Message{..} state = case state of
      LockIdle n -> Just (LockIdle n)
      LockHeld this held -> pure $ if held == head _msgBody
                                   then LockIdle this
                                   else LockHeld this held
      _ -> empty

modifyResource :: Alternative f => Protlet f State
modifyResource = ARPC "Modify" clientStep serverReceive serverRespond
  where
    clientStep :: ClientStep State
    clientStep state = case state of
      ClientAcquired lock resource val token ->
        pure (resource, [val, token], clientReceive lock resource val token)
      _ -> empty

    clientReceive lock resource val token [ans] = case ans of
      0 -> ClientInit lock resource val
      _ -> ClientUpdateDone lock token

    serverReceive :: Receive State
    serverReceive Message{..} state = case state of
      ResourceIdle lock val -> do
        let [val', token] = _msgBody
        pure $ ResourceVerifying lock (_msgFrom, val', token) val
      _ -> empty

    --serverRespond :: Send State
    serverRespond nodeID snode = case snode of
      ResourceModifySucceeded lock client val' ->
        pure (buildReply client [1], ResourceIdle lock val')
      ResourceModifyFailed lock client val ->
        pure (buildReply client [0], ResourceIdle lock val)
      _ -> empty
      where
        buildReply to ans = Message {
          _msgTag = "Modify__Response",
          _msgFrom = nodeID,
          _msgTo = to,
          _msgBody = ans
          }

verifyToken :: Alternative f => Protlet f State
verifyToken = RPC "Verify" clientStep serverStep
  where
    clientStep :: ClientStep State
    clientStep state = case state of
      ResourceVerifying lock (client, write, token) val ->
        pure (lock, [token], clientReceive lock client write val)
      _ -> empty
    clientReceive lock client write val [ans] = case ans of
      0 -> ResourceModifyFailed lock client val
      _ -> ResourceModifySucceeded lock client write

    serverStep :: ServerStep State
    serverStep [token] state = case state of
      LockIdle _ ->
        pure ([0], state)
      LockHeld _ heldBy ->
        pure ([if heldBy == token then 1 else 0], state)
      _ -> empty

initNetwork :: Alternative f => Network f State
initNetwork = initializeNetwork nodes protlets
  where
    nodes = [ (0, ClientInit 2 3 42)
            , (1, ClientInit 2 3 99)
            , (2, LockIdle 0)
            , (3, ResourceIdle 2 0)
            ]
    protlets = [ acquire
               , modifyResource
               , verifyToken
               , release
               ]
