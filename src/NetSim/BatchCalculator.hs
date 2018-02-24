{-# LANGUAGE RecordWildCards #-}
module NetSim.BatchCalculator where

import NetSim.Core
import Data.Map

data PState = ClientInit NodeID NodeID Int Int
            | ClientDone Int
            | ServerBatch Int [(NodeID,Int,Int)]
            | ServerSend Int [(NodeID, Int)]
            deriving Show

computeProtocol :: Protocol PState
computeProtocol = ARPC "Compute" client serverRec serverSend
  where
    client node = case _state node of
      Running (ClientInit _ server a b) ->
        Just (server, [a, b], clientRecieve)
      _ -> Nothing
    clientRecieve [n] = Running $ ClientDone n

    serverRec :: ServerReceive PState
    serverRec Message{..} state = case state of
      (Running (ServerBatch batchSize reqs))
        | length reqs == batchSize - 1 -> do
             let [a, b] = _msgBody
             return .  Running $ ServerSend batchSize (fmap compute $ (_msgFrom, a, b) : reqs)
        | otherwise -> do
            let [a, b] = _msgBody
            return . Running $ ServerBatch  batchSize ((_msgFrom, a, b) : reqs)
      _ -> Nothing
    compute :: (NodeID, Int, Int) -> (NodeID, Int)
    compute (c, a, b) = (c, a + b)

    serverSend :: ServerSend PState
    serverSend nodeID state = case state of
      Running (ServerSend batchSize [(c, n)]) ->
        Just (buildReply c [n], Running $ ServerBatch batchSize [])
      Running (ServerSend batchSize ((c, n):ms)) ->
        Just (buildReply c [n], Running $ ServerSend batchSize ms)
      _ -> Nothing
      where
        buildReply to body = Message {
          _msgTo = to,
          _msgTag = "Compute__Response",
          _msgBody = body,
          _msgFrom = nodeID
          }

initNetwork :: Network PState
initNetwork = NetworkM {
  _nodes = fromList [ (0, initNode $ ServerBatch 3 [])
                    , (1, initNode $ ClientInit 1 0 2 40)
                    , (2, initNode $ ClientInit 2 0 1 10)
                    , (3, initNode $ ClientInit 3 0 10 100)
                    , (4, initNode $ ClientInit 4 0 2 3)
                    , (5, initNode $ ClientInit 5 0 7 7)
                    , (6, initNode $ ClientInit 6 0 100 1000)
                    ],
  _rpcs = [computeProtocol]
  }
