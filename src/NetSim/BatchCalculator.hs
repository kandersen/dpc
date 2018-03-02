{-# LANGUAGE RecordWildCards #-}
module NetSim.BatchCalculator where

import NetSim.Core

data PState = ClientInit NodeID NodeID Int Int
            | ClientDone Int
            | ServerBatch Int [(NodeID, Int, Int)]
            | ServerSend Int [(NodeID, Int)]
            deriving Show

computeProtocol :: Alternative f => Protlet f PState
computeProtocol = ARPC "Compute" client serverRec serverSend
  where
    client state = case state of
      ClientInit _ server a b ->
        pure (server, [a, b], clientRecieve)
      _ -> empty
    clientRecieve [n] = ClientDone n

    serverRec :: Receive PState
    serverRec Message{..} state = case state of
      ServerBatch batchSize reqs
        | length reqs == batchSize - 1 -> do
             let [a, b] = _msgBody
             pure $ ServerSend batchSize (fmap compute $ (_msgFrom, a, b) : reqs)
        | otherwise -> do
            let [a, b] = _msgBody
            pure $ ServerBatch  batchSize ((_msgFrom, a, b) : reqs)
      _ -> empty

    compute :: (NodeID, Int, Int) -> (NodeID, Int)
    compute (c, a, b) = (c, a + b)

    --serverSend :: Send PState
    serverSend nodeID state = case state of
      ServerSend batchSize [(c, n)] ->
        pure (buildReply c [n], ServerBatch batchSize [])
      ServerSend batchSize ((c, n):ms) ->
        pure (buildReply c [n], ServerSend batchSize ms)
      _ -> empty
      where
        buildReply to body = Message {
          _msgTo = to,
          _msgTag = "Compute__Response",
          _msgBody = body,
          _msgFrom = nodeID
          }

initNetwork :: Alternative f => Network f PState
initNetwork = initializeNetwork nodes protlets
  where
    nodes = [ (0, ServerBatch 3 [])
            , (1, ClientInit 1 0 2 40)
            , (2, ClientInit 2 0 1 10)
            , (3, ClientInit 3 0 10 100)
            , (4, ClientInit 4 0 2 3)
            , (5, ClientInit 5 0 7 7)
            , (6, ClientInit 6 0 100 1000)
            ]
    protlets = [computeProtocol]
