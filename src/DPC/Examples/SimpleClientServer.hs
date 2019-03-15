module DPC.Examples.SimpleClientServer where

import DPC.Types
import DPC.Specifications

data State = ClientInit NodeID
           | ClientDone Int
           | Server Int
           deriving Show

queryServer :: Protlet f State
queryServer = RPC "Query" cstep sstep
  where
    cstep :: ClientStep State
    cstep state = case state of
      ClientInit server -> pure (server, [], ClientDone . head)
      _                 -> empty

    sstep :: ServerStep State
    sstep [] state = case state of
      Server n -> pure ([n], state)
      _        -> empty

initNetwork :: SpecNetwork f State
initNetwork = initializeNetwork nodeStates [(0, [queryServer])]
  where
    client0, client1, server :: NodeID
    client0 = 0
    client1 = 2
    server = 1
    nodeStates :: [(NodeID, [(NodeID, State)])]
    nodeStates = [
      (client0, [(0, ClientInit server)]),
      (server, [(0, Server 42)]),
      (client1, [(0, ClientInit server)])
      ]
