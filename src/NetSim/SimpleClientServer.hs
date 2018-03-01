module NetSim.SimpleClientServer where

import NetSim.Core
import Control.Applicative

data State = ClientInit NodeID
            | ClientDone Int
            | Server Int
            deriving Show

queryServer :: Protlet State
queryServer = RPC "Query" cstep sstep
  where
    cstep :: ClientStep State
    cstep state = case state of
      ClientInit server -> pure (server, [], ClientDone . head)
      _ -> empty

    sstep :: ServerStep State
    sstep [] state = case state of
      Server n -> pure ([n], state)
      _ -> empty

initNetwork :: Network State
initNetwork = initializeNetwork nodes [queryServer]
  where
    client0 = 0
    client1 = 2
    server = 1
    nodes = [
      (client0, ClientInit server),
      (server, Server 42),
      (client1, ClientInit server)
      ]
