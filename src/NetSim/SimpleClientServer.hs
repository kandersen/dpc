module NetSim.SimpleClientServer where

import NetSim.Core
import Data.Map as Map

data AppNodeStates = ClientInit NodeID
                   | ClientDone Int
                   | Server Int
                   deriving Show

queryServer :: Protocol AppNodeStates
queryServer = RPC "Query" cstep sstep
  where
  cstep cnode = case _state cnode of
    Running (ClientInit server) -> Just (server, [], Running . ClientDone . head)
    _ -> Nothing
  sstep [] snode = case _state snode of
    Running (Server n) ->
      Just ([n], _state snode)
    _ -> Nothing

initNetwork :: Network AppNodeStates
initNetwork = Network {
  _nodes = Map.fromList [(0, initNode $ ClientInit 1),
                         (1, initNode $ Server 42 ),
                         (2, initNode $ ClientInit 1)],
  _rpcs = [queryServer]
  }

main :: IO ()
main = driveNetwork initNetwork
