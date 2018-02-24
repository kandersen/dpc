module NetSim.SimpleClientServer where

import NetSim.Core
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad
import NetSim.Monadic

data AppNodeStates = ClientInit NodeID
                   | ClientDone Int
                   | Server Int
                   deriving Show

queryServer :: ProtocolM AppNodeStates
queryServer = RPCM "Query" cstep sstep
  where
    cstep :: ClientStepM AppNodeStates
    cstep cnode = case _state cnode of
      Running (ClientInit server) -> return (server, [], Running . ClientDone . head)
      _ -> empty
      
    sstep :: ServerStepM AppNodeStates
    sstep [] snode = case _state snode of
      Running (Server n) ->
        return ([n], _state snode)
      _ -> empty

initNetwork :: NetworkM ProtocolM AppNodeStates
initNetwork = NetworkM {
  _nodes = Map.fromList [(0, initNode $ ClientInit 1),
                         (1, initNode $ Server 42 ),
                         (2, initNode $ ClientInit 1)],
  _rpcs = [queryServer]
  }

main :: IO ()
main = void $ simulateNetworkIO initNetwork stepNetworkM
