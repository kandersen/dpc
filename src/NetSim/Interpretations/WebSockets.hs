{-# LANGUAGE ScopedTypeVariables #-}
module NetSim.Interpretations.WebSockets where

import NetSim.Core
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Control.Monad.Reader

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

type DSocket = Socket Inet Stream TCP

type Address = Int
type NetworkDescription = Map NodeID Address

data NetworkContext = NetCtxt {
    _this :: Int,
    _addressBook :: Map NodeID DSocket
}

type Runner a = ReaderT NetworkContext IO a

run :: NetworkContext -> Runner a -> IO a
run ctxt p = runReaderT p ctxt

defaultMain :: Runner a -> IO ()
defaultMain program = do
  -- open my own socket
  -- open a socket to all peers
  -- -- get peer addresses from file
  (nd :: NetworkDescription) <- read <$> readFile "network.desc"
  print nd
  -- -- open connection to each and store in 
  -- build network context
  let netctxt = NetCtxt 0 Map.empty
  -- start running my program
  void $ run netctxt program
  -- if I ever terminate, cleanup my own and other sockets