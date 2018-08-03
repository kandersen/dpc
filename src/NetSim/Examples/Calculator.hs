{-# LANGUAGE LambdaCase #-}
module NetSim.Examples.Calculator where

import NetSim.Core
import NetSim.Language

import qualified Data.Map as Map

-- Example to demonstrate concurrency!

-- Spec

data S = ClientInit [Int]
       | ClientDone [Int]
       | Server
       deriving Show

compute :: Alternative f => NodeID -> ([Int] -> Int) -> Protlet f S
compute server f = RPC "compute" clientSend serverStep
  where
    clientSend = \case
      ClientInit args ->
        Just (server, args, ClientDone)
      _ -> Nothing

    serverStep args Server = Just ([f args], Server)

initNetwork :: Alternative f => Network f S
initNetwork = initializeNetwork nodes protlets
  where
    addLabel, mulLabel, server :: NodeID
    addLabel = 0
    mulLabel = 1
    server = 0
    nodes :: [(NodeID, [(NodeID, S)])]
    nodes = [ (server, [(addLabel, Server), (mulLabel, Server)])
            , (1, [(addLabel, ClientInit [40, 2]), (mulLabel, ClientInit [5,8])])
            ]
    protlets :: Alternative f => [(NodeID, Protlet f S)]
    protlets = [(addLabel, compute server sum),
                (mulLabel, compute server product)]


--- Implementation

polynomialServer :: (MessagePassing m, Par m) => Label -> Label -> m a
polynomialServer addInstance mulInstance = par [loop mulInstance product, loop addInstance sum] undefined
  where    
    loop label f = do
      (_, _, args, client) <- spinReceive label ["compute__Request"]
      send (label, "compute__Response", [f args], client)
      loop label f

data Arith = Arith :+: Arith
           | Arith :*: Arith
           | ConstInt Int
           deriving (Eq)

instance Num Arith where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger = ConstInt . fromInteger
  negate e = e :*: (-1)
  abs = error "abs not implemented"
  signum = error "signum not implemented"

polynomialClient :: (MessagePassing m) => Label -> Label -> NodeID -> Arith -> m Int
polynomialClient addLabel mulLabel server = go
  where
    go (ConstInt n) = pure n
    go (l :+: r) = do
      l' <- go l
      r' <- go r
      [ans] <- rpcCall addLabel "compute" [l', r'] server 
      return ans
    go (l :*: r) = do
      l' <- go l
      r' <- go r
      [ans] <- rpcCall mulLabel "compute" [l', r'] server 
      return ans

initConf :: (Par m, MessagePassing m) => Configuration m Int
initConf = Configuration {
  _confNodes = [serverID,1,2],
  _confSoup = [],
  _confNodeStates = Map.fromList [
    (2, polynomialClient addLabel mulLabel serverID (40 + 3 * 4)),
    (1, polynomialClient addLabel mulLabel serverID (2 * 32 + 1 * 2 * 3)),
    (serverID, polynomialServer addLabel mulLabel) ]
  }
  where
    serverID = 0
    addLabel = 0
    mulLabel = 1