{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module NetSim.Examples.Calculator.Calculator where

import NetSim.Types
import           NetSim.Specifications
import           NetSim.Language

import qualified Data.Map        as Map
import Data.Map (Map)

-- Example to demonstrate concurrency!

-- Spec

data S = ClientInit [Int]
       | ClientDone [Int]
       | Server
       deriving (Show, Eq)

compute :: Alternative f => NodeID -> ([Int] -> Int) -> Protlet f S
compute server f = RPC "compute" clientStep serverStep
  where
    clientStep = \case
      ClientInit args ->
        Just (server, args, ClientDone)
      _ -> Nothing

    serverStep args Server = Just ([f args], Server)

initStates :: Map NodeID S
initStates = Map.fromList [(0, Server), (1, ClientInit [1,1]), (2, ClientInit [3,2])]

initNetwork :: Alternative f => SpecNetwork f S
initNetwork = initializeNetwork nodes protlets
  where
    addLabel, mulLabel, server :: NodeID
    addLabel = 0
    mulLabel = 1
    server = 0
    nodes = [ (server, [(addLabel, Server), (mulLabel, Server)])
            , (1, [(addLabel, ClientInit [1, 1])])
            ]
    protlets = [(addLabel, compute server sum),
                (mulLabel, compute server product)]

simpleNetwork :: Alternative f => SpecNetwork f S                
simpleNetwork = initializeNetwork nodes protlets
  where
    server, client1, client2 :: NodeID
    server = 0
    client1 = 1
    client2 = 2
    nodes = [ (server, [(0, Server)])
            , (client1, [(0, ClientInit [1, 1])])
            , (client2, [(0, ClientInit [3, 2])])
            ]

    protlets = [(0, compute server sum)]

--- Implementation
addServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Label -> m a
addServer label = loop
  where
    loop = do
      nodeID <- this
      enactingServer (compute nodeID sum) $ do
        Message client _ args _ _ <- spinReceive [(label, "compute__Request")]
        send client label "compute__Response" [sum args]
      loop

mulServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Label -> m a
mulServer label = loop
  where
    loop = do
      nodeID <- this
      enactingServer (compute nodeID product) $ do
        Message client _ args _ _ <- spinReceive [(label, "compute__Request")]
        send client label "compute__Response" [product args]
      loop

polynomialServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Label -> Label -> m a
polynomialServer addLabel mulLabel = loop
  where
    loop = do
      nodeID <- this
      enactingServer (OneOf [compute nodeID product, compute nodeID sum]) $ do
        Message client _ args _ msgLabel <- spinReceive [(addLabel, "compute__Request"), (mulLabel, "compute__Request")]
        let response = if | msgLabel == addLabel -> sum args
                          | msgLabel == mulLabel -> product args
        send client msgLabel "compute__Response" [response]
      loop

parPolynomialServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m, Par m) => Label -> Label -> m a
parPolynomialServer addLabel mulLabel = do
  nodeID <- this
  enactingServer (OneOf [compute nodeID product, compute nodeID sum]) $ 
    par [mulServer mulLabel, addServer addLabel] undefined

data Arith = Arith :+: Arith
           | Arith :*: Arith
           | ConstInt Int
           deriving (Eq, Read, Show)

eval :: Arith -> Int
eval (l :+: r) = eval l + eval r
eval (l :*: r) = eval l * eval r
eval (ConstInt n) = n

instance Num Arith where
  (+) = (:+:)
  (*) = (:*:)
  fromInteger = ConstInt . fromInteger
  negate e = e :*: (-1)
  abs = error "abs not implemented"
  signum = error "signum not implemented"

polynomialClient :: (ProtletAnnotations S m, MessagePassing m) => Label -> Label -> NodeID -> Arith -> m Int
polynomialClient addLabel mulLabel server = go
  where
    go (ConstInt n) = pure n
    go (l :+: r) = do
      l' <- go l
      r' <- go r
      [ans] <- enactingClient (compute server sum) $ rpcCall addLabel "compute" [l', r'] server
      return ans
    go (l :*: r) = do
      l' <- go l
      r' <- go r
      [ans] <- enactingClient (compute server product) $ rpcCall mulLabel "compute" [l', r'] server
      return ans

addClient :: (ProtletAnnotations S m, MessagePassing m) => Int -> Int -> NodeID -> m Int
addClient a b server = do
  [ans] <- enactingClient (compute server sum) $ 
    rpcCall 0 "compute" [a, b] server
  return ans

initConf :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => ImplNetwork m Int
initConf = NetworkState {
  _globalState = [],
  _localStates = Map.fromList [
    (clientID, polynomialClient addLabel mulLabel serverID (1 + 1)),
    (serverID, polynomialServer addLabel mulLabel) ]
  }
  where
    clientID = 1
    serverID = 0
    addLabel = 0
    mulLabel = 1

simpleConf :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => ImplNetwork m Int
simpleConf = NetworkState {
  _globalState = [],
  _localStates = Map.fromList [
          (client2, addClient 3 2 server)
        , (client1, addClient 1 1 server)
        , (server, addServer 0) 
        ]
  }
  where
    client2 = 2
    client1 = 1
    server = 0