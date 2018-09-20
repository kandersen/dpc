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

initNetwork :: Alternative f => Network f S
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

initConf :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Configuration m Int
initConf = Configuration {
  _confNodes = [serverID,1],
  _confSoup = [],
  _confNodeStates = Map.fromList [
    (clientID, polynomialClient addLabel mulLabel serverID (1 + 1)),
    (serverID, polynomialServer addLabel mulLabel) ]
  }
  where
    clientID = 1
    serverID = 0
    addLabel = 0
    mulLabel = 1

-- (fst <$> (take 7 $ runPure (NetSim.Examples.Calculator.Calculator.initConf :: Configuration (DiSeL (NetSim.Examples.Calculator.Calculator.S, Message -> NetSim.Examples.Calculator.Calculator.S)) Int)))
-- modelcheckExecutionTrace (NetSim.Examples.Calculator.Calculator.initNetwork) (fst <$> (take 2 $ runPure (NetSim.Examples.Calculator.Calculator.initConf :: Configuration (DiSeL (NetSim.Examples.Calculator.Calculator.S, Message -> NetSim.Examples.Calculator.Calculator.S)) Int)))