{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DPC.Examples.Calculator.Calculator where

import DPC.Types
import DPC.Specifications
import DPC.Language

import qualified Data.Map        as Map
import Data.Map (Map)

-- Spec

data S = ClientInit NodeID [Int]
       | ClientDone [Int]
       | ServerReady
       deriving (Show, Eq)

compute :: Alternative f => ([Int] -> Int) -> Protlet f S
compute f = RPC "compute" clientStep serverStep
  where
    clientStep = \case
      ClientInit server args ->
        Just (server, args, ClientDone)
      _ -> Nothing

    serverStep args = \case
      ServerReady -> Just ([f args], ServerReady)
      _ -> Nothing

initStates :: Map NodeID S
initStates = Map.fromList [(0, ServerReady), (1, ClientInit 0 [1,1])]

initNetwork :: Alternative f => SpecNetwork f S
initNetwork = initializeNetwork nodeStates protlets
  where
    addLabel, mulLabel, server :: NodeID
    addLabel = 0
    mulLabel = 1
    server = 0
    client = 1
    nodeStates = [ (server, [(addLabel, ServerReady), (mulLabel, ServerReady)])
                 , (client, [(addLabel, ClientInit server [1, 1])])
                 ]
    protlets = [(addLabel, [compute sum]),
                (mulLabel, [compute product])]

simpleNetwork :: Alternative f => SpecNetwork f S                
simpleNetwork = initializeNetwork nodeStates protlets
  where
    server, client1, client2 :: NodeID
    server = 0
    client1 = 1
    client2 = 2
    nodeStates = [ (server, [(0, ServerReady)])
                 , (client1, [(0, ClientInit server [1, 1])])
                 , (client2, [(0, ClientInit server [3, 2])])
                 ]

    protlets = [(0, [compute sum])]

addNetwork :: Alternative f => SpecNetwork f S
addNetwork = initializeNetwork nodeStates protlets
  where
    nodeStates = [ (0, [(0, ServerReady)])
                 , (1, [(0, ClientInit 0 [3, 20])])
                  ]
    protlets = [(0, [compute sum])]
    

--- Implementation
addServer' :: (MessagePassing m) => Label -> m a
addServer' label = do
  Message client _ args _ _ <- spinReceive [(label, "compute__Request")]
  send client label "compute__Response" [sum args]
  addServer' label

addServer :: (ProtletAnnotations S m, MessagePassing m) => Label -> m a
addServer label = loop
  where
    loop :: (ProtletAnnotations S m, MessagePassing m) => m a
    loop = do
      enactingServer (compute sum) $ do
        Message client _ args _ _ <- spinReceive [(label, "compute__Request")];
        send client label "compute__Response" [sum args]    
      loop

mulServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Label -> m a
mulServer label = loop
  where
    loop :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => m a
    loop = do
      enactingServer (compute product) $ do
        Message client _ args _ _ <- spinReceive [(label, "compute__Request")]
        send client label "compute__Response" [product args]
      loop

polynomialServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => Label -> Label -> m a
polynomialServer addLabel mulLabel = loop
  where
    loop :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => m a
    loop = do
      enactingServer (OneOf [compute product, compute sum]) $ do
        Message client _ args _ msgLabel <- spinReceive [(addLabel, "compute__Request"), (mulLabel, "compute__Request")]
        let response = if | msgLabel == addLabel -> sum args
                          | msgLabel == mulLabel -> product args
        send client msgLabel "compute__Response" [response]
      loop

parPolynomialServer :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m, Par m) => Label -> Label -> m a
parPolynomialServer addLabel mulLabel =
  enactingServer (OneOf [compute product, compute sum]) $ 
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
      [ans] <- enactingClient (compute sum) $ rpcCall addLabel "compute" [l', r'] server
      return ans
    go (l :*: r) = do
      l' <- go l
      r' <- go r
      [ans] <- enactingClient (compute product) $ rpcCall mulLabel "compute" [l', r'] server
      return ans

addClient :: (ProtletAnnotations S m, MessagePassing m) => Label -> Int -> Int -> NodeID -> m Int
addClient label a b server = do
  [ans] <- enactingClient (compute sum) $
    rpcCall label "compute" [a, b] server
  return ans

addClient' :: MessagePassing m => Label -> Int -> Int -> NodeID -> m Int
addClient' label a b server = do
  [ans] <- rpcCall label "compute" [a, b] server
  return ans

initConf :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => ImplNetwork m Int
initConf = initializeImplNetwork [
    (clientID, polynomialClient addLabel mulLabel serverID (1 + 1)),
    (serverID, polynomialServer addLabel mulLabel) ]
  where
    clientID, serverID :: NodeID
    clientID = 1
    serverID = 0
    addLabel, mulLabel :: Label
    addLabel = 0
    mulLabel = 1

addConf :: (ProtletAnnotations S m, MessagePassing m) => ImplNetwork m Int
addConf = initializeImplNetwork [
    (1, addClient 0 3 20 0)
  , (0, addServer 0) 
  ]

simpleConf :: (ProtletAnnotations S m, MessagePassing m, NetworkNode m) => ImplNetwork m Int
simpleConf = initializeImplNetwork [
          (client2, addClient 0 3 2 server)
        , (client1, addClient 0 1 1 server)
        , (server, addServer 0) 
        ]
  where
    server, client1, client2 :: NodeID
    client2 = 2
    client1 = 1
    server = 0