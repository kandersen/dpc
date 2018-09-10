{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RecordWildCards     #-}
module NetSim.Examples.Calculator.Calculator where

import           NetSim.Core
import           NetSim.Language

import qualified Data.Map        as Map

-- Example to demonstrate concurrency!

-- Spec

data S = ClientInit [Int]
       | ClientDone [Int]
       | Server
       deriving (Show, Eq)

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
            , (1, [(addLabel, ClientInit [1, 1])])
            ]
    protlets :: Alternative f => [(NodeID, Protlet f S)]
    protlets = [(addLabel, compute server sum),
                (mulLabel, compute server product)]


--- Implementation

polynomialServer :: (MessagePassing (S, Message -> S) m) => Label -> Label -> m a
polynomialServer addInstance mulInstance = loop
  where
    loop :: (MessagePassing (S, Message -> S) m) => m a
    loop = do
      Message client _ args _ lbl <- spinReceive [((Server, \Message{} -> Server), addInstance, "compute__Request"), ((Server, const Server), mulInstance, "compute__Request")]
      let response = if | lbl == addInstance -> sum args
                        | lbl == mulInstance -> product args
      send (Server, \Message{} -> Server) client lbl "compute__Response" [response]
      loop

parPolynomialServer :: (MessagePassing () m, Par m) => Label -> Label -> m a
parPolynomialServer addInstance mulInstance = par [loop mulInstance product, loop addInstance sum] undefined
  where
    loop label f = do
      Message client _ args _ _ <- spinReceive [((), label, "compute__Request")]
      send () client label "compute__Response" [f args]
      loop label f

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

polynomialClient :: (MessagePassing (S, Message -> S) m) => Label -> Label -> NodeID -> Arith -> m Int
polynomialClient addLabel mulLabel server = go
  where
    go :: (MessagePassing (S, Message -> S) m) => Arith -> m Int
    go (ConstInt n) = pure n
    go (l :+: r) = do
      l' <- go l
      r' <- go r
      [ans] <- rpcCall (ClientInit [eval l, eval r], \Message{..} -> ClientDone _msgBody) addLabel "compute" [l', r'] server
      return ans
    go (l :*: r) = do
      l' <- go l
      r' <- go r
      [ans] <- rpcCall (ClientInit [eval l, eval r], \Message{..} -> ClientDone _msgBody) mulLabel "compute" [l', r'] server
      return ans

initConf :: (MessagePassing (S, Message -> S) m) => Configuration m Int
initConf = Configuration {
  _confNodes = [serverID,1],
  _confSoup = [],
  _confNodeStates = Map.fromList [
--    (2, polynomialClient addLabel mulLabel serverID (40 + 3 * 4)),
    (1, polynomialClient addLabel mulLabel serverID (1 + 1)),
    (serverID, polynomialServer addLabel mulLabel) ]
  }
  where
    serverID, addLabel, mulLabel :: NodeID
    serverID = 0
    addLabel = 0
    mulLabel = 1

-- (fst <$> (take 7 $ runPure (NetSim.Examples.Calculator.Calculator.initConf :: Configuration (DiSeL (NetSim.Examples.Calculator.Calculator.S, Message -> NetSim.Examples.Calculator.Calculator.S)) Int)))
-- modelcheckExecutionTrace (NetSim.Examples.Calculator.Calculator.initNetwork) (fst <$> (take 2 $ runPure (NetSim.Examples.Calculator.Calculator.initConf :: Configuration (DiSeL (NetSim.Examples.Calculator.Calculator.S, Message -> NetSim.Examples.Calculator.Calculator.S)) Int)))