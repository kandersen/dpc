{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
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
    addLabel = 0
    mulLabel = 1
    server = 0
    nodes = [ (server, [(addLabel, Server), (mulLabel, Server)])
            , (1, [(addLabel, ClientInit [40, 2]), (mulLabel, ClientInit [5,8])])
            ]
    protlets = [(addLabel, compute server sum),
                (mulLabel, compute server product)]


--- Implementation

polynomialServer :: (MonadDiSeL m) => Label -> Label -> m a
polynomialServer addInstance mulInstance = par [processAdd, processMul] (\_ -> polynomialServer addInstance mulInstance)
  where
    processAdd = do
      (_, _, args, client) <- spinReceive addInstance ["compute__Request"]
      send addInstance "computer__Response" (return $ sum args) client
      processAdd

    processMul = do
      (_, _, args, client) <- spinReceive mulInstance ["compute__Request"]
      send mulInstance "computerMul__Response" (return $ product args) client
      processMul

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

ex1 :: Arith
ex1 = (5 * 2 + 8) * (-4) + 8

exSumOfFirstNNats :: (Num p, Eq p) => p -> p
exSumOfFirstNNats 0 = 0
exSumOfFirstNNats n = n + exSumOfFirstNNats (n - 1)

exTwiceSumOfFirstNNats :: Num a => a -> a
exTwiceSumOfFirstNNats n = (n * n) + n 

polynomialClient :: (MonadDiSeL m) => Label -> Label -> NodeID -> Arith -> m Int
polynomialClient addLabel mulLabel server e = go e
  where
    go (ConstInt n) = return n
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

initConf :: MonadDiSeL m => Configuration m Int
initConf = Configuration {
  _confNodes = [0, 1],
  _confSoup = [],
  _confNodeStates = Map.fromList [
    (0, polynomialServer addLabel mulLabel),
    (1, polynomialClient addLabel mulLabel 0 (2 * 2))]
--    (2, polynomialClient 0 (exTwiceSumOfFirstNNats 0))
  }
  where
    addLabel = 0
    mulLabel = 1