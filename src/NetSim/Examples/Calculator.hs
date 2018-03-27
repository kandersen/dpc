{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module NetSim.Examples.Calculator where

import NetSim.Core
import NetSim.Language

import qualified Data.Map as Map

-- Example to demonstrate concurrency!

data S = ClientCompute String [Int]
       | ClientReceive String [Int]
       | Server [(NodeID, [Int], String)]

snoc :: t -> [t] -> [t]
snoc a [] = [a]
snoc a (x:xs) = x : snoc a xs

compute :: Alternative f => NodeID -> String -> ([Int] -> [Int]) -> Protlet f S
compute server name f = ARPC ("compute" ++ name) clientSend serverReceive serverSend
  where
    clientSend = \case
      ClientCompute func args | func == name ->
        Just (server, args, clientReceive func)
      _ -> Nothing

    clientReceive func ans = 
        ClientReceive func ans

    serverReceive Message{..} (Server q) = Just . Server $ snoc (_msgFrom, _msgBody, name) q
      
    serverSend _ (Server q) = case q of
        (client, args, func) : q' | func == name -> 
            pure (buildResponse client (f args), Server q')
        _ -> empty
    
    buildResponse client body = Message {
        _msgFrom = server,
        _msgBody = body,
        _msgTag = concat ["compute", name, "__Response"],
        _msgTo = client
      }

polynomialServer :: (MonadDiSeL m) => m a
polynomialServer = par [processAdd, processMul] (\_ -> polynomialServer)
  where
    processAdd = do
      (_, args, client) <- spinReceive ["computeAdd__Request"]
      send "computerAdd__Response" (return $ sum args) client
      processAdd

    processMul = do
      (_, args, client) <- spinReceive ["computeMul__Request"]
      send "computerMul__Response" (return $ product args) client
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

ex1 :: Arith
ex1 = (5 * 2 + 8) * (-4) + 8

exSumOfFirstNNats 0 = 0
exSumOfFirstNNats n = n + exSumOfFirstNNats (n - 1)

exTwiceSumOfFirstNNats n = (n * n) + n 

polynomialClient :: (MonadDiSeL m) => NodeID -> Arith -> m Int
polynomialClient server e = go e
  where
    go (ConstInt n) = return n
    go (l :+: r) = do
      l' <- go l
      r' <- go r
      [ans] <- rpcCall "computeAdd" [l', r'] server 
      return ans
    go (l :*: r) = do
        l' <- go l
        r' <- go r
        [ans] <- rpcCall "computeMul" [l', r'] server 
        return ans
        
initConf :: MonadDiSeL m => Configuration m Int
initConf = Configuration {
  _confNodes = [0, 1],
  _confSoup = [],
  _confNodeStates = Map.fromList [
    (0, polynomialServer),
    (1, polynomialClient 0 (2 * 2))]
--    (2, polynomialClient 0 (exTwiceSumOfFirstNNats 0))
    
}