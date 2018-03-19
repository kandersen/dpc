module NetSim.Paxos where

import NetSim.Language
import NetSim.Core

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)

-- Round-Based Register

readRBR :: MonadDiSeL m => [NodeID] -> Int -> m (Bool, Maybe Int)
readRBR participants r = do
  forM_ participants $ send "Read__Request" [r]
  spinForResponses 0 minBound []
  where
    n :: Double
    n = fromIntegral $ length participants

    isDone :: [NodeID] -> Bool
    isDone q = length q == ceiling ((n + 1.0) / 2.0)

    spinForResponses maxKW maxV q = 
      if isDone q
        then return (True, Just maxV)
        else do
          (_, body, sender) <- spinReceive ["Read__Response"]
          case body of
            [1, k, v, kW] | k == r -> 
                if kW >= maxKW 
                    then spinForResponses kW v (sender : q)
                    else spinForResponses maxKW maxV (sender : q)
            [0, k] | k == r -> return (False, Nothing)
            _ -> spinForResponses maxKW maxV q
    

writeRBR :: MonadDiSeL m => [NodeID] -> Int -> Int -> m Bool
writeRBR participants r vW = do
  forM_ participants $ send "Write__Request" [r, vW]
  spinForResponses []
  where
    n :: Double
    n = fromIntegral $ length participants 

    spinForResponses q = do
      (_, body, sender) <- spinReceive ["Write__Response"]
      case body of
        [1, k] | k == r -> 
            if length q == ceiling ((n + 1.0) / 2.0)
              then return True
              else spinForResponses (sender : q)
        [0, k] | k == r -> return False
        _ -> spinForResponses q

acceptor :: MonadDiSeL m => m a
acceptor = go minBound 0 0
  where
    go v r w = do
      (tag, body, sender) <- spinReceive ["Read__Request", "Write__Request"]
      case (tag, body) of
        ("Read__Request", [k]) -> 
            if k < r
              then do 
                send "Read__Response" [0, k] sender
                go v r w
              else do
                send "Read__Response" [1, k, v, w] sender
                go v k w
        ("Write__Request", [k, vW]) ->
            if k < r
                then do
                  send "Write__Response" [0, k] sender
                  go v r w
                else do
                  send "Write__Response" [1, k] sender
                  go vW k k

proposeRC :: MonadDiSeL m => 
  [NodeID] -> Int -> Int -> m (Bool, Maybe Int)
proposeRC participants r v0 = do
  (resR, mv) <- readRBR participants r
  let v = fromMaybe v0 mv
  if resR 
    then do 
      resW <- writeRBR participants r v
      if resW 
        then return (True, Just v)
        else return (False, Nothing)
    else return (False, Nothing)

proposeP :: MonadDiSeL m => [NodeID] -> Int -> m Int
proposeP participants v0 = do
    k <- this
    loopTillSucceed k
  where
    loopTillSucceed k = do
      (res, v) <- proposeRC participants k v0
      if res 
        then return (fromMaybe (error "Shouldn't happen!") v)
        else loopTillSucceed (k + length participants) 
