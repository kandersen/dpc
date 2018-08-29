{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module NetSim.Examples.Paxos where

import           NetSim.Core
import           NetSim.Language

import           Control.Monad   (forM_)
import qualified Data.Map        as Map
import           Data.Maybe      (fromMaybe)

-- Round-Based Register

readRBR :: MessagePassing () m => Label -> [NodeID] -> Int -> m (Bool, Maybe Int)
readRBR lbl participants r = do
  forM_ participants $ \pt -> send () pt lbl "Read__Request" [r]
  spinForResponses 0 Nothing []
  where
    n :: Double
    n = fromIntegral $ length participants

    isDone :: [NodeID] -> Bool
    isDone q = length q == ceiling ((n + 1.0) / 2.0)

    updateV :: Int -> Maybe Int -> Maybe Int
    updateV v = maybe (Just v) (const $ Just v)

    spinForResponses maxKW maxV q =
      if isDone q
        then return (True, maxV)
        else do
          Message sender _ body _ _ <- spinReceive [((), lbl, "Read__Response")]
          case body of
            [1, k, 0, kW] | k == r ->
              if kW >= maxKW
                then spinForResponses kW Nothing (sender : q)
                else spinForResponses maxKW maxV (sender : q)
            [1, k, 1, v, kW] | k == r ->
                if kW >= maxKW
                    then spinForResponses kW (updateV v maxV) (sender : q)
                    else spinForResponses maxKW maxV (sender : q)
            [0, k] | k == r -> return (False, Nothing)
            _ -> spinForResponses maxKW maxV q


writeRBR :: MessagePassing () m => Label -> [NodeID] -> Int -> Int -> m Bool
writeRBR lbl participants r vW = do
  forM_ participants $ \pt -> send () pt lbl "Write__Request" [r, vW]
  spinForResponses []
  where
    n :: Double
    n = fromIntegral $ length participants

    spinForResponses q = do
      Message sender _ body _ _ <- spinReceive [((), lbl, "Write__Response")]
      case body of
        [1, k] | k == r ->
            if length q == ceiling ((n + 1.0) / 2.0)
              then return True
              else spinForResponses (sender : q)
        [0, k] | k == r -> return False
        _ -> spinForResponses q

acceptor :: MessagePassing () m => Label -> m a
acceptor lbl = go Nothing 0 0
  where
    go mv r w = do
      Message sender tag body _ _ <- spinReceive [((), lbl, "Read__Request"), ((), lbl, "Write__Request")]
      case (tag, body) of
        ("Read__Request", [k]) ->
            if k < r
              then do
                send () sender lbl "Read__Response" [0, k]
                go mv r w
              else do
                let msg = case mv of
                            Nothing -> [1, k, 0, w]
                            Just v  -> [1, k, 1, v, w]
                send () sender lbl "Read__Response" msg
                go mv k w
        ("Write__Request", [k, vW]) ->
            if k < r
                then do
                  send () sender lbl "Write__Response" [0, k]
                  go mv r w
                else do
                  send () sender lbl "Write__Response" [1, k]
                  go (Just vW) k k
        _ -> error $ "Illformed request " ++ tag ++ ": " ++ show body

proposeRC :: MessagePassing () m =>
  Label -> [NodeID] -> Int -> Int -> m (Bool, Maybe Int)
proposeRC lbl participants r v0 = do
  (resR, mv) <- readRBR lbl participants r
  let v = fromMaybe v0 mv
  if resR
    then do
      resW <- writeRBR lbl participants r v
      if resW
        then return (True, Just v)
        else return (False, Nothing)
    else return (False, Nothing)

proposeP :: (NetworkNode m, MessagePassing () m) => Label -> [NodeID] -> Int -> m Int
proposeP lbl participants v0 = do
    k <- this 
    loopTillSucceed k
  where
    loopTillSucceed k = do
      (res, v) <- proposeRC lbl participants k v0
      if res
        then return (fromMaybe (error "Shouldn't happen!") v)
        else loopTillSucceed (k + length participants)


initConf :: (NetworkNode m, MessagePassing () m) => Configuration m Int
initConf = Configuration {
    _confNodes = [0..2],
    _confSoup = [],
    _confNodeStates = Map.fromList [
          (0, proposeP 0 [1, 2] 42)
        , (1, acceptor 0)
        , (2, acceptor 0)
      ]
  }
