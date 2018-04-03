module NetSim.Examples.Database where

import NetSim.Language
import NetSim.Core
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad
import Data.Map (fromList)


dbServer :: MonadDiSeL m => [Label] -> m a
dbServer labels = par (oneCell 0 <$> labels) undefined
  where
    oneCell val label = do
      (_, tag, msg, client) <- spinReceive label ["Read__Request", "Write__Request"]
      val' <- case (tag, msg) of
        ("Read__Request", []) -> do
            send label "Read__Response" [val] client
            return val
        ("Write__Request", [v]) -> do
            send label "Write__Response" [1] client
            return v
      oneCell val' label

seconds :: Int -> Int
seconds = (* 1000000)

snapshotter :: [Label] -> NodeID -> Runner a
snapshotter instances server = do
    liftIO $ threadDelay (seconds 8)
    vs <- forM instances $ \lbl -> do
        [v] <- rpcCall lbl "Read" [] server
        return v
    liftIO $ print vs
    snapshotter instances server

clientIO :: (MonadDiSeL m, MonadIO m) => Label -> NodeID -> m a
clientIO lbl server = do
    [v] <- rpcCall lbl "Read" [] server
    liftIO $ putStr $ concat ["Cell[", show lbl, "] has value ", show v, "\nValue to write: " ]
    x <- liftIO $ read <$> getLine
    [1] <- rpcCall lbl "Write" [x] server
    clientIO lbl server

clientPredeterminedVals :: (MonadDiSeL m, MonadIO m) => Label -> NodeID -> [Int] -> m ()
clientPredeterminedVals _ _ [] = liftIO $ putStrLn "Done!"
clientPredeterminedVals lbl server (x:xs) = do
    [v] <- rpcCall lbl "Read" [] server
    liftIO $ putStrLn $ concat ["Cell[", show lbl, "] has value ", show v, "\nValue to write: ", show x]
    [1] <- rpcCall lbl "Write" [x] server
    clientPredeterminedVals lbl server xs


    
initConf :: Configuration Runner ()
initConf = Configuration {
    _confSoup = [],
    _confNodes = [0, 1],
    _confNodeStates = fromList [
        (serverID, dbServer instances),
        (1, clientIO 3 serverID),
        (2, snapshotter instances serverID)
    ]
}
  where
    serverID = 0
    instances = [0..5]