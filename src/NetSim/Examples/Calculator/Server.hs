
module NetSim.Examples.Calculator.Server where

import           NetSim.Types
import           NetSim.Examples.Calculator.Calculator
import           NetSim.Interpretations.WebSockets

import           System.Environment                    (getArgs)
import Control.Monad (void)

main :: IO ()
main = do
    putStrLn "PolyServer Start"
    [ndPath] <- getArgs
    putStrLn $ "Getting network description from " ++ ndPath
    putStrLn "Starting server..."
    runP2P ndPath serverNodeID (void $ polynomialServer addInstance mulInstance)
  where
    serverNodeID :: NodeID
    addInstance, mulInstance :: Label
    serverNodeID = 1
    addInstance = 0
    mulInstance = 1
