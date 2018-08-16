
module NetSim.Examples.Calculator.Server where

import           NetSim.Core
import           NetSim.Examples.Calculator.Calculator
import           NetSim.Interpretations.WebSockets

import           System.Environment                    (getArgs)

main :: IO ()
main = do
    putStrLn "PolyServer Start"
    [ndPath] <- getArgs
    putStrLn $ "Getting network description from " ++ ndPath
    nd <- networkDescriptionFromFile ndPath
    putStrLn "Starting server..."
    defaultMain nd serverNodeID (polynomialServer addInstance mulInstance)
  where
    serverNodeID :: NodeID
    addInstance, mulInstance :: Label
    serverNodeID = 1
    addInstance = 0
    mulInstance = 1
