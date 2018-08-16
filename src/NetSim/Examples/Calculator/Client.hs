{-# LANGUAGE ScopedTypeVariables #-}
module NetSim.Examples.Calculator.Client where

import           NetSim.Core
import           NetSim.Examples.Calculator.Calculator
import           NetSim.Interpretations.WebSockets

import           System.Environment                    (getArgs)

import           Control.Monad
import           Control.Monad.IO.Class

import           System.IO

clientWrapper :: SocketRunner ()
clientWrapper = do
  liftIO . putStrLn $ "Enter arithmetic expression:"
  ae :: Arith <- read <$> liftIO getLine
  res <- polynomialClient addInstance mulInstance serverNodeID ae
  liftIO $ print res
  where
    addInstance, mulInstance :: Label
    addInstance = 0
    mulInstance = 1

    serverNodeID :: NodeID
    serverNodeID = 1


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin NoBuffering
  putStrLn "PolyClient Start"
  [ndPath] <- getArgs
  putStrLn $ "Getting network description from " ++ ndPath
  nd <- networkDescriptionFromFile ndPath
  putStrLn "Starting client..."
  defaultMain nd clientNodeID (forever clientWrapper)
  where
    clientNodeID :: NodeID
    clientNodeID = 2
