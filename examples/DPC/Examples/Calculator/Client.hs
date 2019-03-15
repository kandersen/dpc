{-# LANGUAGE ScopedTypeVariables #-}
module DPC.Examples.Calculator.Client where

import DPC.Types
import DPC.Examples.Calculator.Calculator
import DPC.Interpretations.WebSockets

import System.Environment                    (getArgs)
import System.IO

import Control.Monad
import Control.Monad.IO.Class

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
  putStrLn "Starting client..."
  runP2P ndPath clientNodeID (void $ forever clientWrapper)
    where
      clientNodeID :: NodeID
      clientNodeID = 2