{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
module DPC.Types where

import           Data.Serialize      (Serialize)
import           GHC.Generics        (Generic)

import Data.Map as Map

type NodeID = Int

type Label = Int

data Message = Message {
  _msgFrom  :: NodeID,
  _msgTag   :: String,
  _msgBody  :: [Int],
  _msgTo    :: NodeID,
  _msgLabel :: Label
  }
  deriving (Eq, Show, Generic, Serialize)

data NetworkState global local = NetworkState {
  _globalState :: global,
  _localStates :: Map NodeID local
} deriving Show

nodes :: NetworkState g l -> [NodeID]
nodes = Map.keys . _localStates