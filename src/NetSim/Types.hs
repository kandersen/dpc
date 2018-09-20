{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE DeriveAnyClass   #-}
module NetSim.Types where

import           Data.Serialize      (Serialize)
import           GHC.Generics        (Generic)

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