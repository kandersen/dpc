module NetSim.TwoPhaseCommit where

import NetSim.Core
import Data.Map as Map

data State = CoordinatorInit [NodeID]
           | ParticipantInit
           deriving Show

prepare :: Protlet State
prepare = Broadcast "Prepare" coordinator participantReceive participantSend
  where
    coordinator = undefined
    participantReceive = undefined
    participantSend = undefined
