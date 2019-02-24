{-# LANGUAGE GADTs #-}
module NetSim.Examples.Raft where

import NetSim.Types
import NetSim.Specifications

data S = Actor {
  -- Persistent State
  _currentTerm :: Int,
  _votedFor :: Maybe NodeID,
  _log :: [Int],
  -- Volatile State
  _comittedIndex :: Int,
  _lastApplied :: Int,
  -- Volatile State on Leaders
  _nextIndex :: [Int],
  _matchIndex :: [Int]
  }

  | Client NodeID Int
  | ClientDone

data Mode = Follower
          | Candidate
          | Leader

{-
      [0]                     [1]                       [2]
    ------------> Follower --------------> Candidate --------------> Leader
                  ^  ^                       |                        |
                  |  |         [3]           |                        |
                  |  |_______________________|                        |
                  |                                                   |
                  |                                 [4]               |
                  |___________________________________________________|

    - [0] Starts up | Recovers
    - [1] Times out | Starts election
    - [2] Receives votes from majority of servers and becomes leader
    - [3] Discovers leader of new term | Discovers candidate with a higher term
    - [4] Discovers server with higher term
-}

perform :: Alternative f => Protlet f S
perform = ARPC "perform" clientStep serverRec serverSend
  where
    clientStep s = case s of
      Client server n -> Just ([n], server, const ClientDone)
      _ -> Nothing
    
    serverRec msg s = case s of
      

    serverSend = undefined