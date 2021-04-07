{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DPC.Examples.Raft where

import           DPC.Types
import           DPC.Specifications
import           DPC.Language
import           DPC.Util

import           Control.Monad   (forM_)
import           Control.Monad.State
import           Data.Maybe      (fromMaybe)
import           Data.Ratio
import           Control.Monad.Trans

type LeaderNode = NodeID
type OtherNode = NodeID
type Log = [Int]
type Timeout = Int
type Term = Int
type Value = Int

data ActorState = ActorState {
  -- perisistent state
  _id :: NodeID,
  _currentTerm :: Term,
  _votedFor :: Maybe NodeID,
  _log :: [Int],
  -- volatile state
  _lastReplicated :: Maybe Int,
  -- volatile state on leaders
  _toCommit :: [Int]
} deriving Show

data RaftState = Leader [OtherNode] ActorState
  | LeaderReplicate [OtherNode] ActorState Value
  | LeaderCommit [OtherNode] ActorState
  | Follower LeaderNode [OtherNode] ActorState
  | FollowerVote LeaderNode [OtherNode] ActorState
  | FollowerCommit LeaderNode [OtherNode] ActorState Log
  | Candidate [OtherNode] ActorState
  deriving Show

-- Term and Int (0 for false, 1 for true) are added
-- to the same list in the message body since
-- list is homogenous Bool cannot be used
data Response = RequestVotes NodeID Term Int
              | AppendEntries NodeID Term [Int]
              | Vote NodeID Term Int

respond :: Label -> NodeID -> Response -> Message
respond label from (RequestVotes to term answer) = Message {
  _msgTo = to,
  _msgBody = [term, answer],
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}
respond label from (AppendEntries to term values) = Message {
  _msgTo = to,
  _msgBody = term : values,
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}
respond label from (Vote to term answer) = Message {
  _msgTo = to,
  _msgBody = [term, answer],
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}

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

logReplicate :: Alternative f => Label -> Int -> Protlet f RaftState
logReplicate label n = Quorum "logReplicate" (quorumSize n) makeReplicationRequest receiveReplicationRequest respondToReplicationRequest
  where
    makeReplicationRequest :: RaftState -> Maybe ([(NodeID, [Int])], [(NodeID, [Int])] -> RaftState)
    makeReplicationRequest = \case
      LeaderReplicate followers state@ActorState{..} value ->
        Just (zip followers (repeat [value]), receiveVote followers state value)
      _ -> empty

    -- decide next state based on the number of votes received
    -- in reponse body integer 1 indicates accepted vote
    receiveVote :: [OtherNode] -> ActorState -> Value -> [(NodeID , [Int])] -> RaftState
    receiveVote followers state@ActorState{..} value responses =
      if (fromIntegral . length . filter (\(nodeid, (term:value:values)) -> value == 1) $ responses) >= (quorumSize n)
      then LeaderCommit followers state {_lastReplicated = Just value, _toCommit = [value]}
      else Leader followers state

    quorumSize :: Int -> Rational
    quorumSize n = ((fromIntegral n % 2) + 1)

    receiveReplicationRequest :: Receive RaftState
    receiveReplicationRequest msg = \case
      Follower leader others state -> Just (FollowerVote leader others state)
      _ -> empty

    respondToReplicationRequest :: Alternative f => Send f RaftState
    respondToReplicationRequest self = \case
      FollowerVote leader others state -> 
        (\(vote, otherVotes) -> (respond label self (Vote leader (_currentTerm state) vote), Follower leader others state)) <$> oneOf
        [
          0 -- vote against
        , 1 -- vote for
        ]
      _ -> empty

commit :: Alternative f => Label -> Protlet f RaftState
commit label = Broadcast "commit" sendCommit receiveCommit replyToCommit
  where
    sendCommit :: RaftState -> Maybe ([(NodeID, [Int])], [(NodeID, [Int])] -> RaftState)
    sendCommit = \case
      LeaderCommit followers state@ActorState{_currentTerm = currentTerm, _toCommit = toCommit, _log = log} ->
        Just (zip followers (repeat $ currentTerm:toCommit), receiveCommitResponse followers state { _log = toCommit ++ log})
      _ -> empty

    -- POSSIBLE IMPROVEMENT can use check term value in response to change self term value
    receiveCommitResponse :: [OtherNode] -> ActorState -> [(NodeID , [Int])] -> RaftState
    receiveCommitResponse followers state responses = const (Leader followers state) responses

    receiveCommit :: Receive RaftState
    receiveCommit msg = \case
      Follower leader others state -> Just $ FollowerCommit leader others state (tail . _msgBody $ msg)
      _ -> empty

    replyToCommit :: Alternative f => Send f RaftState
    replyToCommit self = \case
      FollowerCommit leader others state@ActorState{ _currentTerm = currentTerm, _log = log } toCommit ->
        pure (respond label self (Vote leader currentTerm 1), Follower leader others state {_log = toCommit ++ log}) -- accept commited values and add to log
        <|> pure (respond label self (Vote leader currentTerm 0), Follower leader others state ) -- reject commited values
      _ -> empty

-- Candidate starts reelection by sending out new term
-- value to all other nodes. It then receives votes from
-- other nodes + 1 for it's own. If this is greater than majority
-- then it becomes leader else it stays a candidate
reElection :: Alternative f => Label -> Int -> Protlet f RaftState
reElection label n = Quorum "reElection" (quorumSize n) startElection receiveElectionRequest sendElectionVote 
  where
    startElection :: Broadcast RaftState
    startElection = \case
      Candidate others state@ActorState{ _currentTerm = newTerm } ->
        Just (zip others (repeat [newTerm]), receiveVote others state)
      _ -> empty

    -- decide next state based on the number of votes received
    -- in reponse body integer 1 indicates accepted vote
    -- if gets majority vote becomes leader
    -- else stays follower
    receiveVote :: [OtherNode] -> ActorState -> [(NodeID, [Int])] -> RaftState
    receiveVote followers state responses =
      -- count votes and self vote
      if 1 + (fromIntegral . length . filter (\(nodeid, [term,value]) -> value == 1) $ responses) >= (quorumSize n)
      then Leader followers state
      else Candidate followers state

    quorumSize :: Int -> Rational
    quorumSize n = ((fromIntegral n % 2) + 1)

    -- POSSIBLE IMPROVEMENT add checks to only make transition if election
    -- term is higher than current term in state
    receiveElectionRequest :: Receive RaftState
    receiveElectionRequest msg@Message { _msgFrom = candidateID, _msgBody = [newTerm] } = \case
      Follower leader others state -> Just (FollowerVote leader others state { _currentTerm = newTerm })
      Leader others state -> Just (FollowerVote candidateID others state { _currentTerm = newTerm })  -- current leader becomes follower
      _ -> empty

    sendElectionVote :: Alternative f => Send f RaftState
    sendElectionVote self = \case
      FollowerVote leader others state@ActorState { _currentTerm = newTerm } -> 
        pure (respond label self (Vote leader newTerm 1), Follower leader others state ) -- accept candidate
        <|> pure (respond label self (Vote leader newTerm 0), Follower leader others state ) -- reject candidate
      _ -> empty

-- follower times out and becomes candidate
-- this is implemented as a message to self
-- becomes candidate and increments term value
followerTimeout :: Alternative f => Label -> Protlet f RaftState
followerTimeout label = RPC "followerTimeout" sendSelfMessage receiveSelfMessage
  where
    sendSelfMessage :: ClientStep RaftState
    sendSelfMessage = \case
      Follower leader others state@ActorState {_id = selfID, _currentTerm = currentTerm} ->
        Just (_id state, [currentTerm], receiveSelfMessageResponse others state)
      _ -> empty
    
    receiveSelfMessageResponse :: [OtherNode] -> ActorState -> [Int] -> RaftState
    receiveSelfMessageResponse others state [prevTerm] = Candidate others state { _currentTerm = prevTerm + 1 }

    -- this step does is inert
    -- it does not change the state and sends back the message it received
    receiveSelfMessage :: ServerStep RaftState
    receiveSelfMessage messageBody currentState = Just (messageBody, currentState)

initNetwork :: Alternative f => SpecNetwork f RaftState
initNetwork = initializeNetwork nodeStates protlets
  where
    nodeStates :: [(NodeID, [(Label, RaftState)])]
    nodeStates = [
        (0, [(label, LeaderReplicate [1, 2, 3, 4] (ActorState 0 0 Nothing [] Nothing []) 98)])
      , (1, [(label, Follower 0 [0, 2, 3, 4] (ActorState 1 0 Nothing [] Nothing []))])
      , (2, [(label, Follower 0 [0, 1, 3, 4] (ActorState 1 0 Nothing [] Nothing []))])
      , (3, [(label, Follower 0 [0, 1, 2, 4] (ActorState 1 0 Nothing [] Nothing []))])
      , (4, [(label, Follower 0 [0, 1, 2, 3] (ActorState 1 0 Nothing [] Nothing []))])
      ]

    protlets :: Alternative f => [(NodeID, [Protlet f RaftState])]
    protlets = [(label, [OneOf [logReplicate logLabel 4, followerTimeout label], reElection electionLabel 4, commit label])]

    label :: Label
    label = 0

    logLabel = 0
    electionLabel = 1
