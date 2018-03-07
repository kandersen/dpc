{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module NetSim.TwoPhaseCommit(
  initNetwork,
  initNetworkMetadata,
  tpcInvariant,
  main,
  Invariant
  ) where

import NetSim.Core
import qualified Data.Map as Map

data State = CoordinatorInit [NodeID]
           | CoordinatorCommit [NodeID]
           | CoordinatorAbort [NodeID]
           | ParticipantInit
           | ParticipantGotRequest NodeID
           | ParticipantRespondedYes NodeID
           | ParticipantRespondedNo NodeID
           | ParticipantCommit NodeID
           | ParticipantAbort NodeID
           deriving (Show, Eq)

prepare :: Alternative f => Protlet f State
prepare = Broadcast "Prepare" coordinatorBroadcast participantReceive participantSend
  where
    coordinatorBroadcast = \case
      CoordinatorInit participants ->
        pure (zip participants (repeat []), receiveResponses participants)
      _ -> empty
    receiveResponses participants responses =
      if any (/= [1]) responses
      then CoordinatorAbort participants
      else CoordinatorCommit participants

    participantReceive :: Message -> State -> Maybe State
    participantReceive Message{..} = \case
      ParticipantInit ->
        pure $ ParticipantGotRequest _msgFrom
      _ -> empty

    participantSend nodeID = \case
      ParticipantGotRequest coordinator ->
        pure (buildResponse nodeID coordinator False, ParticipantRespondedNo coordinator) <|>
        pure (buildResponse nodeID coordinator True, ParticipantRespondedYes coordinator)
      _ -> empty

    buildResponse nodeID coordinator b = Message {
      _msgFrom = nodeID,
      _msgTag = "Prepare__Response",
      _msgBody = if b then [1] else [0],
      _msgTo = coordinator
      }

decide :: Alternative f => Protlet f State
decide = Broadcast "Decide" coordinatorBroadcast participantReceive participantRespond
  where
    coordinatorBroadcast = \case
      CoordinatorAbort participants ->
        pure (zip participants (repeat [0]), receiveResponses participants)
      CoordinatorCommit participants ->
        pure (zip participants (repeat [1]), receiveResponses participants)
      _ -> empty
    receiveResponses participants _ = CoordinatorInit participants

    participantReceive Message{..} = \case
      ParticipantRespondedYes coordinator | [0] <- _msgBody ->
                                            pure $ ParticipantAbort coordinator
                                          | [1] <- _msgBody ->
                                            pure $ ParticipantCommit coordinator
      ParticipantRespondedNo coordinator |  [_] <- _msgBody ->
                                           pure $ ParticipantAbort coordinator
      _ -> empty

    participantRespond nodeID = \case
      ParticipantAbort coordinator ->
        pure (buildResponse nodeID coordinator False, ParticipantInit)
      ParticipantCommit coordinator ->
        pure (buildResponse nodeID coordinator True, ParticipantInit)
      _ -> empty

    buildResponse nodeID coordinator b = Message {
      _msgFrom = nodeID,
      _msgTag = "Decide__Response",
      _msgBody = if b then [1] else [0],
      _msgTo = coordinator
      }

data TPCMetaData = TPCMetaData {
  _coordinator :: NodeID,
  _participants :: [NodeID]
}

type Invariant m s = forall f. (m, Network f s) -> Bool

(<||>) :: Invariant m s -> Invariant m s -> Invariant m s
l <||> r = (||) <$> l <*> r

(<&&>) :: Invariant m s -> Invariant m s -> Invariant m s
l <&&> r = (&&) <$> l <*> r

tpcInvariant :: Invariant TPCMetaData State
tpcInvariant = everythingInit <||> phaseOneInv <||> phaseTwoInv

forNode :: NodeID -> ((NodeID, NodeState State, [Message]) -> Invariant TPCMetaData State) -> Invariant TPCMetaData State
forNode nodeID p (meta, network) = p node (meta, network)
  where
    node = (nodeID, _states network Map.! nodeID, _inboxes network Map.! nodeID)

forNodes :: [NodeID] ->(NodeID -> Invariant TPCMetaData State) -> Invariant TPCMetaData State
forNodes nodes p = and . sequence (p <$> nodes)

noOutstandingMessagesBetween :: NodeID -> NodeID -> Invariant TPCMetaData State
noOutstandingMessagesBetween a b = forNode a (nothingFrom b) <&&> forNode b (nothingFrom a)
  where
    nothingFrom other (_, _, inbox) = pure $ not . any ((== other) . _msgFrom) $ inbox

runningInState :: State -> NodeID -> Invariant TPCMetaData State
runningInState s node = forNode node inState 
  where
    inState (_, Running s', _) = pure $ s == s'
    inState _ = pure False

everythingInit :: Invariant TPCMetaData State
everythingInit network = runningInState (CoordinatorInit participants) coordinator
                    <&&> forNodes participants (noOutstandingMessagesBetween coordinator)
                    <&&> forNodes participants (runningInState ParticipantInit)
                       $ network
  where
    coordinator = _coordinator . fst $ network
    participants = _participants . fst $ network

phaseOneInv :: Invariant TPCMetaData State
phaseOneInv = const False

phaseTwoInv :: Invariant TPCMetaData State
phaseTwoInv = const False

initNetwork :: Alternative f => Network f State
initNetwork = initializeNetwork nodes protlets
  where
    nodes = [ (0, CoordinatorInit [1,2,3])
            , (1, ParticipantInit)
            , (2, ParticipantInit)
            , (3, ParticipantInit)
            ]
    protlets = [prepare, decide]

initNetworkMetadata :: TPCMetaData
initNetworkMetadata = TPCMetaData {
  _coordinator = 0,
  _participants = [1, 2, 3]
  }

main :: IO ()
main = print $ tpcInvariant (initNetworkMetadata, initNetwork :: Network Maybe State)
