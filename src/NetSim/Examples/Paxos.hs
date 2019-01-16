{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NetSim.Examples.Paxos where

import           NetSim.Types
import           NetSim.Specifications
import           NetSim.Language
import           NetSim.Util

import           Control.Monad   (forM_)
import           Control.Monad.State
import           Control.Monad.Morph
import           Data.Maybe      (fromMaybe)
import           Data.Ratio
import           Control.Monad.Trans

-- Specification
{-
“only a single value is decided uniformly by all non-crashed nodes, 
 it never changes in the future, and the decided value has been
 proposed by some node participating in the protocol”
-}

data PState = ProposerInit   Int      -- Ballot
                             Int      -- Desired value
                             [NodeID] -- List of acceptors   
            | ProposerPolled Int      -- This ballot
                             Int      -- Desired value
                             [NodeID] -- Acceptors
                             Int      -- Highest-ballot value from acceptors
            | ProposerDone   Int      -- The settled value
            | Acceptor AcceptorState
            deriving Show

data AcceptorState = AS {
  _acceptedBallot :: Maybe (Either Int (Int, Int)),
  _outstandingMsgs :: [Response]
} deriving Show

acceptorInit :: PState 
acceptorInit = Acceptor $ AS Nothing []

data Response = FirstBallotOK NodeID
              | CurrentlySupporting NodeID Int
              | PreviouslyAccepted NodeID Int Int    
              deriving Show        


 
respond :: Label -> NodeID -> Response -> Message
respond label from (FirstBallotOK to) = Message {
  _msgTo = to,
  _msgBody = [],
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}
respond label from (CurrentlySupporting to b') = Message {
  _msgTo = to,
  _msgBody = [b'],
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}
respond label from (PreviouslyAccepted to b' v') = Message {
  _msgTo = to,
  _msgBody = [b', v'],
  _msgFrom = from,
  _msgLabel = label,
  _msgTag = "prepare__Response"
}

getVote :: [Int] -> [(Int, Int)]
getVote [] = []
getVote [_] = []
getVote [b',w] = [(b', w)]

findHighestBallotedValue :: (Int, Int) -> [(Int, Int)] -> Int
findHighestBallotedValue (_, w) [] = w
findHighestBallotedValue (b, v) ((b', w):rs) = findHighestBallotedValue (if b' > b then (b', w) else (b, v)) rs

prepare :: Alternative f => Label -> Int -> Protlet f PState
prepare label n = Quorum "prepare" ((fromIntegral n % 2) + 1) propositionCast acceptorReceive acceptorRespond
  where
    propositionCast = \case
      ProposerInit b v as -> Just (zip as (repeat [b]), propositionReceive b v as)
      _ -> empty
    propositionReceive b v as = ProposerPolled b v as . findHighestBallotedValue (b, v) . concatMap (getVote . snd)

    acceptorReceive msg = \case
      Acceptor s@AS{..} -> 
        case _acceptedBallot of
          Nothing -> Just $ Acceptor s { _acceptedBallot = Just (Left $ getBallot msg), 
                                         _outstandingMsgs = FirstBallotOK (_msgFrom msg) : _outstandingMsgs }
          Just (Left b') -> 
            if getBallot msg > b' 
              then Just $ Acceptor s { _acceptedBallot = Just (Left $ getBallot msg),
                                       _outstandingMsgs = FirstBallotOK (_msgFrom msg) : _outstandingMsgs }
              else Just $ Acceptor s { _outstandingMsgs = CurrentlySupporting (_msgFrom msg) b' : _outstandingMsgs }
          Just (Right (b', w)) ->
            if getBallot msg > b'
              then Just $ Acceptor s { _acceptedBallot = Just (Left $ getBallot msg),
                                       _outstandingMsgs = PreviouslyAccepted (_msgFrom msg) b' w : _outstandingMsgs }
              else Just $ Acceptor s
      _ -> empty
        
    getBallot = head . _msgBody

    acceptorRespond acceptorID = \case
      Acceptor s@AS{..} ->
        (\(r, omsgs') -> (respond label acceptorID r, Acceptor $ s { _outstandingMsgs = omsgs'})) <$> oneOf _outstandingMsgs
      _ -> empty
      
    
commit :: Alternative f => Protlet f PState
commit = Quorum "commit" 0 commitCast acceptorReceive acceptorRespond
  where
    commitCast = \case
      ProposerPolled b _ as w -> Just (zip as (repeat [b, w]),\_ -> ProposerDone w)
      _ -> empty

    acceptorReceive msg = \case
      Acceptor s@AS{..} ->
        case _acceptedBallot of
          Just (Left b) | b == getBallot msg -> Just $ Acceptor s { _acceptedBallot = Just $ Right (b, getValue msg) }
          _ -> empty
      _ -> empty

    getBallot = head . _msgBody
    getValue  = head . tail . _msgBody

    acceptorRespond _ _ = empty


initNetwork :: Alternative f => SpecNetwork f PState
initNetwork = initializeNetwork nodeStates protlets
  where
    nodeStates :: [(NodeID, [(NodeID, PState)])]
    nodeStates = [ (0, [(label, ProposerInit 0 0 [2, 3, 4])])
                 , (1, [(label, ProposerInit 1 42 [2, 3, 4])])
                 , (2, [(label, acceptorInit)])
                 , (3, [(label, acceptorInit)])
                 , (4, [(label, acceptorInit)])
                 ]
    protlets :: Alternative f => [(NodeID, [Protlet f PState])]
    protlets = [(label, [prepare label 3, commit])]

    label :: Label
    label = 0

-- Direct Implementation

proposer' :: (MessagePassing m, ProtletAnnotations PState m) => Label -> m [NodeID] -> m Int -> m Int -> m Int
proposer' label getAcceptors getBallot getValue = do
  acceptors <- getAcceptors
  ballot <- getBallot
  poll <- enactingClient (prepare label $ length acceptors) $ 
    broadcastQuorom (fromIntegral (length acceptors) / 2 + 1) label "propose" [ballot] acceptors
  v <- getValue
  let acceptedValue = findHighestBallotedValue (ballot, v) . concatMap extractVote $ poll
  enactingClient commit $ 
    broadcastQuorom 0 label "commit" [ballot, acceptedValue] acceptors
  return acceptedValue
  where
    extractVote Message{..} = case _msgBody of
      [] -> []
      [_] -> []
      [b',w] -> [(b', w)]

acceptor' :: (MessagePassing m, ProtletAnnotations PState m) => 
   Label -> m a
acceptor' label = evalStateT loop Nothing
  where
    loop :: (MessagePassing m, ProtletAnnotations PState m, MonadState (Maybe (Either Int (Int, Int))) m) =>  m a
    loop = do
      enactingServer (OneOf [prepare label undefined, commit]) $ do
        Message{..} <- spinReceive [(label, "propose__Broadcast"), (label, "commit__Broadcast")]
        case _msgTag of
          "commit__Broadcast" -> do
            astate <- get
            case (astate, _msgBody) of
              (Just (Left b), [b', w]) | b == b' -> 
                put $ Just (Right (b', w))
              _ -> return ()
          "propose__Broadcast" -> do
            astate <- get
            case astate of
              Nothing -> 
                send _msgFrom label "propose__Response" []
              Just (Left b) | [b'] <- _msgBody ->
                if b' > b
                  then do
                    put $ Just (Left b') 
                    send _msgFrom label "propose__Response" [b]
                  else 
                    send _msgFrom label "propose__Response" []                
              Just (Right (b, v)) | [b'] <- _msgBody ->
                if b' > b
                  then do 
                    put $ Just (Left b')
                    send _msgFrom label "propose__Response" [b, v]
                  else
                    send _msgFrom label "propose__Response" []
          _ -> error $ "Acceptor' unexpected tag -- " ++ _msgTag
      loop

initConf :: (MessagePassing m, ProtletAnnotations PState m) => ImplNetwork m Int
initConf = initializeImplNetwork [
   (0, proposer' 0 (return [1, 2, 3]) (return 0) (return 0))
 , (1, acceptor' 0)
 , (2, acceptor' 0)
 , (3, acceptor' 0)
 , (4, proposer' 0 (return [1, 2, 3]) (return 4) (return 42))
 , (5, proposer' 0 (return [1, 2, 3]) (return 5) (return 117))
 ] 

-- Round-Based Register Implementation

readRBR :: MessagePassing m => Label -> [NodeID] -> Int -> m (Bool, Maybe Int)
readRBR lbl participants r = do
  forM_ participants $ \pt -> send pt lbl "Read__Request" [r]
  spinForResponses 0 Nothing []
  where
    n :: Double
    n = fromIntegral $ length participants

    isDone :: [NodeID] -> Bool
    isDone q = length q == ceiling ((n + 1.0) / 2.0)

    updateV :: Int -> Maybe Int -> Maybe Int
    updateV v = maybe (Just v) (const $ Just v)

    spinForResponses maxKW maxV q =
      if isDone q
        then return (True, maxV)
        else do
          Message sender _ body _ _ <- spinReceive [(lbl, "Read__Response")]
          case body of
            [1, k, 0, kW] | k == r ->
              if kW >= maxKW
                then spinForResponses kW Nothing (sender : q)
                else spinForResponses maxKW maxV (sender : q)
            [1, k, 1, v, kW] | k == r ->
                if kW >= maxKW
                    then spinForResponses kW (updateV v maxV) (sender : q)
                    else spinForResponses maxKW maxV (sender : q)
            [0, k] | k == r -> return (False, Nothing)
            _ -> spinForResponses maxKW maxV q


writeRBR :: MessagePassing m => Label -> [NodeID] -> Int -> Int -> m Bool
writeRBR lbl participants r vW = do
  forM_ participants $ \pt -> send pt lbl "Write__Request" [r, vW]
  spinForResponses []
  where
    n :: Double
    n = fromIntegral $ length participants

    spinForResponses q = do
      Message sender _ body _ _ <- spinReceive [(lbl, "Write__Response")]
      case body of
        [1, k] | k == r ->
            if length q == ceiling ((n + 1.0) / 2.0)
              then return True
              else spinForResponses (sender : q)
        [0, k] | k == r -> return False
        _ -> spinForResponses q

acceptor :: MessagePassing m => Label -> m a
acceptor lbl = go Nothing 0 0
  where
    go mv r w = do
      Message sender tag body _ _ <- spinReceive [(lbl, "Read__Request"), (lbl, "Write__Request")]
      case (tag, body) of
        ("Read__Request", [k]) ->
            if k < r
              then do
                send sender lbl "Read__Response" [0, k]
                go mv r w
              else do
                let msg = case mv of
                            Nothing -> [1, k, 0, w]
                            Just v  -> [1, k, 1, v, w]
                send sender lbl "Read__Response" msg
                go mv k w
        ("Write__Request", [k, vW]) ->
            if k < r
                then do
                  send sender lbl "Write__Response" [0, k]
                  go mv r w
                else do
                  send sender lbl "Write__Response" [1, k]
                  go (Just vW) k k
        _ -> error $ "Illformed request " ++ tag ++ ": " ++ show body

proposeRC :: MessagePassing m =>
  Label -> [NodeID] -> Int -> Int -> m (Bool, Maybe Int)
proposeRC lbl participants r v0 = do
  (resR, mv) <- readRBR lbl participants r
  let v = fromMaybe v0 mv
  if resR
    then do
      resW <- writeRBR lbl participants r v
      if resW
        then return (True, Just v)
        else return (False, Nothing)
    else return (False, Nothing)

proposeP :: (NetworkNode m, MessagePassing m) => Label -> [NodeID] -> Int -> m Int
proposeP lbl participants v0 = do
    k <- this 
    loopTillSucceed k
  where
    loopTillSucceed k = do
      (res, v) <- proposeRC lbl participants k v0
      if res
        then return (fromMaybe (error "Shouldn't happen!") v)
        else loopTillSucceed (k + length participants)