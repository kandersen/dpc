{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module NetSim.Interpretations.Pure where
import           Data.List        (sortBy, find)
import qualified Data.Map         as Map
import Data.Map (Map)
import           Data.Ord         (comparing)
import Data.Maybe (fromJust)

import Control.Arrow
import Control.Monad.State

import Lens.Micro hiding (to)
import Lens.Micro.Mtl
import Lens.Micro.GHC()

import           NetSim.Types
import           NetSim.Specifications
import           NetSim.Language
import           NetSim.Util


-- Pure implementation.
--
-- DiSeL is a deep embedding of the programming language.
-- This can be used for pure execution of programs, e.g.
-- to allow stepping through computations.
-- The `Par` constructor embeds a 'schedule' into the list of subcomputations
-- This allows a 'semi-stateful' interpretation of the AST,
-- as can be seen in the small-step operational semantics implemented below.
data DiSeL s a = Pure a
               | forall b. Bind (DiSeL s b) (b -> DiSeL s a)
               | Send NodeID Label String [Int] (DiSeL s a)
               | Receive [(Label, String)] (Maybe Message -> DiSeL s a)
               | This (NodeID -> DiSeL s a)
               | forall b. Par [(Int, DiSeL s b)] ([b] -> DiSeL s a)
               | EnactingServer (Protlet [] s) (DiSeL s a)
               | EnactingClient (Protlet [] s) (DiSeL s a)

instance (Show a, Show s) => Show (DiSeL s a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = "Bind ma <Continuation>"
  show (Send nodeid label tag body k) = concat ["Send[", show label, ", ", tag, "] ", show body, show nodeid, "(", show k, ")"]
  show (Receive candidates _) = concat ["Receive[", show candidates, "] <Continuation>"]
  show (This _) = "This <Continuation>"
  show (Par _ _) = "Par Schedule cont"
  show (EnactingServer p k) = "EnactingServer[" ++ show p ++ "]" ++ show k
  show (EnactingClient p k) = "EnactingClient[" ++ show p ++ "]" ++ show k


instance Functor (DiSeL s) where
  fmap f (Pure a)                     = Pure (f a)
  fmap f (Bind ma fb)                 = Bind ma (fmap f . fb)
  fmap f (Send label tag body to k)   = Send label tag body to (fmap f k)
  fmap f (Receive candidates k)       = Receive candidates (fmap f . k)
  fmap f (This k)                     = This (fmap f . k)
  fmap f (Par as k)                   = Par as (fmap f . k)
  fmap f (EnactingServer p k)         = EnactingServer p (fmap f k)
  fmap f (EnactingClient p k)         = EnactingClient p (fmap f k)

instance Applicative (DiSeL s) where
  pure = Pure
  (Pure f) <*> ma = fmap f ma
  (Bind ma fb) <*> mb = Bind ma ((<*> mb) . fb)
  (Send label tag body to k) <*> mb = Send label tag body to (k <*> mb)
  (Receive candidates k) <*> mb = Receive candidates ((<*> mb) . k)
  (This k) <*> mb = This ((<*> mb) . k)
  (Par as k) <*> mb = Par as ((<*> mb) . k)
  (EnactingServer p k) <*> mb = EnactingServer p (k <*> mb)
  (EnactingClient p k) <*> mb = EnactingClient p (k <*> mb)

instance Monad (DiSeL s) where
  (>>=) = Bind

instance NetworkNode (DiSeL s) where
  this = This pure

instance MessagePassing (DiSeL s) where
  send to label tag body = Send to label tag body (pure ())
  receive candidates = Receive candidates pure

instance ProtletAnnotations s (DiSeL s) where
  enactingServer = EnactingServer
  enactingClient = EnactingClient

instance Par (DiSeL s) where
  par as = Par (zip [0..] as)

-- Single-step evaluation. Takes a name for `this`, the
-- message soup and the program under evaluation and produces
-- a network sideeffect description by a trace-action, 
-- an updated message soup and the continuation of the program.
data TraceAction s = InternalAction NodeID
                   | SendAction NodeID Message
                   | ReceiveAction NodeID Message   
                   | ServerAction (Protlet [] s) (TraceAction s)
                   | ClientAction (Protlet [] s) (TraceAction s)

instance Show s => Show (TraceAction s) where
  show (InternalAction nid) = concat ["InternalAction[", show nid, "]"]
  show (SendAction nid msg) = concat ["Send[", show nid, "] ", show msg]
  show (ReceiveAction nid msg) = concat ["Receive[", show nid, "] ", show msg]
  show (ServerAction p t) = "ServerAction[" ++ show p ++ "]:" ++ show t
  show (ClientAction p t) = "ClientAction[" ++ show p ++ "]:" ++ show t

stepDiSeL :: NodeID -> [Message] -> DiSeL s a -> (TraceAction s, [Message], DiSeL s a)
stepDiSeL nodeID soup (Pure a) =
  (InternalAction nodeID, soup, Pure a)
stepDiSeL nodeID soup (Send to label tag body k) =
  let msg = Message nodeID tag body to label in
  (SendAction nodeID msg, msg : soup, k)
stepDiSeL nodeID soup (Receive candidates k) =
  case findMessageAndTransitionInSoup soup of
    Nothing           -> (InternalAction nodeID, soup,  k Nothing)
    Just (msg, soup') -> (ReceiveAction nodeID msg, soup', k $ Just msg)
  where
    findMessageAndTransitionInSoup [] = Nothing
    findMessageAndTransitionInSoup (m : soup') =
      case findInCandidates m of
        Nothing -> second ((:) m) <$> findMessageAndTransitionInSoup soup'
        Just _ -> Just (m, soup')
    findInCandidates Message{..} = find (\(lbl,tag) -> _msgTo == nodeID && _msgLabel == lbl && _msgTag == tag) candidates
stepDiSeL nodeID soup (This k) =
  (InternalAction nodeID, soup, k nodeID)
stepDiSeL nodeID soup (Par mas k) =
  case check mas of
    Nothing ->
      let ((n, ma):mas') = mas in
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (act, soup', Par (snoc mas' (n, ma')) k)
    Just as ->
      (InternalAction nodeID, soup, k $ snd <$> sortBy (comparing fst) as)
  where
    check [] = Just []
    check ((n, ma):mas') = case ma of
      Pure a -> ((n, a):) <$> check mas'
      _      -> Nothing
stepDiSeL nodeID soup (Bind ma fb) =
  case ma of
    Pure a -> (InternalAction nodeID, soup, fb a)
    _ ->
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (act, soup', Bind ma' fb)
stepDiSeL nodeID soup (EnactingServer p ma) = 
  case ma of
    Pure a -> (InternalAction nodeID, soup, Pure a)
    _ -> 
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (ServerAction p act, soup', EnactingServer p ma')
stepDiSeL nodeID soup (EnactingClient p ma) = 
  case ma of
    Pure a -> (InternalAction nodeID, soup, Pure a)
    _ -> 
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (ClientAction p act, soup', EnactingClient p ma')

{-
  Run a distributed program using fair, round-robin scheduling of message deliveries and process
  steps. Produces a trace of send/receive transitions, messages sent and a trace of the entire system at
  every step.

  If a step yields (t, Just m, c)
  then, from the previous network state,
    node n took spec transition t, 
    in this a case a send, as it yielded message m,
    producing a new network configuration c.
-}               
runPure :: ImplNetwork (DiSeL s) a -> [(TraceAction s, ImplNetwork (DiSeL s) a)]
runPure initConf = go (cycle $ nodes initConf) initConf
  where
    go :: [NodeID] -> ImplNetwork (DiSeL s) a -> [(TraceAction s, ImplNetwork (DiSeL s) a)]
    go     []    _ = []
    go (n:ns) conf = do
      let (act, soup', node') = stepDiSeL n (_globalState conf) (_localStates conf Map.! n)
      let schedule' = case node' of
                        Pure _ -> filter (/= n) ns -- if done, no need to schedule this node again
                        _      -> ns
      let states' = Map.insert n node' (_localStates conf)
      let conf' = conf { _localStates = states', _globalState = soup' }
      ((act, conf'):) <$> go schedule' $ conf'

--
-- Model Checking
--

type ValidationError = String

data ValidationState s = Init s
                       | SyncServerFor String NodeID [Int] s
                       | AwaitingResponseFrom String NodeID ([Int] -> s)
                      deriving (Show)

type ValidationM s a = StateT (Map NodeID (ValidationState s)) (Either ValidationError) a

checkTrace :: Show s => SpecNetwork [] s -> [TraceAction s] -> Either ValidationError ()
checkTrace initNetwork trace = case runStateT (go trace) (Init <$> initConf) of
  Left e -> Left e
  Right ((), _) -> Right ()
  where
    initConf = Map.fromList [ (k, s) | (k, (v,_,_)) <- Map.assocs $ _localStates initNetwork, Running s <- [v Map.! 0]]

    go :: Show s => [TraceAction s] -> ValidationM s ()
    go ts = sequence_ $ checkAction <$> ts

    checkAction :: Show s => TraceAction s -> ValidationM s ()
    checkAction (InternalAction _) = return ()
    checkAction (SendAction _ _) =
      fail "Sending outside of protocol annotations!"
    checkAction (ReceiveAction _ _) =
      fail "Receiving outside of protocol annotations!"
    checkAction (ServerAction (RPC pName _ serverStep) (ReceiveAction nodeID msg)) = do
      vs <- fromJust <$> use (at nodeID)
      case vs of 
        Init s ->
          case serverStep (_msgBody msg) s of
            Nothing -> fail . concat $ ["Expecting node ", show nodeID, " to serve ", pName]
            Just (resp, s') -> 
              at nodeID ?= SyncServerFor pName (_msgFrom msg) resp s'
    checkAction (ServerAction (RPC pName _ _) (SendAction nodeID msg)) = do
      vs <- fromJust <$> use (at nodeID)
      case vs of
        SyncServerFor _ client resp s' -> 
          if (_msgTo msg == client) && (_msgBody msg == resp) && (_msgTag msg == (pName ++ "__Response"))
            then at nodeID ?= Init s'
            else fail "The server response did not follow the protocol"
    checkAction (ServerAction _ (InternalAction _)) = return ()

    checkAction (ClientAction (RPC pName clientStep _) (SendAction nodeID msg)) = do
      vs <- fromJust <$> use (at nodeID) 
      case vs of
        Init s ->
          case clientStep s of
            Nothing -> fail . concat $ ["Node expected to initiate rpc ", pName]
            Just (server, body, k) ->
              if _msgBody msg == body && _msgTo msg == server
                then at nodeID ?= AwaitingResponseFrom pName server k
                else fail . concat $ ["Inappropriate request initiating rpc ", pName, "\nExpected:", show body, " to ", show server, "\nGot: ", show msg]
    checkAction (ClientAction (RPC pName _ _) (ReceiveAction nodeID msg)) = do
      vs <- fromJust <$> use (at nodeID)
      case vs of 
        AwaitingResponseFrom pName' server k | pName == pName' ->
          if _msgFrom msg == server && _msgTag msg == pName ++ "__Response"
            then at nodeID ?= Init (k $ _msgBody msg)
            else fail . concat $ ["Expected reception of response at client in RPC", pName]
        AwaitingResponseFrom pName' _ _ | pName == pName' ->
          fail . concat $ ["Expected reception on protlet", pName', "but got message tagged ", _msgTag msg]
    checkAction (ClientAction _ (InternalAction _)) = return ()
    
    checkAction act = fail . concat $ ["Unimplemented trace action", show act]