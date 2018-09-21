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

import Lens.Micro
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
runPure :: Configuration (DiSeL s) a -> [(TraceAction s, Configuration (DiSeL s) a)]
runPure initConf = go (cycle $ _confNodes initConf) initConf
  where
--    go :: [NodeID] -> Configuration (DiSeL s) a -> [(TraceAction s, Configuration (DiSeL s) a)]
    go     [] conf = []
    go (n:ns) conf = do
      let (act, soup', node') = stepDiSeL n (_confSoup conf) (_confNodeStates conf Map.! n)
      let schedule' = case node' of
                        Pure _ -> filter (/= n) ns -- if done, no need to schedule this node again
                        _      -> ns
      let states' = Map.insert n node' (_confNodeStates conf)
      let conf' = conf { _confNodeStates = states', _confSoup = soup' }
      ((act, conf'):) <$> go schedule' $ conf'

--
-- Model Checking
--

type ValidationError = String

data ValidationState s = Init s
                       | ServerFor String NodeID [Int] s
                       | AwaitingResponseFrom String NodeID ([Int] -> s)
                      deriving (Show)

type ValidationM s a = StateT (Map NodeID (ValidationState s)) (Either ValidationError) a

checkTrace :: Show s => Map NodeID s -> [TraceAction s] -> Either ValidationError ()
checkTrace initConf trace = case runStateT (go trace) (Init <$> initConf) of
  Left e -> Left e
  Right ((), _) -> Right ()
  where
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
              at nodeID ?= ServerFor pName (_msgFrom msg) resp s'
    checkAction (ServerAction (RPC pName _ _) (SendAction nodeID msg)) = do
      vs <- fromJust <$> use (at nodeID)
      case vs of
        ServerFor pName' client resp s' -> 
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
                else fail . concat $ ["Inappropriate request initiating rpc ", pName]
    checkAction (ClientAction (RPC pName clientStep _) (ReceiveAction nodeID msg)) = do
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





{-
data Configuration m a = Configuration {
  _confNodes      :: [NodeID],
  _confNodeStates :: Map NodeID (m a),
  _confSoup       :: [Message]
  }
  deriving Show 
-}
-- type Invariant m s a = forall f. (m, Label, Network f s) -> a
-- runPure :: Configuration (DiSeL t) a -> 
{-
checkExecutionTrace :: forall s. (Show s, Eq s) => 
     Network [] s -- ^ Initial specification configuration
  -> [TraceAction s] -- ^Execution trace of running a pure DiSeL program, e.g. using runPure
  -> IO Bool -- ^ Does the program communicate according to the spec.
checkExecutionTrace network trace = evalStateT (stepSpec network trace) Map.empty
  where
    stepSpec :: (Show s, Eq s) => Network [] s -> [TraceAction s] -> StateT (Map (NodeID,Label) (Message -> s)) IO Bool 
    stepSpec _ [] = do
      blockers <- get
      forM_ blockers (const $ return ())
      -- The network halted its execution. 
      -- if we got here, we stayed true to the specification at every step!
      return True
    stepSpec net (act:rest) = do
      lift $ putStr "Current network:\n\t"
      lift $ print net
      lift $ putStrLn $ "\tPerforming " ++ show act
      case act of
        InternalAction -> 
         -- Purely internal transition. The program performed a step without sending/receiving.
          -- This cannot be observed in the spec.
          -- So we simply continue with the next execution step:
          stepSpec net rest
        (ReceiveAction nodeid msg) -> do

          -- Program succeeded with a receive. Nodeid received message msg
          let protletInstance = _msgLabel msg
          let currentState =  _states net Map.! nodeid Map.! protletInstance -- find current state 
          -- check nodeid is in pre
          if not (currentState `matches` pre)
            then do
              lift $ putStrLn "On receive, does not match precondition"
              return False
            else 
              -- check nodeid can receive-step with some message m to post
              case findMatchingReceive nodeid msg (post msg) (possibleTransitions net) of
                Nothing -> do
                  lift $ putStrLn "found no matching receive transition"
                  return False
                Just t -> 
                  -- update the network and loop
                  stepSpec (applyTransition t net) rest
                  
        (SendAction nodeid msg) -> do
          -- program performed a send action. nodeid sent msg in state pre, is now in state post
            let protletInstance = _msgLabel msg
            let currentState = _states net Map.! nodeid Map.! protletInstance
            if not (currentState `matches` pre)
              then do
                lift $ putStrLn "On send, does not match precondition"
                return False
              else do
                -- check nodeid can receive-step with some message m to post
                let nextState = post msg
                lift $ putStrLn . concat $ ["\tTransitioning ", show nodeid, " to ", show nextState]
                case findMatchingSend nodeid msg nextState (possibleTransitions net) of
                  Nothing -> do
                    lift $ putStrLn "found no matching send transition"
                    return False
                  Just t -> 
                    -- update the network and loop
                    stepSpec (applyTransition t net) rest

check :: (Show s, Eq s) => Network [] s -> Configuration (DiSeL (s, Message -> s)) a -> IO Bool
check initNet = modelcheckExecutionTrace initNet . (fst <$>) . runPure

matches :: Eq s => NodeState s -> s -> Bool
matches (Running s) s' = s == s'
matches (BlockingOn s _ _ _) s' = s == s'

findMatchingReceive :: Eq s => NodeID -> Message -> s -> [Transition s] -> Maybe (Transition s)
findMatchingReceive _ _ _ [] = Nothing
findMatchingReceive nodeid msg s (SentMessages{}:ts) = findMatchingReceive nodeid msg s ts
findMatchingReceive nodeid actualMsg actualState (t@(ReceivedMessages lbl nodeid' specMsgs specState _):ts) =
  if _msgLabel actualMsg == lbl && nodeid == nodeid' && specState `matches` actualState && actualMsg `elem` specMsgs
    then return t
    else findMatchingReceive nodeid actualMsg actualState ts

findMatchingSend :: Eq s => NodeID -> Message -> s -> [Transition s] -> Maybe (Transition s)
findMatchingSend _ _ _ [] = Nothing
findMatchingSend nodeid msg s (ReceivedMessages{}:ts) = findMatchingSend nodeid msg s ts
findMatchingSend nodeid actualMsg actualState (t@(SentMessages lbl nodeid' specMsgs specState _):ts) =
  if _msgLabel actualMsg == lbl && nodeid == nodeid' && specState `matches` actualState && actualMsg `elem` specMsgs
    then return t
    else findMatchingSend nodeid actualMsg actualState ts
    -}