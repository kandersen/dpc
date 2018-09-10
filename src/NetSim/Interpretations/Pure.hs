{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module NetSim.Interpretations.Pure where
import           Data.List        (sortBy, find)
import qualified Data.Map         as Map
import           Data.Map         (Map)
import           Data.Ord         (comparing)

import Control.Monad.State

import           NetSim.Core
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
data DiSeL t a = Pure a
               | forall b. Bind (DiSeL t b) (b -> DiSeL t a)
               | Send t NodeID Label String [Int] (DiSeL t a)
               | Receive [(t, Label, String)] (Maybe Message -> DiSeL t a)
               | This (NodeID -> DiSeL t a)
               | forall b. Par [(Int, DiSeL t b)] ([b] -> DiSeL t a)

instance (Show a, Show t) => Show (DiSeL t a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = "Bind ma <Continuation>"
  show (Send _ nodeid label tag body k) = concat ["Send[", show label, ", ", tag, "] ", show body, show nodeid, "(", show k, ")"]
  show (Receive candidates _) = concat ["Receive[", show candidates, "] <Continuation>"]
  show (This _) = "This <Continuation>"
  show (Par _ _) = "Par Schedule cont"

instance Functor (DiSeL t) where
  fmap f (Pure a)                     = Pure (f a)
  fmap f (Bind ma fb)                 = Bind ma (fmap f . fb)
  fmap f (Send t label tag body to k) = Send t label tag body to (fmap f k)
  fmap f (Receive candidates k)       = Receive candidates (fmap f . k)
  fmap f (This k)                     = This (fmap f . k)
  fmap f (Par as k)                   = Par as (fmap f . k)

instance Applicative (DiSeL t) where
  pure = Pure
  (Pure f) <*> ma = fmap f ma
  (Bind ma fb) <*> mb = Bind ma ((<*> mb) . fb)
  (Send t label tag body to k) <*> mb = Send t label tag body to (k <*> mb)
  (Receive candidates k) <*> mb = Receive candidates ((<*> mb) . k)
  (This k) <*> mb = This ((<*> mb) . k)
  (Par as k) <*> mb = Par as ((<*> mb) . k)

instance Monad (DiSeL t) where
  (>>=) = Bind

instance NetworkNode (DiSeL t) where
  this = This pure

instance MessagePassing t (DiSeL t) where
  send t to label tag body = Send t to label tag body (pure ())
  receive candidates = Receive candidates pure

instance Par (DiSeL t) where
  par as = Par (zip [0..] as)

-- Single-step evaluation. Takes a name for `this`, the
-- message soup and the program under evaluation and produces
-- a network sideeffect description by a trace-action, 
-- an updated message soup and the continuation of the program.
data TraceAction t = InternalAction
                   | SendAction NodeID t Message
                   | ReceiveAction NodeID t Message   

instance Show (TraceAction t) where
  show InternalAction = "InternalAction"
  show (SendAction nid _ msg) = concat ["Send[", show nid, "] ", show msg]
  show (ReceiveAction nid _ msg) = concat ["Receive[", show nid, "] ", show msg]

stepDiSeL :: NodeID -> [Message] -> DiSeL t a -> (TraceAction t, [Message], DiSeL t a)
stepDiSeL    _ soup (Pure a) =
  (InternalAction, soup, Pure a)
stepDiSeL nodeID soup (Send t to label tag body k) =
  let msg = Message nodeID tag body to label in
  (SendAction nodeID t msg, msg : soup, k)
stepDiSeL nodeID soup (Receive candidates k) =
  case findMessageAndTransitionInSoup soup of
    Nothing              -> (InternalAction, soup,  k Nothing)
    Just (t, msg, soup') -> (ReceiveAction nodeID t msg, soup', k $ Just msg)
  where
    findMessageAndTransitionInSoup [] = Nothing
    findMessageAndTransitionInSoup (m : soup') =
      case findInCandidates m of
        Nothing -> (\(t, msg, soup'') -> (t, msg, m : soup'')) <$> findMessageAndTransitionInSoup soup'
        Just (t, _, _) -> Just (t, m, soup')
    findInCandidates Message{..} = find (\(_,lbl,tag) -> _msgTo == nodeID && _msgLabel == lbl && _msgTag == tag) candidates
stepDiSeL nodeID soup (This k) =
  (InternalAction, soup, k nodeID)
stepDiSeL nodeID soup (Par mas k) =
  case check mas of
    Nothing ->
      let ((n, ma):mas') = mas in
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (act, soup', Par (snoc mas' (n, ma')) k)
    Just as ->
      (InternalAction, soup, k $ snd <$> sortBy (comparing fst) as)
  where
    check [] = Just []
    check ((n, ma):mas') = case ma of
      Pure a -> ((n, a):) <$> check mas'
      _      -> Nothing
stepDiSeL nodeID soup (Bind ma fb) =
  case ma of
    Pure a -> (InternalAction, soup, fb a)
    _ ->
      let (act, soup', ma') = stepDiSeL nodeID soup ma in
      (act, soup', Bind ma' fb)

{-
  Run a distributed program using fair, round-robin scheduling of message deliveries and process
  steps. Produces a trace of send/receive transitions, messages sent and a trace of the entire system at
  every step.

  If a step yields (Just (n, t), Just m, c)
  then, from the previous network state,
    node n took spec transition t, 
    in this a case a send, as it yielded message m,
    producing a new network configuration c.
-}               
runPure :: Configuration (DiSeL t) a -> [(TraceAction t, Configuration (DiSeL t) a)]
runPure initConf = go (cycle $ _confNodes initConf) initConf
  where
    go :: [NodeID] -> Configuration (DiSeL t) a -> [(TraceAction t, Configuration (DiSeL t) a)]
    go     [] conf = [(InternalAction, conf)]
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
modelcheckExecutionTrace :: forall s. (Show s, Eq s) => 
     Network [] s -- ^ Initial specification configuration
  -> [TraceAction (s, Message -> s)] -- ^Execution trace of running a pure DiSeL program, e.g. using runPure
  -> IO Bool -- ^ Does the program communicate according to the spec.
modelcheckExecutionTrace network trace = evalStateT (stepSpec network trace) Map.empty
  where
    stepSpec :: (Show s, Eq s) => Network [] s -> [TraceAction (s, Message -> s)] -> StateT (Map (NodeID,Label) (Message -> s)) IO Bool 
    stepSpec _ [] = 
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
        (ReceiveAction nodeid (pre, post) msg) -> do

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
                  
        (SendAction nodeid (pre, post) msg) -> do
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

modelcheck :: (Show s, Eq s) => Network [] s -> Configuration (DiSeL (s, Message -> s)) a -> IO Bool
modelcheck initNet = modelcheckExecutionTrace initNet . (fst <$>) . runPure

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
  