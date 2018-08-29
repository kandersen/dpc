{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
module NetSim.Interpretations.Pure where
import           Data.List        (sortBy, find)
import qualified Data.Map         as Map
import           Data.Ord         (comparing)
import Data.Maybe (isJust)
import           NetSim.Core
import           NetSim.Invariant
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
-- possibly a message to be sent, an updated message soup and
-- the continuation of the program.
stepDiSeL :: NodeID -> [Message] -> DiSeL t a -> (Maybe Message, [Message], DiSeL t a)
stepDiSeL    _ soup (Pure a) =
  (Nothing, soup, Pure a)
stepDiSeL nodeID soup (Send _ to label tag body k) =
  (Just $ Message nodeID tag body to label, soup, k)
stepDiSeL nodeID soup (Receive candidates k) =
  case oneOfP isMessageForThis soup of
    Nothing           -> (Nothing, soup, k Nothing)
    Just (msg, soup') -> (Nothing, soup', k $ Just msg)
  where
    isMessageForThis Message{..} = isJust . find (\(_,lbl,t) -> _msgTo == nodeID && _msgLabel == lbl && _msgTag == t) $ candidates
stepDiSeL nodeID soup (This k) =
  (Nothing, soup, k nodeID)
stepDiSeL nodeID soup (Par mas k) =
  case check mas of
    Nothing ->
      let ((n, ma):mas') = mas in
      let (mmsg, soup', ma') = stepDiSeL nodeID soup ma in
      (mmsg, soup', Par (snoc mas' (n, ma')) k)
    Just as ->
      (Nothing, soup, k $ snd <$> sortBy (comparing fst) as)
  where
    check [] = Just []
    check ((n, ma):mas') = case ma of
      Pure a -> ((n, a):) <$> check mas'
      _      -> Nothing
stepDiSeL nodeID soup (Bind ma fb) =
  case ma of
    Pure a -> (Nothing, soup, fb a)
    _ ->
      let (mmsg, soup', ma') = stepDiSeL nodeID soup ma in
      (mmsg, soup', Bind ma' fb)


runPure :: Configuration (DiSeL t) a -> [(Maybe Message, Configuration (DiSeL t) a)]
runPure initConf = go (cycle $ _confNodes initConf) initConf
  where
    go :: [NodeID] -> Configuration (DiSeL t) a -> [(Maybe Message, Configuration (DiSeL t) a)]
    go     [] conf = [(Nothing, conf)]
    go (n:ns) conf = do
      let (mmsg, soup', node') = stepDiSeL n (_confSoup conf) (_confNodeStates conf Map.! n)
      let schedule' = case node' of
                        Pure _ -> filter (/= n) ns
                        _      -> ns
      let states' = Map.insert n node' (_confNodeStates conf)
      let soup'' = case mmsg of
                     Nothing  -> soup'
                     Just msg -> msg : soup'
      let conf' = conf { _confNodeStates = states', _confSoup = soup'' }
      ((mmsg, conf'):) <$> go schedule' $ conf'

--
-- Model Checking
--

-- type Invariant m s a = forall f. (m, Label, Network f s) -> a
modelcheck :: Invariant m s a -> Network f s -> DiSeL (Protlet f s)  a -> Bool
modelcheck _ _ _ = True
