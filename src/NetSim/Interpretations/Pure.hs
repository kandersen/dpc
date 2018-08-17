{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module NetSim.Interpretations.Pure where
import           Data.List        (sortBy)
import qualified Data.Map         as Map
import           Data.Ord         (comparing)
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
data DiSeL a = Pure a
             | forall b. Bind (DiSeL b) (b -> DiSeL a)
             | Send NodeID Label String [Int] (DiSeL a)
             | Receive [Label] [String] (Maybe Message -> DiSeL a)
             | This (NodeID -> DiSeL a)
             | forall b. Par [(Int, DiSeL b)] ([b] -> DiSeL a)

instance Show a => Show (DiSeL a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = "Bind ma <Continuation>"
  show (Send nodeid label tag body k) = concat ["Send[", show label, ", ", tag, "] ", show body, show nodeid, "(", show k, ")"]
  show (Receive labels tags _) = concat ["Receive[", show labels, ", {", show tags, "}] <Continuation>"]
  show (This _) = "This <Continuation>"
  show (Par _ _) = "Par Schedule cont"

instance Functor DiSeL where
  fmap f (Pure a)                   = Pure (f a)
  fmap f (Bind ma fb)               = Bind ma (fmap f . fb)
  fmap f (Send label tag body to k) = Send label tag body to (fmap f k)
  fmap f (Receive label tags k)     = Receive label tags (fmap f . k)
  fmap f (This k)                   = This (fmap f . k)
  fmap f (Par as k)                 = Par as (fmap f . k)

instance Applicative DiSeL where
  pure = Pure
  (Pure f) <*> ma = fmap f ma
  (Bind ma fb) <*> mb = Bind ma ((<*> mb) . fb)
  (Send label tag body to k) <*> mb = Send label tag body to (k <*> mb)
  (Receive label tags k) <*> mb = Receive label tags ((<*> mb) . k)
  (This k) <*> mb = This ((<*> mb) . k)
  (Par as k) <*> mb = Par as ((<*> mb) . k)

instance Monad DiSeL where
  (>>=) = Bind

instance MessagePassing DiSeL where
  send to label tag body = Send to label tag body (pure ())
  receive label tags = Receive label tags pure
  this = This pure

instance Par DiSeL where
  par as = Par (zip [0..] as)

-- Single-step evaluation. Takes a name for `this`, the
-- message soup and the program under evaluation and produces
-- possibly a message to be sent, an updated message soup and
-- the continuation of the program.
stepDiSeL :: NodeID -> [Message] -> DiSeL a -> (Maybe Message, [Message], DiSeL a)
stepDiSeL    _ soup (Pure a) =
  (Nothing, soup, Pure a)
stepDiSeL nodeID soup (Send to label tag body k) =
  (Just $ Message nodeID tag body to label, soup, k)
stepDiSeL nodeID soup (Receive labels tags k) =
  case oneOfP isMessage soup of
    Nothing           -> (Nothing, soup, k Nothing)
    Just (msg, soup') -> (Nothing, soup', k $ Just msg)
  where
    isMessage Message{..} = _msgTag `elem` tags && _msgTo == nodeID && _msgLabel `elem` labels
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


runPure :: Configuration DiSeL a -> [(Maybe Message, Configuration DiSeL a)]
runPure initConf = go (cycle $ _confNodes initConf) initConf
  where
    go :: [NodeID] -> Configuration DiSeL a -> [(Maybe Message, Configuration DiSeL a)]
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
modelcheck :: Invariant m s a -> Network f s -> DiSeL a -> Bool
modelcheck _ _ _ = True
