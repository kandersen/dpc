{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
module NetSim.Language where

import NetSim.Core
import NetSim.Util
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable
import Control.Monad.Reader
import Control.Concurrent.Chan
import Control.Concurrent
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

{-
Packet p = (l, t, p, s)
  where
    l = protocol instance label
    t = tag
    p = payload
    s = sender
-}    
type Packet = (Label, String, [Int], NodeID)

--
-- Language primitives
--
class Monad m => MonadDiSeL m where
  send :: Label -> String -> [Int] -> NodeID -> m ()
  receive :: Label -> [String] -> m (Maybe Packet)
  this :: m NodeID
  par :: [m a] -> ([a] -> m c) -> m c

--
-- Compound operations
--
spinReceive :: MonadDiSeL m => Label -> [String] -> m Packet
spinReceive label tags = do
    mmsg <- receive label tags
    case mmsg of
      Nothing -> spinReceive label tags
      Just msg -> return msg

rpcCall :: MonadDiSeL m =>
  Label -> String -> [Int] -> NodeID -> m [Int]
rpcCall label protlet body to = do
  send label (protlet ++ "__Request") body to
  (_, _, resp, _) <- spinReceive label [protlet ++ "__Response"]
  return resp

broadcastQuorom :: (MonadDiSeL m, Ord fraction, Fractional fraction) =>
  fraction -> Label -> String -> [Int] -> [NodeID] -> m [Packet]
broadcastQuorom fraction label protlet body receivers = do
  traverse_ (send label (protlet ++ "__Broadcast") body) receivers
  spinForResponses []
  where    
    spinForResponses resps 
      | fromIntegral (length resps) >= fraction * fromIntegral (length receivers) =
         return resps
      | otherwise = do
          resp <- spinReceive label [protlet ++ "__Response"]
          spinForResponses (resp:resps)

broadcast :: (MonadDiSeL m) =>
  Label -> String -> [Int] -> [NodeID] -> m [Packet]
broadcast = broadcastQuorom (1 :: Double)

--
-- Network description
--
data Configuration m a = Configuration {
  _confNodes :: [NodeID],
  _confNodeStates :: Map NodeID (m a),
  _confSoup :: [Message]
  }
  deriving Show

ppConf :: Show a => Configuration DiSeL a -> String
ppConf Configuration{..} = unlines $ ("Soup: " ++ show _confSoup) :
   [ concat [show nodeid, ": ", ppDiSeL' state] | (nodeid, state) <- Map.toList _confNodeStates ]
  where
    ppDiSeL' (Pure a) = "Returned " ++ show a
    ppDiSeL' a = ppDiSeL a

    --
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
             | Send Label String [Int] NodeID (DiSeL a)
             | Receive Label [String] (Maybe Packet -> DiSeL a)
             | This (NodeID -> DiSeL a)
             | forall b. Par [(Int, DiSeL b)] ([b] -> DiSeL a)

ppDiSeL :: DiSeL a -> String
ppDiSeL (Pure _) = "Pure <val>"
ppDiSeL (Bind ma _) = concat ["Bind(", ppDiSeL ma, ", <Cont>)"]
ppDiSeL (Send label tag body to k) = concat ["Send[", show label, ", ", tag, "](", show body, ", ", show to, ", ", ppDiSeL k]
ppDiSeL (Receive label tags _) = concat ["Receive[", show label, ", {", show tags, "}] <Cont>)"]
ppDiSeL (This _) = "This <Cont>"
ppDiSeL (Par mas _) = "Par [" ++ intercalate "," (ppDiSeL . snd <$> mas) ++ "]"

instance Show a => Show (DiSeL a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = "Bind ma <Continuation>"
  show (Send label tag body nodeid k) = concat ["Send[", show label, ", ", tag, "] ", show body, show nodeid, "(", show k, ")"]
  show (Receive label tags _) = concat ["Receive[", show label, ", {", show tags, "}] <Continuation>"]
  show (This _) = "This <Continuation>"
  show (Par _ _) = "Par Schedule cont"

instance Functor DiSeL where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind ma fb) = Bind ma (fmap f . fb)
  fmap f (Send label tag body to k) = Send label tag body to (fmap f k)
  fmap f (Receive label tags k) = Receive label tags (fmap f . k)
  fmap f (This k) = This (fmap f . k)
  fmap f (Par as k) = Par as (fmap f . k)

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

instance MonadDiSeL DiSeL where
  send label tag body receiver = Send label tag body receiver (pure ())
  receive label tags = Receive label tags pure
  this = This pure
  par as k = Par (zip [0..] as) k

-- Single-step evaluation. Takes a name for `this`, the
-- message soup and the program under evaluation and produces
-- possibly a message to be sent, an updated message soup and
-- the continuation of the program.
stepDiSeL :: NodeID -> [Message] -> DiSeL a -> (Maybe Message, [Message], DiSeL a)
stepDiSeL    _ soup (Pure a) = 
  (Nothing, soup, Pure a)
stepDiSeL nodeID soup (Send label tag body to k) =
  (Just $ Message nodeID tag body to label, soup, k)
stepDiSeL nodeID soup (Receive label tags k) = 
  case oneOfP isMessage soup of
    Nothing -> (Nothing, soup, k Nothing)
    Just (Message{..}, soup') -> (Nothing, soup', k $ Just (_msgLabel, _msgTag, _msgBody, _msgFrom))
  where
    isMessage Message{..} = _msgTag `elem` tags && _msgTo == nodeID && _msgLabel == label
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
      _ -> Nothing
stepDiSeL nodeID soup (Bind ma fb) = 
  case ma of
    Pure a -> (Nothing, soup, fb a)
    _ -> 
      let (mmsg, soup', ma') = stepDiSeL nodeID soup ma in
      (mmsg, soup', Bind ma' fb)

runPure :: Configuration DiSeL a -> [(Maybe Message, Configuration DiSeL a)]
runPure initConf = go (cycle $ _confNodes initConf) initConf
  where
    go     [] conf = [(Nothing, conf)]
    go (n:ns) conf = do
      let (mmsg, soup', node') = stepDiSeL n (_confSoup conf) (_confNodeStates conf Map.! n)
      let schedule' = case node' of
                        Pure _ -> filter (/= n) ns
                        _ -> ns
      let states' = Map.insert n node' (_confNodeStates conf)
      let soup'' = case mmsg of
                     Nothing -> soup'
                     Just msg -> msg : soup'
      let conf' = conf { _confNodeStates = states', _confSoup = soup'' }
      ((mmsg, conf'):) <$> go schedule' $ conf'
  
stepThrough :: (a -> String) -> [a] -> IO ()
stepThrough format (x:xs) = do
  putStrLn $ format x
  _ <- getLine
  stepThrough format xs

--
-- IO Implementation with real threads!
--
type Runner = ReaderT (NodeID, Chan Packet, Map NodeID (Chan Packet)) IO

instance MonadDiSeL Runner where
  send label tag body to = do
    (nodeID, _, channels) <- ask
    lift $ writeChan (channels Map.! to) (label, tag, body, nodeID)
  receive label tags = do
    (_, inbox, _) <- ask
    pkt@(label', tag, _, _) <- lift $ readChan inbox
    if tag `elem` tags && label' == label
      then return $ Just pkt
      else do
        lift $ writeChan inbox pkt
        return Nothing
  this = (\(nodeID, _, _) -> nodeID) <$> ask
  par mas k = do
    env <- ask
    vars <- forkThreads env mas
    as <-  awaitThreads vars
    k as
    where
      forkThreads _ [] = return []
      forkThreads env (ma:mas') = do
        var <- liftIO newEmptyMVar
        liftIO . forkIO $ ((runReaderT ma env) >>= putMVar var)
        (var:) <$> forkThreads env mas'
      awaitThreads [] = return []
      awaitThreads (v:vs) = do
        a <- liftIO $ takeMVar v
        (a:) <$> awaitThreads vs

runNetworkIO :: Configuration Runner a -> IO [(NodeID, a)]
runNetworkIO conf = do
  let network = Map.toList $ _confNodeStates conf
  envs <- sequence $ do
    (nodeID, code) <- network
    return $ do
      inbox <- newChan
      return (nodeID, inbox, code)
  let mapping = Map.fromList [(nodeID, inbox) | (nodeID, inbox, _) <- envs]
  output <- newChan
  sequence $ flip fmap network  $ \(nodeID, code) ->
    void $ forkIO $ void $ runReaderT (code >>= epilogue output . (nodeID,)) (nodeID, mapping Map.! nodeID, mapping)
  getChanContents output

  where
    epilogue :: Chan a -> a -> Runner ()
    epilogue output a = liftIO $ writeChan output a

--
-- Tests
--
simpleConf :: MonadDiSeL m => m a -> Configuration m a
simpleConf code = Configuration {
  _confNodes = [0],
  _confSoup = [],
  _confNodeStates = Map.fromList [(0, code)]
  }

simpleConf' :: MonadDiSeL m => [m a] -> Configuration m a
simpleConf' codes = Configuration {
  _confNodes = [0..length codes - 1],
  _confSoup = [],
  _confNodeStates = Map.fromList $ zip [0..] codes
}

test1 :: MonadDiSeL m => m Int
test1 = return 42

test2a :: MonadDiSeL m => m Int
test2a = send 0 "test" [42] 1 >> return 0

test2b :: MonadDiSeL m => m Int
test2b = spinReceive 0 ["test"] >>= (\(_, _, [ans],_) -> return ans)

test3s :: MonadDiSeL m => m a
test3s = do
  (_, _, [x], c) <- spinReceive 0 ["testQ"]
  send 0 "testA" [x + 1] c
  test3s

test3c :: MonadDiSeL m => Int -> m Int
test3c n = do
  send 0 "testQ" [n] 0
  (_, _, [ans], _) <- spinReceive 0 ["testA"]
  return ans

test4 :: MonadDiSeL m => m [Int]
test4 = par (pure <$> [0..10]) pure

test3s' :: MonadDiSeL m => m a
test3s' = par [test3s, pure undefined] (const test3s')