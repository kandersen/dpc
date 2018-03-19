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
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable
import Control.Monad.Reader
import Control.Concurrent.Chan
import Control.Concurrent

type Packet = (String, [Int], NodeID)

class Monad m => MonadDiSeL m where
  send :: String -> [Int] -> NodeID -> m ()
  receive :: [String] -> m (Maybe Packet)
  this :: m NodeID

spinReceive :: MonadDiSeL m => [String] -> m Packet
spinReceive tags = do
    mmsg <- receive tags
    case mmsg of
      Nothing -> spinReceive tags
      Just msg -> return msg

rpcCall :: MonadDiSeL m =>
  String -> [Int] -> NodeID -> m [Int]
rpcCall protlet body to = do
  send (protlet ++ "__Request") body to
  (_, resp, _) <- spinReceive [protlet ++ "__Response"]
  return resp

broadcastQuorom :: (MonadDiSeL m, Ord fraction, Fractional fraction) =>
  fraction -> String -> [Int] -> [NodeID] -> m [Packet]
broadcastQuorom fraction protlet body receivers = do
  traverse_ (send (protlet ++ "__Broadcast") body) receivers
  spinForResponses []
  where    
    spinForResponses resps 
      | fromIntegral (length resps) >= fraction * fromIntegral (length receivers) =
         return resps
      | otherwise = do
          resp <- spinReceive [protlet ++ "__Response"]
          spinForResponses (resp:resps)

broadcast :: (MonadDiSeL m) =>
  String -> [Int] -> [NodeID] -> m [Packet]
broadcast = broadcastQuorom (1 :: Double)
 
data DiSeL a = Pure a
             | forall b. Bind (DiSeL b) (b -> DiSeL a)
             | Send String [Int] NodeID (DiSeL a)
             | Receive [String] (Maybe Packet -> DiSeL a)
             | This (NodeID -> DiSeL a)

ppDiSeL :: DiSeL a -> String
ppDiSeL (Pure _) = "Pure <val>"
ppDiSeL (Bind ma _) = concat ["Bind(", ppDiSeL ma, ", <Cont>)"]
ppDiSeL (Send tag body to k) = concat ["Send(", tag, ", ", show body, ", ", show to, ", ", ppDiSeL k]
ppDiSeL (Receive tags _) = concat ["Receive(", show tags, ", <Cont>)"]

instance Show a => Show (DiSeL a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = "Bind ma <Continuation>"
  show (Send tag body nodeid k) = concat ["Send ", tag, show body, show nodeid, "(", show k, ")"]
  show (Receive tags _) = concat ["Receive ", show tags, " <Continuation>"]

instance Functor DiSeL where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind ma fb) = Bind ma (fmap f . fb)
  fmap f (Send tag body to k) = Send tag body to (fmap f k)
  fmap f (Receive tags k) = Receive tags (fmap f . k)

instance Applicative DiSeL where
  pure = Pure
  (Pure f) <*> ma = fmap f ma
  (Bind ma fb) <*> mb = Bind ma ((<*> mb) . fb)
  (Send tag body to k) <*> mb = Send tag body to (k <*> mb)
  (Receive tags k) <*> mb = Receive tags ((<*> mb) . k)

instance Monad DiSeL where
  (>>=) = Bind

instance MonadDiSeL DiSeL where
  send tag body receiver = Send tag body receiver (pure ())
  receive tags = Receive tags pure
  this = This pure

stepDiSeL :: NodeID -> [Message] -> DiSeL a -> (Maybe Message, [Message], DiSeL a)
stepDiSeL    _ soup (Pure a) = 
  (Nothing, soup, Pure a)
stepDiSeL nodeID soup (Send tag body to k) =
  (Just $ Message nodeID tag body to, soup, k)
stepDiSeL nodeID soup (Receive tags k) = 
  case pick isMessage soup of
    Nothing -> (Nothing, soup, k Nothing)
    Just (Message{..}, soup') -> (Nothing, soup', k $ Just (_msgTag, _msgBody, _msgFrom))
  where
    isMessage Message{..} = _msgTag `elem` tags && _msgTo == nodeID
stepDiSeL nodeID soup (This k) =
  (Nothing, soup, k nodeID)
stepDiSeL nodeID soup (Bind ma fb) = 
  case ma of
    Pure a -> (Nothing, soup, fb a)
    _ -> 
      let (mmsg, soup', ma') = stepDiSeL nodeID soup ma in
      (mmsg, soup', Bind ma' fb)

pick :: (a -> Bool) -> [a] -> Maybe (a, [a])
pick _     [] = Nothing
pick p (x:xs) = 
  if p x
    then Just (x, xs)
    else (\(a, ys) -> (a, x:ys)) <$> pick p xs

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

type Runner = ReaderT (NodeID, Chan Packet, Map NodeID (Chan Packet)) IO

instance MonadDiSeL Runner where
  send tag body to = do
    (nodeID, _, channels) <- ask
    lift $ writeChan (channels Map.! to) (tag, body, nodeID)
  receive tags = do
    (_, inbox, _) <- ask
    pkt@(tag, _, _) <- lift $ readChan inbox
    if tag `elem` tags
      then return $ Just pkt
      else do
        lift $ writeChan inbox pkt
        return Nothing
  this = do
    (nodeID, _, _) <- ask
    return nodeID

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