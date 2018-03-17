{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module NetSim.Language where

import NetSim.Core
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Foldable
import Control.Monad.Reader
import Control.Concurrent.Chan

type Packet = (String, [Int], NodeID)

class Monad m => MonadDiSeL m where
  send :: String -> [Int] -> NodeID -> m ()
  receive :: [String] -> m (Maybe Packet)

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

stepDiSeL :: NodeID -> [Message] -> DiSeL a -> (Maybe Message, [Message], DiSeL a)
stepDiSeL    _ soup (Pure a) = 
  (Nothing, soup, Pure a)
stepDiSeL this soup (Send tag body to k) =
  (Just $ Message this tag body to, soup, k)
stepDiSeL this soup (Receive tags k) = 
  case pick isMessage soup of
    Nothing -> (Nothing, soup, k Nothing)
    Just (Message{..}, soup') -> (Nothing, soup', k $ Just (_msgTag, _msgBody, _msgFrom))
  where
    isMessage Message{..} = _msgTag `elem` tags && _msgTo == this
stepDiSeL this soup (Bind ma fb) = 
  case ma of
    Pure a -> (Nothing, soup, fb a)
    _ -> 
      let (mmsg, soup', ma') = stepDiSeL this soup ma in
      (mmsg, soup', Bind ma' fb)

pick :: (a -> Bool) -> [a] -> Maybe (a, [a])
pick _     [] = Nothing
pick p (x:xs) = 
  if p x
    then Just (x, xs)
    else (\(a, ys) -> (a, x:ys)) <$> pick p xs

data Configuration a = Configuration {
  _confNodes :: [NodeID],
  _confNodeStates :: Map NodeID (DiSeL a),
  _confSoup :: [Message]
  }
  deriving Show

ppConf :: Show a => Configuration a -> String
ppConf Configuration{..} = unlines $ ("Soup: " ++ show _confSoup) :
   [ concat [show nodeid, ": ", ppDiSeL' state] | (nodeid, state) <- Map.toList _confNodeStates ]
  where
    ppDiSeL' (Pure a) = "Returned " ++ show a
    ppDiSeL' a = ppDiSeL a

runPure :: Configuration a -> [(Maybe Message, Configuration a)]
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

tpcCoordinator :: MonadDiSeL m => Int -> [NodeID] -> m a
tpcCoordinator n participants = do
  resps <- broadcast "Prepare" [] participants
  _ <- if any isReject resps
    then broadcast "Decide" [0] participants
    else broadcast "Decide" [1] participants
  tpcCoordinator (n + 1) participants
  where 
    isReject (_, [0], _) = True
    isReject          _  = False

tpcClient :: MonadDiSeL m => Int -> Int -> m a
tpcClient n b = do
  (tag, body, server) <- spinReceive ["Prepare__Broadcast", "Decide__Broadcast"]
  case (tag, body) of
      ("Prepare__Broadcast", []) ->
        if n `mod` b == 0
        then send "Prepare__Response" [1] server
        else send "Prepare__Response" [0] server
  tpcClient (n + 1) b
  
  
calculatorServer :: MonadDiSeL m => m a
calculatorServer = do
  (_, [x, y], client) <- spinReceive ["Compute__Request"]
  send "Compute__Response" [x + y] client
  calculatorServer

calculatorClient :: MonadDiSeL m => NodeID -> m Int
calculatorClient server = do
  [x] <- rpcCall "Compute" [40, 2] server
  return x

calcConfiguration :: Configuration Int
calcConfiguration = Configuration {
  _confNodes = [0, 1, 2],
  _confNodeStates = Map.fromList [
                        (0, calculatorServer)
                      , (1, calculatorClient 0)
                      , (2, calculatorClient 0) 
                      ],
  _confSoup = []
}

stepThrough :: (a -> String) -> [a] -> IO ()
stepThrough format (x:xs) = do
  putStrLn $ format x
  _ <- getLine
  stepThrough format xs

type Runner = ReaderT (NodeID, Chan Packet, Map NodeID (Chan Packet)) IO

instance MonadDiSeL Runner where
  send tag body to = do
    (this, _, channels) <- ask
    lift $ writeChan (channels Map.! to) (tag, body, this)
  receive tags = do
    (_, inbox, _) <- ask
    pkt@(tag, _, _) <- lift $ readChan inbox
    if tag `elem` tags
      then return $ Just pkt
      else do
        lift $ writeChan inbox pkt
        return Nothing

runNetworkIO :: [(NodeID, Runner a)] -> IO a
runNetworkIO network = do
  envs <- (sequence :: [IO (NodeID, Chan Packet, Runner a)] -> IO [(NodeID, Chan Packet, Runner a)]) $ do
    (nodeID, code) <- network
    return $ do
      inbox <- newChan
      return (nodeID, inbox, code)
  let mapping = Map.fromList [(nodeID, inbox) | (nodeID, inbox, _) <- envs]
  undefined