{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module NetSim.Language where

import NetSim.Core
import qualified Data.Map as Map
import Data.Map (Map)

class Monad m => MonadDiSeL m where
  send :: String -> [Int] -> NodeID -> m ()
  receive :: [String] -> m (Maybe (String, [Int], NodeID))

spinReceive :: MonadDiSeL m => [String] -> m (String, [Int], NodeID)
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

arpcReceive :: MonadDiSeL m =>
  String -> m (String, [Int], NodeID)
arpcReceive protlet = do 
  spinForRequest
  where
    spinForRequest = do
      mreq <- receive [protlet ++ "__Request"]
      case mreq of
        Nothing -> spinForRequest
        Just req -> return req

arpcRespond :: MonadDiSeL m =>
  String -> [Int] -> NodeID -> m ()
arpcRespond protlet = send (protlet ++ "__Response")

broadcast :: (MonadDiSeL m) =>
  String -> [Int] -> [NodeID] -> m [(String, [Int], NodeID)]
broadcast protlet body receivers = do
  sendBroadcasts receivers
  spinForResponses Map.empty receivers
  where
    sendBroadcasts [] = return ()
    sendBroadcasts (r:rs) = do
      send (protlet ++ "__Broadcast") body r
      sendBroadcasts rs      
    spinForResponses resps [] = return . format . Map.toList $ resps
    spinForResponses resps rs = do
      mresp <- receive [protlet ++ "__Response"]
      case mresp of
        Nothing -> spinForResponses resps rs
        Just (tag, bod, sender) -> do
          let rs' = filter (/= sender) rs
          let resps' = Map.insert sender (tag, bod) resps
          spinForResponses resps' rs'
    format res = [(tag, bod, sender) | (sender, (tag, bod)) <- res ]

data DiSeL a = Pure a
             | forall b. Bind (DiSeL b) (b -> DiSeL a)
             | Send String [Int] NodeID (DiSeL a)
             | Receive [String] (Maybe (String, [Int], NodeID) -> DiSeL a)

ppDiSeL :: DiSeL a -> String
ppDiSeL (Pure _) = "Pure <val>"
ppDiSeL (Bind ma _) = concat ["Bind(", ppDiSeL ma, ", <Cont>)"]
ppDiSeL (Send tag body to k) = concat ["Send(", tag, ", ", show body, ", ", show to, ", ", ppDiSeL k]
ppDiSeL (Receive tags _) = concat ["Receive(", show tags, ", <Cont>)"]

instance Show a => Show (DiSeL a) where
  show (Pure a) = "Pure " ++ show a
  show (Bind _ _) = concat ["Bind ma <Continuation>"]
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
ppConf Configuration{..} = unlines $ concat ["Soup: ", show _confSoup] :
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
  _confNodes = [0, 1],
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