{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ExistentialQuantification #-}
module NetSim.Language where

import NetSim.Core
import qualified Data.Map as Map

class (Monad m) => MonadDiSeL m where
  send :: String -> Message -> m ()
  receive :: [String] -> m (Maybe Message)

rpcCall :: MonadDiSeL m =>
  String -> Message -> m Message
rpcCall protlet msg = do
  send (protlet ++ "__Request") msg
  spinForResponse
  where
    spinForResponse = do
      mresp <- receive [protlet ++ "__Response"]
      case mresp of
        Nothing -> spinForResponse
        Just resp -> return resp

rpcRespond :: MonadDiSeL m =>
  String -> (Message -> Message) -> m ()
rpcRespond protlet handler = do
  req <- spinForRequest
  send (protlet ++ "__Response") (handler req)
  where
    spinForRequest = do
      mreq <- receive [protlet ++ "__Request"]
      case mreq of
        Nothing -> spinForRequest
        Just req -> return req

arpcReceive :: MonadDiSeL m =>
  String -> m Message
arpcReceive protlet = do 
  spinForRequest
  where
    spinForRequest = do
      mreq <- receive [protlet ++ "__Request"]
      case mreq of
        Nothing -> spinForRequest
        Just req -> return req

arpcRespond :: MonadDiSeL m =>
  String -> Message -> m ()
arpcRespond protlet = send (protlet ++ "__Response")

broadcast :: MonadDiSeL m =>
  String -> Message -> [NodeID] -> m [(NodeID, Message)]
broadcast protlet msg receivers = do
  sendBroadcasts receivers
  spinForResponses Map.empty receivers
  where
    sendBroadcasts [] = return ()
    sendBroadcasts (r:rs) = do
      let msg' = msg { _msgTo = r}
      send (protlet ++ "__Broadcast") msg'
      sendBroadcasts rs      
    spinForResponses resps [] = return $ Map.toList resps
    spinForResponses resps rs = do
      mresp <- receive [protlet ++ "__Response"]
      case mresp of
        Nothing -> spinForResponses resps rs
        Just resp@Message{..} -> do
          let rs' = filter (/= _msgFrom) rs
          let resps' = Map.insert _msgFrom resp resps
          spinForResponses resps' rs'

data DiSeL a = Pure a 
             | forall b. Bind (DiSeL b) (b -> DiSeL a)
             | Send String Message (DiSeL a)
             | Receive [String] (Maybe Message -> DiSeL a)

instance Functor DiSeL where
  fmap f (Pure a) = Pure (f a)
  fmap f (Bind ma fb) = Bind ma (fmap f . fb)
  fmap f (Send tag msg k) = Send tag msg (fmap f k)
  fmap f (Receive tags k) = Receive tags (fmap f . k)

instance Applicative DiSeL where
  pure = Pure
  (Pure f) <*> ma = fmap f ma
  (Bind ma fb) <*> mb = Bind ma ((<*> mb) . fb)
  (Send tag msg k) <*> mb = Send tag msg (k <*> mb)
  (Receive tags k) <*> mb = Receive tags ((<*> mb) . k)

instance Monad DiSeL where
  (>>=) = Bind

instance MonadDiSeL DiSeL where
  send tag msg = Send tag msg (pure ())
  receive tags = Receive tags pure

