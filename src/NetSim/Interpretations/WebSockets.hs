{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleInstances          #-}
module NetSim.Interpretations.WebSockets where

import           Data.Foldable
import           Data.List                  (delete)
import           Data.Map                   (Map, (!))
import qualified Data.Map                   as Map

import           Control.Exception          (bracket, catch)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State

import           System.Socket              as Socket
import           System.Socket.Family.Inet
import           System.Socket.Protocol.TCP
import           System.Socket.Type.Stream

import           Control.Concurrent
import           Control.Concurrent.STM

import           NetSim.Types
import           NetSim.Language

import           Lens.Micro.Mtl
import           Lens.Micro.TH

import           Data.ByteString            (ByteString)
import           Data.Serialize

type DSocket = Socket Inet Stream TCP

type NetworkDescription = Map NodeID (SocketAddress Inet)

parseSocketAddr :: (ByteString, ByteString) -> IO (SocketAddress Inet)
parseSocketAddr (bs, port) = socketAddress . head <$> (getAddressInfo (Just bs) (Just port) mempty :: IO [AddressInfo Inet Stream TCP])

parseNetworkDescription :: String -> IO NetworkDescription
parseNetworkDescription input = do
  let rawMap :: Map NodeID (ByteString, ByteString) = read input
  traverse parseSocketAddr rawMap

networkDescriptionFromFile :: FilePath -> IO NetworkDescription
networkDescriptionFromFile fp = readFile fp >>= parseNetworkDescription

data NetworkContext = NetworkContext {
    _this        :: NodeID,
    _addressBook :: Map NodeID DSocket,
    _inbox       :: TChan Message
}
makeLenses ''NetworkContext

establishMesh :: NodeID -> NetworkDescription -> IO NetworkContext
establishMesh thisID nd = do
  mySocket <- socket :: IO DSocket
  setSocketOption mySocket (ReuseAddress True)
  setSocketOption mySocket (KeepAlive True)
  bind mySocket (nd ! thisID)
  listen mySocket (Map.size nd - 1)
  putStrLn $ "Listening on " ++ show (nd ! thisID) ++ "..."
  emptyInbox <- newTChanIO
  res <- execStateT (go mySocket $ Map.keys nd) (NetworkContext thisID Map.empty emptyInbox)
  close mySocket
  return res
    where
      go :: DSocket -> [NodeID] -> StateT NetworkContext IO ()
      go        _         [] = return ()
      go mySocket (nid:nids) = do
        if nid == thisID
          then do
            lift $ putStrLn "Accepting Peers"
            acceptConnectionForPeers mySocket (delete thisID $ Map.keys nd)
          else do
            lift . putStrLn $ "Connecting to " ++ show nid
            peerSocket <-lift $ retryConnect (nd ! nid)
            lift . void $ Socket.send peerSocket (encode thisID) mempty
            addressBook %= Map.insert nid peerSocket
        go mySocket nids

      retryConnect :: SocketAddress Inet -> IO DSocket
      retryConnect a = do
        peerSocket :: DSocket <- socket
        (Socket.connect peerSocket a >> return peerSocket) `catch` \(_ :: SocketException) -> close peerSocket >> retryConnect a

      acceptConnectionForPeers :: DSocket -> [NodeID] -> StateT NetworkContext IO ()
      acceptConnectionForPeers        _ [] = return ()
      acceptConnectionForPeers mySocket ps = do
        (peerSocket, peerAddr) <- lift $ accept mySocket
        lift $ setSocketOption peerSocket (KeepAlive True)
        peerID <- lift $ either error id . decode <$> Socket.receive peerSocket 8 mempty
        lift . putStrLn $ "Accepted connection from " ++ show peerID ++ " @ " ++ show peerAddr
        addressBook %= Map.insert peerID peerSocket
        acceptConnectionForPeers mySocket (delete peerID ps)

newtype SocketRunnerT m a = SocketRunnerT {
    runSocketRunnerT :: ReaderT NetworkContext m a }
    deriving (
      Functor,
      Applicative,
      Monad,
      MonadTrans,
      MonadIO,
      MonadReader NetworkContext
    )

instance Monad m => NetworkNode (SocketRunnerT m) where
  this = _this <$> ask

instance ProtletAnnotations s (SocketRunnerT m) where
  enactingServer _ m = m
  enactingClient _ m = m

instance (MonadIO m) => MessagePassing (SocketRunnerT m) where
  send to lbl tag body = do
    thisID <- NetSim.Language.this
    let p = encode $ Message thisID tag body to lbl
    peerSocket <- (!to) <$> view addressBook
    void . liftIO $ Socket.send peerSocket p mempty
    
  receive candidates = do
    inboxChan <- view inbox
    mmsg <- liftIO . atomically $ tryReadTChan inboxChan
    case mmsg of
      Just msg@Message{..} | (Just _) <- find (\(lbl,t) -> _msgLabel == lbl && _msgTag == t) candidates ->
                               return mmsg
                           | otherwise -> do
                               liftIO . atomically $ writeTChan inboxChan msg
                               return Nothing
      Nothing -> return Nothing

type SocketRunner = SocketRunnerT IO

run :: NetworkContext -> SocketRunner a -> IO a
run ctxt p = runReaderT (runSocketRunnerT p) ctxt

mailman :: SocketRunner ()
mailman = do
  sockets <- view addressBook
  p <- findMessageAmongstSockets $ Map.elems sockets
  inb <- view inbox
  liftIO . atomically . writeTChan inb $ p
  mailman
  where
    findMessageAmongstSockets :: [DSocket] -> SocketRunner Message
    findMessageAmongstSockets sockets = go sockets
      where
        go []     = findMessageAmongstSockets sockets
        go (s:ss) = do
          mmsg :: Either String Message <- liftIO $ decode <$> Socket.receive s 1024 mempty
          case mmsg of
            Right m -> return m
            Left _  -> go ss

runP2P :: Show a => FilePath -> NodeID -> SocketRunner a -> IO ()
runP2P ndPath thisID program = do
  nd <- networkDescriptionFromFile ndPath
  bracket (establishMesh thisID nd) releaseNetworkContext $ \netctxt -> do
    mailmanThread <- forkIO $ run netctxt mailman
    print =<< run netctxt program
    killThread mailmanThread

  where
    releaseNetworkContext = traverse_ Socket.close . Map.elems . _addressBook
