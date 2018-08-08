{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module NetSim.Interpretations.WebSockets where

import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.List (delete)
import Data.Foldable

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (catch, bracket)

import System.Socket as Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

import NetSim.Core
import NetSim.Language

import Lens.Micro.TH
import Lens.Micro.Mtl

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Serialize

type DSocket = Socket Inet Stream TCP

type NetworkDescription = Map NodeID (SocketAddress Inet)

data NetworkContext = NetworkContext {
    _this :: NodeID,
    _addressBook :: Map NodeID DSocket
}
makeLenses ''NetworkContext

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

instance (MonadIO m, Monad m) => MessagePassing (SocketRunnerT m) where
  this = _this <$> ask
  send (lbl,tag,body,to) = do
    let p = encode (lbl, tag, body)
    let n = BS.length p
    peerSocket <- (!to) <$> view addressBook
    sent <- liftIO $ Socket.send peerSocket p mempty
    when (n /= sent) $
      liftIO . putStrLn $ "Whoops, sent " ++ show sent ++ " but expected " ++ show n ++ "."

  receive lbl tags = do
    undefined

type SocketRunner = SocketRunnerT IO

run :: NetworkContext -> SocketRunner a -> IO a
run ctxt p = runReaderT (runSocketRunnerT p) ctxt

establishMesh :: NodeID -> NetworkDescription -> IO NetworkContext
establishMesh thisID nd = do
  mySocket <- socket :: IO DSocket
  setSocketOption mySocket (ReuseAddress True)
  setSocketOption mySocket (KeepAlive True)
  bind mySocket (nd ! thisID)
  listen mySocket (Map.size nd - 1)
  print $ "Listening on " ++ show (nd ! thisID) ++ "..."
  execStateT (go mySocket $ Map.keys nd) (NetworkContext thisID Map.empty)
  where
    go :: DSocket -> [NodeID] -> StateT NetworkContext IO ()
    go        _ [] = return ()
    go mySocket (nid:nids) = do
      if nid == thisID
        then do 
          lift $ print "My turn!"
          acceptConnectionForPeers mySocket (delete thisID $ Map.keys nd)
        else do
          lift . print $ "Connecting to " ++ show nid
          peerSocket <-lift $ retryConnect (nd ! nid)
          sentBytes <- lift $ Socket.send peerSocket (encode thisID) mempty
          lift . print $ "Sent " ++ show sentBytes ++ " to peer!"
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
      lift $ print ("Accepted connection from " ++ show peerID ++ " @ " ++ show peerAddr)
      addressBook %= Map.insert peerID peerSocket
      acceptConnectionForPeers mySocket (delete peerID ps)

parseSocketAddr :: (ByteString, ByteString) -> IO (SocketAddress Inet)
parseSocketAddr (bs, port) = socketAddress . head <$> (getAddressInfo (Just bs) (Just port) mempty :: IO [AddressInfo Inet Stream TCP])

parseNetworkDescription :: String -> IO NetworkDescription
parseNetworkDescription input = do
  let rawMap :: Map NodeID (ByteString, ByteString) = read input
  traverse parseSocketAddr rawMap

releaseNetworkContext :: NetworkContext -> IO ()
releaseNetworkContext = traverse_ Socket.close . Map.elems . _addressBook

defaultMain :: NodeID -> SocketRunner a -> IO ()
defaultMain thisID program = do
  -- get peer addresses from file
  (nd :: NetworkDescription) <- parseNetworkDescription =<< readFile "network.desc"
  print nd
  -- create mesh network of socket connections
  bracket (establishMesh thisID nd) releaseNetworkContext $ \netctxt ->
    void $ run netctxt program
  