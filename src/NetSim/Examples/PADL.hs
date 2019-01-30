{-# LANGUAGE FlexibleContexts    #-}
module NetSim.Examples.PADL where

import NetSim.Types
import NetSim.Specifications
import NetSim.Language
import NetSim.Interpretations.Pure
import Test.QuickCheck 

data S = ClientInit NodeID [Int]
       | ClientDone [Int]

       | ServerReady 
       | ServerReceived NodeID [Int]       
       deriving (Show, Eq)

compute :: Alternative f => ([Int] -> Int) -> Protlet f S
compute f = ARPC "compute" clientStep serverRec serverSend
  where
    clientStep s = case s of
        ClientInit server xs -> Just (server, xs, ClientDone)
        _ -> Nothing
    
    serverRec :: Message -> S -> Maybe S
    serverRec msg s = case s of
        ServerReady -> Just $ ServerReceived (_msgFrom msg) (_msgBody msg)
        _ -> Nothing
    
    serverSend this s = case s of
        ServerReceived client xs -> pure (mkResp client this (f xs), ServerReady)
        _ -> empty
    
    mkResp :: NodeID -> NodeID -> Int -> Message
    mkResp from t payload = Message {
        _msgFrom = t,
        _msgTo = from,
        _msgTag = "compute__Response",
        _msgBody = [payload],
        _msgLabel = 0
    }
        

addNetwork :: Alternative f => Int -> Int -> SpecNetwork f S
addNetwork x y = initializeNetwork nodeStates protlets
  where
    nodeStates = [ (0, [(0, ServerReady)])
                 , (1, [(0, ClientInit 0 [x, y])])
                 ]
    protlets   = [ (0, [compute sum])]

addClient :: (MessagePassing m, ProtletAnnotations S m) => Label -> NodeID -> Int -> Int -> m [Int]
addClient label server a b = 
    enactingClient (compute sum) $ 
      rpcCall label "compute" [a, b] server

addServer :: (MessagePassing m, ProtletAnnotations S m) => Label -> m a
addServer label = do
    enactingServer (compute sum) $ do
        Message client _ args _ _ <- spinReceive [(label, "compute__Request")]
        send client label "compute__Response" [sum   args]
    addServer label

addConf :: (MessagePassing m, ProtletAnnotations S m) => Int -> Int -> ImplNetwork m [Int]
addConf x y = initializeImplNetwork [
            (1, addClient 0 0 x y)
          , (0, addServer 0) 
          ]


prop_simpleAddNetwork :: Int -> Int -> Bool
prop_simpleAddNetwork x y = 
    let trace = take 100 $ roundRobinTrace (addConf x y) in
        checkTrace (addNetwork x y) trace == Right ()