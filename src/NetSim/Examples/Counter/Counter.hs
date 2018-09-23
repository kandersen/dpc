{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module NetSim.Examples.Counter.Counter where

import NetSim.Types
import NetSim.Specifications
import NetSim.Language

data S = Server Int
       | Client NodeID

tick :: Protlet f S
tick = RPC "Tick" clientStep serverStep
  where
    clientStep = \case
      Client serverID -> Just (serverID, [], const $ Client serverID)
      _ -> Nothing
    serverStep [] (Server n) = Just([], Server $ n + 1)

-- | Implementation

tickServer :: MessagePassing m => Label -> m a 
tickServer lbl = go 0
  where
    go n = do
      Message client _ _ _ _ <- spinReceive [(lbl, "Tick__Request")]
      send client lbl "Tick__Response" []
      go (n + 1)

tickClient :: MessagePassing m => Label -> NodeID -> m ()
tickClient lbl server = do
  [] <- rpcCall lbl "Tick" [] server
  return ()


