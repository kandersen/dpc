{-# LANGUAGE RecordWildCards #-}
module NetSim.PrettyPrint where

import NetSim.Core

ppMessage :: Message -> String
ppMessage Message{..} = concat [
  _msgTag
  , "(from "
  , show _msgFrom
  , ", "
  , show _msgBody
  , ", to "
  , show _msgTo
  , ")"
  ]

ppProtocol :: Protlet f s -> String
ppProtocol (RPC name _ _) =
    unwords [ "RPC(", name, ")" ]
ppProtocol (ARPC name _ _ _) =
    unwords [ "ARPC(", name, ")" ]
ppProtocol (Notification name _ _) =
    unwords [ "Notification(", name, ")" ]
ppProtocol (Broadcast name _ _ _) =
    unwords [ "BroadcastQourom(", name, ")" ]

ppNetwork :: (s -> String) -> Network f s -> String
ppNetwork _ _ = "Network"
