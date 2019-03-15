{-# LANGUAGE RecordWildCards #-}
module DPC.PrettyPrint where

import DPC.Types
import DPC.Specifications
import Data.List
import DPC.Interpretations.Pure

ppMessage :: Message -> String
ppMessage Message{..} = concat [
  _msgTag
  , "(from "
  , show _msgFrom
  , ", "
  , show _msgBody
  , ", to "
  , show _msgTo
  , ", label "
  , show _msgLabel
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

ppNetworkState :: (g -> String) -> (l -> String) -> NetworkState g l -> String
ppNetworkState = undefined

ppDiSeL :: DiSeL t a -> String
ppDiSeL (Pure _) = "Pure <val>"
ppDiSeL (Bind ma _) = concat ["Bind(", ppDiSeL ma, ", <Cont>)"]
ppDiSeL (Send to label tag body k) = concat ["Send[", show label, ", ", tag, "](", show body, ", ", show to, ", ", ppDiSeL k]
ppDiSeL (Receive candidates _) = concat ["Receive[", show candidates, "}] <Cont>)"]
ppDiSeL (This _) = "This <Cont>"
ppDiSeL (Par mas _) = "Par [" ++ intercalate "," (ppDiSeL . snd <$> mas) ++ "]"
