{-# LANGUAGE RecordWildCards #-}
module NetSim.PrettyPrint where

import NetSim.Core
import NetSim.Language
import Data.Map as Map
import Data.List
import NetSim.Interpretations.Pure

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

ppNetwork :: (s -> String) -> Network m s -> String
ppNetwork _ _ = "Network"

ppConf :: Show a => Configuration DiSeL a -> String
ppConf Configuration{..} = unlines $ ("Soup: " ++ show _confSoup) :
   [ concat [show nodeid, ": ", ppDiSeL' state] | (nodeid, state) <- Map.toList _confNodeStates ]
  where
    ppDiSeL' (Pure a) = "Returned " ++ show a
    ppDiSeL' a = ppDiSeL a

ppDiSeL :: DiSeL a -> String
ppDiSeL (Pure _) = "Pure <val>"
ppDiSeL (Bind ma _) = concat ["Bind(", ppDiSeL ma, ", <Cont>)"]
ppDiSeL (Send label tag body to k) = concat ["Send[", show label, ", ", tag, "](", show body, ", ", show to, ", ", ppDiSeL k]
ppDiSeL (Receive label tags _) = concat ["Receive[", show label, ", {", show tags, "}] <Cont>)"]
ppDiSeL (This _) = "This <Cont>"
ppDiSeL (Par mas _) = "Par [" ++ intercalate "," (ppDiSeL . snd <$> mas) ++ "]"
