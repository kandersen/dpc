{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Rank2Types #-}

module DPC.GUI (
  runGUI,
  runGUIWithInvariant
  ) where

import DPC.Types
import DPC.Specifications
import DPC.Invariant
import DPC.PrettyPrint
import DPC.Util

import Brick
import Brick.Forms
import Brick.Main
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Monad (void, when)
import Graphics.Vty hiding ((<|>))
import Control.Monad.IO.Class
import Lens.Micro

import Data.Map as Map

data ResourceName = ChoiceSelection
                  deriving (Show, Eq, Ord)

data AppState m s = AppState {
  _network :: SpecNetwork [] s,
  _transitions :: [Transition s],
  _form :: Form Int () ResourceName,
  _metadata :: m,
  _invariant :: Invariant m s Bool
  }

handleEvent :: Show s => AppState m s -> BrickEvent ResourceName () -> EventM ResourceName (Next (AppState m s))
handleEvent as (VtyEvent (EvKey KEsc _)) = halt as
handleEvent as (VtyEvent (EvKey KEnter _)) = do
  let selection = formState . _form $ as
  if 0 < selection && selection <= length (_transitions as)
  then do
    let step = _transitions as !! (selection - 1)
    let nextNetwork = applyTransition step (_network as)
    continue $ as {
      _network = nextNetwork,
      _transitions = possibleTransitions nextNetwork <|> possibleCrashes nextNetwork
      }
  else continue as
handleEvent as e = do
  f' <- handleFormEvent e (_form as)
  let selection = formState f'
  let options = length . _transitions $ as
  continue $ as { _form = setFieldValid (0 < selection && selection <= options) ChoiceSelection f' }

enableMouse :: EventM e ()
enableMouse = do
  vty <- Brick.Main.getVtyHandle
  let output = outputIface vty
  when (supportsMode output Mouse) $
    liftIO $ setMode output Mouse True

renderNode :: Show s => (NodeID, Map Label (NodeState s), [Message], Bool) -> Widget ResourceName
renderNode (nodeID, state, inbox, status) =
  borderWithLabel (str $ "Node " ++ show nodeID ++ if status then " -- ONLINE" else " -- OFFLINE") $ vBox
    [ borderWithLabel (str "State") (str $ show state),
      borderWithLabel (str "Inbox") (vBox $ str . ppMessage <$> inbox)]

renderNetwork :: Show s => AppState m s -> [Widget ResourceName]
renderNetwork AppState{..} = return $
  withBorderStyle unicode $ 
        border (str "Invariant satisfied: " <+> (str . show $ _invariant (_metadata, 0, _network)))
    <=> vBox (vCenter . hBox <$> groupsOf 2 [ center . renderNode $ (nodeID, state, inbox,status) |
                                                                    (nodeID, (state,inbox,status)) <- Map.toList $ _localStates _network])
    <=> borderWithLabel (str "Choices") 
          (vBox (fmap (str . show) _transitions))
    <=> borderWithLabel (str "Make a choice by pressing enter while the field reads <n> for 1 to # of options")
          (renderForm _form)

renderNetworkApp :: Show s => App (AppState m s) () ResourceName
renderNetworkApp = App {
  appDraw = renderNetwork,
  appChooseCursor = neverShowCursor,
  appHandleEvent = handleEvent,
  appStartEvent = \s -> enableMouse >> return s,
  appAttrMap = const $ attrMap Graphics.Vty.defAttr []
  }

inputForm :: Int -> Form Int () ResourceName
inputForm = newForm [(str "Choice: " <+>)
                       @@= editShowableField (lens id (flip const)) ChoiceSelection]

runGUIWithInvariant :: Show s => Invariant m s Bool -> m -> SpecNetwork [] s -> IO ()
runGUIWithInvariant inv meta n = void $ defaultMain renderNetworkApp initialState
  where
    initialState = AppState {
      _metadata = meta,
      _invariant = inv,
      _network = n,
      _transitions = possibleTransitions n <|> possibleCrashes n,
      _form = inputForm 0
    }

runGUI :: Show s => SpecNetwork [] s -> IO ()
runGUI = runGUIWithInvariant (const True) ()
