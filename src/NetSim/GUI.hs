{-# LANGUAGE RecordWildCards, TemplateHaskell #-}

module NetSim.GUI where

import NetSim.Core
import Brick
import Brick.Forms
import Brick.Main
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style
import Control.Monad (void, when)
import Graphics.Vty
import Control.Monad.IO.Class
import Lens.Micro
import Lens.Micro.TH

import Data.Map as Map

data ResourceName = ChoiceSelection
                  deriving (Show, Eq, Ord)

data AppState s = AppState {
  _network :: Network [] s,
  _transitions :: [Transition s],
  _form :: Form Int () ResourceName
  }
makeLenses ''AppState

handleEvent :: Show s => AppState s -> BrickEvent ResourceName () -> EventM ResourceName (Next (AppState s))
handleEvent as (VtyEvent (EvKey KEsc _)) = halt as
handleEvent as (VtyEvent (EvKey KEnter _)) = do
  let selection = formState . _form $ as
  if 0 < selection && selection <= length (_transitions as)
  then do
    let step = _transitions as !! (selection - 1)
    let nextNetwork = applyTransition step (_network as)
    continue $ as {
      _network = nextNetwork,
      _transitions = possibleTransitions nextNetwork
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

renderNode :: Show s => (NodeID, NodeState s, [Message]) -> Widget ResourceName
renderNode (nodeID, state, inbox) =
  borderWithLabel (str $ "Node " ++ show nodeID) $
    center (str $ show state)
    <=> center (str $ show inbox)

renderNetwork :: Show s => AppState s -> [Widget ResourceName]
renderNetwork AppState{..} = return $
  withBorderStyle unicode $
    vBox (renderNode <$> [ (nodeID, state, inbox) |
                           (nodeID, state) <- Map.toList $ _states _network,
                           (nodeID', inbox) <- Map.toList $ _inboxes _network,
                           nodeID == nodeID'])
    <=> vBox (fmap (str . show) _transitions)
    <=> renderForm _form

renderNetworkApp :: Show s => App (AppState s) () ResourceName
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

runGUI :: Show s => Network [] s -> IO ()
runGUI n = void $ defaultMain renderNetworkApp initialState
  where
    initialState = AppState {
      _network = n,
      _transitions = possibleTransitions n,
      _form = inputForm 0
    }

