module Magus where

import Brick
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import System.Process

import MagusTypes
import UI

app :: App AppState AppEvent ResourceName
app =
  App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const customAttrMap
  }

handleEvent :: AppState -> BrickEvent ResourceName AppEvent -> EventM ResourceName (Next AppState)
handleEvent appState (VtyEvent ev) =
    case ev of
        V.EvKey (V.KChar 'q') [] -> Brick.halt appState
        V.EvKey (V.KChar 's') [] -> do
            changes <- liftIO gitStatus
            let newState = appState { _gitChanges = changes }
            Brick.continue newState
        _                        -> Brick.continue appState
handleEvent appState _ = Brick.continue appState

gitStatus :: IO String
gitStatus = return =<< readProcess "/usr/bin/git" ["status", "-s"] []

customAttrMap :: AttrMap
customAttrMap = attrMap (defAttr) ([])

initialState :: IO AppState
initialState = do
    changes <- gitStatus
    return $ AppState changes
