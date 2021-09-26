module Magus where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Control.Monad.IO.Class
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes

import System.Process

type AppEvent = ()
type ResourceName = String

data AppState = AppState {
                  _gitChanges :: String
                }

app :: App AppState AppEvent ResourceName
app =
  App
  { appDraw = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleEvent
  , appStartEvent = return
  , appAttrMap = const customAttrMap
  }

drawUI :: AppState -> [Widget ResourceName]
drawUI appState = [
                    (hLimitPercent 40 $
                    vBox [
                           drawRepoInfo appState
                         , drawStagedChanges appState
                         , drawUnstagedChanges appState
                         ])
                    <+>
                    vBox [
                           drawFileDiff appState
                         , hBox [
                                  drawCommitLog appState
                                , drawBranches appState
                                ]
                         ]
                  ]

drawRepoInfo :: AppState -> Widget ResourceName
drawRepoInfo appState = vLimit 3 $ simplePanel "Repo Info"

drawStagedChanges :: AppState -> Widget ResourceName
drawStagedChanges appState = simplePanel "Staged Changes"

drawUnstagedChanges :: AppState -> Widget ResourceName
drawUnstagedChanges appState = simplePanel "Unstaged Changes"

drawFileDiff :: AppState -> Widget ResourceName
drawFileDiff appState = vLimitPercent 70 $ simplePanel "Diff"

drawCommitLog :: AppState -> Widget ResourceName
drawCommitLog appState = hLimitPercent 60 $ simplePanel "Commit Log"

drawBranches :: AppState -> Widget ResourceName
drawBranches appState = simplePanel "Branches"

simplePanel :: String -> Widget ResourceName
simplePanel title = borderWithLabel (str title) $
                    padRightAndBottom Max $
                    str " "

padRightAndBottom :: Padding -> Widget ResourceName -> Widget ResourceName
padRightAndBottom padding = padRight padding . padBottom padding

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
