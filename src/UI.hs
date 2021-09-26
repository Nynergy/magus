module UI where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center

import MagusTypes

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
drawRepoInfo appState = vLimit 3 $ borderWithLabel (str "Repo Info") $
                        padRightAndBottom Max $ hCenter $ repoInfo
                            where
                                repoInfo     = hBox [ upToDateInfo
                                                    , repoName
                                                    , branchName
                                                    ]
                                upToDateInfo = str ((show $ _upToDate appState) ++ " :: ")
                                repoName     = str (_repo appState ++ " -> ")
                                branchName   = str (_currentBranch appState)

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
