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
drawStagedChanges appState = borderWithLabel (str "Staged Changes") $
                             padRightAndBottom Max $ stagedChanges <=> str " "
                                where
                                    stagedChanges = vBox $ map (str . show) files
                                    files         = filter isStaged $ _changes appState
                                    isStaged      = (\c -> (_index c) /= Unmodified && (_index c) /= Untracked)

drawUnstagedChanges :: AppState -> Widget ResourceName
drawUnstagedChanges appState = borderWithLabel (str "Unstaged Changes") $
                               padRightAndBottom Max $ unstagedChanges <=> str " "
                                where
                                    unstagedChanges = vBox $ map (str . show) files
                                    files           = filter isUnstaged $ _changes appState
                                    isUnstaged      = (\c -> (_workingTree c) /= Unmodified)

-- TODO: At the moment, this draws the entire diff. Eventually, we want only a single file.
drawFileDiff :: AppState -> Widget ResourceName
drawFileDiff appState = vLimitPercent 70 $ borderWithLabel (str "Diff") $
                        padRightAndBottom Max $ fileDiff
                            where
                                fileDiff = str $ (_diff appState) ++ " "

drawCommitLog :: AppState -> Widget ResourceName
drawCommitLog appState = hLimitPercent 60 $ borderWithLabel (str "Commit Log") $
                         padRightAndBottom Max $ commitLog
                            where
                                commitLog = str $ (_commits appState) ++ " "

drawBranches :: AppState -> Widget ResourceName
drawBranches appState = simplePanel "Branches"

simplePanel :: String -> Widget ResourceName
simplePanel title = borderWithLabel (str title) $
                    padRightAndBottom Max $
                    str " "

padRightAndBottom :: Padding -> Widget ResourceName -> Widget ResourceName
padRightAndBottom padding = padRight padding . padBottom padding
