module Magus where

import Brick
import Control.Monad.IO.Class
import Data.List
import Data.List.Split
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes
import System.Process

import MagusTypes
import UI

app :: App AppState AppEvent ResourceName
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const customAttrMap
          }

handleEvent :: AppState -> BrickEvent ResourceName AppEvent -> EventM ResourceName (Next AppState)
handleEvent appState (VtyEvent ev) =
    case ev of
        V.EvKey (V.KChar 'q') [] -> Brick.halt appState
        _                        -> Brick.continue appState
handleEvent appState _ = Brick.continue appState

customAttrMap :: AttrMap
customAttrMap = attrMap (defAttr) ([])

initialState :: IO AppState
initialState = do
    repoPath <- gitRepo
    let repo = repoFromPath repoPath
    status <- gitStatus
    let branch = branchFromStatus status
    let upToDate = upToDateFromStatus status
    shortStatus <- gitShortStatus
    let changes = changesFromShortStatus shortStatus
    diff <- gitDiff
    return $ AppState repo branch upToDate changes diff

gitRepo :: IO String
gitRepo = return =<< readProcess "/usr/bin/git" ["rev-parse", "--show-toplevel"] []

repoFromPath :: FilePath -> String
repoFromPath path = init $ last $ splitOn "/" path

gitStatus :: IO String
gitStatus = return =<< readProcess "/usr/bin/git" ["status"] []

branchFromStatus :: String -> String
branchFromStatus status = last $ words $ head $ lines status

upToDateFromStatus :: String -> UpToDateStatus
upToDateFromStatus status =
    let l = head $ drop 1 $ lines status
    in case "up to date" `isInfixOf` l of
        True  -> UpToDate
        False -> let direction = (words l) !! 3
                     amount    = read (last $ init $ words l) :: Int
                 in  if direction == "behind" then Behind amount else Ahead amount

gitShortStatus :: IO String
gitShortStatus = return =<< readProcess "/usr/bin/git" ["status", "-s"] []

changesFromShortStatus :: String -> [Change]
changesFromShortStatus ss = map changeFromStatusLine $ lines ss

changeFromStatusLine :: String -> Change
changeFromStatusLine s =
    let indexChar       = s !! 0
        workingTreeChar = s !! 1
        filepaths       = filter (/= "->") $ tail $ words s
    in  Change { _index       = makeStatus indexChar
               , _workingTree = makeStatus workingTreeChar
               , _files       = filepaths
               }

gitDiff :: IO String
gitDiff = return =<< readProcess "/usr/bin/git" ["diff"] []
