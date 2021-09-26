module MagusTypes where

import Data.List

type AppEvent = ()
type ResourceName = String

data AppState = AppState {
                  _repo :: String
                , _currentBranch :: String
                , _upToDate :: UpToDateStatus
                , _changes :: [Change]
                , _diff :: String
                }

data UpToDateStatus = UpToDate
                    | Ahead Int
                    | Behind Int

instance Show UpToDateStatus where
    show (UpToDate) = "(Up to date)"
    show (Ahead i)  = "(Ahead by " ++ (show i) ++ " commits)"
    show (Behind i) = "(Behind by " ++ (show i) ++ " commits)"

data Status = Modified
            | Added
            | Deleted
            | Renamed
            | Copied
            | Updated
            | Untracked
            | Unmodified
            deriving(Eq)

-- Effectively "read" instance for Status
makeStatus :: Char -> Status
makeStatus 'M' = Modified
makeStatus 'A' = Added
makeStatus 'D' = Deleted
makeStatus 'R' = Renamed
makeStatus 'C' = Copied
makeStatus 'U' = Updated
makeStatus '?' = Untracked
makeStatus ' ' = Unmodified

instance Show Status where
    show Modified   = "M"
    show Added      = "A"
    show Deleted    = "D"
    show Renamed    = "R"
    show Copied     = "C"
    show Updated    = "U"
    show Untracked  = "?"
    show Unmodified = " "

type IndexStatus = Status
type WorkingTreeStatus = Status

data Change = Change {
                _index :: IndexStatus
              , _workingTree :: WorkingTreeStatus
              , _files :: [FilePath]
              }

instance Show Change where
    show (Change i wt fp) = (show i) ++ (show wt) ++ " " ++ (intercalate "->" fp)
