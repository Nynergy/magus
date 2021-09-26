module MagusTypes where

type AppEvent = ()
type ResourceName = String

data AppState = AppState {
                  _repo :: String
                , _currentBranch :: String
                , _upToDate :: UpToDateStatus
                }

data UpToDateStatus = UpToDate
                    | Ahead Int
                    | Behind Int

instance Show UpToDateStatus where
    show (UpToDate) = "(Branch up to date)"
    show (Ahead i)  = "(Branch ahead by " ++ (show i) ++ " commits)"
    show (Behind i) = "(Branch behind by " ++ (show i) ++ " commits)"
