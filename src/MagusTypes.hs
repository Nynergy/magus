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
    show (UpToDate) = "(Up to date)"
    show (Ahead i)  = "(Ahead by " ++ (show i) ++ " commits)"
    show (Behind i) = "(Behind by " ++ (show i) ++ " commits)"
