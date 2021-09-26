module MagusTypes where

type AppEvent = ()
type ResourceName = String

data AppState = AppState {
                  _gitChanges :: String
                }
