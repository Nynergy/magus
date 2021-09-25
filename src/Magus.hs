module Magus where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Graphics.Vty.Attributes

type AppState = ()
type AppEvent = ()
type ResourceName = String

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
drawUI _ = [ center $ (str "Hello" <=> str "World") ]

handleEvent :: AppState -> BrickEvent ResourceName AppEvent -> EventM ResourceName (Next AppState)
handleEvent appState _ = Brick.halt appState

customAttrMap :: AttrMap
customAttrMap = attrMap (defAttr) ([])

initialState :: AppState
initialState = ()
