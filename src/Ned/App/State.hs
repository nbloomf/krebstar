module Ned.App.State (
    AppState(..)
  , initAppState
  , EditorMode(..)

  , renderDebugMessage

  , getEditorMode
  , setEditorMode

  , getAbsCursorPos
  , modifyAbsCursorPos

  , updateStateCache

  , setLastError
  , clearLastError

  , alterActivePanel

  , setWindowDim

  , StatusBar(..)
  , RenderedStatusBar(..)
) where

import Data.List (unlines)

import Ned.Data



data AppState = AppState
  { windowDim     :: (Int, Int)
  , editorMode    :: EditorMode
  , absCursorPos  :: (Int, Int)
  , tabbedBuffers :: Tabs
  , statusBar     :: StatusBar
  } deriving (Eq, Show)

initAppState :: (Int, Int) -> AppState
initAppState (w,h) = AppState
  { windowDim     = (w,h)
  , editorMode    = NormalMode
  , absCursorPos  = (0,0)
  , tabbedBuffers = initTabs (w,h) 4
  , statusBar     = defaultStatusBar
  }


setWindowDim
  :: (Int, Int) -> AppState -> AppState
setWindowDim dim st =
  let
    (tabs, pos) =
      setTabsDim dim $ tabbedBuffers st
  in st
    { windowDim = dim
    , absCursorPos = pos
    , tabbedBuffers = tabs
    }



getEditorMode :: AppState -> EditorMode
getEditorMode st = editorMode st

setEditorMode :: EditorMode -> AppState -> AppState
setEditorMode m st = st { editorMode = m }

getAbsCursorPos :: AppState -> (Int, Int)
getAbsCursorPos st = absCursorPos st

modifyAbsCursorPos :: (Int, Int) -> AppState -> AppState
modifyAbsCursorPos (dx, dy) st =
  let (x, y) = absCursorPos st in
  st { absCursorPos = (x + dx, y + dy) }


updateStateCache :: AppState -> AppState
updateStateCache = foldr (.) id
  [ updateAbsCursorPos
  , updateRenderedState
  ]


updateRenderedState :: AppState -> AppState
updateRenderedState st =
  let
    mode = editorMode st
    (w,h) = windowDim st
    sb = statusBar st
    tabs = tabbedBuffers st
  in st
    { tabbedBuffers = updateRenderedTabs (w,h-2) tabs
    , statusBar = updateRenderedStatusBar mode sb
    }

updateAbsCursorPos :: AppState -> AppState
updateAbsCursorPos st =
  let
    dim = windowDim st
    mode = editorMode st
    tabs = tabbedBuffers st
  in st
    { absCursorPos = getAbsCursorPosTabs dim mode tabs
    }



data AppStateAction
  = AppStateAlterActivePanel [PanelAction]
  deriving (Eq, Show)




alterActivePanel
  :: (Panel -> Panel)
  -> AppState -> AppState
alterActivePanel f st =
  let tabs = tabbedBuffers st in
  st { tabbedBuffers = alterActivePanelTabs f tabs }


setLastError :: String -> AppState -> AppState
setLastError msg st =
  let sb = statusBar st in
  st { statusBar = sb { lastError = Just msg } }

clearLastError :: AppState -> AppState
clearLastError st =
  let sb = statusBar st in
  st { statusBar = sb { lastError = Nothing } }


data StatusBar = StatusBar
  { lastError         :: Maybe String
  , renderedStatusBar :: Maybe RenderedStatusBar
  } deriving (Eq, Show)

defaultStatusBar :: StatusBar
defaultStatusBar = StatusBar
  { lastError = Nothing
  , renderedStatusBar = Nothing
  }

data RenderedStatusBar = RenderedStatusBar
  { statusbarMode  :: String
  , statusbarError :: String
  } deriving (Eq, Show)

updateRenderedStatusBar
  :: EditorMode -> StatusBar -> StatusBar
updateRenderedStatusBar m sb =
  let
    mode = case m of
      NormalMode  -> "Nor"
      InsertMode  -> "Ins"
      CommandMode -> "Cmd"
      _ -> "???"

    err = case lastError sb of
      Nothing -> ""
      Just xs -> xs

    rs = RenderedStatusBar
      { statusbarMode  = mode
      , statusbarError = err
      }
  in sb { renderedStatusBar = Just rs }



renderDebugMessage :: AppState -> String
renderDebugMessage st = unlines
  [ "==== State ===="
  , "windowDim: " ++ show (windowDim st)
  , "absCursorPos: " ++ show (absCursorPos st)
  , "editorMode: " ++ show (editorMode st)
  , ""
  , debugShowTabs (tabbedBuffers st)
  , ""
  , show (statusBar st)
  , ""
  ]
