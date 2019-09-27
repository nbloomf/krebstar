{-# LANGUAGE KindSignatures #-}

module Kreb.Editor.State (
    AppState(..)
  , initAppState
  , EditorMode(..)

  , Hook(..)

  , renderDebugMessage

  , getEditorMode
  , setEditorMode

  , getAbsCursorPos
  , modifyAbsCursorPos

  , updateStateCache

  , setLastError
  , clearLastError

  , alterActivePanel
  , queryActivePanel

  , setWindowDim

  , StatusBar(..)
  , RenderedStatusBar(..)
) where

import Data.List (unlines)
import Control.Monad (ap)

import Kreb.Text
import Kreb.Lang

import Kreb.Editor.Tab
import Kreb.Editor.Settings
import Kreb.Editor.Panel



data AppState (m :: * -> *) = AppState
  { windowDim     :: (Int, Int)
  , editorMode    :: EditorMode
  , bufferRenderSettings :: BufferRenderSettings
  , glyphRenderSettings :: GlyphRenderSettings
  , absCursorPos  :: (Int, Int)
  , tabbedBuffers :: Tabs
  , statusBar     :: StatusBar
  , runtimeSt     :: RuntimeState (Hook m)
  }

instance Show (AppState m) where
  show st = unlines
    [ "windowDim = ", show $ windowDim st
    , "editorMode = ", show $ editorMode st
    , "absCursorPos = ", show $ absCursorPos st
    , "bufferRenderSettings = ", show $ bufferRenderSettings st
    , "glyphRenderSettings = ", show $ glyphRenderSettings st
    , "tabbedBuffers = ", show $ tabbedBuffers st
    , "statusBar = ", show $ statusBar st
    ]

initAppState :: RuntimeState (Hook m) -> (Int, Int) -> AppState m
initAppState rts (w,h) = AppState
  { windowDim     = (w,h)
  , editorMode    = NormalMode
  , absCursorPos  = (0,0)
  , tabbedBuffers = initTabs (w,h) 4
  , glyphRenderSettings = defaultGlyphRenderSettings
  , bufferRenderSettings = defaultBufferRenderSettings
  , statusBar     = defaultStatusBar
  , runtimeSt     = rts
  }

newtype Hook m a = Hook
  { unHook :: AppState m -> m (a, AppState m)
  }

instance
  ( Monad m
  ) => Monad (Hook m)
  where
    return a = Hook $ \st -> return (a, st)

    (Hook x) >>= f = Hook $ \st -> do
      (a, st2) <- x st
      unHook (f a) st2

instance ( Monad m ) => Applicative (Hook m) where
  pure = return
  (<*>) = ap

instance ( Monad m ) => Functor (Hook m) where
  fmap f x = x >>= (return . f)


setWindowDim
  :: (Int, Int) -> AppState m -> AppState m
setWindowDim dim st =
  let
    (tabs, pos) =
      setTabsDim dim $ tabbedBuffers st
  in st
    { windowDim = dim
    , absCursorPos = pos
    , tabbedBuffers = tabs
    }



getEditorMode :: AppState m -> EditorMode
getEditorMode st = editorMode st

setEditorMode :: EditorMode -> AppState m -> AppState m
setEditorMode m st = st { editorMode = m }

getAbsCursorPos :: AppState m -> (Int, Int)
getAbsCursorPos st = absCursorPos st

modifyAbsCursorPos :: (Int, Int) -> AppState m -> AppState m
modifyAbsCursorPos (dx, dy) st =
  let (x, y) = absCursorPos st in
  st { absCursorPos = (x + dx, y + dy) }


updateStateCache :: AppState m -> AppState m
updateStateCache = foldr (.) id
  [ updateAbsCursorPos
  , updateRenderedState
  ]


updateRenderedState :: AppState m -> AppState m
updateRenderedState st =
  let
    mode = editorMode st
    (w,h) = windowDim st
    sb = statusBar st
    tabs = tabbedBuffers st
    opts = bufferRenderSettings st
    settings = glyphRenderSettings st
  in st
    { tabbedBuffers = updateRenderedTabs opts settings (w,h) tabs
    , statusBar = updateRenderedStatusBar mode sb
    }

updateAbsCursorPos :: AppState m -> AppState m
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
  -> AppState m -> AppState m
alterActivePanel f st =
  let tabs = tabbedBuffers st in
  st { tabbedBuffers = alterActivePanelTabs f tabs }

queryActivePanel
  :: (Panel -> a)
  -> AppState m -> Maybe a
queryActivePanel f st =
  let tabs = tabbedBuffers st in
  queryActivePanelTabs f tabs


setLastError :: String -> AppState m -> AppState m
setLastError msg st =
  let sb = statusBar st in
  st { statusBar = sb { lastError = Just msg } }

clearLastError :: AppState m -> AppState m
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



renderDebugMessage :: AppState m -> String
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
