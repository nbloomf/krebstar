{-# LANGUAGE KindSignatures #-}

module Kreb.Editor.State (
    AppState(..)
  , initAppState
  , EditorMode(..)

  , Hook(..)
  , liftHook

  , renderDebugMessage

  , getEditorMode
  , setEditorMode

  , getAbsCursorPos
  , modifyAbsCursorPos

  , updateStateCache

  , alterActivePanel
  , queryActivePanel

  , setWindowDim
) where

import Data.List (unlines)
import Control.Monad (ap)

import Kreb.Text
import Kreb.Lang

import Kreb.Editor.Tab
import Kreb.Editor.Settings
import Kreb.Editor.Panel



data AppState (m :: * -> *) = AppState
  { windowDim            :: (Int, Int)
  , editorMode           :: EditorMode
  , absCursorPos         :: (Int, Int)
  , tabbedBuffers        :: Tabs
  , tabWidth             :: Int
  , glyphRenderSettings  :: GlyphRenderSettings

  , runtimeSt            :: RuntimeState (Hook m)
  , stdLibPath           :: FilePath
  }

instance Show (AppState m) where
  show st = unlines
    [ "windowDim = ", show $ windowDim st
    , "editorMode = ", show $ editorMode st
    , "absCursorPos = ", show $ absCursorPos st
    , "tabbedBuffers = ", show $ tabbedBuffers st
    ]

initAppState :: FilePath -> PanelDim -> RuntimeState (Hook m) -> (Int, Int) -> AppState m
initAppState stdLib dim rts (w,h) = AppState
  { windowDim            = (w,h)
  , editorMode           = NormalMode
  , absCursorPos         = (0,0)
  , tabWidth             = 4
  , tabbedBuffers        = initTabs "" (w,h) dim 4
  , glyphRenderSettings  = defaultGlyphRenderSettings

  , runtimeSt            = rts { _rtStack = Cons Empty V_Eff }
  , stdLibPath           = stdLib
  }

newtype Hook m a = Hook
  { unHook :: AppState m -> m (a, AppState m)
  }

liftHook :: ( Monad m ) => m a -> Hook m a
liftHook x = Hook $ \st -> do
  a <- x
  return (a, st)

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
    tabs =
      setTabsDim dim $ tabbedBuffers st
  in st
    { windowDim     = dim
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
    tab = tabWidth st
    tabs = tabbedBuffers st
    settings = glyphRenderSettings st
  in st
    { tabbedBuffers = updateRenderedTabs settings mode tab tabs
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






renderDebugMessage :: AppState m -> String
renderDebugMessage st = unlines
  [ "==== State ===="
  , "windowDim: " ++ show (windowDim st)
  , "absCursorPos: " ++ show (absCursorPos st)
  , "editorMode: " ++ show (editorMode st)
  , ""
  , debugShowTabs (tabbedBuffers st)
  , ""
  ]
