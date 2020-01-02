{-# LANGUAGE KindSignatures, StandaloneDeriving, UndecidableInstances #-}

module Kreb.Editor.Core.Data.State (
    AppState(..)
  , EditorMode(..)

  , Hook(..)
  , liftHook
  , runHook

  , initPanelDim

  , renderDebugMessage

  , getAbsCursorPos
  , modifyAbsCursorPos
  , updateAbsCursorPos

  , alterActivePanel
  , queryActivePanel

  , alterActivePanelM

  , getActionCounter
  , tickActionCounter

  , setWindowDim
) where

import Data.List (unlines)
import Control.Monad (ap)

import Kreb.Effect
import Kreb.Text
import Kreb.Lang

import Kreb.Editor.Core.Data.Tab
import Kreb.Editor.Core.Data.Settings
import Kreb.Editor.Core.Data.Panel



data AppState (m :: * -> *) = AppState
  { windowDim            :: (Int, Int)
  , absCursorPos         :: (Int, Int)
  , tabbedBuffers        :: Tabs
  , tabWidth             :: Int
  , glyphRenderSettings  :: GlyphRenderSettings

  , runtimeSt            :: RuntimeState (Hook m)
  , stdLibPath           :: FilePath
  , actionCounter        :: Integer
  }

getActionCounter :: AppState m -> Integer
getActionCounter st = actionCounter st

tickActionCounter :: AppState m -> AppState m
tickActionCounter st = st
  { actionCounter = 1 + actionCounter st }

instance Show (AppState m) where
  show st = unlines
    [ "windowDim = ", show $ windowDim st
    , "absCursorPos = ", show $ absCursorPos st
    , "tabbedBuffers = ", show $ tabbedBuffers st
    ]



-- does this belong here?
initPanelDim
  :: (Int, Int) -> PanelDim
initPanelDim (width, height) =
  let
    w1 = max 4 $ width `div` 2
    w2 = width - w1 - 1
    h = height
  in PanelDim
    { _textLabelDim = (2, h-2)
    , _textDim = (w1-3, h-2)
    , _historyDim = (w2, h-4)
    , _commandDim = (w2, 1)
    , _statusDim = (width, 1)
    }

newtype Hook m a = Hook
  { unHook :: AppState m -> m (a, AppState m)
  }

runHook
  :: ( Monad m )
  => AppState m -> Hook m a -> m (a, AppState m)
runHook st (Hook x) = x st

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
  :: ( Monad m )
  => EventId -> (Int, Int)
  -> AppState m -> m (AppState m)
setWindowDim eId dim st = do
  tabs <- setTabsDim eId dim $ tabbedBuffers st
  return $ st
    { windowDim     = dim
    , tabbedBuffers = tabs
    }



getAbsCursorPos :: AppState m -> (Int, Int)
getAbsCursorPos st = absCursorPos st

modifyAbsCursorPos :: (Int, Int) -> AppState m -> AppState m
modifyAbsCursorPos (dx, dy) st =
  let (x, y) = absCursorPos st in
  st { absCursorPos = (x + dx, y + dy) }




updateAbsCursorPos :: EditorMode -> AppState m -> AppState m
updateAbsCursorPos mode st =
  let
    dim = windowDim st
    tabs = tabbedBuffers st
  in st
    { absCursorPos = getAbsCursorPosTabs dim mode tabs
    }



data AppStateAction (m :: * -> *)
  = AppStateAlterActivePanel [PanelAction m]

deriving instance
  ( Show (FileReader m)
  , Show (FileWriter m)
  , Show (ClipboardWriter m)
  , Show (ClipboardReader m)
  ) => Show (AppStateAction m)



alterActivePanelM
  :: ( Monad m )
  => (Panel -> m Panel)
  -> AppState m -> m (AppState m)
alterActivePanelM f st = do
  let tabs = tabbedBuffers st
  tabs' <- alterActivePanelTabsM f tabs
  return st { tabbedBuffers = tabs' }



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
  , ""
  , debugShowTabs (tabbedBuffers st)
  , ""
  ]
