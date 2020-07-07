{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Kreb.Editor.Core.Data.Tab (
    Tabs()

  , initTabs

  , getActiveTab

  , getAbsCursorPosTabs

  , alterActivePanelTabs
  , queryActivePanelTabs

  , alterActivePanelTabsM

  , setTabsDim

 -- , debugShowTabs
) where

import Kreb.Effect
import Kreb.Editor.Core.Data.Settings
import Kreb.Text.Buffer
import Kreb.Text.MeasureText
import Kreb.Text.TextBox
import Kreb.Editor.Core.Data.Panel
import Kreb.Struct.Class
import Kreb.Struct.Data.FingerTree
import Kreb.Struct.Data.Zipper
import qualified Kreb.Struct.Data.Deque.Zipper as DZ




data Tabs = Tabs
  { unTabs :: DZ.NonEmptyDequeZipper Panel
  } deriving (Eq, Show)

setTabsDim
  :: ( Monad m )
  => TextBoxFX m -> EventId -> (Int, Int)
  -> Tabs -> m Tabs
setTabsDim fx eId dim (Tabs ts) = do
  ts' <- mapM (setPanelDim fx eId dim) ts
  return $ Tabs ts'




initTabs
  :: FilePath
  -> (Int, Int)
  -> PanelDim
  -> Int
  -> Tabs
initTabs stdLib (w,h) dim t = Tabs
  { unTabs = singleton (initPanel stdLib (w,h) dim t)
  }

getActiveTab
  :: Tabs -> Maybe Panel
getActiveTab (Tabs xs) =
  readPointer xs



getAbsCursorPosTabs
  :: (Int, Int) -> EditorMode -> Tabs -> (Int, Int)
getAbsCursorPosTabs dim mode tabs =
  case readPointer $ unTabs tabs of
    Nothing -> (0,0)
    Just ts -> getAbsCursorPosPanel dim mode ts


alterActivePanelTabsM
  :: ( Monad m )
  => (Panel -> m Panel)
  -> Tabs -> m Tabs
alterActivePanelTabsM f (Tabs x) =
  alterPointerM f x >>= (return . Tabs)

alterActivePanelTabs
  :: (Panel -> Panel) -> Tabs -> Tabs
alterActivePanelTabs f =
  Tabs . alterPointer f . unTabs

queryActivePanelTabs
  :: (Panel -> a) -> Tabs -> Maybe a
queryActivePanelTabs f =
  fmap f . readPointer . unTabs



-- debugShowTabs :: Tabs -> String
-- debugShowTabs (Tabs ts) = show $ debugShowPanel ts
