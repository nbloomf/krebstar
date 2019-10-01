{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Kreb.Editor.Tab (
    Tabs()

  , initTabs

  , getActiveTab

  , updateRenderedTabs
  , getAbsCursorPosTabs

  , alterActivePanelTabs
  , queryActivePanelTabs

  , setTabsDim

  , debugShowTabs
) where

import Kreb.Editor.Settings
import Kreb.Text.Buffer
import Kreb.Text.Glyph
import Kreb.Editor.Panel
import Kreb.Struct.FingerTree
import Kreb.Struct.FingerTreeZip
import Kreb.Struct.Seq



instance Valued Count Panel where
  value _ = Count 1

data Tabs = Tabs
  { unTabs :: Seq Panel
  } deriving (Eq, Show)

setTabsDim
  :: (Int, Int) -> Tabs -> Tabs
setTabsDim dim (Tabs ts) =
  Tabs $ fmapSeq (setPanelDim dim) ts




initTabs
  :: FilePath
  -> (Int, Int)
  -> Int
  -> Tabs
initTabs stdLib (w,h) t = Tabs
  { unTabs = mkTapeFocus [] (initPanel stdLib (w,h) t) []
  }

getActiveTab
  :: Tabs -> Maybe Panel
getActiveTab (Tabs xs) =
  headRead xs

updateRenderedTabs
  :: BufferRenderSettings -> GlyphRenderSettings -> EditorMode -> (Int, Int) -> Int -> Tabs -> Tabs
updateRenderedTabs opts settings mode dim tab =
  Tabs . headAlter (updateRenderedPanel opts settings mode dim tab) . unTabs


getAbsCursorPosTabs
  :: (Int, Int) -> EditorMode -> Tabs -> (Int, Int)
getAbsCursorPosTabs dim mode tabs =
  case headRead $ unTabs tabs of
    Nothing -> (0,0)
    Just ts -> getAbsCursorPosPanel dim mode ts



alterActivePanelTabs
  :: (Panel -> Panel) -> Tabs -> Tabs
alterActivePanelTabs f =
  Tabs . headAlter f . unTabs

queryActivePanelTabs
  :: (Panel -> a) -> Tabs -> Maybe a
queryActivePanelTabs f =
  fmap f . headRead . unTabs



debugShowTabs :: Tabs -> String
debugShowTabs (Tabs ts) = debugShowSeq debugShowPanel ts
