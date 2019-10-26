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
import Kreb.Struct.Valued
import Kreb.Struct.FingerTree
import qualified Kreb.Struct.Seq as Seq



instance Valued Count Panel where
  value _ = Count 1

data Tabs = Tabs
  { unTabs :: Seq.Seq Panel
  } deriving (Eq, Show)

setTabsDim
  :: (Int, Int) -> Tabs -> Tabs
setTabsDim dim (Tabs ts) =
  Tabs $ Seq.fmapSeq (setPanelDim dim) ts




initTabs
  :: FilePath
  -> (Int, Int)
  -> PanelDim
  -> Int
  -> Tabs
initTabs stdLib (w,h) dim t = Tabs
  { unTabs = Seq.singleton (initPanel stdLib (w,h) dim t)
  }

getActiveTab
  :: Tabs -> Maybe Panel
getActiveTab (Tabs xs) =
  Seq.readPoint xs

updateRenderedTabs
  :: GlyphRenderSettings -> EditorMode -> Int -> Tabs -> Tabs
updateRenderedTabs settings mode tab =
  Tabs . Seq.alterPoint (updateRenderedPanel settings mode tab) . unTabs


getAbsCursorPosTabs
  :: (Int, Int) -> EditorMode -> Tabs -> (Int, Int)
getAbsCursorPosTabs dim mode tabs =
  case Seq.readPoint $ unTabs tabs of
    Nothing -> (0,0)
    Just ts -> getAbsCursorPosPanel dim mode ts



alterActivePanelTabs
  :: (Panel -> Panel) -> Tabs -> Tabs
alterActivePanelTabs f =
  Tabs . Seq.alterPoint f . unTabs

queryActivePanelTabs
  :: (Panel -> a) -> Tabs -> Maybe a
queryActivePanelTabs f =
  fmap f . Seq.readPoint . unTabs



debugShowTabs :: Tabs -> String
debugShowTabs (Tabs ts) = Seq.debugShowSeq debugShowPanel ts
