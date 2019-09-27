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
  :: (Int, Int) -> Tabs -> (Tabs, (Int, Int))
setTabsDim dim (Tabs ts) =
  let
    us = fmapSeq (setPanelDim dim) ts
  in
    case headRead us of
      Nothing -> error "setTabsDim: no tabs"
      Just (_,pos) ->
        ( Tabs $ fmapSeq fst us
        , pos
        )



initTabs
  :: (Int, Int)
  -> Int
  -> Tabs
initTabs (w,h) t = Tabs
  { unTabs = mkTapeFocus [] (initPanel (w,h) t) []
  }

getActiveTab
  :: Tabs -> Maybe Panel
getActiveTab (Tabs xs) =
  headRead xs

updateRenderedTabs
  :: BufferRenderSettings -> GlyphRenderSettings -> (Int, Int) -> Tabs -> Tabs
updateRenderedTabs opts settings dim =
  Tabs . headAlter (updateRenderedPanel opts settings dim) . unTabs


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
