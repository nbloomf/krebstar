{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Ned.Data.Tab (
    Tabs()
  , Tab(..)

  , initTab
  , initTabs

  , getActiveTab

  , updateRenderedTabs
  , getAbsCursorPosTabs

  , alterActivePanelTabs

  , setTabsDim
  , setTabDim

  , debugShowTab
  , debugShowTabs
) where

import Ned.Data.Settings
import Ned.Data.Buffer
import Ned.Data.Panel
import Ned.Data.Tile
import Ned.Data.FingerTree
import Ned.Data.FingerTreeZip
import Ned.Data.Seq

data Tab = Tab
  { panels :: Tiled Panel
  } deriving (Eq, Show)

alterPanels
  :: (Tiled Panel -> Tiled Panel)
  -> Tab -> Tab
alterPanels f tab =
  let p = panels tab in
  tab { panels = f p }

instance Valued Count Tab where
  value _ = Count 1

initTab
  :: (Int,Int)
  -> Int
  -> Tab
initTab (w,h) t = Tab
  { panels = mkTiled (initPanel (w,h) t)
  }



data Tabs = Tabs
  { unTabs :: Seq Tab
  } deriving (Eq, Show)

setTabsDim
  :: (Int, Int) -> Tabs -> (Tabs, (Int, Int))
setTabsDim dim (Tabs ts) =
  let
    us = fmapSeq (setTabDim dim) ts
  in
    case headRead us of
      Nothing -> error "setTabsDim: no tabs"
      Just (_,pos) ->
        ( Tabs $ fmapSeq fst us
        , pos
        )

setTabDim
  :: (Int, Int) -> Tab -> (Tab, (Int, Int))
setTabDim dim (Tab x) =
  let (y,z) = setTiledDim setPanelDim dim x
  in (Tab y, z)



initTabs
  :: (Int, Int)
  -> Int
  -> Tabs
initTabs (w,h) t = Tabs
  { unTabs = mkTapeFocus [] (initTab (w,h) t) []
  }

getActiveTab
  :: Tabs -> Maybe Tab
getActiveTab (Tabs xs) =
  headRead xs

updateRenderedTabs
  :: (Int, Int) -> Tabs -> Tabs
updateRenderedTabs dim =
  Tabs . headAlter (updateRenderedTab dim) . unTabs

updateRenderedTab
  :: (Int, Int) -> Tab -> Tab
updateRenderedTab dim =
  alterPanels (fmap (updateRenderedPanel defaultBufferRenderSettings dim))

getAbsCursorPosTabs
  :: (Int, Int) -> EditorMode -> Tabs -> (Int, Int)
getAbsCursorPosTabs dim mode tabs =
  case headRead $ unTabs tabs of
    Nothing -> (0,0)
    Just ts -> getAbsCursorPosTab dim mode ts

getAbsCursorPosTab
  :: (Int, Int) -> EditorMode -> Tab -> (Int, Int)
getAbsCursorPosTab dim mode tab =
  getAbsCursorPosTiled (getAbsCursorPosPanel dim mode) $ panels tab



alterActivePanelTabs
  :: (Panel -> Panel) -> Tabs -> Tabs
alterActivePanelTabs f =
  Tabs . headAlter (alterActivePanelTab f) . unTabs

alterActivePanelTab
  :: (Panel -> Panel) -> Tab -> Tab
alterActivePanelTab f tab =
  let tiles = panels tab in
  tab { panels = alterFocusTiled f tiles }



debugShowTabs :: Tabs -> String
debugShowTabs (Tabs ts) = debugShowSeq debugShowTab ts

debugShowTab :: Tab -> String
debugShowTab (Tab t) = debugShowTiled debugShowPanel t
