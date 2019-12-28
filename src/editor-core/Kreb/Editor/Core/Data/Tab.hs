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

  , debugShowTabs
) where

import Kreb.Editor.Core.Data.Settings
import Kreb.Text.Buffer
import Kreb.Text.Glyph
import Kreb.Text.Rune
import Kreb.Editor.Core.Data.Panel
import Kreb.Struct.Valued
import Kreb.Struct.FingerTree
import qualified Kreb.Struct.Sequence as Seq



instance Valued Count Panel where
  value _ = Count 1

data Tabs = Tabs
  { unTabs :: Seq.Sequence Panel
  } deriving (Eq, Show)

setTabsDim
  :: ( Monad m )
  => EventId -> (Int, Int)
  -> Tabs -> m Tabs
setTabsDim eId dim (Tabs ts) = do
  ts' <- mapM (setPanelDim eId dim) ts
  return $ Tabs ts'




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



getAbsCursorPosTabs
  :: (Int, Int) -> EditorMode -> Tabs -> (Int, Int)
getAbsCursorPosTabs dim mode tabs =
  case Seq.readPoint $ unTabs tabs of
    Nothing -> (0,0)
    Just ts -> getAbsCursorPosPanel dim mode ts


alterActivePanelTabsM
  :: ( Monad m )
  => (Panel -> m Panel)
  -> Tabs -> m Tabs
alterActivePanelTabsM f (Tabs x) =
  Seq.alterPointM f x >>= (return . Tabs)

alterActivePanelTabs
  :: (Panel -> Panel) -> Tabs -> Tabs
alterActivePanelTabs f =
  Tabs . Seq.alterPoint f . unTabs

queryActivePanelTabs
  :: (Panel -> a) -> Tabs -> Maybe a
queryActivePanelTabs f =
  fmap f . Seq.readPoint . unTabs



debugShowTabs :: Tabs -> String
debugShowTabs (Tabs ts) = Seq.debugShowSequence debugShowPanel ts
