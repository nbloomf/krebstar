{-# LANGUAGE FlexibleContexts #-}

module Ned.Data.Panel (
    Panel(renderedPanel)
  , mkPanel

  , initPanel

  , alterPanel
  , PanelAction(..)

  , setPanelDim

  , getPanelCmdString

  , RenderedPanel(..)
  , updateRenderedPanel
  , getAbsCursorPosPanel

  , debugShowPanel
) where

import Data.List (unlines)

import Ned.Data.Settings
import Kreb.Struct.FingerTree
import Kreb.Text



data Panel = Panel
  { textBox       :: TextBox
  , textOffset    :: (Int, Int) -- ^ Relative cursor position of NW corner
  , cmdBox        :: TextBox
  , cmdOffset     :: (Int, Int) -- ^ Relative cursor position of NW corner
  , cmdHeight     :: Int
  , renderedPanel :: Maybe RenderedPanel
  } deriving (Eq, Show)

getPanelCmdString :: Panel -> String
getPanelCmdString = getTextBoxString . cmdBox


initPanel
  :: (Int, Int) -- (Width, Height)
  -> Int        -- Tab
  -> Panel
initPanel (w,h) t = Panel
  { textBox       = initTextBox (w-3, h-4) t
  , textOffset    = (3,0)
  , cmdBox        = initTextBox (w,1) t
  , cmdOffset     = (0,h-1)
  , cmdHeight     = 1
  , renderedPanel = Nothing
  }


setPanelDim
  :: (Int, Int) -> Panel -> (Panel, (Int, Int))
setPanelDim (w,h) panel =
  ( panel
      { textBox =
          alterTextBox [TextBoxResize (w, h-4)] $ textBox panel
      , cmdBox =
          alterTextBox [TextBoxResize (w, 1)] $ cmdBox panel
      }
  , (0,0)
  )



mkPanel
  :: [Glyph] -> Panel
mkPanel xs = undefined




data RenderedPanel = RenderedPanel
  { lineLabels :: [Maybe Int]
  , labelSep   :: [[Char]]
  , textLines  :: [[Glyph]]
  , cmdSep     :: [Char]
  , cmdLines   :: [[Glyph]]
  } deriving (Eq, Show)

updateRenderedPanel
  :: BufferRenderSettings
  -> (Int, Int)
  -> Panel
  -> Panel
updateRenderedPanel opts (w,h) panel =
  let
    labW  = textboxLabelWidth $ textBox panel
    textW = w - 1 - labW
    textH = h - 1 - (cmdHeight panel)

    textL = textboxOffset $ textBox panel

    (labels, text) =
      renderTextBox opts (textBox panel)

    (_, cmd) =
      renderTextBox opts (cmdBox panel)
  
    rp = RenderedPanel
      { lineLabels = labels
      , labelSep   = replicate textH [fromChar '░']
      , textLines  = text
      , cmdSep     = replicate w (fromChar '░')
      , cmdLines   = if null cmd then [[]] else cmd
      }
  in panel
      { textOffset = (labW + 1, 0)
      , cmdOffset = (0, textH + 1)
      , renderedPanel = Just rp
      }

getAbsCursorPosPanel
  :: (Int, Int) -> EditorMode -> Panel -> (Int, Int)
getAbsCursorPosPanel dim mode panel =
  case mode of
    CommandMode -> (cmdOffset panel) <> (textboxCursor $ cmdBox panel)
    _ -> (textOffset panel) <> (textboxCursor $ textBox panel)

instance Semigroup Int where
  (<>) = (+)





data PanelAction
  = PanelAlterText [TextBoxAction]
  | PanelAlterCmd [TextBoxAction]
  | PanelClearCmd
  deriving (Eq, Show)

alterPanel
  :: [PanelAction]
  -> Panel -> Panel
alterPanel acts panel =
  foldl (flip alterPanelPrimitive) panel acts



alterPanelPrimitive
  :: PanelAction
  -> Panel -> Panel
alterPanelPrimitive act = case act of
  PanelAlterText as ->
    panelAlterText as

  PanelAlterCmd as ->
    panelAlterCmd as

  PanelClearCmd ->
    panelClearCmd





-- ================= --
-- Primitive Actions --
-- ================= --

panelAlterText
  :: [TextBoxAction]
  -> Panel -> Panel
panelAlterText as panel =
  let box = textBox panel in
  panel { textBox = alterTextBox as box }

panelAlterCmd
  :: [TextBoxAction]
  -> Panel -> Panel
panelAlterCmd as panel =
  let box = cmdBox panel in
  panel { cmdBox = alterTextBox as box }

panelClearCmd
  :: Panel -> Panel
panelClearCmd panel =
  let box = cmdBox panel in
  panel { cmdBox = alterTextBox [TextBoxClear] box }



debugShowPanel :: Panel -> String
debugShowPanel p = unlines
  [ ""
  , "Panel"
  , "-----"
  , ""
  , "textOffset: " ++ show (textOffset p)
  , "cmdOffset: " ++ show (cmdOffset p)
  , "cmdHeight: " ++ show (cmdHeight p)
  , ""
  , "textBox:"
  , show $ debugTextBox (textBox p)
  , ""
  , "cmdBox:"
  , show $ debugTextBox (cmdBox p)
  , ""
  , "renderedPanel:"
  , show (renderedPanel p)
  ]
