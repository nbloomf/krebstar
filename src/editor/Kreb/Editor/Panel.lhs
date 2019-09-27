> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Editor.Panel (
>     Panel(renderedPanel)
>   , mkPanel

>   , initPanel

>   , alterPanel
>   , PanelAction(..)

>   , setPanelDim

>   , getPanelCmdString
>   , getPanelString

>   , RenderedPanel(..)
>   , updateRenderedPanel
>   , getAbsCursorPosPanel

>   , debugShowPanel
> ) where

> import Data.List (unlines)

> import Kreb.Editor.Settings
> import Kreb.Struct.FingerTree
> import Kreb.Text



> data Panel = Panel
>   { textBox       :: TextBox
>   , textOffset    :: (Int, Int) -- ^ Relative cursor position of NW corner
>   , textChanged   :: Bool
>   , textOrigin    :: Maybe FilePath
>   , textWidth     :: Int

>   , cmdBox        :: TextBox
>   , cmdOffset     :: (Int, Int) -- ^ Relative cursor position of NW corner
>   , cmdHeight     :: Int

>   , histBox       :: TextBox
>   , histChanged   :: Bool

>   , statusBox     :: TextBox

>   , renderedPanel :: Maybe RenderedPanel
>   } deriving (Eq, Show)


> initPanel
>   :: (Int, Int) -- (Width, Height)
>   -> Int        -- Tab
>   -> Panel
> initPanel (width, height) tab =
>   let
>     w1 = max 4 $ width `div` 2
>     w2 = width - w1 - 1
>     h = height
>   in Panel
>     { textBox       = initTextBox (w1-3, h-2) tab
>     , textOffset    = (3,0)
>     , textChanged   = False
>     , textOrigin    = Nothing
>     , textWidth     = w1-3

>     , histBox       = initTextBox (w2, h-2) tab
>     , histChanged   = False

>     , statusBox     = initTextBox (width, 1) tab

>     , cmdBox        = initTextBox (w2,1) tab
>     , cmdOffset     = (0,h-1)
>     , cmdHeight     = 1
>     , renderedPanel = Nothing
>     }



> getPanelCmdString :: Panel -> String
> getPanelCmdString = getTextBoxString . cmdBox

> getPanelString :: Panel -> String
> getPanelString = getTextBoxString . textBox




> mkPanel
>   :: [Glyph] -> Panel
> mkPanel xs = undefined


> setPanelDim
>   :: (Int, Int) -> Panel -> (Panel, (Int, Int))
> setPanelDim (panelW, panelH) panel =
>   let
>     labW = textboxLabelWidth $ textBox panel
>     labH = panelH - 2
> 
>     textW = textWidth panel
>     textH = panelH - 2
> 
>     cmdW = panelW - textWidth panel - 1 - labW - 1
>     cmdH = cmdHeight panel
> 
>     histW = cmdW
>     histH = panelH - cmdH - 3
>   in
>     ( panel
>         { textBox =
>             alterTextBox [TextBoxResize (textW, textH)] $ textBox panel
>         , histBox =
>             alterTextBox [TextBoxResize (histW, histH)] $ histBox panel
>         , cmdBox =
>             alterTextBox [TextBoxResize (cmdW, cmdH)] $ cmdBox panel
>         , statusBox =
>             alterTextBox [TextBoxResize (panelW, 1)] $ statusBox panel
>         }
>     , (0,0)
>     )

> data RenderedPanel = RenderedPanel
>   { lineLabels :: [Maybe Int]
>   , labelSep   :: [[Char]]
>   , textLines  :: [[Rune]]

>   , histSep    :: [[Char]]
>   , histLines  :: [[Rune]]

>   , cmdSep     :: [Char]
>   , cmdLines   :: [[Rune]]

>   , statusSep  :: [Char]
>   , statusLine :: [[Rune]]
>   } deriving (Eq, Show)

> updateRenderedPanel
>   :: BufferRenderSettings
>   -> GlyphRenderSettings
>   -> (Int, Int)
>   -> Panel
>   -> Panel
> updateRenderedPanel opts settings (panelW, panelH) panel =
>   let
>     labW = textboxLabelWidth $ textBox panel
>     labH = panelH - 2
> 
>     textW = textWidth panel
>     textH = panelH - 2
> 
>     cmdW = panelW - textWidth panel - 1 - labW - 1
>     cmdH = cmdHeight panel
> 
>     histW = cmdW
>     histH = panelH - cmdH - 3

>     textL = textboxOffset $ textBox panel

>     (labels, text) =
>       renderTextBox opts (textBox panel)

>     (_, cmd) =
>       renderTextBox opts (cmdBox panel)

>     (_, hist) =
>       renderTextBox opts (histBox panel)
> 
>     (_, stat) =
>       renderTextBox opts (statusBox panel)
> 
>     cropAndRender :: (Int, Int) -> [[Glyph]] -> [[Rune]]
>     cropAndRender (w, h) gss =
>       take h $ (++ repeat (repeat $ plainRune ' ')) $
>         map (take w . map (renderGlyph settings)) gss
> 
>     rp = RenderedPanel
>       { lineLabels = labels
>       , labelSep   = replicate (panelH - 2) [fromChar '|']
>       , textLines  = cropAndRender (textW, textH) text
>       , histSep    = replicate (panelH - 2) [fromChar '|']
>       , histLines  = cropAndRender (histW, histH) hist
>       , statusSep  = replicate panelW (fromChar '-')
>       , statusLine = cropAndRender (panelW, 1) stat
>       , cmdSep     = replicate histW (fromChar '-')
>       , cmdLines   = cropAndRender (cmdW, cmdH) cmd
>       }
>   in panel
>       { textOffset = (labW + 1, 0)
>       , cmdOffset = (labW + 1 + textW + 1, histH + 1)
>       , renderedPanel = Just rp
>       }

> getAbsCursorPosPanel
>   :: (Int, Int) -> EditorMode -> Panel -> (Int, Int)
> getAbsCursorPosPanel dim mode panel =
>   case mode of
>     CommandMode -> (cmdOffset panel) <> (textboxCursor $ cmdBox panel)
>     _ -> (textOffset panel) <> (textboxCursor $ textBox panel)

> instance Semigroup Int where
>   (<>) = (+)





> data PanelAction
>   = PanelAlterText [TextBoxAction]
>   | PanelAlterCmd [TextBoxAction]
>   | PanelClearCmd
>   deriving (Eq, Show)

> alterPanel
>   :: [PanelAction]
>   -> Panel -> Panel
> alterPanel acts panel =
>   foldl (flip alterPanelPrimitive) panel acts



> alterPanelPrimitive
>   :: PanelAction
>   -> Panel -> Panel
> alterPanelPrimitive act = case act of
>   PanelAlterText as ->
>     panelAlterText as

>   PanelAlterCmd as ->
>     panelAlterCmd as

>   PanelClearCmd ->
>     panelClearCmd





-- ================= --
-- Primitive Actions --
-- ================= --

> panelAlterText
>   :: [TextBoxAction]
>   -> Panel -> Panel
> panelAlterText as panel =
>   let box = textBox panel in
>   panel
>     { textBox = alterTextBox as box
>     , textChanged = True
>     }

> panelAlterCmd
>   :: [TextBoxAction]
>   -> Panel -> Panel
> panelAlterCmd as panel =
>   let box = cmdBox panel in
>   panel { cmdBox = alterTextBox as box }

> panelClearCmd
>   :: Panel -> Panel
> panelClearCmd panel =
>   let box = cmdBox panel in
>   panel { cmdBox = alterTextBox [TextBoxClear] box }



> debugShowPanel :: Panel -> String
> debugShowPanel p = unlines
>   [ ""
>   , "Panel"
>   , "-----"
>   , ""
>   , "textOffset: " ++ show (textOffset p)
>   , "cmdOffset: " ++ show (cmdOffset p)
>   , "cmdHeight: " ++ show (cmdHeight p)
>   , ""
>   , "textBox:"
>   , show $ debugTextBox (textBox p)
>   , ""
>   , "cmdBox:"
>   , show $ debugTextBox (cmdBox p)
>   , ""
>   , "renderedPanel:"
>   , show (renderedPanel p)
>   ]
