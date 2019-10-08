> {-# LANGUAGE FlexibleContexts, RecordWildCards #-}

> module Kreb.Editor.Panel (
>     Panel(renderedPanel)
>   , mkPanel

>   , initPanel

>   , alterPanel
>   , PanelAction(..)

>   , getTextBox

>   , ShellCommand(..)

>   , updateHistory
>   , showDebugMessage

>   , setPanelDim

>   , getPanelCmdString
>   , getPanelString

>   , RenderedPanel(..)
>   , updateRenderedPanel
>   , getAbsCursorPosPanel

>   , debugShowPanel
> ) where

> import Data.List (unlines)

> import Kreb.Format
> import Kreb.Editor.Settings
> import Kreb.Struct.FingerTree
> import Kreb.Text
> import Kreb.Lang



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

>   , commandHistory :: [ShellCommand]

>   , statusBox     :: TextBox

>   , renderedPanel :: Maybe RenderedPanel

>   , libPath :: Maybe FilePath
>   } deriving (Eq, Show)

> getTextBox :: Panel -> TextBox
> getTextBox = textBox

> data ShellCommand
>   = TypeQuery String Scheme
>   | RunCommand Phrase DataStack
>   deriving (Eq, Show)

> instance DisplayNeat ShellCommand where
>   displayNeat x = case x of
>     TypeQuery str sch -> concat
>       [ ":t " ++ str, "\n", displayNeat sch ]
>     RunCommand ph st -> concat
>       [ displayNeat ph, "\n", displayNeat st ]

> updateHistory
>   :: ShellCommand -> Panel -> Panel
> updateHistory cmd panel =
>   let
>     append :: String -> [Glyph]
>     append = map fromChar
>   in panel
>     { commandHistory = cmd : commandHistory panel
>     , histBox = alterTextBox [TextBoxInsertMany $ append ("$> " ++ displayNeat cmd ++ "\n\n")] $ histBox panel
>     }

> showDebugMessage
>   :: String -> Panel -> Panel
> showDebugMessage msg panel = panel
>   { histBox = alterTextBox [TextBoxInsertMany $ map fromChar ("#> " ++ msg ++ "\n\n")] $ histBox panel
>   }


> initPanel
>   :: FilePath   -- Lib
>   -> (Int, Int) -- (Width, Height)
>   -> Int        -- Tab
>   -> Panel
> initPanel lib (width, height) tab =
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

>     , commandHistory = []

>     , cmdBox        = initTextBox (w2,1) tab
>     , cmdOffset     = (0,h-1)
>     , cmdHeight     = 1
>     , renderedPanel = Nothing
>     , libPath       = Just lib
>     }



> getPanelCmdString :: Panel -> String
> getPanelCmdString = getTextBoxString . cmdBox

> getPanelString :: Panel -> String
> getPanelString = getTextBoxString . textBox




> mkPanel
>   :: [Glyph] -> Panel
> mkPanel xs = undefined


> data ComponentSizes = ComponentSizes
>   { _textLabelSize :: (Int, Int)
>   , _textSize      :: (Int, Int)
>   , _historySize   :: (Int, Int)
>   , _commandSize   :: (Int, Int)
>   , _statusSize    :: (Int, Int)
>   } deriving (Eq, Show)

> computeComponentSizes
>   :: (Int, Int) -> Panel -> ComponentSizes
> computeComponentSizes (panelW, panelH) panel =
>   let
>     textLabelW = textboxLabelWidth $ textBox panel
>     textLabelH = panelH - 2
> 
>     textW = textWidth panel
>     textH = panelH - 2
> 
>     commandW = panelW - textWidth panel - 1 - textLabelW - 1
>     commandH = cmdHeight panel
> 
>     historyW = commandW
>     historyH = panelH - commandH - 3
> 
>     statusW = panelW - 4
>     statusH = 1
>   in ComponentSizes
>     { _textLabelSize = (textLabelW, textLabelH)
>     , _textSize      = (textW, textH)
>     , _historySize   = (historyW, historyH)
>     , _commandSize   = (commandW, commandH)
>     , _statusSize    = (statusW, statusH)
>     }




> setPanelDim
>   :: (Int, Int) -> Panel -> Panel
> setPanelDim dim panel =
>   let
>     ComponentSizes{..} =
>       computeComponentSizes dim panel
>   in panel
>     { textBox =
>         alterTextBox [TextBoxResize _textSize] $ textBox panel
>     , histBox =
>         alterTextBox [TextBoxResize _historySize] $ histBox panel
>     , cmdBox =
>         alterTextBox [TextBoxResize _commandSize] $ cmdBox panel
>     , statusBox =
>         alterTextBox [TextBoxResize _statusSize] $ statusBox panel
>     }

> data RenderedPanel = RenderedPanel
>   { lineLabels :: [Maybe Int]
>   , labelSep   :: [[Rune]]
>   , textLines  :: [[Rune]]

>   , histSep    :: [[Rune]]
>   , histLines  :: [[Rune]]

>   , cmdSep     :: [Rune]
>   , cmdLines   :: [[Rune]]

>   , statusSep  :: [Rune]
>   , statusLine :: [[Rune]]
>   } deriving (Eq, Show)

> updateRenderedPanel
>   :: BufferRenderSettings
>   -> GlyphRenderSettings
>   -> EditorMode
>   -> (Int, Int)
>   -> Int
>   -> Panel
>   -> Panel
> updateRenderedPanel opts settings mode (panelW, panelH) tab panel =
>   let
>     ComponentSizes{..} =
>       computeComponentSizes (panelW, panelH) panel
> 
>     (labW, labH) = _textLabelSize
>     (textW, textH) = _textSize
>     (cmdW, cmdH) = _commandSize
>     (histW, histH) = _historySize
>     (statW, statH) = _statusSize

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
>     m = case mode of
>       InsertMode -> map plainRune "INS "
>       CommandMode -> map plainRune "CMD "
>       NormalMode -> map plainRune "NOR "
> 
>     cropAndRender :: (Int, Int) -> [[(Glyph, Int)]] -> [[Rune]]
>     cropAndRender (w, h) gss =
>       take h $ (++ repeat (repeat $ plainRune ' ')) $
>         map (take w . concatMap (renderGlyph settings tab)) gss
> 
>     rp = RenderedPanel
>       { lineLabels = labels
>       , labelSep   = replicate (panelH - 2) [dimRune '│']
>       , textLines  = cropAndRender (textW, textH) text
>       , histSep    = concat
>           [ replicate histH [dimRune '│']
>           , [[dimRune '├']]
>           , replicate cmdH [dimRune '│']
>           ]
>       , histLines  = cropAndRender (histW, histH) hist
>       , statusSep  = concat
>           [ replicate labW (dimRune '═')
>           , [dimRune '╧']
>           , replicate textW (dimRune '═')
>           , [dimRune '╧']
>           , replicate histW (dimRune '═')
>           ]
>       , statusLine = fmap (m ++) $ cropAndRender (statW, statH) stat
>       , cmdSep     = replicate histW (dimRune '─')
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
