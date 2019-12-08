> {-# LANGUAGE FlexibleContexts, RecordWildCards, KindSignatures #-}

> module Kreb.Editor.Panel (
>     Panel(renderedPanel)
>   , mkPanel

>   , initPanel

>   , alterPanelM
>   , PanelAction(..)

>   , PanelDim(..)

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
>   :: ( Monad m )
>   => EventId -> ShellCommand
>   -> Panel -> m Panel
> updateHistory eId cmd panel = do
>   let
>     append :: String -> [Glyph Char]
>     append = map fromChar
>   histBox' <- alterTextBoxM eId [TextBoxInsertMany $ append ("$> " ++ displayNeat cmd ++ "\n\n")] $ histBox panel
>   return $ panel
>     { commandHistory = cmd : commandHistory panel
>     , histBox = histBox'
>     }

> showDebugMessage
>   :: ( Monad m )
>   => EventId -> String
>   -> Panel -> m Panel
> showDebugMessage eId msg panel = do
>   histBox' <- alterTextBoxM eId [TextBoxInsertMany $ map fromChar ("#> " ++ msg ++ "\n\n")] $ histBox panel
>   return $ panel { histBox = histBox' }

> data PanelDim = PanelDim
>   { _textLabelDim :: (Int, Int)
>   , _textDim      :: (Int, Int)
>   , _historyDim   :: (Int, Int)
>   , _commandDim   :: (Int, Int)
>   , _statusDim    :: (Int, Int)
>   } deriving (Eq, Show)


> initPanel
>   :: FilePath   -- Lib
>   -> (Int, Int) -- (Width, Height)
>   -> PanelDim   -- Component sizes
>   -> Int        -- Tab
>   -> Panel
> initPanel lib (width, height) dim tab =
>   let
>     w1 = max 4 $ width `div` 2
>     w2 = width - w1 - 1
>     h = height
>   in Panel
>     { textBox       = emptyTextBox (_textDim dim) tab
>     , textOffset    = (3,0)
>     , textChanged   = False
>     , textOrigin    = Nothing
>     , textWidth     = w1-3

>     , histBox       = emptyTextBox (_historyDim dim) tab
>     , histChanged   = False

>     , statusBox     = emptyTextBox (_statusDim dim) tab

>     , commandHistory = []

>     , cmdBox        = emptyTextBox (_commandDim dim) tab
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
>   :: [Glyph Char] -> Panel
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
>   :: ( Monad m )
>   => EventId -> (Int, Int)
>   -> Panel -> m Panel
> setPanelDim eId dim panel = do
>   let
>     ComponentSizes{..} =
>       computeComponentSizes dim panel
>   textBox' <- alterTextBoxM eId [TextBoxResize _textSize] $ textBox panel
>   histBox' <- alterTextBoxM eId [TextBoxResize _historySize] $ histBox panel
>   cmdBox' <- alterTextBoxM eId [TextBoxResize _commandSize] $ cmdBox panel
>   statusBox' <- alterTextBoxM eId [TextBoxResize _statusSize] $ statusBox panel
>   return $ panel
>     { textBox = textBox'
>     , histBox = histBox'
>     , cmdBox = cmdBox'
>     , statusBox = statusBox'
>     }

> data RenderedPanel = RenderedPanel
>   { lineLabels :: ([[Glyph String]], (Int, Int))
>   , textLines  :: ([[Glyph String]], (Int, Int), (Int, Int))
>   , histLines  :: ([[Glyph String]], (Int, Int))
>   , cmdLines   :: ([[Glyph String]], (Int, Int), (Int, Int))
>   , statusLine :: ([[Glyph String]], (Int, Int))
>   } deriving (Eq, Show)

> updateRenderedPanel
>   :: GlyphRenderSettings
>   -> EditorMode
>   -> Int
>   -> Panel
>   -> Panel
> updateRenderedPanel settings mode tab panel =
>   let
>     textL = textboxOffset $ textBox panel

>     (labels, labW, text, tDim, tCursor) =
>       renderTextBox (textBox panel)

>     (_, _, cmd, cDim, cCursor) =
>       renderTextBox (cmdBox panel)

>     (_, _, hist, hDim, _) =
>       renderTextBox (histBox panel)
> 
>     (_, _, stat, sDim, _) =
>       renderTextBox (statusBox panel)
> 
>     m = case mode of
>       InsertMode -> map plainRune "INS "
>       CommandMode -> map plainRune "CMD "
>       NormalMode -> map plainRune "NOR "
> 
>     cropAndRender :: (Int, Int) -> [[(Glyph Char, Int)]] -> [[Glyph String]]
>     cropAndRender (w, h) gss =
>       take h $ (++ repeat (repeat $ plainRune ' ')) $
>         map (take w . concatMap (renderGlyph settings tab)) gss
> 
>     rp = RenderedPanel
>       { lineLabels = (cropAndRender (labW, snd tDim) labels, (labW, snd tDim))
>       , textLines  = (cropAndRender tDim text, tDim, tCursor)
>       , histLines  = (cropAndRender hDim hist, hDim)
>       , statusLine = (fmap (m ++) $ cropAndRender sDim stat, sDim)
>       , cmdLines   = (cropAndRender cDim cmd, cDim, cCursor)
>       }
>   in panel
>       { -- textOffset = (labW + 1, 0)
>    --   , cmdOffset = (labW + 1 + textW + 1, histH + 1)
>        renderedPanel = Just rp
>       }

> getAbsCursorPosPanel
>   :: (Int, Int) -> EditorMode -> Panel -> (Int, Int)
> getAbsCursorPosPanel dim mode panel =
>   case mode of
>     CommandMode -> (cmdOffset panel) <> (textboxCursor $ cmdBox panel)
>     _ -> (textOffset panel) <> (textboxCursor $ textBox panel)

> instance Semigroup Int where
>   (<>) = (+)





> data PanelAction (m :: * -> *)
>   = PanelAlterText [TextBoxAction]
>   | PanelAlterCmd [TextBoxAction]
>   | PanelClearCmd
>   deriving (Eq, Show)

 > alterPanel
 >   :: EventId -> [PanelAction m]
 >   -> Panel -> Panel
 > alterPanel eId acts panel =
 >   foldl (flip (alterPanelPrimitive eId)) panel acts

> alterPanelM
>   :: ( Monad m )
>   => EventId -> [PanelAction m]
>   -> Panel -> m Panel
> alterPanelM eId acts panel = case acts of
>   [] -> return panel
>   a:as ->
>     alterPanelPrimitive eId a panel >>= alterPanelM eId as



> alterPanelPrimitive
>   :: ( Monad m )
>   => EventId -> PanelAction m
>   -> Panel -> m Panel
> alterPanelPrimitive eId act = case act of
>   PanelAlterText as ->
>     panelAlterTextM eId as

>   PanelAlterCmd as ->
>     panelAlterCmdM eId as

>   PanelClearCmd ->
>     panelClearCmdM eId





-- ================= --
-- Primitive Actions --
-- ================= --

> panelAlterTextM
>   :: ( Monad m )
>   => EventId -> [TextBoxAction]
>   -> Panel -> m Panel
> panelAlterTextM eId as panel = do
>   box <- alterTextBoxM eId as (textBox panel)
>   return $ panel
>     { textBox = box
>     , textChanged = True
>     }

> panelAlterCmdM
>   :: ( Monad m )
>   => EventId -> [TextBoxAction]
>   -> Panel -> m Panel
> panelAlterCmdM eId as panel = do
>   box <- alterTextBoxM eId as (cmdBox panel)
>   return $ panel { cmdBox = box }

> panelClearCmdM
>   :: ( Monad m )
>   => EventId
>   -> Panel -> m Panel
> panelClearCmdM eId panel = do
>   box <- alterTextBoxM eId [TextBoxClear] (cmdBox panel)
>   return $ panel { cmdBox = box }

 > panelAlterText
 >   :: ( Monad m )
 >   => EventId -> [TextBoxAction]
 >   -> Panel -> m Panel
 > panelAlterText eId as panel = do
 >   let box = textBox panel
 >   return $ panel
 >     { textBox = alterTextBox eId as box
 >     , textChanged = True
 >     }

 > panelAlterCmd
 >   :: ( Monad m )
 >   => EventId -> [TextBoxAction]
 >   -> Panel -> m Panel
 > panelAlterCmd eId as panel = do
 >   let box = cmdBox panel
 >   return $ panel { cmdBox = alterTextBox eId as box }

 > panelClearCmd
 >   :: ( Monad m )
 >   => EventId -> Panel -> m Panel
 > panelClearCmd eId panel = do
 >   let box = cmdBox panel
 >   return $ panel { cmdBox = alterTextBox eId [TextBoxClear] box }



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
