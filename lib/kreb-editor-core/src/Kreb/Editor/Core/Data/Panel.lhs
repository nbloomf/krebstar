> {-# LANGUAGE FlexibleContexts, RecordWildCards, KindSignatures, StandaloneDeriving, UndecidableInstances #-}

> module Kreb.Editor.Core.Data.Panel (
>     Panel(textBox, cmdBox, histBox, statusBox)
>  -- , mkPanel

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

>   , getAbsCursorPosPanel

>   , debugShowPanel
> ) where

> import Data.List (unlines)

> import Kreb.Effect
> import Kreb.Struct
> import Kreb.Editor.Core.Data.Settings
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

>   , libPath :: Maybe FilePath
>   } deriving (Eq, Show)

> getTextBox :: Panel -> TextBox
> getTextBox = textBox

> data ShellCommand
>   = TypeQuery String Scheme
>   | RunCommand Phrase DataStack
>   deriving (Eq, Show)

> {- instance DisplayNeat ShellCommand where
>   displayNeat x = case x of
>     TypeQuery str sch -> concat
>       [ ":t " ++ str, "\n", displayNeat sch ]
>     RunCommand ph st -> concat
>       [ displayNeat ph, "\n", displayNeat st ] -}

> updateHistory
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> ShellCommand
>   -> Panel -> m Panel
> updateHistory fx eId cmd panel = do
>   let
>     append :: String -> [Char]
>     append = id
>   histBox' <- alterTextBoxM fx eId [TextBoxInsertMany $ append ("$> " ++ show cmd ++ "\n\n")] $ histBox panel
>   return $ panel
>     { commandHistory = cmd : commandHistory panel
>     , histBox = histBox'
>     }

> showDebugMessage
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> String
>   -> Panel -> m Panel
> showDebugMessage fx eId msg panel = do
>   histBox' <- alterTextBoxM fx eId [TextBoxInsertMany $ ("#> " ++ msg ++ "\n\n")] $ histBox panel
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
>   => TextBoxFX m -> EventId -> (Int, Int)
>   -> Panel -> m Panel
> setPanelDim fx eId dim panel = do
>   let
>     ComponentSizes{..} =
>       computeComponentSizes dim panel
>   textBox' <- alterTextBoxM fx eId [TextBoxResize _textSize] $ textBox panel
>   histBox' <- alterTextBoxM fx eId [TextBoxResize _historySize] $ histBox panel
>   cmdBox' <- alterTextBoxM fx eId [TextBoxResize _commandSize] $ cmdBox panel
>   statusBox' <- alterTextBoxM fx eId [TextBoxResize _statusSize] $ statusBox panel
>   return $ panel
>     { textBox = textBox'
>     , histBox = histBox'
>     , cmdBox = cmdBox'
>     , statusBox = statusBox'
>     }



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
 >   :: EventId -> [PanelAction]
 >   -> Panel -> Panel
 > alterPanel eId acts panel =
 >   foldl (flip (alterPanelPrimitive eId)) panel acts

> alterPanelM
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> [PanelAction]
>   -> Panel -> m Panel
> alterPanelM fx eId acts panel = case acts of
>   [] -> return panel
>   a:as ->
>     alterPanelPrimitive fx eId a panel >>= alterPanelM fx eId as



> alterPanelPrimitive
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> PanelAction
>   -> Panel -> m Panel
> alterPanelPrimitive fx eId act = case act of
>   PanelAlterText as ->
>     panelAlterTextM fx eId as

>   PanelAlterCmd as ->
>     panelAlterCmdM fx eId as

>   PanelClearCmd ->
>     panelClearCmdM fx eId





-- ================= --
-- Primitive Actions --
-- ================= --

> panelAlterTextM
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> [TextBoxAction]
>   -> Panel -> m Panel
> panelAlterTextM fx eId as panel = do
>   box <- alterTextBoxM fx eId as (textBox panel)
>   return $ panel
>     { textBox = box
>     , textChanged = True
>     }

> panelAlterCmdM
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> [TextBoxAction]
>   -> Panel -> m Panel
> panelAlterCmdM fx eId as panel = do
>   box <- alterTextBoxM fx eId as (cmdBox panel)
>   return $ panel { cmdBox = box }

> panelClearCmdM
>   :: ( Monad m )
>   => TextBoxFX m -> EventId
>   -> Panel -> m Panel
> panelClearCmdM fx eId panel = do
>   box <- alterTextBoxM fx eId [TextBoxClear] (cmdBox panel)
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
>   ]
