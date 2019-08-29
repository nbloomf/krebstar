module Ned.App.Action (
    Action(..)
  , Then(..)
  , performAction
) where

import Ned.App.State
import Ned.App.Error
import Ned.Data
import Ned.App.Hooks



data Then
  = GoOn
  | Stop
  | Bail AppError
  deriving (Eq, Show)



data Action
  -- Nothing
  = NoOp
  | Quit

  -- Cursor Movement
  | CursorUp
  | CursorDown
  | CursorRight
  | CursorLeft






  | CursorLineStart
  | CursorLineEnd

  | CursorDocStart
  | CursorDocEnd

  -- Selection Management
  | SelectionMark
  | SelectionUnmark

  | SelectionCut
  | SelectionCopy
  | SelectionPaste

  -- Edit Operations
  | CharInsertAfter Char
  | CharInsertBefore Char
  | CharOverwrite Char
  | CharDelete
  | CharBackspace

  | LineDelete

  | CharInsertCmdAfter Char

  | RunCmd

  -- Modes
  | SetMode EditorMode

  | SetError String
  | ClearError

  | WindowResize (Int, Int)
  deriving (Eq, Show)



performAction
  :: ( Monad m )
  => Action -> AppState m
  -> m (Then, AppState m)
performAction act st = case act of
  NoOp ->
    return (GoOn, st)

  SetError msg -> do
    let st' = setLastError msg st
    return (GoOn, st')

  ClearError -> do
    let st' = clearLastError st
    return (GoOn, st')

  Quit ->
    return (Stop, st)

  SetMode mode -> do
    let st' = setEditorMode mode st
    return (GoOn, st')

  CharInsertAfter c -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxInsert (fromChar c)]]) st
    return (GoOn, st')

  CharInsertCmdAfter c -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterCmd [TextBoxInsert (fromChar c)]]) st
    return (GoOn, st')

  CharBackspace -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxBackspace]]) st
    return (GoOn, st')

  CursorUp -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxCursorUp]]) st
    return (GoOn, st')

  CursorDown -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxCursorDown]]) st
    return (GoOn, st')

  CursorRight -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxCursorRight]]) st
    return (GoOn, st')

  CursorLeft -> do
    let
      st' = alterActivePanel (alterPanel
        [PanelAlterText [TextBoxCursorLeft]]) st
    return (GoOn, st')

  WindowResize (w,h) -> do
    let
      st' = setWindowDim (w,h) st
    return (GoOn, st')

  RunCmd -> do
    let
      cmd = queryActivePanel getPanelCmdString st
      st2 = alterActivePanel (alterPanel
        [PanelClearCmd]) st
    case cmd of
      Nothing -> do
        let st3 = setLastError "no command" st2
        return (GoOn, st2)
      Just str -> do
        r <- evalHook str st2
        case r of
          Left err -> do
            let st3 = setLastError (show err) st2
            return (GoOn, st3)
          Right st' -> return (GoOn, st')









