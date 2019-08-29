module Ned.App.Handler (
    handler
) where

import Ned.App.State
import Ned.App.Event
import Ned.App.Error
import Ned.App.Action



handler
  :: ( Monad m )
  => AppState m -> AppEvent -> m (Then, AppState m)
handler st event =
  performAction (eventMapping (getEditorMode st) event) st



eventMapping :: EditorMode -> AppEvent -> Action
eventMapping mode event = case event of
  EventResize (w,h) ->
    WindowResize (w,h)

  EventKeyPress KeyEsc [] ->
    SetMode NormalMode

  _ -> case mode of
    NormalMode -> case event of
      EventKeyPress (KeyChar 'i') [] ->
        SetMode InsertMode

      EventKeyPress (KeyChar 'c') [] ->
        SetMode CommandMode

      EventKeyPress (KeyChar 'q') [] ->
        Quit

      _ ->
        SetError $ " eventMapping (Nor): " ++ show event

    InsertMode -> case event of
      EventKeyPress (KeyChar c) [] ->
        CharInsertAfter c

      EventKeyPress KeyEnter [] ->
        CharInsertAfter '\n'

      EventKeyPress KeyBackSpace [] ->
        CharBackspace

      EventKeyPress KeyDown [] ->
        CursorDown

      EventKeyPress KeyUp [] ->
        CursorUp

      EventKeyPress KeyRight [] ->
        CursorRight

      EventKeyPress KeyLeft [] ->
        CursorLeft

      _ ->
        SetError $ " eventMapping (Ins): " ++ show event

    CommandMode -> case event of
      EventKeyPress (KeyChar c) [] ->
        CharInsertCmdAfter c

      EventKeyPress KeyEnter [] ->
        RunCmd

      _ ->
        SetError $ " eventMapping (Cmd): " ++ show event
