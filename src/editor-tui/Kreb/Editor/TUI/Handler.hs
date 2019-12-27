module Kreb.Editor.TUI.Handler (
    eventMapping
) where

import qualified Graphics.Vty as V

import Kreb.Editor.Core




eventMapping :: EditorMode -> V.Event -> [Action]
eventMapping mode event =
  if mode == NormalMode
    then case event of
      V.EvKey (V.KChar 'i') [] ->
        [ SetMode InsertMode ]

      V.EvKey (V.KChar 'c') [] ->
        [ SetMode CommandMode ]

      V.EvKey (V.KChar 'q') [] ->
        [ Quit ]

      V.EvKey (V.KChar 's') [V.MCtrl] ->
        [ FileSave ]

      _ ->
        [ ShowDebug $ " eventMapping (Nor): " ++ show event ]

    else case event of
      V.EvResize w h ->
        [ WindowResize (w,h) ]

      V.EvKey V.KEsc [] ->
        [ SetMode NormalMode ]

      V.EvKey (V.KChar c) [] ->
        [ CharInsert c ]

      V.EvKey V.KBS [] ->
        [ CharBackspace ]

      V.EvKey V.KLeft [] ->
        [ ClearMark, CursorLeft ]

      V.EvKey V.KRight [] ->
        [ ClearMark, CursorRight ]

      _ -> case mode of
        InsertMode -> case event of
          V.EvKey V.KEnter [] ->
            [ CharInsert '\n' ]

          V.EvKey V.KDown [] ->
            [ ClearMark, CursorDown ]
          V.EvKey V.KUp [] ->
            [ ClearMark, CursorUp ]

          V.EvKey V.KDown [V.MShift] ->
            [ LeaveMark, CursorDown ]
          V.EvKey V.KUp [V.MShift] ->
            [ LeaveMark, CursorUp ]
          V.EvKey V.KRight [V.MShift] ->
            [ LeaveMark, CursorRight ]
          V.EvKey V.KLeft [V.MShift] ->
            [ LeaveMark, CursorLeft ]

          _ ->
            [ ShowDebug $ " eventMapping (Ins): " ++ show event ]

        CommandMode -> case event of
          V.EvKey V.KEnter [] ->
            [ RunCmd ]

          _ ->
            [ ShowDebug $ " eventMapping (Cmd): " ++ show event ]
