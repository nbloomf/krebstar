module Kreb.Editor.CLI.Handler (
    eventMapping
) where

import qualified Graphics.Vty as V

import Kreb.Editor




eventMapping :: EditorMode -> V.Event -> Action
eventMapping mode event = case event of
  V.EvResize w h ->
    WindowResize (w,h)

  V.EvKey V.KEsc [] ->
    SetMode NormalMode

  _ -> case mode of
    NormalMode -> case event of
      V.EvKey (V.KChar 'i') [] ->
        SetMode InsertMode

      V.EvKey (V.KChar 'c') [] ->
        SetMode CommandMode

      V.EvKey (V.KChar 'q') [] ->
        Quit

      V.EvKey (V.KChar 's') [V.MCtrl] ->
        FileSave

      _ ->
        ShowDebug $ " eventMapping (Nor): " ++ show event

    InsertMode -> case event of
      V.EvKey (V.KChar c) [] ->
        CharInsert c

      V.EvKey V.KEnter [] ->
        CharInsert '\n'

      V.EvKey V.KBS [] ->
        CharBackspace

      V.EvKey V.KDown [] ->
        CursorDown

      V.EvKey V.KUp [] ->
        CursorUp

      V.EvKey V.KRight [] ->
        CursorRight

      V.EvKey V.KLeft [] ->
        CursorLeft

      _ ->
        ShowDebug $ " eventMapping (Ins): " ++ show event

    CommandMode -> case event of
      V.EvKey (V.KChar c) [] ->
        CharInsertCmd c

      V.EvKey V.KBS [] ->
        CharBackspaceCmd

      V.EvKey V.KEnter [] ->
        RunCmd

      _ ->
        ShowDebug $ " eventMapping (Cmd): " ++ show event
