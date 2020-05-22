module Kreb.Editor.TUI.Handler (
    eventMapping
) where

import qualified Graphics.Vty as V

import Kreb.Editor.Core
import Kreb.Editor.TUI.Data.Layout


clickDebug :: Layout -> (Int, Int) -> String
clickDebug l pos = concat
  [ "Locating mouse event:\n"
  , case isInside pos $ _textRect l of
      Nothing -> "- not inside text rect\n"
      Just x -> "- inside text rect at " ++ show x ++ "\n"
  , case isInside pos $ _cmdRect l of
      Nothing -> "- not inside cmd rect\n"
      Just x -> "- inside cmd rect at " ++ show x ++ "\n"
  , case isInside pos $ _histRect l of
      Nothing -> "- not inside hist rect\n"
      Just x -> "- inside hist rect at " ++ show x ++ "\n"
  , case isInside pos $ _statusRect l of
      Nothing -> "- not inside status rect\n"
      Just x -> "- inside status rect at " ++ show x ++ "\n"
  ]



eventMapping
  :: EditorMode -> Layout -> V.Event -> (EditorMode, [(EditorMode, Action)])
eventMapping mode layout event =
  case (mode, event) of


    -- All Modes --

    (_, V.EvMouseDown x y _ _) ->
      case isInside (x,y) $ _textRect layout of
        Just pos ->
          (mode, [ (mode, CursorDrag pos) ])
        Nothing ->
          (mode, [ (mode, ShowDebug $ "eventMapping (Nor): " ++ show event)
          , (mode, ShowDebug $ show layout)
          , (mode, ShowDebug $ clickDebug layout (x,y))
          ])

    (_, V.EvMouseUp x y _) ->
      case isInside (x,y) $ _textRect layout of
        Just pos ->
          (mode, [ (mode, CancelDrag) ])
        Nothing ->
          (mode, [ (mode, ShowDebug $ "eventMapping (Nor): " ++ show event)
          , (mode, ShowDebug $ show layout)
          , (mode, ShowDebug $ clickDebug layout (x,y))
          ])

    (_, V.EvResize w h) ->
      (mode, [ (mode, WindowResize (w,h)) ])

    (_, V.EvKey V.KEsc []) ->
      (NormalMode, [])


    -- Normal Mode --

    (NormalMode, V.EvKey (V.KChar 'i') []) ->
      (InsertMode, [])

    (NormalMode, V.EvKey (V.KChar 'c') []) ->
      (CommandMode, [])

    (NormalMode, V.EvKey (V.KChar 's') [V.MCtrl]) ->
      (mode, [ (mode, FileSave) ])

    (NormalMode, V.EvKey (V.KChar 'q') []) ->
      (mode, [ (mode, Quit) ])

    (NormalMode, _) ->
      (mode, [ (mode, ShowDebug $ "eventMapping (Nor): " ++ show event) ])


    -- Command or Insert Mode --

    (_, V.EvKey V.KBS []) ->
      (mode, [ (mode, CharBackspace) ])

    (_, V.EvKey V.KLeft []) ->
      (mode, [ (mode, ClearMark), (mode, CursorLeft) ])

    (_, V.EvKey V.KRight []) ->
      (mode, [ (mode, ClearMark), (mode, CursorRight) ])

    (_, V.EvKey (V.KChar 'x') [V.MCtrl]) ->
      (mode, [ (mode, RegionClip), (mode, RegionDelete) ])

    (_, V.EvKey (V.KChar 'c') [V.MCtrl]) ->
      (mode, [ (mode, RegionClip) ])

    (_, V.EvKey (V.KChar 'v') [V.MCtrl]) ->
      (mode, [ (mode, RegionPaste) ])

    (_, V.EvKey (V.KChar c) []) ->
      (mode, [ (mode, CharInsert c) ])


    -- Command Mode --

    (CommandMode, V.EvKey V.KEnter []) ->
      (mode, [ (mode, RunCmd) ])


    -- Insert Mode --

    (InsertMode, V.EvKey V.KEnter []) ->
      (mode, [ (mode, CharInsert '\n') ])

    (InsertMode, V.EvKey V.KDown []) ->
      (mode, [ (mode, ClearMark), (mode, CursorDown) ])

    (InsertMode, V.EvKey V.KUp []) ->
      (mode, [ (mode, ClearMark), (mode, CursorUp) ])

    (InsertMode, V.EvKey V.KDown [V.MShift]) ->
      (mode, [ (mode, LeaveMark), (mode, CursorDown) ])

    (InsertMode, V.EvKey V.KUp [V.MShift]) ->
      (mode, [ (mode, LeaveMark), (mode, CursorUp) ])

    (InsertMode, V.EvKey V.KRight [V.MShift]) ->
      (mode, [ (mode, LeaveMark), (mode, CursorRight) ])

    (InsertMode, V.EvKey V.KLeft [V.MShift]) ->
      (mode, [ (mode, LeaveMark), (mode, CursorLeft) ])

    _ -> (mode, [ (mode, ShowDebug $ "eventMapping: " ++ show event) ])
