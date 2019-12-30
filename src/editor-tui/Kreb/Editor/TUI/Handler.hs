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
  :: EditorMode -> Layout -> V.Event -> [Action]
eventMapping mode layout event =
  case (mode, event) of


    -- All Modes --

    (_, V.EvMouseDown x y _ _) ->
      case isInside (x,y) $ _textRect layout of
        Just pos ->
          [ CursorDrag pos ]
        Nothing ->
          [ ShowDebug $ "eventMapping (Nor): " ++ show event
          , ShowDebug $ show layout
          , ShowDebug $ clickDebug layout (x,y)
          ]

    (_, V.EvMouseUp x y _) ->
      case isInside (x,y) $ _textRect layout of
        Just pos ->
          [ CancelDrag ]
        Nothing ->
          [ ShowDebug $ "eventMapping (Nor): " ++ show event
          , ShowDebug $ show layout
          , ShowDebug $ clickDebug layout (x,y)
          ]

    (_, V.EvResize w h) ->
      [ WindowResize (w,h) ]

    (_, V.EvKey V.KEsc []) ->
      [ SetMode NormalMode ]


    -- Normal Mode --

    (NormalMode, V.EvKey (V.KChar 'i') []) ->
      [ SetMode InsertMode ]

    (NormalMode, V.EvKey (V.KChar 'c') []) ->
      [ SetMode CommandMode ]

    (NormalMode, V.EvKey (V.KChar 's') [V.MCtrl]) ->
      [ FileSave ]

    (NormalMode, V.EvKey (V.KChar 'q') []) ->
      [ Quit ]

    (NormalMode, _) ->
      [ ShowDebug $ "eventMapping (Nor): " ++ show event ]


    -- Command or Insert Mode --

    (_, V.EvKey (V.KChar c) []) ->
      [ CharInsert c ]

    (_, V.EvKey V.KBS []) ->
      [ CharBackspace ]

    (_, V.EvKey V.KLeft []) ->
      [ ClearMark, CursorLeft ]

    (_, V.EvKey V.KRight []) ->
      [ ClearMark, CursorRight ]


    -- Command Mode --

    (CommandMode, V.EvKey V.KEnter []) ->
      [ RunCmd ]

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
            [ ShowDebug $ "eventMapping (Cmd): " ++ show event ]
