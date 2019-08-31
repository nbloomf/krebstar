module Kreb.Editor.Settings (
    EditorMode(..)
) where

data EditorMode
  = NormalMode
  | InsertMode
  | CommandMode
  | LayoutMode
  deriving (Eq, Show)
