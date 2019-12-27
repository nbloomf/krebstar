> module Kreb.Editor.TUI.Layout where

> import Data.Array

> import Kreb.Editor.Core.Action

> data MouseEvent

> data Window = Window
>   { cells :: Array (Int, Int) Char
>   , onMouseEvent :: MouseEvent -> Action
>   }

> data Layout