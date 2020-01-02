> module Kreb.Editor.Core.Data.Action (
>     Action(..)
> ) where

> import Kreb.Editor.Core.Data.Settings

> data Action
>   -- Nothing
>   = NoOp
>   | Quit

>   -- Meta
>   | SetMode EditorMode

>   -- Cursor Movement
>   | CursorUp
>   | CursorDown
>   | CursorRight
>   | CursorLeft
>   | CursorTo (Int, Int)

>   | CursorDrag (Int, Int)
>   | CancelDrag

>   -- Editing
>   | CharInsert Char
>   | CharBackspace
>   | StringInsert String

>   -- Load and Save
>   | FileLoad FilePath
>   | FileSetPath
>   | FileSave

>   | LeaveMark
>   | ClearMark

>   | RegionDelete
>   | RegionClip
>   | RegionPaste

>   | CursorLineStart
>   | CursorLineEnd

>   | CursorDocStart
>   | CursorDocEnd

>   -- Selection Management
>   | SelectionMark
>   | SelectionUnmark

>   -- Edit Operations
>   | CharOverwrite Char
>   | CharDelete


>   | LineDelete

>   | RunCmd

>   | ShowDebug String

>   | WindowResize (Int, Int)
>   deriving (Eq, Show)
