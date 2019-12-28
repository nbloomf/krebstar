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

>   | CursorLineStart
>   | CursorLineEnd

>   | CursorDocStart
>   | CursorDocEnd

>   -- Selection Management
>   | SelectionMark
>   | SelectionUnmark

>   | SelectionCut
>   | SelectionCopy
>   | SelectionPaste

>   -- Edit Operations
>   | CharOverwrite Char
>   | CharDelete


>   | LineDelete

>   | RunCmd

>   | ShowDebug String

>   | WindowResize (Int, Int)
>   deriving (Eq, Show)
