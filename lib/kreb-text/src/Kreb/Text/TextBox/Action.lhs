> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE FlexibleContexts #-}

> module Kreb.Text.TextBox.Action where

> import Kreb.Prop

> data TextBoxAction
>   -- Text Manipulation
>   = TextBoxInsert (Char)
>   | TextBoxInsertMany [Char]
>   | TextBoxBackspace
> 
>   -- Navigation
>   | TextBoxCursorDown
>   | TextBoxCursorUp
>   | TextBoxCursorRight
>   | TextBoxCursorLeft
>   | TextBoxCursorTo (Int, Int)
>   | TextBoxCursorDrag (Int, Int)
>   | TextBoxCancelDrag
> 
>   | TextBoxLineStart
>   | TextBoxLineEnd
>   | TextBoxPageUp
>   | TextBoxPageDown
>   | TextBoxToTop
>   | TextBoxToBottom
> 
>   | TextBoxLeaveMark
>   | TextBoxClearMark
> 
>   -- Cut/Copy/Paste
>   | TextBoxDeleteRegion
>   | TextBoxClipRegion
>   | TextBoxPasteRegion
> 
>   -- Load/Save
>   | TextBoxLoad Bool
>   | TextBoxSave
> 
>   -- 
>   | TextBoxResize (Int, Int)

>   | TextBoxSetSource FilePath
> 

>   | TextBoxClear
>   deriving (Eq, Show)





> instance Arb TextBoxAction where
>   arb = selectFrom
>     [ TextBoxInsert <$> arb
>     , TextBoxInsertMany <$> arb
>     , return TextBoxBackspace

>     , return TextBoxCursorLeft
>     , return TextBoxCursorRight
>     , return TextBoxCursorUp
>     , return TextBoxCursorDown

>     ]

> instance Prune TextBoxAction where
>   prune x = case x of
>     TextBoxInsertMany cs ->
>       map TextBoxInsertMany $ prune cs
>     _ -> []




 >     , TextBoxCursorTo <$> arb
 >     , return TextBoxLeaveMark
 >     , return TextBoxClearMark
 >     , do
 >         Positive w <- arb
 >         Positive h <- arb
 >         return $ TextBoxResize (w,h)
 >     ]
 > 

