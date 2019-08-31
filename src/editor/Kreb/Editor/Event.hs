module Kreb.Editor.Event (
    AppEvent(..)
  , Key(..)
  , Modifier(..)
  , Button(..)
) where

import Data.ByteString



data AppEvent
  = EventKeyPress Key [Modifier]
  | EventMouseDown (Int, Int) Button [Modifier]
  | EventMouseUp (Int, Int) (Maybe Button)
  | EventResize (Int, Int)
  | EventPaste ByteString
  | EventFocusLost
  | EventFocusGained
  deriving (Eq, Show)



data Key
  = KeyChar Char
  | KeyEsc
  | KeyBackSpace
  | KeyBackTab
  | KeyEnter

  | KeyLeft
  | KeyRight
  | KeyUp
  | KeyDown
  | KeyUpLeft
  | KeyUpRight
  | KeyDownLeft
  | KeyDownRight
  | KeyCenter

  | KeyHome
  | KeyBegin
  | KeyEnd
  | KeyPageUp
  | KeyPageDown
  | KeyMenu

  | KeyFunc Int
  | KeyPrintScreen
  | KeyPause

  | KeyIns
  | KeyDel
  deriving (Eq, Show)



data Modifier
  = ModAlt
  | ModControl
  | ModShift
  | ModMeta
  deriving (Eq, Show)



data Button
  = BtnLeft
  | BtnMiddle
  | BtnRight
  | BtnScrollUp
  | BtnScrollDown
  deriving (Eq, Show)
