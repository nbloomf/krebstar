module Ned.App.IO.Event (
    convert
) where

import Ned

import qualified Graphics.Vty as V



convert :: V.Event -> AppEvent
convert event = case event of
  V.EvKey k ms ->
    EventKeyPress (key k) (map modifier ms)
  V.EvMouseDown row col b ms ->
    EventMouseDown (row, col) (button b) (map modifier ms)
  V.EvMouseUp row col b ->
    EventMouseUp (row, col) (fmap button b)
  V.EvResize row col ->
    EventResize (row, col)
  V.EvPaste bs ->
    EventPaste bs
  V.EvLostFocus ->
    EventFocusLost
  V.EvGainedFocus ->
    EventFocusGained



key :: V.Key -> Key
key k = case k of
  V.KChar c -> KeyChar c
  V.KEsc -> KeyEsc
  V.KBS -> KeyBackSpace
  V.KBackTab -> KeyBackTab
  V.KEnter -> KeyEnter

  V.KLeft -> KeyLeft
  V.KRight -> KeyRight
  V.KUp -> KeyUp
  V.KDown -> KeyDown
  V.KUpLeft -> KeyUpLeft
  V.KUpRight -> KeyUpRight
  V.KDownLeft -> KeyDownLeft
  V.KDownRight -> KeyDownRight
  V.KCenter -> KeyCenter

  V.KHome -> KeyHome
  V.KBegin -> KeyBegin
  V.KEnd -> KeyEnd
  V.KPageUp -> KeyPageUp
  V.KPageDown -> KeyPageDown
  V.KMenu -> KeyMenu

  V.KFun k -> KeyFunc k
  V.KPrtScr -> KeyPrintScreen
  V.KPause -> KeyPause

  V.KIns -> KeyIns
  V.KDel -> KeyDel



modifier :: V.Modifier -> Modifier
modifier m = case m of
  V.MAlt -> ModAlt
  V.MCtrl -> ModControl
  V.MShift -> ModShift
  V.MMeta -> ModMeta



button :: V.Button -> Button
button b = case b of
  V.BLeft -> BtnLeft
  V.BMiddle -> BtnMiddle
  V.BRight -> BtnRight
  V.BScrollUp -> BtnScrollUp
  V.BScrollDown -> BtnScrollDown
