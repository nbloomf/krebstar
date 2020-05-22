---
title: Kreb.Text.Buffer.Test
---

> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleContexts #-}
> 
> module Kreb.Text.Buffer.Test (
>     test_Buffer
> ) where
> 
> import Data.Proxy
> import Data.List
> import qualified Data.Foldable as Fold
> import System.Environment
> 
> import Test.Tasty
> 
> import           Kreb.Prop
> import           Kreb.Reflect
> import           Kreb.Arith
> import           Kreb.Struct.Class.Valued
> import           Kreb.Text.Grapheme
> import           Kreb.Text.MeasureText
> import           Kreb.Text.ScreenOffset
> import qualified Kreb.Struct.Data.FingerTree as FT
> import           Kreb.Text.Buffer



> test_Buffer :: TestTree
> test_Buffer =
>   testGroup "Buffers"
>     [ test_Buffer_properties "Char" nat30 nat8 nat10
>     , test_Buffer_properties "Char" nat30 nat4 nat10
>     , test_Buffer_properties "Char" nat15 nat2 nat10
>     , test_Buffer_properties "Char" nat8  nat2 nat10
> 
>     , test_Buffer_properties "Char" nat30 nat8 nat100
>     , test_Buffer_properties "Char" nat30 nat4 nat100
>     , test_Buffer_properties "Char" nat15 nat2 nat100
>     , test_Buffer_properties "Char" nat8  nat2 nat100
>     ]

> test_Buffer_properties
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => String -> Proxy w -> Proxy t -> Proxy d
>   -> TestTree
> test_Buffer_properties label pw pt pd =
>   let title = "Buffer (" ++ label ++ ")"
>   in testGroup title
>     [ krebProp
>         "validate as == True" $
>         \(as :: Buffer w t d) ->
>           claimTrue (validate as)
> 
>     , krebProp
>         "hasValidCursorData" $
>         \(as :: Buffer w t d) ->
>           claimTrue (hasValidCursorData as)



Value Getters Group
===================

>     , krebProp
>         "getBufferWidth == getBufferWidth . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (clearFocusMark as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (clearFocusMark as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (clearFocusMark as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (clearFocusMark as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (clearFocusMark as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (clearFocusMark as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (clearFocusMark as))

>     , krebProp
>         "getBufferWidth == getBufferWidth . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (jumpFocusRegionToEnd as))

>     , krebProp
>         "getBufferWidth == getBufferWidth . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (moveFocusMarkRight as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (moveFocusPointLeft as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (moveFocusPointRight as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (moveFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (moveFocusMarkRight as))

>     , krebProp
>         "getBufferWidth == getBufferWidth . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferWidth == getBufferWidth . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferTabStop == getBufferTabStop . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferCharCount == getBufferCharCount . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferByteCount == getBufferByteCount . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferLineCol == getBufferLineCol . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getBufferLineCol (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferScreenCoords == getBufferScreenCoords . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getBufferScreenCoords (fillFocusMarkRight as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (fillFocusPointLeft as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (fillFocusPointRight as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (fillFocusMarkLeft as))
> 
>     , krebProp
>         "getBufferScreenOffset == getBufferScreenOffset . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenOffset as)
>             (getBufferScreenOffset (fillFocusMarkRight as))














To Be Categorized
=================


>     , krebProp
>         "jumpFocusRegionToStart . jumpFocusRegionToStart == jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (jumpFocusRegionToStart as)
>             (jumpFocusRegionToStart (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "jumpFocusRegionToEnd . jumpFocusRegionToEnd == jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (jumpFocusRegionToEnd as)
>             (jumpFocusRegionToEnd (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "jumpFocusRegionToStart . jumpFocusRegionToEnd . jumpFocusRegionToStart == jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (jumpFocusRegionToStart as)
>             (jumpFocusRegionToStart (jumpFocusRegionToEnd (jumpFocusRegionToStart as)))
> 
>     , krebProp
>         "jumpFocusRegionToEnd . jumpFocusRegionToStart . jumpFocusRegionToEnd == jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (jumpFocusRegionToEnd as)
>             (jumpFocusRegionToEnd (jumpFocusRegionToStart (jumpFocusRegionToEnd as)))



>     , krebProp
>         "isFocusRegionAtStart (jumpFocusRegionToStart as) == true" $
>         \(as :: Buffer w t d) ->
>           claimTrue (isFocusRegionAtStart (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "isFocusRegionAtStart (jumpFocusRegionToEnd as) == isBufferEmpty as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isFocusRegionAtStart (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "isFocusRegionAtEnd (jumpFocusRegionToEnd as) == true" $
>         \(as :: Buffer w t d) ->
>           claimTrue (isFocusRegionAtEnd (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "isFocusRegionAtEnd (jumpFocusRegionToStart as) == isBufferEmpty as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isFocusRegionAtEnd (jumpFocusRegionToStart as))




>     , krebProp
>         "clearFocusMark . clearFocusMark == clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (clearFocusMark as)
>             (clearFocusMark (clearFocusMark as))



>     , krebProp
>         "moveFocusPointLeft . moveFocusPointRight . moveFocusPointLeft == moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointLeft as)
>             (moveFocusPointLeft (moveFocusPointRight (moveFocusPointLeft as)))
> 
>     , krebProp
>         "moveFocusPointRight . moveFocusPointLeft . moveFocusPointRight == moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointRight as)
>             (moveFocusPointRight (moveFocusPointLeft (moveFocusPointRight as)))
> 
>     , krebProp
>         "moveFocusMarkLeft . moveFocusMarkRight . moveFocusMarkLeft == moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusMarkLeft as)
>             (moveFocusMarkLeft (moveFocusMarkRight (moveFocusMarkLeft as)))
> 
>     , krebProp
>         "moveFocusMarkRight . moveFocusMarkLeft . moveFocusMarkRight == moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusMarkRight as)
>             (moveFocusMarkRight (moveFocusMarkLeft (moveFocusMarkRight as)))
> 
>     , krebProp
>         "moveFocusPointLeft . moveFocusMarkLeft == moveFocusMarkLeft . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointLeft (moveFocusMarkLeft as))
>             (moveFocusMarkLeft (moveFocusPointLeft as))
> 
>     , krebProp
>         "moveFocusPointLeft . moveFocusMarkRight == moveFocusMarkRight . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointLeft (moveFocusMarkRight as))
>             (moveFocusMarkRight (moveFocusPointLeft as))
> 
>     , krebProp
>         "moveFocusPointRight . moveFocusMarkLeft == moveFocusMarkLeft . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointRight (moveFocusMarkLeft as))
>             (moveFocusMarkLeft (moveFocusPointRight as))
> 
>     , krebProp
>         "moveFocusPointRight . moveFocusMarkRight == moveFocusMarkRight . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusPointRight (moveFocusMarkRight as))
>             (moveFocusMarkRight (moveFocusPointRight as))



>     , krebProp
>         "fillFocusPointLeft . fillFocusPointLeft == fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft as)
>             (fillFocusPointLeft (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusPointRight . fillFocusPointRight == fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight as)
>             (fillFocusPointRight (fillFocusPointRight as))
> 
>     , krebProp
>         "fillFocusPointRight . fillFocusPointLeft . fillFocusPointRight == fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (fillFocusPointLeft (fillFocusPointRight as)))
>             (fillFocusPointRight as)
> 
>     , krebProp
>         "fillFocusPointLeft . fillFocusPointRight . fillFocusPointLeft == fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (fillFocusPointRight (fillFocusPointLeft as)))
>             (fillFocusPointLeft as)
> 
>     , krebProp
>         "fillFocusMarkLeft . fillFocusMarkLeft == fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkLeft as)
>             (fillFocusMarkLeft (fillFocusMarkLeft as))
> 
>     , krebProp
>         "fillFocusMarkRight . fillFocusMarkRight == fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkRight as)
>             (fillFocusMarkRight (fillFocusMarkRight as))
> 
>     , krebProp
>         "fillFocusMarkRight . fillFocusMarkLeft . fillFocusMarkRight == fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkRight (fillFocusMarkLeft (fillFocusMarkRight as)))
>             (fillFocusMarkRight as)
> 
>     , krebProp
>         "fillFocusMarkLeft . fillFocusMarkRight . fillFocusMarkLeft == fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkLeft (fillFocusMarkRight (fillFocusMarkLeft as)))
>             (fillFocusMarkLeft as)
> 
>     , krebProp
>         "fillFocusPointLeft . fillFocusMarkLeft == fillFocusMarkLeft . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (fillFocusMarkLeft as))
>             (fillFocusMarkLeft (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusPointRight . fillFocusMarkLeft == fillFocusMarkLeft . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (fillFocusMarkLeft as))
>             (fillFocusMarkLeft (fillFocusPointRight as))
> 
>     , krebProp
>         "fillFocusPointLeft . fillFocusMarkRight == fillFocusMarkRight . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (fillFocusMarkRight as))
>             (fillFocusMarkRight (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusPointRight . fillFocusMarkRight == fillFocusMarkRight . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (fillFocusMarkRight as))
>             (fillFocusMarkRight (fillFocusPointRight as))



>     , krebProp
>         "valuesAroundFocusPoint == valuesAroundFocusPoint . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusPoint as)
>             (valuesAroundFocusPoint (moveFocusMarkLeft as))
> 
>     , krebProp
>         "valuesAroundFocusPoint == valuesAroundFocusPoint . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusPoint as)
>             (valuesAroundFocusPoint (moveFocusMarkRight as))
> 
>     , krebProp
>         "valuesAroundFocusMark == valuesAroundFocusMark . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusMark as)
>             (valuesAroundFocusMark (moveFocusPointLeft as))
> 
>     , krebProp
>         "valuesAroundFocusMark == valuesAroundFocusMark . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusMark as)
>             (valuesAroundFocusMark (moveFocusPointRight as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (moveFocusPointLeft as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (moveFocusPointRight as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (moveFocusMarkLeft as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (moveFocusMarkRight as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (moveFocusPointLeft as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (moveFocusPointRight as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (moveFocusMarkLeft as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (moveFocusMarkRight as))



>     , krebProp
>         "valuesAroundFocusPoint == valuesAroundFocusPoint . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusPoint as)
>             (valuesAroundFocusPoint (fillFocusMarkLeft as))
> 
>     , krebProp
>         "valuesAroundFocusPoint == valuesAroundFocusPoint . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusPoint as)
>             (valuesAroundFocusPoint (fillFocusMarkRight as))
> 
>     , krebProp
>         "valuesAroundFocusMark == valuesAroundFocusMark . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusMark as)
>             (valuesAroundFocusMark (fillFocusPointLeft as))
> 
>     , krebProp
>         "valuesAroundFocusMark == valuesAroundFocusMark . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valuesAroundFocusMark as)
>             (valuesAroundFocusMark (fillFocusPointRight as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (fillFocusPointLeft as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (fillFocusPointRight as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (fillFocusMarkLeft as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (fillFocusMarkRight as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (fillFocusPointLeft as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (fillFocusPointRight as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (fillFocusMarkLeft as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (fillFocusMarkRight as))



>     , krebProp
>         "valueAtStart == valueAtStart . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "valueAtStart == valueAtStart . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtStart as)
>             (valueAtStart (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "valueAtEnd == valueAtEnd . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (valueAtEnd as)
>             (valueAtEnd (jumpFocusRegionToEnd as))














>     , krebProp
>         "hasValidCursorData == hasValidCursorData . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "hasChars == hasChars . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "hasChars == hasChars . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (jumpFocusRegionToEnd as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . jumpFocusRegionToStart" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (jumpFocusRegionToStart as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . jumpFocusRegionToEnd" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (jumpFocusRegionToEnd as))


>     , krebProp
>         "hasValidCursorData == hasValidCursorData . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (clearFocusMark as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (clearFocusMark as))
> 
>     , krebProp
>         "hasChars == hasChars . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (clearFocusMark as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (clearFocusMark as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . clearFocusMark" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (clearFocusMark as))



>     , krebProp
>         "hasValidCursorData == hasValidCursorData . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (moveFocusPointLeft as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (moveFocusPointRight as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (moveFocusMarkLeft as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (moveFocusMarkRight as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (moveFocusPointLeft as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (moveFocusPointRight as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (moveFocusMarkLeft as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (moveFocusMarkRight as))
> 
>     , krebProp
>         "hasChars == hasChars . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (moveFocusPointLeft as))
> 
>     , krebProp
>         "hasChars == hasChars . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (moveFocusPointRight as))
> 
>     , krebProp
>         "hasChars == hasChars . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (moveFocusMarkLeft as))
> 
>     , krebProp
>         "hasChars == hasChars . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (moveFocusMarkRight as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (moveFocusPointLeft as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (moveFocusPointRight as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (moveFocusMarkLeft as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (moveFocusMarkRight as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . moveFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (moveFocusPointLeft as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . moveFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (moveFocusPointRight as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . moveFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (moveFocusMarkLeft as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . moveFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (moveFocusMarkRight as))



>     , krebProp
>         "hasValidCursorData == hasValidCursorData . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (fillFocusPointLeft as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (fillFocusPointRight as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (fillFocusMarkLeft as))
> 
>     , krebProp
>         "hasValidCursorData == hasValidCursorData . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasValidCursorData as)
>             (hasValidCursorData (fillFocusMarkRight as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (fillFocusPointLeft as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (fillFocusPointRight as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (fillFocusMarkLeft as))
> 
>     , krebProp
>         "hasUniqueRegion == hasUniqueRegion . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasUniqueRegion as)
>             (hasUniqueRegion (fillFocusMarkRight as))
> 
>     , krebProp
>         "hasChars == hasChars . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (fillFocusPointLeft as))
> 
>     , krebProp
>         "hasChars == hasChars . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (fillFocusPointRight as))
> 
>     , krebProp
>         "hasChars == hasChars . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (fillFocusMarkLeft as))
> 
>     , krebProp
>         "hasChars == hasChars . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (hasChars as)
>             (hasChars (fillFocusMarkRight as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (fillFocusPointLeft as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (fillFocusPointRight as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (fillFocusMarkLeft as))
> 
>     , krebProp
>         "isBufferEmpty == isBufferEmpty . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferEmpty as)
>             (isBufferEmpty (fillFocusMarkRight as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (fillFocusPointLeft as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (fillFocusPointRight as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (fillFocusMarkLeft as))
> 
>     , krebProp
>         "isBufferNotEmpty == isBufferNotEmpty . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (isBufferNotEmpty as)
>             (isBufferNotEmpty (fillFocusMarkRight as))



>     , krebProp
>         "fillFocusPointLeft . moveFocusPointLeft == moveFocusPointLeft . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (moveFocusPointLeft as))
>             (moveFocusPointLeft (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusMarkLeft . moveFocusPointLeft == moveFocusPointLeft . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkLeft (moveFocusPointLeft as))
>             (moveFocusPointLeft (fillFocusMarkLeft as))
> 
>     , krebProp
>         "fillFocusMarkRight . moveFocusPointLeft == moveFocusPointLeft . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkRight (moveFocusPointLeft as))
>             (moveFocusPointLeft (fillFocusMarkRight as))
> 
>     , krebProp
>         "fillFocusMarkLeft . moveFocusMarkLeft == moveFocusMarkLeft . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkLeft (moveFocusMarkLeft as))
>             (moveFocusMarkLeft (fillFocusMarkLeft as))
> 
>     , krebProp
>         "fillFocusPointLeft . moveFocusMarkLeft == moveFocusMarkLeft . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (moveFocusMarkLeft as))
>             (moveFocusMarkLeft (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusPointRight . moveFocusMarkLeft == moveFocusMarkLeft . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (moveFocusMarkLeft as))
>             (moveFocusMarkLeft (fillFocusPointRight as))
> 
>     , krebProp
>         "fillFocusPointRight . moveFocusPointRight == moveFocusPointRight . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (moveFocusPointRight as))
>             (moveFocusPointRight (fillFocusPointRight as))
> 
>     , krebProp
>         "fillFocusMarkLeft . moveFocusPointRight == moveFocusPointRight . fillFocusMarkLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkLeft (moveFocusPointRight as))
>             (moveFocusPointRight (fillFocusMarkLeft as))
> 
>     , krebProp
>         "fillFocusMarkRight . moveFocusPointRight == moveFocusPointRight . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkRight (moveFocusPointRight as))
>             (moveFocusPointRight (fillFocusMarkRight as))
> 
>     , krebProp
>         "fillFocusMarkRight . moveFocusMarkRight == moveFocusMarkRight . fillFocusMarkRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusMarkRight (moveFocusMarkRight as))
>             (moveFocusMarkRight (fillFocusMarkRight as))
> 
>     , krebProp
>         "fillFocusPointLeft . moveFocusMarkRight == moveFocusMarkRight . fillFocusPointLeft" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointLeft (moveFocusMarkRight as))
>             (moveFocusMarkRight (fillFocusPointLeft as))
> 
>     , krebProp
>         "fillFocusPointRight . moveFocusMarkRight == moveFocusMarkRight . fillFocusPointRight" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fillFocusPointRight (moveFocusMarkRight as))
>             (moveFocusMarkRight (fillFocusPointRight as))




>     , krebProp
>         "hasChars (insertAtStart eId c xs) == True" $
>         \(e :: EventId) (c :: Char) (as :: Buffer w t d) ->
>           claimTrue
>             (hasChars (fst $ insertAtStart e c as))
> 
>     , krebProp
>         "hasChars (insertAtEnd eId c xs) == True" $
>         \(e :: EventId) (c :: Char) (as :: Buffer w t d) ->
>           claimTrue
>             (hasChars (fst $ insertAtEnd e c as))

>     ]








To Be Refactored
================




> {-



>     ]

> 
>     , krebProp
>         "isBufferEmpty empty == True" $
>         let e = empty :: Buffer w t d
>         in claimTrue (isBufferEmpty e)
> 
>     , krebProp
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           let as = singleton (EventId 0 "t") a :: Buffer w t d
>           in claimTrue (isSingleton as)
> 
>     , krebProp
>         "isBufferEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton (EventId 0 "t") a :: Buffer w t d
>           in claimFalse (isBufferEmpty as)
> 

> 
>     , krebProp
>         "moveFocusToStart (moveMarkToStart as) == moveMarkToStart (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusToStart (moveMarkToStart as))
>             (moveMarkToStart (moveFocusToStart as))
> 
>     , krebProp
>         "moveFocusToStart (moveMarkToEnd as) == moveMarkToEnd (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusToStart (moveMarkToEnd as))
>             (moveMarkToEnd (moveFocusToStart as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkToStart as) == moveMarkToStart (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointToEnd (moveMarkToStart as))
>             (moveMarkToStart (movePointToEnd as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkToEnd as) == moveMarkToEnd (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointToEnd (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToEnd as))
> 
>     , krebProp
>         "(isPointAtEnd as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isPointAtEnd as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , krebProp
>         "(isPointAtStart as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isPointAtStart as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , krebProp
>         "(isMarkAtEnd as) || (as == moveMarkLeft (moveMarkRight as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isMarkAtEnd as)
>             , claimEqual
>                 (as)
>                 (moveMarkLeft (moveMarkRight as))
>             ]
> 
>     , krebProp
>         "(isMarkAtStart as) || (as == moveMarkRight (moveMarkLeft as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isMarkAtStart as)
>             , claimEqual
>                 (as)
>                 (moveMarkRight (moveMarkLeft as))
>             ]
> 

> 
>     , krebProp
>         "movePointLeft (moveMarkToStart as) == moveMarkToStart (movePointLeft as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointLeft (moveMarkToStart as))
>             (moveMarkToStart (movePointLeft as))
> 
>     , krebProp
>         "movePointRight (moveMarkToStart as) == moveMarkToStart (movePointRight as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointRight (moveMarkToStart as))
>             (moveMarkToStart (movePointRight as))
> 
>     , krebProp
>         "movePointLeft (moveMarkToEnd as) == moveMarkToEnd (movePointLeft as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointLeft (moveMarkToEnd as))
>             (moveMarkToEnd (movePointLeft as))
> 
>     , krebProp
>         "movePointRight (moveMarkToEnd as) == moveMarkToEnd (movePointRight as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointRight (moveMarkToEnd as))
>             (moveMarkToEnd (movePointRight as))
> 
>     , krebProp
>         "moveFocusToStart (moveMarkLeft as) == moveMarkLeft (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusToStart (moveMarkLeft as))
>             (moveMarkLeft (moveFocusToStart as))
> 
>     , krebProp
>         "moveFocusToStart (moveMarkRight as) == moveMarkRight (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusToStart (moveMarkRight as))
>             (moveMarkRight (moveFocusToStart as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkLeft as) == moveMarkLeft (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointToEnd (moveMarkLeft as))
>             (moveMarkLeft (movePointToEnd as))
> 
>     , krebProp
>         "movePointToEnd (moveMarkRight as) == moveMarkRight (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (movePointToEnd (moveMarkRight as))
>             (moveMarkRight (movePointToEnd as))
> 
>     , krebProp
>         "(isBufferEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isBufferEmpty as)
>             , claimEqual
>                 (fst (insertAtStart (EventId 0 "t") u (fst (insertAtEnd (EventId 0 "t") v as))))
>                 (fst (insertAtEnd (EventId 0 "t") v (fst (insertAtStart (EventId 0 "t") u as))))
>             ]
> 
>     , krebProp
>         "(isBufferEmpty as) || (deletePointLeft (insertPointLeft u as) == as)" $
>         \(u :: a) (as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isBufferEmpty as)
>             , claimEqual
>                 (as)
>                 (fst (deletePointLeft (EventId 0 "t") (fst (insertPointLeft (EventId 0 "t") u as))))
>             ]
> 
>     , krebProp
>         "(isBufferEmpty as) || (readPoint (insertAtStart u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isBufferEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (fst (insertAtStart (EventId 0 "t") u as)))
>             ]
> 
>     , krebProp
>         "(isBufferEmpty as) || (readPoint (insertAtEnd u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isBufferEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (fst (insertAtEnd (EventId 0 "t") u as)))
>             ]
> 
>     , krebProp
>         "deleteAtStart (insertAtStart u as) == as" $
>         \(u :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (as)
>             (fst (deleteAtStart (EventId 0 "t") (fst (insertAtStart (EventId 0 "t") u as))))
> 
>     , krebProp
>         "deleteAtEnd (insertAtEnd u as) == as" $
>         \(u :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (as)
>             (fst (deleteAtEnd (EventId 0 "t") (fst (insertAtEnd (EventId 0 "t") u as))))
> 
>     , krebProp
>         "deleteAtStart (deleteAtEnd as) == deleteAtEnd (deleteAtStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (fst (deleteAtEnd (EventId 0 "t") (fst (deleteAtStart (EventId 0 "t") as))))
>             (fst (deleteAtStart (EventId 0 "t") (fst (deleteAtEnd (EventId 0 "t") as))))
> 
>     , krebProp
>         "(isBufferEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t d) ->
>           claimAny
>             [ claimTrue (isBufferEmpty as)
>             , claimEqual
>                 (fst (insertAtStart (EventId 0 "t") u (fst (insertAtEnd (EventId 0 "t") v as))))
>                 (fst (insertAtEnd (EventId 0 "t") v (fst (insertAtStart (EventId 0 "t") u as))))
>             ]
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertAtStart (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertAtStart (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertAtEnd (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertAtEnd (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertPointLeft (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertPointLeft (EventId 0 "t") a as)))
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (deleteAtStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deleteAtStart (EventId 0 "t") as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (deleteAtStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deleteAtStart (EventId 0 "t") as)))
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (deleteAtEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deleteAtEnd (EventId 0 "t") as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (deleteAtEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deleteAtEnd (EventId 0 "t") as)))
> 
>     , krebProp
>         "getBufferWidth as == getBufferWidth (deletePointLeft as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deletePointLeft (EventId 0 "t") as)))
> 
>     , krebProp
>         "getBufferTabStop as == getBufferTabStop (deletePointLeft as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deletePointLeft (EventId 0 "t") as)))
> 
>     , krebProp
>         "mempty == getPointLineCol (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (mempty)
>             (getPointLineCol (moveFocusToStart as))
> 
>     , krebProp
>         "getBufferLineCol as == getPointLineCol (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getPointLineCol (movePointToEnd as))
> 
>     , krebProp
>         "(hasMark as == False) || (getBufferLineCol as == getMarkLineCol (moveMarkToEnd as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferLineCol as)
>                 (getMarkLineCol (moveMarkToEnd as))
>             ]
> 
>     , krebProp
>         "(0,0) == getPointScreenCoords (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (0,0)
>             (getPointScreenCoords (moveFocusToStart as))
> 
>     , krebProp
>         "getBufferScreenCoords as == getPointScreenCoords (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getPointScreenCoords (movePointToEnd as))
> 
>     , krebProp
>         "(hasMark as == False) || (getBufferScreenCoords as == getMarkScreenCoords (moveMarkToEnd as))" $
>         \(as :: Buffer w t d) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferScreenCoords as)
>                 (getMarkScreenCoords (moveMarkToEnd as))
>             ]
> 
>     , krebProp
>         "getBufferByteCount as == getBufferByteCount (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveFocusToStart as))
> 
>     , krebProp
>         "getBufferCharCount as == getBufferCharCount (moveFocusToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveFocusToStart as))
> 
>     , krebProp
>         "getBufferByteCount as == getBufferByteCount (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (movePointToEnd as))
> 
>     , krebProp
>         "getBufferCharCount as == getBufferCharCount (movePointToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (movePointToEnd as))
> 
>     , krebProp
>         "getBufferByteCount as == getBufferByteCount (moveMarkToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToStart as))
> 
>     , krebProp
>         "getBufferCharCount as == getBufferCharCount (moveMarkToStart as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToStart as))
> 
>     , krebProp
>         "getBufferByteCount as == getBufferByteCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToEnd as))
> 
>     , krebProp
>         "getBufferCharCount as == getBufferCharCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToEnd as))
> 
>     , krebProp
>         "atOrAfterLineCol is monotone on buffers" $
>         \(lc :: LineCol) (as :: Buffer w t d) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterLineCol lc m | (_, m) <- toAnnotatedList as])
> 
>     , krebProp
>         "atOrAfterScreenCoords is monotone on buffers" $
>         \((NonNegative u, NonNegative v)
>             :: (NonNegative Int, NonNegative Int))
>          (as :: Buffer w t d) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenCoords (u,v) m | (_, m) <- toAnnotatedList as])
> 
>     , krebProp
>         "atOrAfterScreenLine is monotone on buffers" $
>         \(NonNegative u :: NonNegative Int)
>          (as :: Buffer w t d) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenLine u m | (_, m) <- toAnnotatedList as])
> 
>     , krebProp
>         "as == toList (fromList as)" $
>         \(xs :: [a]) ->
>           let as = fromList (EventId 0 "t") xs :: Buffer w t d
>           in claimEqual (xs) (toList as)
> 
>     , krebProp
>         "moveFocusToStart (clearMark as) == fromList (toList as)" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (moveFocusToStart (clearMark as))
>             (fromList (EventId 0 "t") (toList as))
> 
>     , krebProp
>         "as == movePointToScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (as)
>             (movePointToScreenCoords (getPointScreenCoords as) as )
> 
>     , krebProp
>         "getPointLineCol then movePointToLineCol" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (as)
>             (movePointToLineCol (getPointLineCol as) as)
> 
>     , krebProp
>         "getPointScreenCoords as == seekScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getPointScreenCoords as)
>             (seekScreenCoords (getPointScreenCoords as) as)
> 
>     , krebProp
>         "(0,0) == seekScreenCoords (0,0) as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (0,0)
>             (seekScreenCoords (0,0) as)
> 
>     , krebProp
>         "getBufferScreenCoords as == seekScreenCoords (getBufferScreenCoords as) as" $
>         \(as :: Buffer w t d) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (seekScreenCoords (getBufferScreenCoords as) as)
> 
>     , krebProp
>         "movePointToLineCol (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t d
>             zs = fromCellList (EventId 0 "t") $ concat
>               [ map Cell as, [ Focus, Cell x ], map Cell bs, [ EOF ] ]
>             ws = fromList (EventId 0 "t") (as ++ [x])
>             m = value ws :: MeasureText w t d
>           in claimEqual
>             (makePointOnlyBuffer pw pt pd (EventId 0 "t") as x bs)
>             (movePointToLineCol (logicalCoords m) zs)
> 
>     , krebProp
>         "movePointToScreenCoords (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t d
>             zs = fromCellList (EventId 0 "t") $ concat
>               [ map Cell as, [ Focus, Cell x ], map Cell bs, [ EOF ] ]
>             ws = fromList (EventId 0 "t") as
>             m = value ws :: MeasureText w t d
>           in claimEqual
>             (makePointOnlyBuffer pw pt pd (EventId 0 "t") as x bs)
>             (movePointToScreenCoords (applyScreenOffset (screenCoords m) (0,0)) zs)
> 
>     , krebProp
>         "prepend as (prepend bs xs) == prepend (as <> bs) xs" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d) ->
>           claimEqual
>             (fst (prepend (EventId 0 "t") as (fst (prepend (EventId 0 "t") bs xs))))
>             (fst (prepend (EventId 0 "t") (as <> bs) xs))
> 
>     , krebProp
>         "append as (append bs xs) == append (bs <> as) xs" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d) ->
>           claimEqual
>             (fst (append (EventId 0 "t") as (fst (append (EventId 0 "t") bs xs))))
>             (fst (append (EventId 0 "t") (bs <> as) xs))
> 
>     , krebProp
>         "prepend as (append bs xs) == append bs (prepend as xs)" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d) ->
>           claimEqual
>             (fst (prepend (EventId 0 "t") as (fst (append (EventId 0 "t") bs xs))))
>             (fst (append (EventId 0 "t") bs (fst (prepend (EventId 0 "t") as xs))))
> 
>     , krebProp
>         "prepend mempty xs == xs" $
>         \(xs :: Buffer w t d) ->
>           claimEqual
>             (xs)
>             (fst (prepend (EventId 0 "t") [] xs))
> 
>     , krebProp
>         "append mempty xs == xs" $
>         \(xs :: Buffer w t d) ->
>           claimEqual
>             (xs)
>             (fst (append (EventId 0 "t") [] xs))
> 
>     , krebProp
>         "applyBufferOp u . applyBufferOp u == applyBufferOp u" $
>         \(xs :: Buffer w t d) (u :: BufferOp d a) ->
>           claimEqual
>             (applyBufferOp u xs)
>             (applyBufferOp u (applyBufferOp u xs))
> 
>     , krebProp
>         "applyBufferOp u . applyBufferOp v == applyBufferOp v . applyBufferOp u" $
>         \(xs :: Buffer w t d) (u :: BufferOp d a) (v :: BufferOp d a) ->
>           claimEqual
>             (applyBufferOp u (applyBufferOp v xs))
>             (applyBufferOp v (applyBufferOp u xs))
> 
>   {-  , krebProp
>         "takeFirstScreenLine is a cat factorization" $
>         \(as :: Buffer w t d) ->
>           let bs = moveFocusToStart $ clearMark as in
>           claimAny
>             [ claimFalse (hasFullScreenLine bs)
>             , let (u, Just x) = takeFirstScreenLine bs
>               in claimEqual
>                 (bs)
>                 (moveFocusToStart (fst (prepend (EventId 0 "t") (Fold.toList u) x)))
>             ] -}
> 
>     , krebProp
>         "hasFullScreenLine empty == False" $
>         claimFalse (hasFullScreenLine (empty :: Buffer w t d))
> 
>     , krebProp
>         "takeScreenLines length bound" $
>         \(NonNegative k :: NonNegative Int)
>          (as :: Buffer w t d) ->
>           claimLEQ
>             (length $ takeScreenLines k as)
>             (k)
> 
>     , krebProp
>         "takeFirstScreenLine EOF property" $
>         \(as :: Buffer w t d) ->
>           let (u, v) = takeFirstScreenLine as in
>           claimEqual
>             (v == Nothing)
>             (elem EOF $ Fold.toList u)
>     ]

> -}
