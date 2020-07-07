---
title: Buffers
---

::: contents
* [Basics](#basics): Type definition and constructors
* [Queries](#queries): Learning things about buffers
* [Conversions](#conversions): To other types
* [Value Getters](#value-getters):
* [Focus Navigation](#navigation):
:::



::: frontmatter

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Text.Buffer (
>   -- Basics
>     Buffer(..)
>   , emptyBuffer
>   , validate
> 
>   , makeFocusOnlyBuffer
>   , makeFocusMarkBuffer
>   , makeMarkFocusBuffer



>   -- Conversions
>   , toFingerTree
>   , toList
>   , toControls
>   , toGraphemes
>   , toAnnotatedList
>   , ToString(toString)
>   , fromString
>   , ($==)
>   , resizeBuffer



>   -- Value Getters
>   , getBufferWidth
>   , getBufferTabStop
>   , getBufferCharCount
>   , getBufferByteCount
>   , getBufferLineCol
>   , getBufferScreenCoords
>   , getBufferScreenOffset

>   , getFocusPointCharCount
>   , getFocusPointByteCount
>   , getFocusPointLineCol
>   , getFocusPointLineColOffset
>   , getFocusPointScreenCoords
>   , getFocusPointScreenOffset



>   -- Predicates
>   , hasValidCursorData
>   , hasUniqueRegion
>   , hasChars
>   , isBufferEmpty
>   , isBufferNotEmpty
> 
>   , isFocusRegionAtStart
>   , isFocusRegionAtEnd



>   -- Focus Navigation
>   , clearFocusMark
> 
>   , jumpFocusRegionToStart
>   , jumpFocusRegionToEnd
> 
>   , moveFocusPointLeft
>   , moveFocusPointRight
>   , moveFocusMarkLeft
>   , moveFocusMarkRight
> 
>   , fillFocusPointLeft
>   , fillFocusPointRight
>   , fillFocusMarkLeft
>   , fillFocusMarkRight



>   -- Finger Queries
>   , graphemeAtStart
>   , graphemeAtEnd
>   , graphemesAroundFocusPoint
>   , graphemesAroundFocusMark
> 
>   , valueAtStart
>   , valueAtEnd
>   , valuesAroundFocusPoint
>   , valuesAroundFocusMark
> 
>   , charAtStart
>   , charAtEnd
>   , charsAroundFocusPoint
>   , charsAroundFocusMark



>   -- Focus Insert
>   , insertAtStart
>   , insertAtEnd
>   , insertAtFocusPointLeft
>   , insertAtFocusPointRight


>   , alterRegion

>   -- Focus Delete
>   , deleteAtFocusPointLeft

>   -- Region Navigation

>   -- Region Insert

>   -- Region Delete

>   -- Splitting
>   , moveFocusRegionToLineCol
>   , moveFocusRegionToLineColOffset
>   , moveFocusRegionToScreenCoords
>   , moveFocusRegionToScreenOffset
>   , moveFocusRegionToScreenLine
>   , seekScreenCoords

>   , clearCursorsExceptFocus

>   -- Rendering
>   , takeFirstScreenLine

>   , RenderedBuffer()
>   , renderScreenLines
>   , mungeRenderedBuffer
>   , getRenderedBufferLines

>   , ProcRenderedBufferEntry

>   , LogicalLineNumber
>   , ScreenLineNumber
>   , ScreenColumnNumber
>   , RenderedWidth
>   , RenderedHeight
>   , TabStopWidth
>   , IsInsideRegion
>   , IsLineStart
>   , IsLineEnd

>   , showRenderedBufferWith
>   , showRenderedBufferDebug
>   , printRenderedBufferWith
>   , printGraphemePlain_test

> ) where

> import           Prelude hiding (reverse)
> import qualified Prelude (reverse)

> import           Data.Proxy
> import qualified Data.Foldable as Fold
> 
> import           Kreb.Category
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Prop
> import           Kreb.Arith
> import           Kreb.Reflect
> 
> import           Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree as FT
> import qualified Kreb.Struct.Data.RedBlackTree as RBT
> import qualified Kreb.Struct.Data.StemTree.Zipper as PRT
> 
> import qualified Kreb.Text.CursorWord as CD
> import           Kreb.Text.ScreenOffset
> import           Kreb.Text.MeasureText
> import           Kreb.Text.Grapheme
> import           Kreb.Text.BufferOp

:::





Basics
------

A buffer has three different components:

1. A sequence of characters and control data
2. A set of deleted characters
3. A tree of edit histories

> data Buffer w t d = Buffer
>   { bufContents :: TBuf w t d
>   , bufRemnants :: TDel d
>   , bufInstants :: TMem d
>   }
> 
> deriving instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Eq (Buffer w t d)

> instance Fmt.Display (Buffer w t d) where
>   display buf = "Buffer"
>     <+> display (bufContents buf)
>     <+> display (bufRemnants buf)
>     <+> display (bufInstants buf)

> data TBuf w t d where
>   FocusOnly
>     :: ( IsWidth w, IsTab t, IsBase d )
>     => FT.FingerTree (Control (Grapheme w t d))
>     -> FT.FingerTree (Control (Grapheme w t d))
>     -> TBuf w t d
>   FocusMark
>     :: ( IsWidth w, IsTab t, IsBase d )
>     => FT.FingerTree (Control (Grapheme w t d))
>     -> FT.NonEmptyFingerTree (Grapheme w t d)
>     -> FT.FingerTree (Control (Grapheme w t d))
>     -> TBuf w t d
>   MarkFocus
>     :: ( IsWidth w, IsTab t, IsBase d )
>     => FT.FingerTree (Control (Grapheme w t d))
>     -> FT.NonEmptyFingerTree (Grapheme w t d)
>     -> FT.FingerTree (Control (Grapheme w t d))
>     -> TBuf w t d
> 
> deriving instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Eq (TBuf w t d)

> instance Fmt.Display (TBuf w t d) where
>   display x = case x of
>     FocusOnly as bs -> "FocusOnly"
>       <+> display as <+> display bs
>     FocusMark as rs bs -> "FocusMark"
>       <+> display as <+> display rs <+> display bs
>     MarkFocus as rs bs -> "MarkFocus"
>       <+> display as <+> display rs <+> display bs

> data TDel d where
>   TDel
>     :: ( IsBase d )
>     => RBT.RedBlackTree (Loc d EventId Char)
>     -> TDel d
> 
> deriving instance
>   ( IsBase d
>   ) => Eq (TDel d)

> instance Fmt.Display (TDel d) where
>   display (TDel x) = "TDel" <+> display x

> data TMem d where
>   MemVacant
>     :: TMem d
>   MemOccupied
>     :: ( IsBase d )
>     => PRT.StemTreeZipper (RBT.RedBlackTree (BufferOp d))
>     -> TMem d
> 
> deriving instance
>   ( IsBase d
>   ) => Eq (TMem d)
> 
> instance Fmt.Display (TMem d) where
>   display x = case x of
>     MemVacant     -> "MemVacant"
>     MemOccupied z -> "MemOccupied" <+> display z
> 
> fmapMem
>   :: ( IsBase d )
>   => (PRT.StemTreeZipper (RBT.RedBlackTree (BufferOp d)) -> PRT.StemTreeZipper (RBT.RedBlackTree (BufferOp d)))
>   -> TMem d -> TMem d
> fmapMem f x = case x of
>   MemVacant -> MemVacant
>   MemOccupied z -> MemOccupied (f z)



> emptyBuffer
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d
> emptyBuffer = Buffer
>   { bufContents = FocusOnly mempty mempty
>   , bufRemnants = TDel RBT.empty
>   , bufInstants = MemVacant
>   }



For testing purposes we'll need a way to construct buffers with very specific shapes. These are exported from the module but 

> makeFocusOnlyBuffer
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Proxy w -> Proxy t -> Proxy d
>   -> [Control (Grapheme w t d)] -> [Control (Grapheme w t d)]
>   -> Buffer w t d
> makeFocusOnlyBuffer _ _ _ as bs =
>   (emptyBuffer :: Buffer w t d)
>     { bufContents =
>         FocusOnly (fromList as) (fromList bs)
>     }
> 
> makeFocusMarkBuffer
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Proxy w -> Proxy t -> Proxy d
>   -> [Control (Grapheme w t d)] -> [Grapheme w t d] -> [Control (Grapheme w t d)]
>   -> Buffer w t d
> makeFocusMarkBuffer _ _ _ as rs bs =
>   case fromListMaybe rs of
>     Nothing -> error "makeFocusMarkBuffer: empty rs"
>     Just x -> (emptyBuffer :: Buffer w t d)
>       { bufContents =
>           FocusMark (fromList as) x (fromList bs)
>       }
> 
> makeMarkFocusBuffer
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Proxy w -> Proxy t -> Proxy d
>   -> [Control (Grapheme w t d)] -> [Grapheme w t d] -> [Control (Grapheme w t d)]
>   -> Buffer w t d
> makeMarkFocusBuffer _ _ _ as rs bs =
>   case fromListMaybe rs of
>     Nothing -> error "makeMarkFocusBuffer: empty rs"
>     Just x -> (emptyBuffer :: Buffer w t d)
>       { bufContents =
>           MarkFocus (fromList as) x (fromList bs)
>       }

> makeFingerTree
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Proxy w -> Proxy t -> Proxy d
>   -> [Control (Grapheme w t d)]
>   -> FT.FingerTree (Control (Grapheme w t d))
> makeFingerTree _ _ _ = fromList





Queries
-------

> hasControls
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => MeasureText w t d -> Bool
> hasControls m = CD.E /= CD.cursorWord (cursorData m)



> isBufferEmpty, isBufferNotEmpty
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> isBufferEmpty (Buffer contents _ _) =
>   case contents :: TBuf w t d of
>     FocusOnly as bs -> (isEmpty as) && (isEmpty bs)
>     _ -> False
> 
> isBufferNotEmpty = not . isBufferEmpty

> hasValidCursorData
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> hasValidCursorData (Buffer w _ _) =
>   let
>     wellFormed :: FT.FingerTree (Control (Grapheme w t d)) -> Bool
>     wellFormed x = CD.X /= CD.cursorWord (cursorData (value x))
>   in case w of
>     FocusOnly as   bs -> (wellFormed as) && (wellFormed bs)
>     FocusMark as _ bs -> (wellFormed as) && (wellFormed bs)
>     MarkFocus as _ bs -> (wellFormed as) && (wellFormed bs)

> hasUniqueRegion
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> hasUniqueRegion (Buffer w _ _) =
>   let
>     noControls :: FT.FingerTree (Control (Grapheme w t d)) -> Bool
>     noControls = not . hasControls . value
>   in case w of
>     FocusOnly as   bs -> (noControls as) && (noControls bs)
>     FocusMark as _ bs -> (noControls as) && (noControls bs)
>     MarkFocus as _ bs -> (noControls as) && (noControls bs)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in hasUniqueRegion x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [ Point Nothing, Cell 'b', Cell 'c'], [MarkLeft, Cell 'e', Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in hasUniqueRegion x
> -- :}
> -- False
> --
> -- >>> :{
> -- let
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [ Point (Just OnLeft), MarkRight, Cell 'c' ]
> --     [ 'd', 'e', 'f' ]
> --     [ Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in hasUniqueRegion x
> -- :}
> -- False

:::

> hasChars
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> hasChars (Buffer w _ _) =
>   let
>     hasChars' :: FT.FingerTree (Control (Grapheme w t d)) -> Bool
>     hasChars' xs = 0 < charCount (value xs)
>   in case w of
>     FocusOnly as bs -> (hasChars' as) || (hasChars' bs)
>     _ -> True

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in hasChars x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Point Nothing], [MarkLeft, Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in hasChars x
> -- :}
> -- False

:::

> isFocusRegionAtStart
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> isFocusRegionAtStart (Buffer contents _ _) =
>   case contents of
>     FocusOnly a   _ -> isEmpty a
>     FocusMark a _ _ -> isEmpty a
>     MarkFocus _ _ _ -> False
> 
> isFocusRegionAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> isFocusRegionAtEnd (Buffer contents _ _) =
>   case contents of
>     FocusOnly _   b -> isEmpty b
>     FocusMark _ _ _ -> False
>     MarkFocus _ _ b -> isEmpty b






Conversions
-----------

> toFingerTree
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> FT.FingerTree (Control (Grapheme w t d))
> toFingerTree (Buffer contents _ _) = mconcat $ case contents of
>   FocusOnly as bs ->
>     [ as, singleton (Focus Nothing), bs ]
>   FocusMark as rs bs ->
>     [ as, singleton (Focus (Just OnLeft))
>     , FT.NonEmpty $ fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs
>     , singleton MarkRight, bs
>     ]
>   MarkFocus as rs bs ->
>     [ as, singleton MarkLeft
>     , FT.NonEmpty $ fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs
>     , singleton (Focus (Just OnRight)), bs
>     ]
> 
> toAnnotatedList
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> [(Control (Grapheme w t d), MeasureText w t d)]
> toAnnotatedList = FT.toAnnotatedList . toFingerTree
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Valued (Buffer w t d)
>   where
>     type Value (Buffer w t d) = MeasureText w t d
> 
>     value :: Buffer w t d -> MeasureText w t d
>     value = value . toFingerTree



> toList
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> [Control (Grapheme w t d)]
> toList = Fold.toList . toFingerTree
> 
> toControls
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> [Control Char]
> toControls = map (fmap getGraphemeChar) . toList
> 
> toControlsFT
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => FT.FingerTree (Control (Grapheme w t d)) -> [Control Char]
> toControlsFT = map (fmap getGraphemeChar) . Fold.toList
> 
> toGraphemes
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> [Grapheme w t d]
> toGraphemes = fromControls . toList
> 

> class ToString a where
>   toString :: a -> String
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => ToString (FT.FingerTree (Control (Grapheme w t d)))
>   where
>     toString
>       :: ( IsWidth w, IsTab t, IsBase d )
>       => FT.FingerTree (Control (Grapheme w t d)) -> String
>     toString = fromControls . map (fmap getGraphemeChar) . Fold.toList
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => ToString (Buffer w t d)
>   where
>     toString
>       :: ( IsWidth w, IsTab t, IsBase d )
>       => Buffer w t d -> String
>     toString = map getGraphemeChar . toGraphemes



::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["abc", "def"]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 (map Cell as) (map Cell bs)
> -- in toString x
> -- :}
> -- "abcdef"
> --
> -- >>> :{
> -- let
> --   [as, bs, cs] = makePlainGraphemesL (EventId 0 "") ["abc", "def", "ghi"]
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 (map Cell as) bs (map Cell cs)
> -- in toString x
> -- :}
> -- "abcdefghi"
> --
> -- >>> :{
> -- let
> --   [as, bs, cs] = makePlainGraphemesL (EventId 0 "") ["abc", "def", "ghi"]
> --   x = makeMarkFocusBuffer nat8 nat2 nat10 (map Cell as) bs (map Cell cs)
> -- in toString x
> -- :}
> -- "abcdefghi"

:::



::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["abc", "def"]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 (map Cell as) (map Cell bs)
> --   --
> --   y =
> --     [ Cell 'a', Cell 'b', Cell 'c', Focus Nothing, Cell 'd', Cell 'e', Cell 'f'
> --     ]
> -- in toControls x == y
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as, bs, cs] = makePlainGraphemesL (EventId 0 "") ["abc", "def", "ghi"]
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 (map Cell as) bs (map Cell cs)
> --   --
> --   y =
> --     [ Cell 'a', Cell 'b', Cell 'c', Focus (Just OnLeft), Cell 'd', Cell 'e', Cell 'f'
> --     , MarkRight, Cell 'g', Cell 'h', Cell 'i'
> --     ]
> -- in toControls x == y
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as, bs, cs] = makePlainGraphemesL (EventId 0 "") ["abc", "def", "ghi"]
> --   x = makeMarkFocusBuffer nat8 nat2 nat10 (map Cell as) bs (map Cell cs)
> --   --
> --   y =
> --     [ Cell 'a', Cell 'b', Cell 'c', MarkLeft, Cell 'd', Cell 'e', Cell 'f'
> --     , Focus (Just OnRight), Cell 'g', Cell 'h', Cell 'i'
> --     ]
> -- in toControls x == y
> -- :}
> -- True

:::

> fromString
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> String -> Buffer w t d
> fromString eId cs = Buffer
>   { bufContents =
>       FocusOnly mempty (fromList $ map Cell $ makePlainGraphemes eId cs)
>   , bufRemnants = TDel RBT.empty
>   , bufInstants = MemVacant
>   }

> class StringEq a where
>   ($==) :: a -> a -> Bool
> 
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => StringEq (Buffer w t d)
>   where
>     u $== v =
>       (toString u) == (toString v)
> 
> instance (StringEq a) => StringEq (Maybe a) where
>   u $== v = case (u,v) of
>     (Nothing, Nothing) -> True
>     (Just a,  Just b ) -> a $== b
>     _ -> False



> resizeBuffer
>   :: forall w1 t1 w2 t2 d a
>    . ( IsBase d, IsWidth w1, IsTab t1, IsWidth w2, IsTab t2 )
>   => Buffer w1 t1 d -> Buffer w2 t2 d
> resizeBuffer (Buffer c r m) = Buffer (f c) r m
>   where
>     f :: TBuf w1 t1 d -> TBuf w2 t2 d
>     f x = case x of
>       FocusOnly as bs -> FocusOnly
>         (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) as) (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) bs)
>       FocusMark as rs bs -> FocusMark
>         (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) as) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree resize rs) (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) bs)
>       MarkFocus as rs bs -> MarkFocus
>         (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) as) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree resize rs) (fmapC @Valued @(->) @Valued @(->) @FT.FingerTree (fmap resize) bs)





Value Getters
-------------

> getBufferWidth
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getBufferWidth _ = toWidth (Proxy :: Proxy w)
> 
> getBufferTabStop
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getBufferTabStop _ = toTab (Proxy :: Proxy t)

> getBufferCharCount
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getBufferCharCount = charCount . value
> 
> getBufferByteCount
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getBufferByteCount = byteCount . value
> 
> getBufferLineCol
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> LineCol
> getBufferLineCol = logicalCoords . value
> 
> getBufferScreenCoords
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Int, Int)
> getBufferScreenCoords buf =
>   let m = value buf :: MeasureText w t d
>   in applyScreenOffset (screenCoords m) (0,0)
> 
> getBufferScreenOffset
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Int, Int)
> getBufferScreenOffset buf =
>   let m = value buf :: MeasureText w t d
>   in applyScreenOffset (screenCoords m <> screenOffset m) (0,0)



> valueUpToFocusPoint
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> MeasureText w t d
> valueUpToFocusPoint (Buffer w _ _) = case w of
>   FocusOnly as    _ -> value as
>   FocusMark as _  _ -> value as
>   MarkFocus as rs _ -> (value as) <> (value rs)

> getFocusPointCharCount
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getFocusPointCharCount =
>   charCount . valueUpToFocusPoint
> 
> getFocusPointByteCount
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Int
> getFocusPointByteCount =
>   byteCount . valueUpToFocusPoint
> 
> getFocusPointLineCol
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> LineCol
> getFocusPointLineCol =
>   logicalCoords . valueUpToFocusPoint
> 
> getFocusPointLineColOffset
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> LineCol
> getFocusPointLineColOffset buf =
>   let m = valueUpToFocusPoint buf
>   in (logicalCoords m) <> (logicalOffset m)
>  
> getFocusPointScreenCoords
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Int, Int)
> getFocusPointScreenCoords buf =
>   let m = valueUpToFocusPoint buf
>   in applyScreenOffset (screenCoords m) (0,0)
> 
> getFocusPointScreenOffset
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Int, Int)
> getFocusPointScreenOffset buf =
>   let m = valueUpToFocusPoint buf
>   in applyScreenOffset (screenCoords m <> screenOffset m) (0,0)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["abc", "def"]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 (map Cell as) (map Cell bs)
> -- in getFocusPointScreenCoords x
> -- :}
> -- (2,0)

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["", "abcdef"]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 (map Cell as) (map Cell bs)
> -- in getFocusPointScreenOffset x
> -- :}
> -- (0,0)
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["abc", "def"]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 (map Cell as) (map Cell bs)
> -- in getFocusPointScreenOffset x
> -- :}
> -- (3,0)
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesL (EventId 0 "") ["abcdef", "ghi"]
> --   x = makeFocusOnlyBuffer nat4 nat2 nat10 (map Cell as) (map Cell bs)
> -- in getFocusPointScreenOffset x
> -- :}
> -- (2,1)

:::



Navigation
----------

> clearFocusMark
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> clearFocusMark (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs ->
>         FocusOnly as bs
>       FocusMark as rs bs ->
>         FocusOnly as ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>       MarkFocus as rs bs ->
>         FocusOnly (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) bs
>   in Buffer tbuf del mem

> jumpFocusRegionToStart
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> jumpFocusRegionToStart (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly a b ->
>         FocusOnly mempty (a <> b)
>       FocusMark a r b ->
>         let z = fmapC @Valued @(->) @Valued @(->) @FT.FingerTree Cell $ FT.NonEmpty r
>         in FocusOnly mempty (a <> z <> b)
>       MarkFocus a r b ->
>         let z = fmapC @Valued @(->) @Valued @(->) @FT.FingerTree Cell $ FT.NonEmpty r
>         in FocusOnly mempty (a <> z <> b)
>   in Buffer tbuf del mem
> 
> jumpFocusRegionToEnd
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> jumpFocusRegionToEnd (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly a b ->
>         FocusOnly (a <> b) mempty
>       FocusMark a r b ->
>         let z = fmapC @Valued @(->) @Valued @(->) @FT.FingerTree Cell $ FT.NonEmpty r
>         in FocusOnly (a <> z <> b) mempty
>       MarkFocus a r b ->
>         let z = fmapC @Valued @(->) @Valued @(->) @FT.FingerTree Cell $ FT.NonEmpty r
>         in FocusOnly (a <> z <> b) mempty
>   in Buffer tbuf del mem

> moveFocusPointLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> moveFocusPointLeft (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case unsnoc as of
>         Just (Cell u, us) -> FocusMark us (singleton u) bs
>         _ -> contents
>       FocusMark as rs bs -> case unsnoc as of
>         Just (Cell u, us) ->
>           FocusMark us (cons u rs) bs
>         _ -> contents
>       MarkFocus as rs bs ->
>         let (u, w) = unsnocNonEmpty rs
>         in case w of
>           FT.Empty -> FocusOnly as (cons (Cell u) bs)
>           FT.NonEmpty us -> MarkFocus as us (cons (Cell u) bs)
>   in Buffer tbuf del mem
> 
> moveFocusPointRight
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> moveFocusPointRight (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case uncons bs of
>         Just (Cell u, us) -> MarkFocus as (singleton u) us
>         _ -> contents
>       FocusMark as rs bs ->
>         let (u, w) = unconsNonEmpty rs
>         in case w of
>           FT.Empty -> FocusOnly (snoc (Cell u) as) bs
>           FT.NonEmpty us -> FocusMark (snoc (Cell u) as) us bs
>       MarkFocus as rs bs -> case uncons bs of
>         Just (Cell u, us) ->
>           MarkFocus as (snoc u rs) us
>         _ -> contents
>   in Buffer tbuf del mem

> moveFocusMarkLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> moveFocusMarkLeft (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case unsnoc as of
>         Just (Cell u, us) -> MarkFocus us (singleton u) bs
>         _ -> contents
>       FocusMark as rs bs ->
>         let (u, w) = unsnocNonEmpty rs
>         in case w of
>           FT.Empty -> FocusOnly as (cons (Cell u) bs)
>           FT.NonEmpty us -> FocusMark as us (cons (Cell u) bs)
>       MarkFocus as rs bs -> case unsnoc as of
>         Just (Cell u, us) -> MarkFocus us (cons u rs) bs
>         _ -> contents
>   in Buffer tbuf del mem
> 
> moveFocusMarkRight
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> moveFocusMarkRight (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case uncons bs of
>         Just (Cell u, us) -> FocusMark as (singleton u) us
>         _ -> contents
>       FocusMark as rs bs -> case uncons bs of
>         Just (Cell u, us) -> FocusMark as (snoc u rs) us
>         _ -> contents
>       MarkFocus as rs bs ->
>         let (u, w) = unconsNonEmpty rs
>         in case w of
>           FT.Empty -> FocusOnly (snoc (Cell u) as) bs
>           FT.NonEmpty us -> MarkFocus (snoc (Cell u) as) us bs
>   in Buffer tbuf del mem

"Fill" moves the focus point/mark as far to the left or right as possible without crossing a control.

> fillFocusPointLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> fillFocusPointLeft (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> FocusMark mempty (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) bs
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> FocusMark (snoc x ds1) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) bs
>       FocusMark as rs bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> FocusMark mempty ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) <> rs) bs
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.NonEmpty vs -> FocusMark (snoc x ds1) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) <> rs) bs
>           FT.Empty -> contents
>       MarkFocus as rs bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> FocusOnly mempty ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>           FT.NonEmpty vs -> FocusMark mempty (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.Empty -> FocusOnly as ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>           FT.NonEmpty vs -> FocusMark (snoc x ds1) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>   in Buffer tbuf del mem
> 
> fillFocusPointRight
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> fillFocusPointRight (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> MarkFocus as (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.Empty -> contents
>           FT.NonEmpty vs ->  MarkFocus as (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) (cons x ds2)
>       FocusMark as rs bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> FocusOnly (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) mempty
>           FT.NonEmpty vs -> MarkFocus (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.Empty -> FocusOnly (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) bs
>           FT.NonEmpty vs -> MarkFocus (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) (cons x ds2)
>       MarkFocus as rs bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> MarkFocus as (rs <> (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs)) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.NonEmpty vs -> MarkFocus as (rs <> (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs)) (cons x ds2)
>           FT.Empty -> contents
>   in Buffer tbuf del mem

> fillFocusMarkLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> fillFocusMarkLeft (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> MarkFocus mempty (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) bs
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> MarkFocus (snoc x ds1) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) bs
>       FocusMark as rs bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> FocusOnly mempty ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>           FT.NonEmpty vs -> MarkFocus mempty (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.Empty -> FocusOnly as ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>           FT.NonEmpty vs -> MarkFocus (snoc x ds1) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs)
>       MarkFocus as rs bs -> case FT.splitR hasControls as of
>         FT.NotFound -> case as of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> MarkFocus mempty ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) <> rs) bs
>         FT.Found ds1 x ds2 -> case ds2 of
>           FT.NonEmpty vs -> MarkFocus (snoc x ds1) ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) <> rs) bs
>           FT.Empty -> contents
>   in Buffer tbuf del mem
> 
> fillFocusMarkRight
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> fillFocusMarkRight (Buffer contents del mem) =
>   let
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> FocusMark as (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.Empty -> contents
>           FT.NonEmpty vs ->  FocusMark as (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) (cons x ds2)
>       FocusMark as rs bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> contents
>           FT.NonEmpty vs -> FocusMark as (rs <> (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs)) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.NonEmpty vs -> FocusMark as (rs <> (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs)) (cons x ds2)
>           FT.Empty -> contents
>       MarkFocus as rs bs -> case FT.splitL hasControls bs of
>         FT.NotFound -> case bs of
>           FT.Empty -> FocusOnly (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) mempty
>           FT.NonEmpty vs -> FocusMark (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) mempty
>         FT.Found ds1 x ds2 -> case ds1 of
>           FT.Empty -> FocusOnly (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) bs
>           FT.NonEmpty vs -> FocusMark (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree unCell vs) (cons x ds2)
>   in Buffer tbuf del mem






> alterRegion
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Grapheme w t d -> Grapheme w t d)
>   -> Buffer w t d -> Buffer w t d
> alterRegion f (Buffer buf del mem) =
>   let
>     tbuf = case buf :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly as bs
>       FocusMark as rs bs -> FocusMark as
>         (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree f rs) bs
>       MarkFocus as rs bs -> MarkFocus as
>         (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree f rs) bs
>   in Buffer tbuf del mem







::: contents
* [Point Manipulation](#point-manipulation): Moving the read heads around
* [Basic Mutation](#basic-mutation): Cut, copy, alter, insert
* [Splitting](#splitting): Moving the read heads around (again)
* [Rendering](#rendering): Displaying buffers to a virtual screen
* [Testing and Debugging](#testing-and-debugging): For when things go wrong
:::



::: frontmatter









> {-

>   , BufferOp(..)
>   , isSingleton

>   , adjustBuffer
>   , prepend
>   , append

>   , isPointAtEnd
>   , hasMark
>   , isCoincident
> 
>   , readPoint
>   , deletePointLeft
>   , deleteAtStart
>   , deleteAtEnd



>   , cutRegion
>   , copyRegion
>   , insertRegion

> {-
>   , alterRegion
>   , mapBuffer
>   , mapRegion
> -}

>   , applyBaseBufferOp

>   , applyBufferOp
> 
>   , hasFullScreenLine
>   , takeFirstScreenLine
>   , takeScreenLines
>   , getScreenLines
>   , attachLineNumbers
>   , attachColumnIndices
>   , 
> 
>   , validate
>   , toAnnotatedList

> import Data.List (groupBy, sortOn)
> import qualified Data.Map.Strict as M
> import Control.Monad (join)
> import Debug.Trace
> 

> import           Kreb.Control
> import           Kreb.Reflect


> import qualified Kreb.Struct.TwoPointedList as TPL

> import Kreb.Text.Pigment
> import Kreb.Text.BufferOp

> -}

:::














































 > split
 >   :: ( IsWidth w, IsTab t, IsBase d )
 >   => (MeasureText w t d -> Bool)       -- point predicate
 >   -> Maybe (MeasureText w t d -> Bool) -- mark predicate
 >   -> Buffer w t d -> Maybe (Buffer w t d)
 > split pointP q w =
 >   let xs = integrate w
 >   in case FT.split pointP xs of
 >     Nothing -> Nothing
 >     Just (as, x, bs) -> Just $ case q of
 >       Nothing -> PointOnly (as, x, bs)
 >       Just markP -> case FT.split markP as of
 >         Just (us, y, vs) ->
 >           MarkPoint (us, y, vs, x, bs)
 >         Nothing -> case FT.split markP bs of
 >           Just (us, y, vs) ->
 >             PointMark (as, x, us, y, vs)
 >           Nothing -> PointOnly (as, x, bs)



> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Arb (Buffer w t d)
>   where
>     arb = do
>       k <- size
>       let d = k `div` 3
>       buf <- pickFrom3
>         ( FocusOnly <$> arbBalancedFT k <*> arbBalancedFT k
>         , FocusMark <$> arbBalancedFT k <*> arbBalancedFT' <*> arbBalancedFT k
>         , MarkFocus <$> arbBalancedFT k <*> arbBalancedFT' <*> arbBalancedFT k
>         )
>       return $ Buffer buf (TDel RBT.empty) MemVacant

> arbBalancedFT
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Int -> Sample (FT.FingerTree (Control (Grapheme w t d)))
> arbBalancedFT depth = do
>   parts <- makeColorfulGraphemesL <$> arb <*> (nestOf arb)
>   mconcat <$> mapM (wrapBalanced . fromList . map Cell) parts

> wrapBalanced
>   :: ( Valued (Control a) )
>   => FT.FingerTree (Control a) -> Sample (FT.FingerTree (Control a))
> wrapBalanced xs = pickFrom5
>   ( return xs
>   , return $ mconcat [singleton (Point Nothing), xs]
>   , return $ mconcat [singleton (Point (Just OnLeft)), xs, singleton MarkRight]
>   , return $ mconcat [xs, singleton (Point Nothing)]
>   , return $ mconcat [singleton MarkLeft, xs, singleton (Point (Just OnRight))]
>   )

> arbBalancedFT'
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Sample (FT.NonEmptyFingerTree (Grapheme w t d))
> arbBalancedFT' = do
>   x <- fromListMaybe
>     <$> (makeColorfulGraphemes <$> arb <*> ((:) <$> arb <*> arb))
>   case x of
>     Nothing -> error "arbBalancedFT'"
>     Just a -> return a

> -- TODO: fix this; pruning does not preserve well-formedness
> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Prune (TBuf w t d)
>   where
>     prune x =
>       let
>         wellFormed :: FT.FingerTree (Control (Grapheme w t d)) -> Bool
>         wellFormed x = CD.X /= CD.cursorWord (cursorData (value x))
>       in case x of
>         FocusOnly a b ->
>           [ FocusOnly a' b | a' <- prune a, wellFormed a' ] ++
>           [ FocusOnly a b' | b' <- prune b, wellFormed b' ]
>         FocusMark a r b ->
>           [ FocusMark a' r b | a' <- prune a, wellFormed a' ] ++
>           [ FocusMark a r' b | r' <- prune r ] ++
>           [ FocusMark a r b' | b' <- prune b, wellFormed b' ]
>         MarkFocus a r b ->
>           [ MarkFocus a' r b | a' <- prune a, wellFormed a' ] ++
>           [ MarkFocus a r' b | r' <- prune r ] ++
>           [ MarkFocus a r b' | b' <- prune b, wellFormed b' ]

> instance (IsWidth w, IsTab t, IsBase d) => Prune (Buffer w t d) where
>   prune (Buffer buf del mem) =
>     [ Buffer buf' del mem | buf' <- prune buf ]


> instance
>   ( IsWidth w, IsTab t, IsBase d
>   ) => Show (Buffer w t d)
>   where
>     show (Buffer w (TDel r) _) =
>       let
>         wid = showWidth (Proxy :: Proxy w)
>         tab = showTab (Proxy :: Proxy t)
>         bas = showBase (Proxy :: Proxy d)
>       in case w of
>         FocusOnly as bs -> concat
>           [ "makeFocusOnlyBuffer "
>           , wid, " ", tab, " ", bas, " "
>           , show $ Fold.toList as, " "
>           , show $ Fold.toList bs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         FocusMark as rs bs -> concat
>           [ "makeFocusMarkBuffer "
>           , wid, " ", tab, " ", bas, " "
>           , show $ Fold.toList as, " "
>           , show $ Fold.toList rs, " "
>           , show $ Fold.toList bs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]
>         MarkFocus as rs bs -> concat
>           [ "makeMarkFocusBuffer "
>           , wid, " ", tab, " ", bas, " "
>           , show $ Fold.toList as, " "
>           , show $ Fold.toList rs, " "
>           , show $ Fold.toList bs
>           , "\ndeleted: ", show $ RBT.toList r
>           ]

> validate
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> validate x =
>   let v = value x
>   in and
>     [ elem (CD.cursorWord $ cursorData v)
>       [ CD.P, CD.PP, CD.W, CD.PWP, CD.WP, CD.PW ]
>     ]






> splitRightmostNonControl
>   :: ( FT.Split f, IsWidth w, IsTab t, IsBase d )
>   => f (Control (Grapheme w t d))
>   -> Maybe (Grapheme w t d, FT.FingerTree (Control (Grapheme w t d)))
> splitRightmostNonControl xs = case FT.splitR (\v -> 0 < charCount v) xs of
>   FT.NotFound -> Nothing
>   FT.Found as (Cell c) bs -> Just (c, as <> bs)
>   _ -> error "getRightmostNonControl: panic!"

> getRightmostNonControl
>   :: ( FT.Split f, IsWidth w, IsTab t, IsBase d )
>   => f (Control (Grapheme w t d)) -> Maybe (Grapheme w t d)
> getRightmostNonControl = fmap fst . splitRightmostNonControl

::: doctest

> -- $
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Cell 'a', Cell 'b', Cell 'c' ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getRightmostNonControl xs
> -- :}
> -- Just 'c'
> --
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Cell 'a', Cell 'b', Cell 'c', Point Nothing ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getRightmostNonControl xs
> -- :}
> -- Just 'c'
> --
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Cell 'a', Cell 'b', Point Nothing, Cell 'c', Point Nothing ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getRightmostNonControl xs
> -- :}
> -- Just 'c'

:::

> getLeftmostNonControl
>   :: ( FT.Split f, IsWidth w, IsTab t, IsBase d )
>   => f (Control (Grapheme w t d)) -> Maybe (Grapheme w t d)
> getLeftmostNonControl xs = case FT.splitL (\v -> 0 < charCount v) xs of
>   FT.NotFound -> Nothing
>   FT.Found _ (Cell c) _ -> Just c
>   _ -> error "getLeftmostNonControl: panic!"

::: doctest

> -- $
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Cell 'a', Cell 'b', Cell 'c' ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getLeftmostNonControl xs
> -- :}
> -- Just 'a'
> --
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Focus Nothing, Cell 'a', Cell 'b', Cell 'c' ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getLeftmostNonControl xs
> -- :}
> -- Just 'a'
> --
> -- >>> :{
> -- let
> --   as = makePlainGraphemesF (EventId 0 "")
> --     [ Focus Nothing, Cell 'a', Cell 'b', Point Nothing, Cell 'c' ]
> --   xs = makeFingerTree nat8 nat2 nat10 as
> -- in fmap getGraphemeChar $ getLeftmostNonControl xs
> -- :}
> -- Just 'a'

:::

> graphemeAtStart
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe (Grapheme w t d)
> graphemeAtStart (Buffer w _ _) = case w of
>   FocusOnly as bs -> case getLeftmostNonControl as of
>     Just a -> Just a
>     Nothing -> getLeftmostNonControl bs
>   FocusMark as rs _ -> case getLeftmostNonControl as of
>     Just a -> Just a
>     Nothing -> Just $ fst $ unconsNonEmpty rs
>   MarkFocus as rs _ -> case getLeftmostNonControl as of
>     Just a -> Just a
>     Nothing -> Just $ fst $ unconsNonEmpty rs
> 
> charAtStart
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe Char
> charAtStart = fmap getGraphemeChar . graphemeAtStart
> 
> valueAtStart
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe (MeasureText w t d)
> valueAtStart = fmap value . graphemeAtStart

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charAtStart x
> -- :}
> -- Just 'a'
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Point Nothing, Cell 'b', Cell 'c'], [MarkLeft, Cell 'e', Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charAtStart x
> -- :}
> -- Just 'b'
> --
> -- >>> :{
> -- let
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [Cell 'a', Cell 'b', Cell 'c']
> --     [ 'd', 'e', 'f' ]
> --     [Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in charAtStart x
> -- :}
> -- Just 'a'

:::

> graphemeAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe (Grapheme w t d)
> graphemeAtEnd (Buffer w _ _) = case w of
>   FocusOnly as bs -> case getRightmostNonControl bs of
>     Just b -> Just b
>     Nothing -> getRightmostNonControl as
>   FocusMark _ rs bs -> case getRightmostNonControl bs of
>     Just b -> Just b
>     Nothing -> Just $ fst $ unsnocNonEmpty rs
>   MarkFocus _ rs bs -> case getRightmostNonControl bs of
>     Just b -> Just b
>     Nothing -> Just $ fst $ unsnocNonEmpty rs
> 
> charAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe Char
> charAtEnd = fmap getGraphemeChar . graphemeAtEnd
> 
> valueAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Maybe (MeasureText w t d)
> valueAtEnd = fmap value . graphemeAtEnd

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charAtEnd x
> -- :}
> -- Just 'f'
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Point Nothing, Cell 'c'], [MarkLeft, Cell 'e', Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charAtEnd x
> -- :}
> -- Just 'e'
> --
> -- >>> :{
> -- let
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [Cell 'a', Cell 'b', Cell 'c']
> --     [ 'd', 'e', 'f' ]
> --     [Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in charAtEnd x
> -- :}
> -- Just 'i'

:::

> graphemesAroundFocusPoint
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe (Grapheme w t d), Maybe (Grapheme w t d))
> graphemesAroundFocusPoint (Buffer w _ _) = case w of
>   FocusOnly as bs ->
>     ( getRightmostNonControl as
>     , getLeftmostNonControl bs
>     )
>   FocusMark as rs bs ->
>     ( getRightmostNonControl as
>     , Just $ fst $ unconsNonEmpty rs
>     )
>   MarkFocus as rs bs ->
>     ( Just $ fst $ unsnocNonEmpty rs
>     , getLeftmostNonControl bs
>     )
> 
> charsAroundFocusPoint
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe Char, Maybe Char)
> charsAroundFocusPoint x =
>   let (u,v) = graphemesAroundFocusPoint x
>   in (fmap getGraphemeChar u, fmap getGraphemeChar v)
> 
> valuesAroundFocusPoint
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe (MeasureText w t d), Maybe (MeasureText w t d))
> valuesAroundFocusPoint x =
>   let (u,v) = graphemesAroundFocusPoint x
>   in (fmap value u, fmap value v)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charsAroundFocusPoint x
> -- :}
> -- (Just 'c',Just 'd')
> --
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Point (Just OnLeft), Cell 'c'], [MarkRight, Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in charsAroundFocusPoint x
> -- :}
> -- (Just 'c',Just 'e')
> --
> -- >>> :{
> -- let
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [Cell 'a', Cell 'b', Point Nothing]
> --     [ 'd', 'e', 'f' ]
> --     [Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in charsAroundFocusPoint x
> -- :}
> -- (Just 'b',Just 'd')

:::

> graphemesAroundFocusMark
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe (Grapheme w t d), Maybe (Grapheme w t d))
> graphemesAroundFocusMark (Buffer w _ _) = case w of
>   FocusOnly as bs ->
>     ( getRightmostNonControl as
>     , getLeftmostNonControl bs
>     )
>   FocusMark as rs bs ->
>     ( Just $ fst $ unsnocNonEmpty rs
>     , getLeftmostNonControl bs
>     )
>   MarkFocus as rs bs ->
>     ( getRightmostNonControl as
>     , Just $ fst $ unconsNonEmpty rs
>     )
> 
> valuesAroundFocusMark
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe (MeasureText w t d), Maybe (MeasureText w t d))
> valuesAroundFocusMark x =
>   let (u,v) = graphemesAroundFocusMark x
>   in (fmap value u, fmap value v)
> 
> charsAroundFocusMark
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> (Maybe Char, Maybe Char)
> charsAroundFocusMark x =
>   let (u,v) = graphemesAroundFocusMark x
>   in (fmap getGraphemeChar u, fmap getGraphemeChar v)










> insertAtStart
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Char -> Buffer w t d
>   -> (Buffer w t d, BufferOp d)
> insertAtStart eId a buf@(Buffer contents del mem) =
>   let
>     u = case valueAtStart buf of
>       Nothing -> Supremum
>       Just x  -> runeId x
>     v = newLocBetween eId a Infimum u
>     c = Cell $ makePlainGraphemeWithLoc v
>     op = BufferOpIns v
> 
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly (cons c as) bs
>       FocusMark as rs bs -> FocusMark (cons c as) rs bs
>       MarkFocus as rs bs -> MarkFocus (cons c as) rs bs
> 
>     tmem = fmapMem (PRT.spliceTree (singleton (RBT.singleton op))) mem
> 
>   in (Buffer tbuf del tmem, op)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in toString $ fst $ insertAtStart e 'Z' x
> -- :}
> -- "Zabcdef"
> --
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Point Nothing, Cell 'b', Cell 'c'], [MarkLeft, Cell 'e', Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in toString $ fst $ insertAtStart e 'Z' x
> -- :}
> -- "Zbce"
> --
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [Cell 'a', Cell 'b', Cell 'c']
> --     [ 'd', 'e', 'f' ]
> --     [Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in toString $ fst $ insertAtStart e 'Z' x
> -- :}
> -- "Zabcdefghi"

:::

> insertAtEnd
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Char -> Buffer w t d
>   -> (Buffer w t d, BufferOp d)
> insertAtEnd eId a buf@(Buffer contents del mem) =
>   let
>     u = case valueAtStart buf of
>       Nothing -> Supremum
>       Just x  -> runeId x
>     v = newLocBetween eId a Infimum u
>     c = Cell $ makePlainGraphemeWithLoc v
>     op = BufferOpIns v
> 
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly as    (snoc c bs)
>       FocusMark as rs bs -> FocusMark as rs (snoc c bs)
>       MarkFocus as rs bs -> MarkFocus as rs (snoc c bs)
> 
>     tmem = fmapMem (PRT.spliceTree (singleton (RBT.singleton op))) mem
> 
>   in (Buffer tbuf del tmem, op)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Cell 'a', Cell 'b', Cell 'c'], [Cell 'd', Cell 'e', Cell 'f'] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in toString $ fst $ insertAtEnd e 'Z' x
> -- :}
> -- "abcdefZ"
> --
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [Point Nothing, Cell 'b', Cell 'c'], [MarkLeft, Cell 'e', Point (Just OnRight)] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in toString $ fst $ insertAtEnd e 'Z' x
> -- :}
> -- "bceZ"
> --
> -- >>> :{
> -- let
> --   e = EventId 0 "A"
> --   (as, rs, bs) = makePlainGraphemesTF (EventId 0 "")
> --     [Cell 'a', Cell 'b', Cell 'c']
> --     [ 'd', 'e', 'f' ]
> --     [Cell 'g', Cell 'h', Cell 'i']
> --   x = makeFocusMarkBuffer nat8 nat2 nat10 as rs bs
> -- in toString $ fst $ insertAtEnd e 'Z' x
> -- :}
> -- "abcdefghiZ"

:::

> insertAtFocusPointLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Char -> Buffer w t d
>   -> (Buffer w t d, BufferOp d)
> insertAtFocusPointLeft eId a buf@(Buffer contents del mem) =
>   let
>     (u,v) = case valuesAroundFocusPoint buf of
>       (Nothing, Nothing) -> (Infimum,  Supremum)
>       (Just x,  Nothing) -> (runeId x, Supremum)
>       (Nothing, Just y)  -> (Infimum,  runeId y)
>       (Just x,  Just y)  -> (runeId x, runeId y)
>     w = newLocBetween eId a u v
>     c = makePlainGraphemeWithLoc w
>     op = BufferOpIns w
> 
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly (snoc (Cell c) as)                bs
>       FocusMark as rs bs -> FocusMark (snoc (Cell c) as) rs             bs
>       MarkFocus as rs bs -> MarkFocus as                    (snoc c rs) bs
> 
>     tmem = fmapMem (PRT.spliceTree (singleton (RBT.singleton op))) mem
> 
>   in (Buffer tbuf del tmem, op)

> deleteAtFocusPointLeft
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d
>   -> (Buffer w t d, BufferOp d)
> deleteAtFocusPointLeft (Buffer contents (TDel del) mem) =
>   let
>     (tbuf, a) = case contents :: TBuf w t d of
>       FocusOnly as bs -> case splitRightmostNonControl as of
>         Nothing -> (contents, Nothing)
>         Just (c, as') -> (FocusOnly as' bs, Just c)
>       FocusMark as rs bs -> case splitRightmostNonControl as of
>         Nothing -> (contents, Nothing)
>         Just (c, as') -> (FocusMark as' rs bs, Just c)
>       MarkFocus as rs bs ->
>         let (c, rs') = unconsNonEmpty rs
>         in case rs' of
>           FT.Empty -> (FocusOnly as bs, Just c)
>           FT.NonEmpty ws -> (MarkFocus as ws bs, Just c)
> 
>     (op, tdel) = case fmap (runeId . value) a of
>       Nothing -> (BufferNoOp, del)
>       Just (Augmented u)  -> (BufferOpDel u, RBT.insert u del)
> 
>     tmem = fmapMem (PRT.spliceTree (singleton (RBT.singleton op))) mem
>   in (Buffer tbuf (TDel tdel) tmem, op)

> insertAtFocusPointRight
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Char -> Buffer w t d
>   -> (Buffer w t d, BufferOp d)
> insertAtFocusPointRight eId a buf@(Buffer contents del mem) =
>   let
>     (u,v) = case valuesAroundFocusPoint buf of
>       (Nothing, Nothing) -> (Infimum,  Supremum)
>       (Just x,  Nothing) -> (runeId x, Supremum)
>       (Nothing, Just y)  -> (Infimum,  runeId y)
>       (Just x,  Just y)  -> (runeId x, runeId y)
>     w = newLocBetween eId a u v
>     c = makePlainGraphemeWithLoc w
>     op = BufferOpIns w
> 
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly as                (cons (Cell c) bs)
>       FocusMark as rs bs -> FocusMark as (cons c rs) bs
>       MarkFocus as rs bs -> MarkFocus as rs             (cons (Cell c) bs)
> 
>     tmem = fmapMem (PRT.spliceTree (singleton (RBT.singleton op))) mem
> 
>   in (Buffer tbuf del tmem, op)



Splitting
---------

Recall that finger trees, the structure underlying our buffers, admit an efficient _splitting_ operation, which we can take advantage of to move the point and mark to specific locations in the buffer. First we define a utility function, not exposed outside this module, that attempts to split a buffer on an arbitrary predicate.

> clearCursorsExceptFocus
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Buffer w t d
> clearCursorsExceptFocus (Buffer contents del mem) =
>   let
>     p
>       :: MeasureText w t d -> Bool
>     p m = CD.E /= CD.cursorWord (cursorData m)
> 
>     f
>       :: FT.FingerTree (Control (Grapheme w t d))
>       -> FT.FingerTree (Control (Grapheme w t d))
>     f = FT.removeSplitsL p
> 
>     tbuf = case contents :: TBuf w t d of
>       FocusOnly as    bs -> FocusOnly (f as) (f bs)
>       FocusMark as rs bs -> FocusOnly (f as)
>         (f ((fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs) >@> bs))
>       MarkFocus as rs bs -> FocusOnly
>         (f (as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs))) (f bs)
>   in Buffer tbuf del mem

> clearAllCursors
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => TBuf w t d -> FT.FingerTree (Control (Grapheme w t d))
> clearAllCursors buf =
>   let
>     p :: MeasureText w t d -> Bool
>     p m = CD.E /= CD.cursorWord (cursorData m)
>   in case buf of
>     FocusOnly as bs -> FT.removeSplitsL p (as <> bs)
>     FocusMark as rs bs -> FT.removeSplitsL p
>       ((as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) <> bs)
>     MarkFocus as rs bs -> FT.removeSplitsL p
>       ((as <@< (fmapC @Valued @(->) @Valued @(->) @FT.NonEmptyFingerTree Cell rs)) <> bs)
> 
> moveFocusRegion
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => (MeasureText w t d -> Bool)
>   -> Buffer w t d -> Maybe (Buffer w t d)
> moveFocusRegion p (Buffer buf del mem) =
>   case FT.splitL p (clearAllCursors buf) of
>     FT.NotFound -> Nothing
>     FT.Found as u bs -> Just $ Buffer (FocusOnly as (cons u bs)) del mem

For ergonomics' sake we'll expose specialized splitting functions. First at a given line and column position:

> moveFocusRegionToLineCol
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => LineCol -> Buffer w t d -> Buffer w t d
> moveFocusRegionToLineCol lc buf =
>   if lc == LineCol 0 0
>     then jumpFocusRegionToStart buf
>     else case moveFocusRegion (atOrAfterLineCol lc) buf of
>       Nothing -> jumpFocusRegionToEnd buf
>       Just xs -> xs
> 
> atOrAfterLineCol
>   :: LineCol -> MeasureText w t d -> Bool
> atOrAfterLineCol lc m =
>   lc < (logicalCoords m) <> (logicalOffset m)

We should also test some simple cases to make sure we understand how this splitting works.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abc\ndef\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToLineCol (LineCol 0 0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\ndef\ng"
> --     , map Cell "hi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToLineCol (LineCol 2 1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc"
> --     , map Cell "\ndef\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToLineCol (LineCol 0 4) x
> -- :}
> -- True

:::

> moveFocusRegionToLineColOffset
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => LineCol -> Buffer w t d -> Buffer w t d
> moveFocusRegionToLineColOffset lc buf =
>   if lc == LineCol 0 0
>     then jumpFocusRegionToStart buf
>     else case moveFocusRegion (atOrAfterLineCol lc) buf of
>       Nothing -> jumpFocusRegionToEnd buf
>       Just xs -> xs
> 
> atOrAfterLineColOffset
>   :: LineCol -> MeasureText w t d -> Bool
> atOrAfterLineColOffset lc m =
>   lc <= (logicalCoords m) <> (logicalOffset m)

Next we'd like to split at a given pair of screen coordinates. It's straightforward how this should work if we try to split at a pair of coordinates corresponding to a character in the buffer (split there). It's also (slightly less but still fairly) straightforward how to handle coordinates that fall off the end of the buffer; we'll just jump to the end. But there's a universe of pairs whose $y$ coordinates correspond to _lines_ on the screen, but whose $x$ coordinates march off the right edge. What should these do?

One option is to just fail. But I think a better option is to try our best to return a reasonable result: in this case, to move the point to the last position on the screen line. In some sense that is "as close as we can get" in this situation.

> moveFocusRegionToScreenCoords
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> Buffer w t d -> Buffer w t d
> moveFocusRegionToScreenCoords pos buf =
>   if pos == (0,0)
>     then jumpFocusRegionToStart buf
>     else case moveFocusRegion (atOrAfterScreenCoords pos) buf of
>       Nothing -> jumpFocusRegionToEnd buf
>       Just xs -> xs
> 
> atOrAfterScreenCoords
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> MeasureText w t d -> Bool
> atOrAfterScreenCoords (u,v) m =
>   let
>     (h,k) = applyScreenOffset (screenCoords m) (0,0)
>   in (v < k) || ((v == k) && (u < h))

We can test our understanding with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abc\ndef\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (0,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "a"
> --     , map Cell "bc\ndef\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (1,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (2,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "ab"
> --     , map Cell "cdefgh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abcdefg"
> --     , map Cell "h"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (7,0) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "ab"
> --     , map Cell "cdefghijklmnop"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abcdefghij"
> --     , map Cell "klmnop"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (2,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde\nf"
> --     , map Cell "gh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "\nfgh"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (5,1) x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abcdefghijklmn"
> --     , map Cell "opqrst"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abcdefg"
> --     , map Cell "hijklmnopqrst"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenCoords (10,0) x
> -- :}
> -- True

:::

> moveFocusRegionToScreenOffset
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> Buffer w t d -> Buffer w t d
> moveFocusRegionToScreenOffset pos buf =
>   if pos == (0,0)
>     then jumpFocusRegionToStart buf
>     else case moveFocusRegion (atOrAfterScreenOffset pos) buf of
>       Nothing -> jumpFocusRegionToEnd buf
>       Just xs -> xs
> 
> atOrAfterScreenOffset
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> MeasureText w t d -> Bool
> atOrAfterScreenOffset (u,v) m =
>   let
>     (h,k) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in (v < k) || ((v == k) && (u < h))

Next we define a utility which moves the point to the first character of a given screen line -- which is crucial for rendering buffers to the screen. This is a little trickier than moving to a specific line and character or pair of screen coordinates, because it involves a _relative_ position and nailing down exactly what it means to say that a character is at the start of a new screen line.

Suppose the final screen coordinate in the buffer is $(W,H)$ and we want to move to the first position of screen line $y$. If $y$ is in the interval $[0,H]$, then some character has screen coordinate $(0,y)$ and we can find it with an ordinary split. Similarly simple, if $y$ is at least $H+2$ then no such character exists, and the EOF sigil can't help us. The delicate case is when we want to move to the first position of screen line $H+1$. This case depends on (1) the final character in the buffer and (2) the width of the final screen line in the buffer (that is, $W$). If the final character is a newline, we'd like to consider the EOF sigil to be the start of the next screen line. If the final character is not a newline, then we consider the EOF sigil to be on the last line (not the start of the next line) _unless_ the final character's effective width plus $W$ exceeds the screen width.

That's a mouthful!

> moveFocusRegionToScreenLine
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -> Buffer w t d -> Buffer w t d
> moveFocusRegionToScreenLine k buf =
>   let (_,h) = getBufferScreenOffset buf
>   in if k > 1 + h
>     then jumpFocusRegionToEnd buf
>     else if k <= 0
>       then jumpFocusRegionToStart buf
>       else case moveFocusRegion (atOrAfterScreenLine k) buf of
>         Just z -> z
>         Nothing -> jumpFocusRegionToEnd buf
> 
> atOrAfterScreenLine
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -> MeasureText w t d -> Bool
> atOrAfterScreenLine k m =
>   let
>     offset = (screenCoords m) <> (screenOffset m)
>     (u,v) = applyScreenOffset offset (0,0)
>   in (v > k) || ((v == k) && ((u >= 1) || ((u == 0) && (endOnControl m))))

> toAnnotatedList_test
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d )
>   => (MeasureText w t d -> a)
>   -> Buffer w t d -> [(Control Char, a)]
> toAnnotatedList_test p =
>   map f . toAnnotatedList
>   where
>     f (x,y) = (fmap getGraphemeChar x, p y)

::: doctest

> -- $
> -- First line not filled
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc", [] ]
> --   x = toAnnotatedList_test (atOrAfterScreenLine 1) $
> --         makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   y = [ (Cell 'a', False), (Cell 'b', False)
> --       , (Cell 'c', False), (Focus Nothing, False)]
> -- in x == y
> -- :}
> -- True
> --
> -- Control after a newline
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ [ Cell '\n' ], [] ]
> --   x = toAnnotatedList_test (atOrAfterScreenLine 1) $
> --         makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   y = [ (Cell '\n', False), (Focus Nothing, True)]
> -- in x == y
> -- :}
> -- True
> --
> -- Control after a newline, with another character
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ [ Cell '\n' ], [ Cell 'a' ] ]
> --   x = toAnnotatedList_test (atOrAfterScreenLine 1) $
> --         makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   y = [ (Cell '\n', False), (Focus Nothing, True), (Cell 'a', True)]
> -- in x == y
> -- :}
> -- True

:::

And some example splittings:

::: doctest

> -- $
> -- Split at line 0 < h
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde"
> --     , map Cell "f\nghi"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abc\ndef\nghi"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 0 x
> -- :}
> -- True
> --
> -- Split at line 1 < h
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde\nf"
> --     , map Cell "gh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\n"
> --     , map Cell "de\nfgh"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 2 < h
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\n"
> --     , map Cell "de\nfgh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nde\n"
> --     , map Cell "fgh"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 2 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an interior char)
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "a"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "a"
> --     , map Cell ""
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is a newline)
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "\n"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "\n"
> --     , map Cell ""
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end char)
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "aaaaaaaa"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "aaaaaaaa"
> --     , map Cell ""
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line h+1 (last cell is an end tab)
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "\t\t\t\t"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "\t\t\t\t"
> --     , map Cell ""
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True
> --
> -- Split at line 1 == h (last cell is a newline)
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "\n\n"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> --   --
> --   [as2, bs2] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "\n"
> --     , map Cell "\n"
> --     ]
> --   y = makeFocusOnlyBuffer nat8 nat2 nat10 as2 bs2
> -- in y $== moveFocusRegionToScreenLine 1 x
> -- :}
> -- True

:::

With `moveFocusRegionToScreenCoords` in hand, we can also figure out the coordinates "nearest" to a given pair which actually appear in the buffer.

> seekScreenCoords
>   :: ( IsWidth w, IsTab t, IsBase d )
>   => (Int, Int) -> Buffer w t d -> (Int, Int)
> seekScreenCoords z =
>   getFocusPointScreenCoords . moveFocusRegionToScreenCoords z

And some tests for our intuition:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nd"
> --     , map Cell "ef\ngh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in seekScreenCoords (0,0) x
> -- :}
> -- (0,0)
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nd"
> --     , map Cell "ef\ngh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in seekScreenCoords (2,0) x
> -- :}
> -- (2,0)
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nd"
> --     , map Cell "ef\ngh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in seekScreenCoords (3,0) x
> -- :}
> -- (3,0)
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "abc\nd"
> --     , map Cell "ef\ngh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in seekScreenCoords (4,0) x
> -- :}
> -- (3,0)

:::





Rendering
---------

Buffers are a data structure for storing and manipulating text, but they're not very helpful for _viewing_ the text. We'll also need some code to convert the internal representation of a buffer into a format suitable for rendering on a screen.

We're making a major simplifying assumption that comes in handy here: text will be drawn in a monospaced typeface, meaning we can pretend that the display window is divided into a rectangular grid of character positions and each character occupies a whole number of grid cells.

The buffer is naturally divided into _screen lines_ -- these are contiguous chunks of text that will render at the same $y$ coordinate. We've set up the `MeasureText` type so that these are easy to find. All but the last of these screen lines will be _full_, meaning that every cell is occupied (possibly by a newline or tab). The last screen line is not full; this is where the EOF sigil hangs out.

To flesh out the theory of buffers, we'll include here a function which detects whether or not the buffer has a full screen line.

> hasFullScreenLine
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Buffer w t d -> Bool
> hasFullScreenLine w =
>   let
>     m = value w :: MeasureText w t d
>     (_,h) = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>   in h > 0

This function is primarily used in testing to help scaffold the consistency of our main rendering code. Here are some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abc"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in hasFullScreenLine x
> -- :}
> -- False
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abc\n"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in hasFullScreenLine x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   [as1, bs1] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "abcdefgh"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as1 bs1
> -- in hasFullScreenLine x
> -- :}
> -- True

:::

To render the buffer we'll first need to extract the visible screen lines. Working toward that goal, we'll start with something simpler: extracting the _first_ screen line from a buffer. Note the return type; we can always extract _something_ from a valid buffer, even if it's just the EOF sigil. The second entry in the return type indicates whether this happened -- it's `Nothing` if the EOF appears in the first entry, and `Just` otherwise.

> takeFirstScreenLine
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => FT.FingerTree (Control (Grapheme w t d))
>   -> Maybe
>       ( FT.FingerTree (Control (Grapheme w t d))
>       , FT.FingerTree (Control (Grapheme w t d))
>       )
> takeFirstScreenLine w =
>   if isEmpty w
>     then Nothing
>     else case FT.splitL (atOrAfterScreenLine 1) w of
>       FT.NotFound -> Just (w, empty)
>       FT.Found as x bs -> Just (as, cons x bs)

It is crucial that we understand exactly how this function works, since the rest of the rendering code is built on it. Here are some examples covering some of the different ways a buffer can be populated.

::: doctest

> -- $
> -- >>> -- 'Normal' text with a newline
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toControlsFT y
> -- :}
> -- [Cell 'a',Cell 'b',Cell 'c',Cell '\n']
> --
> -- >>> -- Controls after a new line should not be included
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\n", [ Focus Nothing ] ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toControlsFT y
> -- :}
> -- [Cell 'a',Cell 'b',Cell 'c',Cell '\n']
> --
> -- >>> -- No full screen lines
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "a", [ Focus Nothing ], map Cell "bc" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toString y
> -- :}
> -- "abc"
> --
> -- >>> -- All newlines
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "\n\n\n", [ Focus Nothing ], map Cell "\n\n" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toString y
> -- :}
> -- "\n"
> --
> -- >>> -- Full screen line with no newlines
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abcdefghij", [ Focus Nothing ], map Cell "klmn" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toString y
> -- :}
> -- "abcdefgh"
> --
> -- >>> -- 'Normal' text with a newline
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --       [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> --   Just (y, _) = takeFirstScreenLine x
> -- in toString y
> -- :}
> -- "abc\n"

:::

After taking the first screen line, the next step in complexity is to take some number of screen lines from the front of the buffer. This is a pretty straightforward application of an accumulating recursive function.

> takeScreenLines
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -> FT.FingerTree (Control (Grapheme w t d))
>   -> [ FT.FingerTree (Control (Grapheme w t d)) ]
> takeScreenLines n = unfoldN n takeFirstScreenLine

> unfoldN :: forall a b. Int -> (b -> Maybe (a,b)) -> b -> [a]
> unfoldN n f = fst . accum 0 []
>   where
>     accum :: Int -> [a] -> b -> ([a], Maybe b)
>     accum k as z = if k >= n
>       then (Prelude.reverse as, Just z)
>       else case f z of
>         Nothing -> (Prelude.reverse as, Nothing)
>         Just (a, z') -> accum (k+1) (a:as) z'

Interesting examples of this are a little more verbose:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> -- in map toString $ takeScreenLines 2 x
> -- :}
> -- ["abc\n","defg\n"]
> --
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "" ]
> --   x = makeFingerTree nat8 nat2 nat10 cs
> -- in map toString $ takeScreenLines 3 x
> -- :}
> -- ["abc\n","def"]

:::

Now we're prepared to extract a 'screenful' of screen lines from anywhere in the buffer with `getScreenLines`. This function returns the accumulated `MeasureText` up to the extracted lines; we'll need this to compute the (logical) line numbers.

> getScreenLines
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -- top screen line
>   -> Int -- view height
>   -> FT.FingerTree (Control (Grapheme w t d))
>   -> ( MeasureText w t d -- value before first line
>      , Bool -- is there an open region at the beginning?
>      , [ FT.FingerTree (Control (Grapheme w t d)) ] -- screen lines
>      )
> getScreenLines t h w = case FT.splitL (atOrAfterScreenLine t) w of
>   FT.NotFound -> ( value w, False, [] )
>   FT.Found as x bs ->
>     let
>       u = CD.isRegionBoundary
>         (CD.cursorWord $ cursorData $ value as)
>         (CD.cursorWord $ cursorData $ value (cons x bs))
>     in ( value as, u, takeScreenLines h (cons x bs) )
> 
> getScreenLines_test
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -- top screen line
>   -> Int -- view height
>   -> FT.FingerTree (Control (Grapheme w t d))
>   -> ( Bool, [[ Control Char ]] )
> getScreenLines_test t h w =
>   let (v, p, x) = getScreenLines t h w
>   in (p, map (map (fmap getGraphemeChar) . Fold.toList) x)

::: doctest

> -- $
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ [ Focus Nothing ] ]
> --   x = makeFingerTree nat4 nat2 nat10 cs
> -- in getScreenLines_test 0 1 x
> -- :}
> -- (False,[[Focus Nothing]])
> --
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat4 nat2 nat10 cs
> -- in getScreenLines_test 0 1 x
> -- :}
> -- (False,[[Cell 'a',Cell 'b',Cell 'c',Cell '\n']])
> --
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ map Cell "abc\ndef", [ Focus Nothing ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat4 nat2 nat10 cs
> -- in getScreenLines_test 1 1 x
> -- :}
> -- (False,[[Cell 'd',Cell 'e',Cell 'f',Focus Nothing,Cell 'g']])
> --
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ [ MarkLeft ], map Cell "abc\ndef", [ Focus (Just OnRight) ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat4 nat2 nat10 cs
> -- in getScreenLines_test 0 1 x
> -- :}
> -- (False,[[MarkLeft,Cell 'a',Cell 'b',Cell 'c',Cell '\n']])
> --
> -- >>> :{
> -- let
> --   cs = makePlainGraphemesF (EventId 0 "") $ concat
> --     [ [ MarkLeft ], map Cell "abc\ndef", [ Focus (Just OnRight) ], map Cell "g\nhi" ]
> --   x = makeFingerTree nat4 nat2 nat10 cs
> -- in getScreenLines_test 1 1 x
> -- :}
> -- (True,[[Cell 'd',Cell 'e',Cell 'f',Focus (Just OnRight),Cell 'g']])

:::

Next we can attach the logical line numbers to the screen lines. (_Logical_ here means lines as separated by newline characters; these can be broken across several screen lines.)

> attachLineNumbers
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d
>      , Valued a, Value a ~ MeasureText w t d )
>   => ( MeasureText w t d, [a] )
>   -> [ (Maybe Int, a) ]
> attachLineNumbers (ctx, xs) = case xs of
>   [] -> []
>   u:us ->
>     let
>       LineCol k h = logicalCoords ctx <> logicalOffset ctx
>       v = if h == 0 then Just k else Nothing
>     in (v,u) : attachLineNumbers ( ctx <> value u, us )

We will also need to know the expected column index of each rendered character later on -- this is so we can render tabs correctly.

> attachColumnIndices
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d
>      , Valued a, Value a ~ MeasureText w t d )
>   => FT.FingerTree a
>   -> [(a, Int)]
> attachColumnIndices xs = map f $ FT.toAnnotatedList xs
>   where
>     f
>       :: (a, MeasureText w t d)
>       -> (a, Int)
>     f (a, m) =
>       let (w, _) = applyScreenOffset (screenCoords m) (0,0)
>       in (a, w)



> data RenderedBuffer a = RB
>   { rbHasRegionAtStart :: Bool
>   , rbRenderedWidth :: Int
>   , rbRenderedHeight :: Int
>   , rbTabStopWidth :: Int
>   , rbLines :: [RenderedLine a]
>   } deriving (Eq, Show)
> 
> data RenderedLine a = RL
>   { rlLineNumber :: Maybe Int
>   , rlGraphemes :: [RenderedGrapheme a]
>   } deriving (Eq, Show)
> 
> data RenderedGrapheme a = RG
>   { rgGrapheme :: a
>   , rgColumnNumber :: Int
>   } deriving (Eq, Show)

> showRenderedBufferWith
>   :: forall a
>    . (a -> String)
>   -> RenderedBuffer (Control a) -> [String]
> showRenderedBufferWith f (RB _ _ _ _ ls) =
>   map printLine ls
>   where
>     printLine
>       :: RenderedLine (Control a) -> String
>     printLine (RL k gs) =
>       let
>         pfx = case k of
>           Nothing -> "-:"
>           Just n  -> show n ++ ":"
>         f (RG g _) = showGrapheme g
>       in pfx ++ concatMap f gs
> 
>     showGrapheme :: Control a -> String
>     showGrapheme g = case g of
>       Cell c -> f c
>       Focus Nothing -> "[F]"; Focus (Just OnLeft) -> "[FL]"; Focus (Just OnRight) -> "[FR]"
>       Point Nothing -> "[P]"; Point (Just OnLeft) -> "[PL]"; Point (Just OnRight) -> "[PR]"
>       MarkLeft -> "[L]"; MarkRight -> "[R]"

> showRenderedBufferDebug
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => RenderedBuffer (Control (Grapheme w t d)) -> [String]
> showRenderedBufferDebug =
>   showRenderedBufferWith printGraphemePlain_test

> printRenderedBufferWith
>   :: forall a
>    . (a -> String)
>   -> RenderedBuffer (Control a) -> IO ()
> printRenderedBufferWith f =
>   mapM_ putStrLn . showRenderedBufferWith f

> printGraphemePlain_test
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Grapheme w t d -> String
> printGraphemePlain_test g =
>   case getGraphemeChar g of
>     '\n' -> "\\n"; '\t' -> "\\t"; u -> [u]


> renderScreenLines
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Int -- top screen line
>   -> Int -- height
>   -> Buffer w t d
>   -> RenderedBuffer (Control (Grapheme w t d))
> renderScreenLines t h w =
>   let
>     wid = toWidth (Proxy :: Proxy w)
>     tab = toTab (Proxy :: Proxy t)
>     (m, p, u) = getScreenLines t h (toFingerTree w)
> 
>     renderLine
>       :: (Maybe Int, FT.FingerTree (Control (Grapheme w t d)))
>       -> RenderedLine (Control (Grapheme w t d))
>     renderLine (k, ln) = RL
>       { rlLineNumber = k
>       , rlGraphemes = map (uncurry RG) $ attachColumnIndices ln
>       }
> 
>   in RB
>     { rbHasRegionAtStart = p
>     , rbRenderedWidth = wid
>     , rbRenderedHeight = h
>     , rbTabStopWidth = tab
>     , rbLines = take h $ map renderLine $ attachLineNumbers (m,u)
>     }

::: doctest

> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell ""
> --     , map Cell "a"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in printRenderedBufferWith printGraphemePlain_test $ renderScreenLines 0 1 x
> -- :}
> -- 0:[F]a
> --
> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "a"
> --     , map Cell "bc"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in printRenderedBufferWith printGraphemePlain_test $ renderScreenLines 0 1 x
> -- :}
> -- 0:a[F]bc
> --
> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ map Cell "a\n"
> --     , map Cell "bc"
> --     ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in printRenderedBufferWith printGraphemePlain_test $ renderScreenLines 0 2 x
> -- :}
> -- 0:a\n
> -- 1:[F]bc
> --
> -- $
> -- >>> :{
> -- let
> --   [as, bs] = makePlainGraphemesLF (EventId 0 "")
> --     [ [ Cell '\n' ], [] ]
> --   x = makeFocusOnlyBuffer nat8 nat2 nat10 as bs
> -- in printRenderedBufferWith printGraphemePlain_test $ renderScreenLines 0 2 x
> -- :}
> -- 0:\n
> -- 1:[F]

:::

Putting it all together, `renderScreenLinesWithRegion` extracts (1) a list of screen lines with column indices attached to each character, and (2) a list of logical line numbers.

> type LogicalLineNumber = Int
> type ScreenLineNumber = Int
> type ScreenColumnNumber = Int
> type IsInsideRegion = Bool
> type IsLineStart = Bool
> type IsLineEnd = Bool
> type RenderedWidth = Int
> type RenderedHeight = Int
> type TabStopWidth = Int

> type ProcRenderedBufferEntry s a b
>    = Maybe LogicalLineNumber
>   -> ScreenLineNumber
>   -> ScreenColumnNumber
>   -> RenderedWidth
>   -> RenderedHeight
>   -> TabStopWidth
>   -> IsInsideRegion
>   -> IsLineStart
>   -> IsLineEnd
>   -> s -> a -> (b, s)

> mungeRenderedBuffer
>   :: forall a b s
>    . s -> ProcRenderedBufferEntry s a b
>   -> RenderedBuffer (Control a) -> (RenderedBuffer (Control b), s)
> mungeRenderedBuffer s0 f (RB p wid ht tab ls) =
>   let
>     (ls', (s1, _)) = mungeLines (s0, p) ls
> 
>     mungeLines
>       :: (s, IsInsideRegion) -> [RenderedLine (Control a)]
>       -> ([RenderedLine (Control b)], (s, IsInsideRegion))
>     mungeLines (s,q) us = accum [] (zip us [0..]) (s,q)
>       where
>         accum
>           :: [RenderedLine (Control b)]
>           -> [(RenderedLine (Control a), ScreenLineNumber)]
>           -> (s, IsInsideRegion)
>           -> ([RenderedLine (Control b)], (s, IsInsideRegion))
>         accum xs ys st = case ys of
>           [] -> (Prelude.reverse xs, st)
>           (RL n gs, k):vs ->
>             let (gs', st') = mungeGraphemes n k st gs
>             in accum ((RL n gs'):xs) vs st'
> 
>     mungeGraphemes
>       :: Maybe LogicalLineNumber -> ScreenLineNumber -> (s, IsInsideRegion)
>       -> [RenderedGrapheme (Control a)] -> ([RenderedGrapheme (Control b)], (s, IsInsideRegion))
>     mungeGraphemes n k (s,q) gs = accum [] gs (s,q) True
>       where
>         accum
>           :: [RenderedGrapheme (Control b)]
>           -> [RenderedGrapheme (Control a)]
>           -> (s, IsInsideRegion)
>           -> IsLineStart
>           -> ([RenderedGrapheme (Control b)], (s, IsInsideRegion))
>         accum xs ys st h = case ys of
>           [] -> (Prelude.reverse xs, st)
>           (RG w c):vs ->
>             let
>               (s,q) = st
>               (w', st') = case w of
>                 Cell a ->
>                   let (b,s') = f n k c wid ht tab q h (null vs) s a
>                   in (Cell b, (s', q))
>                 Focus m   -> case m of
>                   Nothing      -> (Focus Nothing, (s, q))
>                   Just OnLeft  -> (Focus (Just OnLeft), (s, True))
>                   Just OnRight -> (Focus (Just OnRight), (s, False))
>                 Point m   -> case m of
>                   Nothing      -> (Point Nothing, (s, q))
>                   Just OnLeft  -> (Point (Just OnLeft), (s, True))
>                   Just OnRight -> (Point (Just OnRight), (s, False))
>                 MarkLeft  -> (MarkLeft, (s, True))
>                 MarkRight -> (MarkRight, (s, False))
>             in accum ((RG w' c):xs) vs st' False
> 
>   in (RB p wid ht tab ls', s1)

> getRenderedBufferLines
>   :: forall a
>    . RenderedBuffer (Control a)
>   -> ([Maybe Int], [[(a, Int)]])
> getRenderedBufferLines (RB _ _ _ _ ls) =
>   let (ns, gss) = unzip $ map (\(RL n gs) -> (n, gs)) ls
>   in (ns, map (foldr f []) gss)
>     where
>       f :: RenderedGrapheme (Control a) -> [(a, Int)] -> [(a, Int)]
>       f x as = case rgGrapheme x of
>         Cell a -> (a, rgColumnNumber x):as
>         _      -> as










Our primary data structure for representing and manipulating text is the _buffer_. At the semantic level we can treat buffers like lists of characters, although under the hood they use a more complex representation to make some important operations more efficient. The buffer also has one and possibly two distinguished positions, called the _point_ and the _mark_, which are used to specify where edits take effect.

The actual definition of buffers builds on two other abstractions: two-pointed lists and our text measurement type. However these details aren't exposed to client code.



> {-

Note that `MeasureText` takes two type parameters, `w` and `t`. These are type level representations of the width of the screen in cells and the tab stop width, respectively. Our underlying finger tree implementation makes it necessary that these be type parameters; essentially they are required by the monoid instance on text measurements. Later on we'll wrap buffers inside an existential type to partially hide this detail while maintaining type safety.

We also impose one invariant on buffers that can't be represented in the type: the final item in the list of characters must be a special end-of-file sigil. This simplifies the usual insert and delete operations on text by making it possible to assert that the "cursor" is at the left edge of a cell. We will have to be careful that our operations on buffers maintain this invariant (only in this module; client code shouldn't need to worry about that).

Because buffers are "just" two pointed lists, they inherit the API of two pointed lists, giving us a decent amount of code nearly for free (modulo maintaining the eof invariant). We can measure the entire buffer:

We also get the usual `empty` and `singleton` constructors.

More generally, we can convert lists to buffers (and back again).

> cutRegion
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => EventId -> Buffer w t d
>   -> Maybe (String, Buffer w t d, [BufferOp d])
> cutRegion eId (Buffer w del) = do
>   (region, rest) <- TPL.cutRegionL w
>   let
>     xs :: [Rune d a]
>     xs = concatMap listCell $ Fold.toList region 
>   return
>     ( map getRuneValue xs
>     , Buffer rest (RBT.insertAll xs del)
>     , map (\x -> BufferOpDel (setEventId eId x)) xs )

> 
> fromCellList
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [Cell a] -> Buffer w t d a
> fromCellList eId xs = Buffer (TPL.fromList $ makeRunesF eId xs) RBT.empty
> 
> fromFingerTree
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> FT.FingerTree (MeasureText w t d) a -> Buffer w t d a
> fromFingerTree eId = fromList eId . Fold.toList
> 


Due to the EOF sigil, detecting when a buffer is empty or a singleton is a little more involved.




> isSingleton
>   :: forall w t d a
>    . ( IsChar a, Eq a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isSingleton (Buffer w _) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "isSingleton: panic (expected eof)"
>     Just (a, as) ->
>       (a == (eof :: Cell (Rune d a))) && (TPL.isSingleton as)

It will also be handy to be able to resize the buffer in two different ways, depending on whether we want to specify the new parameters 'statically' or 'dynamically'.

> 
> adjustBuffer
>   :: ( IsBase d
>      , IsWidth w1, IsTab t1, Valued (MeasureText w1 t1 d) a
>      , IsWidth w2, IsTab t2, Valued (MeasureText w2 t2 d) a )
>   => Proxy w2 -> Proxy t2 -> Proxy d
>   -> Buffer w1 t1 d a -> Buffer w2 t2 d a
> adjustBuffer _ _ _ = resizeBuffer

We'll also go ahead and define some special constructors for building buffers of a very specific structure. These should only be used for testing and debugging, but we need to define them early in the module so we can use them in examples as we go. These correspond to the nontrivial constructors of two-pointed lists.



Point Manipulation
------------------

The point and mark of a buffer represent the read head, and moving them around is one of the most important and common tasks our code needs to handle. We inherit a decent API for this from two-pointed lists which we can basically re-expose. First some queries on the point and mark:

> hasMark
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> hasMark =
>   TPL.hasMark . bufContents
> 
> 
> isMarkAtStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isMarkAtStart =
>   TPL.isMarkAtStart . bufContents
> 
> isMarkAtEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Bool
> isMarkAtEnd =
>   TPL.isMarkAtEnd . bufContents

Next some basic operators for moving either the point or the mark to one of the ends of the buffer.

> 
> moveMarkToStart
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkToStart (Buffer w del) =
>   Buffer (TPL.moveMarkToStart w) del
> 
> moveMarkToEnd
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkToEnd (Buffer w del) =
>   Buffer (TPL.moveMarkToEnd w) del

We also have operators for moving the point and mark one cell to the left or right.

> 
> moveMarkLeft
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkLeft (Buffer w del) =
>   Buffer (TPL.moveMarkLeft w) del
> 
> moveMarkRight
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Buffer w t d a
> moveMarkRight (Buffer w del) =
>   Buffer (TPL.moveMarkRight w) del

We also have utilities for setting and clearing the mark. Recall that both `clearMark` and `leaveMark` leave the point unchanged; `clearMark` removes the mark, and `leaveMark` sets or resets it to coincide with the point.





Basic Mutation
--------------

Next we need buffer versions of the region operators. We can read the character at the point:

> readPoint
>   :: Buffer w t d a -> Maybe (Cell a)
> readPoint (Buffer w _) =
>   fmap (fmap getRuneValue) $ TPL.readPoint w

And we need left-biased insert and delete at the point.

> insertPointLeft
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a
>   -> (Buffer w t d a, EventId -> BaseBufferOp a)
> insertPointLeft eId a (Buffer w del) =
>   let
>     u = case TPL.valuesAroundPoint w of
>       Nothing    -> (Infimum, Supremum)
>       Just (x,y) -> (runeId x, runeId y)
>     v = newRuneId eId u a
>   in
>     ( Buffer (TPL.insertPointLeft (Cell v) w) del
>     , \eId' -> BaseBufferOp (BufferOpIns (setEventId eId' v))
>     )
> 





> copyRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => Buffer w t d a -> Maybe [a]
> copyRegion (Buffer w _) = do
>   region <- TPL.copyRegionL w
>   return $ map getRuneValue $ concatMap listCell $ Fold.toList region

> insertRegion
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
>   -> (Buffer w t d a, EventId -> [BaseBufferOp a])
> insertRegion eId ins (Buffer w del) =
>   let

>     (old, rest) = case TPL.cutRegionL w of
>       Nothing -> (mempty, w)
>       Just z -> z

>     xs :: [Rune d a]
>     xs = concatMap listCell $ Fold.toList old
> 
>     u = case TPL.valuesAroundPoint rest of
>       Nothing    -> (Infimum, Supremum)
>       Just (x,y) -> (runeId x, runeId y)
>     vs = newRunesId eId u ins
>   in
>     ( Buffer
>         (TPL.insertRegionL (fromList $ map Cell vs) rest)
>         (RBT.insertAll xs del)
>     , \eId' -> map BaseBufferOp $ concat
>         [ map (\x -> BufferOpDel (setEventId eId' x)) xs
>         , map (\v -> BufferOpIns (setEventId eId' v)) vs ] )

We can also efficiently manipulate the start and end of the buffer.


> 
> prepend
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
>   -> (Buffer w t d a, [BufferOp d a])
> prepend eId cs b = prepend' b (reverse cs) []
>   where
>     prepend' x as ops = case as of
>       [] -> (x, ops)
>       z:zs ->
>         let (y, op) = insertAtStart eId z x
>         in prepend' y zs (op:ops)
> 
> deleteAtStart
>   :: ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => EventId -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> deleteAtStart eId (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtStart: panic (empty buffer)"
>     Just (u, us) ->
>       case TPL.deleteAtStart' us of
>         Nothing -> (Buffer w del, BufferNoOp)
>         Just (w', a) -> case a of
>           EOF -> error "deletePointLeft: panic"
>           Cell c ->
>             ( Buffer (TPL.insertAtEnd u w') (RBT.insert c del)
>             , BufferOpDel (setEventId eId c)
>             )
> 
> insertAtEnd
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> a -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> insertAtEnd eId a (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "insertAtEnd: panic (empty buffer)"
>     Just (z, us) ->
>       let
>         u = case TPL.viewAtEnd us of
>           Nothing    -> (Infimum, Supremum)
>           Just (x,y) -> (runeId (value x :: MeasureText w t d), Supremum)
>         v = newRuneId eId u a
>       in
>         ( Buffer (TPL.insertAtEnd z $ TPL.insertAtEnd (Cell v) us) del
>         , BufferOpIns v
>         )
> 
> append
>   :: forall w t d a
>    . ( Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, IsChar a )
>   => EventId -> [a] -> Buffer w t d a
>   -> (Buffer w t d a, [BufferOp d a])
> append eId cs b = append' b cs []
>   where
>     append' x as ops = case as of
>       [] -> (x, ops)
>       z:zs ->
>         let (y, op) = insertAtEnd eId z x
>         in append' y zs (op:ops)
> 
> deleteAtEnd
>   :: ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a )
>   => EventId -> Buffer w t d a -> (Buffer w t d a, BufferOp d a)
> deleteAtEnd eId (Buffer w del) =
>   case TPL.viewAtEnd w of
>     Nothing -> error "deleteAtEnd: panic (empty buffer)"
>     Just (u, us) ->
>       case TPL.viewAtEnd us of
>         Nothing -> (Buffer w del, BufferNoOp)
>         Just (a, w') -> case a of
>           EOF -> error "deletePointLeft: panic"
>           Cell c ->
>             ( Buffer (TPL.insertAtEnd u w') (RBT.insert c del)
>             , BufferOpDel (setEventId eId c)
>             )

> {-

We can also map over the entries in a buffer.

> mapBuffer
>   :: ( IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a1
>      , Valued (MeasureText w t d) a2 )
>   => (a1 -> a2) -> Buffer w t d a1 -> Buffer w t d a2
> mapBuffer f (Buffer c r) =
>   Buffer (TPL.fmapTPL (fmap (fmap f)) c) r
> 
> mapRegion
>   :: ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a )
>   => (a -> a) -> Buffer w t d a -> Buffer w t d a
> mapRegion f =
>   Buffer . TPL.alterRegionL (fmap (fmap f)) . unBuffer

> -}

Queries
-------

More than any of the data structures we've seen so far, buffers have _properties_. We now define an interface through which other modules can query these properties; for consistency's sake these all start with `get`. First some functions for reifying the type level parameters: width and tab stop.







Splitting
---------







Finally, we'll also need to split the buffer at a given rune ID.

> movePointToRuneId
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => Augmented (RuneId d) -> Buffer w t d a -> Buffer w t d a
> movePointToRuneId rId buf =
>   case movePoint (atOrAfterRuneId rId) buf of
>     Nothing -> movePointToEnd buf
>     Just xs -> xs
> 
> atOrAfterRuneId
>   :: forall w t d
>    . ( IsWidth w, IsTab t, IsBase d )
>   => Augmented (RuneId d) -> MeasureText w t d -> Bool
> atOrAfterRuneId rId m =
>   rId < (runeId m)

And with this splitting in hand, we can apply reified buffer operations.

> applyBufferOp
>   :: forall w t d a
>    . ( IsChar a, IsWidth w, IsTab t, IsBase d
>      , Valued (MeasureText w t d) a, Eq a, Ord a )
>   => BufferOp d a -> Buffer w t d a -> Buffer w t d a
> applyBufferOp op buf = case op of
>   BufferNoOp -> buf
> 
>   BufferOpIns rune ->
>     if RBT.member rune (bufRemnants buf)
>       then buf
>       else
>         let
>           buf' = movePointToRuneId (Augmented (getRuneId rune)) buf
>           pId = getPointRuneId buf'
>           Buffer contents remnants = buf'
>         in if pId == Augmented (getRuneId rune)
>           then buf
>           else Buffer (TPL.insertPointLeft (Cell rune) contents) remnants
> 
>   BufferOpDel rune ->
>     if RBT.member rune (bufRemnants buf)
>       then buf
>       else 
>         let
>           buf' = movePointToRuneId (Augmented (getRuneId rune)) buf
>           pId = getPointRuneId buf'
>           Buffer contents remnants = buf'
>         in if pId /= Augmented (getRuneId rune)
>           then Buffer contents (RBT.insert rune remnants)
>           else Buffer (TPL.deletePointLeft contents) (RBT.insert rune remnants)

::: doctest

> -- $
> -- >>> :{
> -- -- Delete and insert the same rune, but in different orders
> -- let
> --   rune = newRuneId (EventId 0 "foo") (Infimum, Supremum) 'a'
> --   op1 = BufferOpDel rune
> --   op2 = BufferOpIns rune
> --   e = makeVacantBuffer nat8 nat2 nat3 (Proxy :: Proxy Char)
> --   zig = applyBufferOp op1 (applyBufferOp op2 e)
> --   zag = applyBufferOp op2 (applyBufferOp op1 e)
> -- in zig == zag
> -- :}
> -- True
> --
> -- $
> -- >>> :{
> -- -- Delete and insert runes that differ only by a color in different orders
> -- let
> --   rune1 = newRuneId (EventId 0 "foo") (Infimum, Supremum) $
> --     Glyph 'A' (gray24 0) (gray24 0)
> --   rune2 = newRuneId (EventId 0 "foo") (Infimum, Supremum) $
> --     Glyph 'A' (rgb6 0 0 0) (rgb6 0 0 0)
> --   op1 = BufferOpIns rune1
> --   op2 = BufferOpDel rune2
> --   e = makeVacantBuffer nat8 nat2 nat3 (Proxy :: Proxy (Glyph Char))
> --   zig = applyBufferOp op1 (applyBufferOp op2 e)
> --   zag = applyBufferOp op2 (applyBufferOp op1 e)
> -- in zig == zag
> -- :}
> -- True

:::



> applyBaseBufferOp
>   :: forall w t d a
>    . ( IsChar a, Ord a, IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a, Eq a )
>   => BaseBufferOp a -> Buffer w t d a -> Buffer w t d a
> applyBaseBufferOp (BaseBufferOp op) buf =
>   applyBufferOp (witnessBufferOp (Proxy :: Proxy d) op) buf








Testing and Debugging
---------------------

As usual, we wrap up this module with some helper code for writing tests. First we need class instances for working with our property testing library.


> 

> 
> instance
>   ( IsBase d, IsChar a, Eq a, Arb a
>   ) => Arb (BufferOp d a)
>   where
>     arb = pickFrom3
>       ( BufferOpIns <$> arb
>       , BufferOpDel <$> arb
>       , pure BufferNoOp
>       )
> 
> instance
>   ( IsBase d, IsChar a, Eq a, Prune a
>   ) => Prune (BufferOp d a)
>   where
>     prune x = case x of
>       BufferNoOp -> []
>       BufferOpIns r -> map BufferOpIns $ prune r
>       BufferOpDel r -> map BufferOpDel $ prune r

Next, recall that buffers need to satisfy some invariants: first of all they are built on finger trees, which have invariants of their own, but moreover the buffer must end with an EOF sigil. We expose a helper to check for this.



We also expose a function that converts a buffer into a list with all the gritty value details exposed.



Finally we give a `Show` instance to match our structure-aware constructors. This is filed with the debugging methods because we won't need it in "production".



> -}
