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
> import           Kreb.Check
> import           Kreb.Reflect
> import           Kreb.Struct.Valued
> import           Kreb.Text.Glyph
> import           Kreb.Text.Cell
> import           Kreb.Text.MeasureText
> import           Kreb.Text.Rune
> import           Kreb.Text.ScreenOffset
> import qualified Kreb.Struct.FingerTree as FT
> import           Kreb.Text.Buffer



> test_Buffer :: TestTree
> test_Buffer =
>   testGroup "Buffers"
>     [ test_Buffer_properties "Char" nat30 nat8 nat10 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat30 nat4 nat10 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat15 nat2 nat10 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat8  nat2 nat10 (Proxy :: Proxy Char)
> 
>     , test_Buffer_properties "Glyph" nat30 nat8 nat10 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat30 nat4 nat10 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat15 nat2 nat10 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat8  nat2 nat10 (Proxy :: Proxy (Glyph Char))
> 
>     , test_Buffer_properties "Char" nat30 nat8 nat100 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat30 nat4 nat100 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat15 nat2 nat100 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat8  nat2 nat100 (Proxy :: Proxy Char)
> 
>     , test_Buffer_properties "Glyph" nat30 nat8 nat100 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat30 nat4 nat100 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat15 nat2 nat100 (Proxy :: Proxy (Glyph Char))
>     , test_Buffer_properties "Glyph" nat8  nat2 nat100 (Proxy :: Proxy (Glyph Char))
>     ]

> test_Buffer_properties
>   :: forall w t d a
>    . ( IsWidth w, IsTab t, IsBase d, Valued (MeasureText w t d) a
>      , Eq a, Ord a, Show a, Arb a, Prune a, IsChar a )
>   => String -> Proxy w -> Proxy t -> Proxy d -> Proxy a
>   -> TestTree
> test_Buffer_properties label pw pt pd _ =
>   let title = "Buffer (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "validate as == True" $
>         \(as :: Buffer w t d a) ->
>           claimTrue (validate as)
> 
>     , testKreb
>         "isEmpty empty == True" $
>         let e = empty :: Buffer w t d a
>         in claimTrue (isEmpty e)
> 
>     , testKreb
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           let as = singleton (EventId 0 "t") a :: Buffer w t d a
>           in claimTrue (isSingleton as)
> 
>     , testKreb
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           let as = singleton (EventId 0 "t") a :: Buffer w t d a
>           in claimFalse (isEmpty as)
> 
>     , testKreb
>         "movePointToStart (movePointToStart as) == movePointToStart as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (movePointToStart as))
>             (movePointToStart as)
> 
>     , testKreb
>         "movePointToEnd (movePointToEnd as) == movePointToEnd as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToEnd (movePointToEnd as))
>             (movePointToEnd as)
> 
>     , testKreb
>         "moveMarkToStart (moveMarkToStart as) == moveMarkToStart as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (moveMarkToStart (moveMarkToStart as))
>             (moveMarkToStart as)
> 
>     , testKreb
>         "moveMarkToEnd (moveMarkToEnd as) == moveMarkToEnd as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (moveMarkToEnd (moveMarkToEnd as))
>             (moveMarkToEnd as)
> 
>     , testKreb
>         "movePointToStart (moveMarkToStart as) == moveMarkToStart (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (moveMarkToStart as))
>             (moveMarkToStart (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkToEnd as) == moveMarkToEnd (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToStart as) == moveMarkToStart (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToStart as))
>             (moveMarkToStart (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToEnd as) == moveMarkToEnd (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToEnd as))
> 
>     , testKreb
>         "(isPointAtEnd as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isPointAtEnd as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , testKreb
>         "(isPointAtStart as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isPointAtStart as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , testKreb
>         "(isMarkAtEnd as) || (as == moveMarkLeft (moveMarkRight as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isMarkAtEnd as)
>             , claimEqual
>                 (as)
>                 (moveMarkLeft (moveMarkRight as))
>             ]
> 
>     , testKreb
>         "(isMarkAtStart as) || (as == moveMarkRight (moveMarkLeft as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isMarkAtStart as)
>             , claimEqual
>                 (as)
>                 (moveMarkRight (moveMarkLeft as))
>             ]
> 
>     , testKreb
>         "movePointLeft (moveMarkLeft as) == moveMarkLeft (movePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointLeft (moveMarkLeft as))
>             (moveMarkLeft (movePointLeft as))
> 
>     , testKreb
>         "movePointLeft (moveMarkRight as) == moveMarkRight (movePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointLeft (moveMarkRight as))
>             (moveMarkRight (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkLeft as) == moveMarkLeft (movePointRight as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointRight (moveMarkLeft as))
>             (moveMarkLeft (movePointRight as))
> 
>     , testKreb
>         "movePointRight (moveMarkRight as) == moveMarkRight (movePointRight as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointRight (moveMarkRight as))
>             (moveMarkRight (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToStart as) == moveMarkToStart (movePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointLeft (moveMarkToStart as))
>             (moveMarkToStart (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToStart as) == moveMarkToStart (movePointRight as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointRight (moveMarkToStart as))
>             (moveMarkToStart (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToEnd as) == moveMarkToEnd (movePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointLeft (moveMarkToEnd as))
>             (moveMarkToEnd (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToEnd as) == moveMarkToEnd (movePointRight as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointRight (moveMarkToEnd as))
>             (moveMarkToEnd (movePointRight as))
> 
>     , testKreb
>         "movePointToStart (moveMarkLeft as) == moveMarkLeft (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (moveMarkLeft as))
>             (moveMarkLeft (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkRight as) == moveMarkRight (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (moveMarkRight as))
>             (moveMarkRight (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkLeft as) == moveMarkLeft (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToEnd (moveMarkLeft as))
>             (moveMarkLeft (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkRight as) == moveMarkRight (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToEnd (moveMarkRight as))
>             (moveMarkRight (movePointToEnd as))
> 
>     , testKreb
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (fst (insertAtStart (EventId 0 "t") u (fst (insertAtEnd (EventId 0 "t") v as))))
>                 (fst (insertAtEnd (EventId 0 "t") v (fst (insertAtStart (EventId 0 "t") u as))))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointLeft (insertPointLeft u as) == as)" $
>         \(u :: a) (as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (fst (deletePointLeft (EventId 0 "t") (fst (insertPointLeft (EventId 0 "t") u as))))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtStart u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (fst (insertAtStart (EventId 0 "t") u as)))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtEnd u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (fst (insertAtEnd (EventId 0 "t") u as)))
>             ]
> 
>     , testKreb
>         "deleteAtStart (insertAtStart u as) == as" $
>         \(u :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (as)
>             (fst (deleteAtStart (EventId 0 "t") (fst (insertAtStart (EventId 0 "t") u as))))
> 
>     , testKreb
>         "deleteAtEnd (insertAtEnd u as) == as" $
>         \(u :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (as)
>             (fst (deleteAtEnd (EventId 0 "t") (fst (insertAtEnd (EventId 0 "t") u as))))
> 
>     , testKreb
>         "deleteAtStart (deleteAtEnd as) == deleteAtEnd (deleteAtStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (fst (deleteAtEnd (EventId 0 "t") (fst (deleteAtStart (EventId 0 "t") as))))
>             (fst (deleteAtStart (EventId 0 "t") (fst (deleteAtEnd (EventId 0 "t") as))))
> 
>     , testKreb
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t d a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (fst (insertAtStart (EventId 0 "t") u (fst (insertAtEnd (EventId 0 "t") v as))))
>                 (fst (insertAtEnd (EventId 0 "t") v (fst (insertAtStart (EventId 0 "t") u as))))
>             ]
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertAtStart (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertAtStart (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertAtEnd (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertAtEnd (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (insertPointLeft (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (insertPointLeft (EventId 0 "t") a as)))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deleteAtStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deleteAtStart (EventId 0 "t") as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deleteAtStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deleteAtStart (EventId 0 "t") as)))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deleteAtEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deleteAtEnd (EventId 0 "t") as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deleteAtEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deleteAtEnd (EventId 0 "t") as)))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deletePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (fst (deletePointLeft (EventId 0 "t") as)))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deletePointLeft as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (fst (deletePointLeft (EventId 0 "t") as)))
> 
>     , testKreb
>         "mempty == getPointLineCol (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (mempty)
>             (getPointLineCol (movePointToStart as))
> 
>     , testKreb
>         "getBufferLineCol as == getPointLineCol (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getPointLineCol (movePointToEnd as))
> 
>     , testKreb
>         "(hasMark as == False) || (getBufferLineCol as == getMarkLineCol (moveMarkToEnd as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferLineCol as)
>                 (getMarkLineCol (moveMarkToEnd as))
>             ]
> 
>     , testKreb
>         "(0,0) == getPointScreenCoords (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (0,0)
>             (getPointScreenCoords (movePointToStart as))
> 
>     , testKreb
>         "getBufferScreenCoords as == getPointScreenCoords (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getPointScreenCoords (movePointToEnd as))
> 
>     , testKreb
>         "(hasMark as == False) || (getBufferScreenCoords as == getMarkScreenCoords (moveMarkToEnd as))" $
>         \(as :: Buffer w t d a) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferScreenCoords as)
>                 (getMarkScreenCoords (moveMarkToEnd as))
>             ]
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (movePointToStart as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (movePointToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (movePointToStart as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (movePointToEnd as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (movePointToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (movePointToEnd as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (moveMarkToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToStart as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (moveMarkToStart as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToStart as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToEnd as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToEnd as))
> 
>     , testKreb
>         "atOrAfterLineCol is monotone on buffers" $
>         \(lc :: LineCol) (as :: Buffer w t d a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterLineCol lc m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "atOrAfterScreenCoords is monotone on buffers" $
>         \((NonNegative u, NonNegative v)
>             :: (NonNegative Int, NonNegative Int))
>          (as :: Buffer w t d a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenCoords (u,v) m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "atOrAfterScreenLine is monotone on buffers" $
>         \(NonNegative u :: NonNegative Int)
>          (as :: Buffer w t d a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenLine u m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "as == toList (fromList as)" $
>         \(xs :: [a]) ->
>           let as = fromList (EventId 0 "t") xs :: Buffer w t d a
>           in claimEqual (xs) (toList as)
> 
>     , testKreb
>         "movePointToStart (clearMark as) == fromList (toList as)" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (movePointToStart (clearMark as))
>             (fromList (EventId 0 "t") (toList as))
> 
>     , testKreb
>         "as == movePointToScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (as)
>             (movePointToScreenCoords (getPointScreenCoords as) as )
> 
>     , testKreb
>         "getPointLineCol then movePointToLineCol" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (as)
>             (movePointToLineCol (getPointLineCol as) as)
> 
>     , testKreb
>         "getPointScreenCoords as == seekScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getPointScreenCoords as)
>             (seekScreenCoords (getPointScreenCoords as) as)
> 
>     , testKreb
>         "(0,0) == seekScreenCoords (0,0) as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (0,0)
>             (seekScreenCoords (0,0) as)
> 
>     , testKreb
>         "getBufferScreenCoords as == seekScreenCoords (getBufferScreenCoords as) as" $
>         \(as :: Buffer w t d a) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (seekScreenCoords (getBufferScreenCoords as) as)
> 
>     , testKreb
>         "movePointToLineCol (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t d a
>             zs = fromList (EventId 0 "t") (as ++ [x] ++ bs)
>             ws = fromList (EventId 0 "t") (as ++ [x])
>             m = value ws :: MeasureText w t d
>           in claimEqual
>             (makePointOnlyBuffer pw pt pd (EventId 0 "t") as x bs)
>             (movePointToLineCol (logicalCoords m) zs)
> 
>     , testKreb
>         "movePointToScreenCoords (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t d a
>             zs = fromList (EventId 0 "t") (as ++ [x] ++ bs)
>             ws = fromList (EventId 0 "t") as
>             m = value ws :: MeasureText w t d
>           in claimEqual
>             (makePointOnlyBuffer pw pt pd (EventId 0 "t") as x bs)
>             (movePointToScreenCoords (applyScreenOffset (screenCoords m) (0,0)) zs)
> 
>     , testKreb
>         "prepend as (prepend bs xs) == prepend (as <> bs) xs" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d a) ->
>           claimEqual
>             (fst (prepend (EventId 0 "t") as (fst (prepend (EventId 0 "t") bs xs))))
>             (fst (prepend (EventId 0 "t") (as <> bs) xs))
> 
>     , testKreb
>         "append as (append bs xs) == append (bs <> as) xs" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d a) ->
>           claimEqual
>             (fst (append (EventId 0 "t") as (fst (append (EventId 0 "t") bs xs))))
>             (fst (append (EventId 0 "t") (bs <> as) xs))
> 
>     , testKreb
>         "prepend as (append bs xs) == append bs (prepend as xs)" $
>         \(as :: [a])
>          (bs :: [a])
>          (xs :: Buffer w t d a) ->
>           claimEqual
>             (fst (prepend (EventId 0 "t") as (fst (append (EventId 0 "t") bs xs))))
>             (fst (append (EventId 0 "t") bs (fst (prepend (EventId 0 "t") as xs))))
> 
>     , testKreb
>         "prepend mempty xs == xs" $
>         \(xs :: Buffer w t d a) ->
>           claimEqual
>             (xs)
>             (fst (prepend (EventId 0 "t") [] xs))
> 
>     , testKreb
>         "append mempty xs == xs" $
>         \(xs :: Buffer w t d a) ->
>           claimEqual
>             (xs)
>             (fst (append (EventId 0 "t") [] xs))
> 
>     , testKreb
>         "applyBufferOp u . applyBufferOp u == applyBufferOp u" $
>         \(xs :: Buffer w t d a) (u :: BufferOp d a) ->
>           claimEqual
>             (applyBufferOp u xs)
>             (applyBufferOp u (applyBufferOp u xs))
> 
>     , testKreb
>         "applyBufferOp u . applyBufferOp v == applyBufferOp v . applyBufferOp u" $
>         \(xs :: Buffer w t d a) (u :: BufferOp d a) (v :: BufferOp d a) ->
>           claimEqual
>             (applyBufferOp u (applyBufferOp v xs))
>             (applyBufferOp v (applyBufferOp u xs))
> 
>   {-  , testKreb
>         "takeFirstScreenLine is a cat factorization" $
>         \(as :: Buffer w t d a) ->
>           let bs = movePointToStart $ clearMark as in
>           claimAny
>             [ claimFalse (hasFullScreenLine bs)
>             , let (u, Just x) = takeFirstScreenLine bs
>               in claimEqual
>                 (bs)
>                 (movePointToStart (fst (prepend (EventId 0 "t") (Fold.toList u) x)))
>             ] -}
> 
>     , testKreb
>         "hasFullScreenLine empty == False" $
>         claimFalse (hasFullScreenLine (empty :: Buffer w t d a))
> 
>     , testKreb
>         "takeScreenLines length bound" $
>         \(NonNegative k :: NonNegative Int)
>          (as :: Buffer w t d a) ->
>           claimLEQ
>             (length $ fst $ takeScreenLines k as)
>             (k)
> 
>     , testKreb
>         "takeFirstScreenLine EOF property" $
>         \(as :: Buffer w t d a) ->
>           let (u, v) = takeFirstScreenLine as in
>           claimEqual
>             (v == Nothing)
>             (elem EOF $ Fold.toList u)
>     ]
