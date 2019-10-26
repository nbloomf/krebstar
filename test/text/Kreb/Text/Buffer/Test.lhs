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
> import           Kreb.Text.ScreenOffset
> import qualified Kreb.Struct.FingerTree as FT
> import           Kreb.Text.Buffer



> test_Buffer :: TestTree
> test_Buffer =
>   testGroup "Buffers"
>     [ test_Buffer_properties "Char" nat30 nat8 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat30 nat4 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat15 nat2 (Proxy :: Proxy Char)
>     , test_Buffer_properties "Char" nat8 nat2 (Proxy :: Proxy Char)
> 
>     , test_Buffer_properties "Glyph" nat30 nat8 (Proxy :: Proxy Glyph)
>     , test_Buffer_properties "Glyph" nat30 nat4 (Proxy :: Proxy Glyph)
>     , test_Buffer_properties "Glyph" nat15 nat2 (Proxy :: Proxy Glyph)
>     , test_Buffer_properties "Glyph" nat8 nat2 (Proxy :: Proxy Glyph)
>     ]

> test_Buffer_properties
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Eq a, Show a, Arb a, Prune a, IsChar a )
>   => String -> Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_properties label pw pt _ =
>   let title = "Buffer (" ++ label ++ ")"
>   in testGroup title
>     [ testKreb
>         "validate as == True" $
>         \(as :: Buffer w t a) ->
>           claimTrue (validate as)
> 
>     , testKreb
>         "isEmpty empty == True" $
>         let e = empty :: Buffer w t a
>         in claimTrue (isEmpty e)
> 
>     , testKreb
>         "isSingleton (singleton a) == True" $
>         \(a :: a) ->
>           let as = singleton a :: Buffer w t a
>           in claimTrue (isSingleton as)
> 
>     , testKreb
>         "isEmpty (singleton a) == Flase" $
>         \(a :: a) ->
>           let as = singleton a :: Buffer w t a
>           in claimFalse (isEmpty as)
> 
>     , testKreb
>         "movePointToStart (movePointToStart as) == movePointToStart as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (movePointToStart as))
>             (movePointToStart as)
> 
>     , testKreb
>         "movePointToEnd (movePointToEnd as) == movePointToEnd as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToEnd (movePointToEnd as))
>             (movePointToEnd as)
> 
>     , testKreb
>         "moveMarkToStart (moveMarkToStart as) == moveMarkToStart as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (moveMarkToStart (moveMarkToStart as))
>             (moveMarkToStart as)
> 
>     , testKreb
>         "moveMarkToEnd (moveMarkToEnd as) == moveMarkToEnd as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (moveMarkToEnd (moveMarkToEnd as))
>             (moveMarkToEnd as)
> 
>     , testKreb
>         "movePointToStart (moveMarkToStart as) == moveMarkToStart (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (moveMarkToStart as))
>             (moveMarkToStart (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkToEnd as) == moveMarkToEnd (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToStart as) == moveMarkToStart (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToStart as))
>             (moveMarkToStart (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkToEnd as) == moveMarkToEnd (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToEnd (moveMarkToEnd as))
>             (moveMarkToEnd (movePointToEnd as))
> 
>     , testKreb
>         "(isPointAtEnd as) || (as == movePointLeft (movePointRight as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isPointAtEnd as)
>             , claimEqual
>                 (as)
>                 (movePointLeft (movePointRight as))
>             ]
> 
>     , testKreb
>         "(isPointAtStart as) || (as == movePointRight (movePointLeft as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isPointAtStart as)
>             , claimEqual
>                 (as)
>                 (movePointRight (movePointLeft as))
>             ]
> 
>     , testKreb
>         "(isMarkAtEnd as) || (as == moveMarkLeft (moveMarkRight as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isMarkAtEnd as)
>             , claimEqual
>                 (as)
>                 (moveMarkLeft (moveMarkRight as))
>             ]
> 
>     , testKreb
>         "(isMarkAtStart as) || (as == moveMarkRight (moveMarkLeft as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isMarkAtStart as)
>             , claimEqual
>                 (as)
>                 (moveMarkRight (moveMarkLeft as))
>             ]
> 
>     , testKreb
>         "movePointLeft (moveMarkLeft as) == moveMarkLeft (movePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointLeft (moveMarkLeft as))
>             (moveMarkLeft (movePointLeft as))
> 
>     , testKreb
>         "movePointLeft (moveMarkRight as) == moveMarkRight (movePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointLeft (moveMarkRight as))
>             (moveMarkRight (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkLeft as) == moveMarkLeft (movePointRight as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointRight (moveMarkLeft as))
>             (moveMarkLeft (movePointRight as))
> 
>     , testKreb
>         "movePointRight (moveMarkRight as) == moveMarkRight (movePointRight as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointRight (moveMarkRight as))
>             (moveMarkRight (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToStart as) == moveMarkToStart (movePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointLeft (moveMarkToStart as))
>             (moveMarkToStart (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToStart as) == moveMarkToStart (movePointRight as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointRight (moveMarkToStart as))
>             (moveMarkToStart (movePointRight as))
> 
>     , testKreb
>         "movePointLeft (moveMarkToEnd as) == moveMarkToEnd (movePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointLeft (moveMarkToEnd as))
>             (moveMarkToEnd (movePointLeft as))
> 
>     , testKreb
>         "movePointRight (moveMarkToEnd as) == moveMarkToEnd (movePointRight as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointRight (moveMarkToEnd as))
>             (moveMarkToEnd (movePointRight as))
> 
>     , testKreb
>         "movePointToStart (moveMarkLeft as) == moveMarkLeft (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (moveMarkLeft as))
>             (moveMarkLeft (movePointToStart as))
> 
>     , testKreb
>         "movePointToStart (moveMarkRight as) == moveMarkRight (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (moveMarkRight as))
>             (moveMarkRight (movePointToStart as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkLeft as) == moveMarkLeft (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToEnd (moveMarkLeft as))
>             (moveMarkLeft (movePointToEnd as))
> 
>     , testKreb
>         "movePointToEnd (moveMarkRight as) == moveMarkRight (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToEnd (moveMarkRight as))
>             (moveMarkRight (movePointToEnd as))
> 
>     , testKreb
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertAtStart u (insertAtEnd v as))
>                 (insertAtEnd v (insertAtStart u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (deletePointLeft (insertPointLeft u as) == as)" $
>         \(u :: a) (as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (as)
>                 (deletePointLeft (insertPointLeft u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtStart u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtStart u as))
>             ]
> 
>     , testKreb
>         "(isEmpty as) || (readPoint (insertAtEnd u as) == readPoint as)" $
>         \(u :: a) (as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (readPoint as)
>                 (readPoint (insertAtEnd u as))
>             ]
> 
>     , testKreb
>         "Just (a, as) == viewAtStart (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (Just (Cell a, as))
>             (viewAtStart (insertAtStart a as))
> 
>     , testKreb
>         "Just (a, as) == viewAtEnd (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (Just (Cell a, as))
>             (viewAtEnd (insertAtEnd a as))
> 
>     , testKreb
>         "deleteAtStart (insertAtStart u as) == as" $
>         \(u :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (as)
>             (deleteAtStart (insertAtStart u as))
> 
>     , testKreb
>         "deleteAtEnd (insertAtEnd u as) == as" $
>         \(u :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (as)
>             (deleteAtEnd (insertAtEnd u as))
> 
>     , testKreb
>         "deleteAtStart (deleteAtEnd as) == deleteAtEnd (deleteAtStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (deleteAtEnd (deleteAtStart as))
>             (deleteAtStart (deleteAtEnd as))
> 
>     , testKreb
>         "(isEmpty as) || (insertAtStart u (insertAtEnd v as) == insertAtEnd v (insertAtStart u as))" $
>         \(u :: a) (v :: a) (as :: Buffer w t a) ->
>           claimAny
>             [ claimTrue (isEmpty as)
>             , claimEqual
>                 (insertAtStart u (insertAtEnd v as))
>                 (insertAtEnd v (insertAtStart u as))
>             ]
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (insertAtStart a as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertAtStart a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (insertAtStart a as))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (insertAtEnd a as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertAtEnd a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (insertAtEnd a as))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (insertPointLeft a as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (insertPointLeft a as)" $
>         \(a :: a) (as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (insertPointLeft a as))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deleteAtStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (deleteAtStart as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deleteAtStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (deleteAtStart as))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deleteAtEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (deleteAtEnd as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deleteAtEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (deleteAtEnd as))
> 
>     , testKreb
>         "getBufferWidth as == getBufferWidth (deletePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferWidth as)
>             (getBufferWidth (deletePointLeft as))
> 
>     , testKreb
>         "getBufferTabStop as == getBufferTabStop (deletePointLeft as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferTabStop as)
>             (getBufferTabStop (deletePointLeft as))
> 
>     , testKreb
>         "mempty == getPointLineCol (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (mempty)
>             (getPointLineCol (movePointToStart as))
> 
>     , testKreb
>         "getBufferLineCol as == getPointLineCol (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferLineCol as)
>             (getPointLineCol (movePointToEnd as))
> 
>     , testKreb
>         "(hasMark as == False) || (getBufferLineCol as == getMarkLineCol (moveMarkToEnd as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferLineCol as)
>                 (getMarkLineCol (moveMarkToEnd as))
>             ]
> 
>     , testKreb
>         "(0,0) == getPointScreenCoords (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (0,0)
>             (getPointScreenCoords (movePointToStart as))
> 
>     , testKreb
>         "getBufferScreenCoords as == getPointScreenCoords (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (getPointScreenCoords (movePointToEnd as))
> 
>     , testKreb
>         "(hasMark as == False) || (getBufferScreenCoords as == getMarkScreenCoords (moveMarkToEnd as))" $
>         \(as :: Buffer w t a) ->
>           claimAny
>             [ claimFalse (hasMark as)
>             , claimEqual
>                 (getBufferScreenCoords as)
>                 (getMarkScreenCoords (moveMarkToEnd as))
>             ]
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (movePointToStart as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (movePointToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (movePointToStart as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (movePointToEnd as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (movePointToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (movePointToEnd as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (moveMarkToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToStart as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (moveMarkToStart as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToStart as))
> 
>     , testKreb
>         "getBufferByteCount as == getBufferByteCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferByteCount as)
>             (getBufferByteCount (moveMarkToEnd as))
> 
>     , testKreb
>         "getBufferCharCount as == getBufferCharCount (moveMarkToEnd as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferCharCount as)
>             (getBufferCharCount (moveMarkToEnd as))
> 
>     , testKreb
>         "atOrAfterLineCol is monotone on buffers" $
>         \(lc :: LineCol) (as :: Buffer w t a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterLineCol lc m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "atOrAfterScreenCoords is monotone on buffers" $
>         \((NonNegative u, NonNegative v)
>             :: (NonNegative Int, NonNegative Int))
>          (as :: Buffer w t a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenCoords (u,v) m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "atOrAfterScreenLine is monotone on buffers" $
>         \(NonNegative u :: NonNegative Int)
>          (as :: Buffer w t a) ->
>           claimEqual
>             ([])
>             (dropWhile (== True) $ dropWhile (== False)
>               [ atOrAfterScreenLine u m | (_, m) <- toAnnotatedList as])
> 
>     , testKreb
>         "as == toList (fromList as)" $
>         \(xs :: [a]) ->
>           let as = fromList xs :: Buffer w t a
>           in claimEqual (xs) (toList as)
> 
>     , testKreb
>         "movePointToStart (clearMark as) == fromList (toList as)" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (movePointToStart (clearMark as))
>             (fromList (toList as))
> 
>     , testKreb
>         "as == movePointToScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (as)
>             (movePointToScreenCoords (getPointScreenCoords as) as )
> 
>     , testKreb
>         "getPointLineCol then movePointToLineCol" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (as)
>             (movePointToLineCol (getPointLineCol as) as)
> 
>     , testKreb
>         "getPointScreenCoords as == seekScreenCoords (getPointScreenCoords as) as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getPointScreenCoords as)
>             (seekScreenCoords (getPointScreenCoords as) as)
> 
>     , testKreb
>         "(0,0) == seekScreenCoords (0,0) as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (0,0)
>             (seekScreenCoords (0,0) as)
> 
>     , testKreb
>         "getBufferScreenCoords as == seekScreenCoords (getBufferScreenCoords as) as" $
>         \(as :: Buffer w t a) ->
>           claimEqual
>             (getBufferScreenCoords as)
>             (seekScreenCoords (getBufferScreenCoords as) as)
> 
>     , testKreb
>         "movePointToLineCol (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t a
>             zs = fromList (as ++ [x] ++ bs)
>             ws = fromList (as ++ [x])
>             m = value ws :: MeasureText w t
>           in claimEqual
>             (makePointOnlyBuffer pw pt as x bs)
>             (movePointToLineCol (logicalCoords m) zs)
> 
>     , testKreb
>         "movePointToScreenCoords (point only, prepared focus)" $
>         \(as :: [a]) (x :: a) (bs :: [a]) ->
>           let
>             zs, ws :: Buffer w t a
>             zs = fromList (as ++ [x] ++ bs)
>             ws = fromList (as ++ [x])
>             m = value ws :: MeasureText w t
>           in claimEqual
>             (makePointOnlyBuffer pw pt as x bs)
>             (movePointToScreenCoords (applyScreenOffset (screenCoords m) (0,0)) zs)
> 
>     , testKreb
>         "prependFT as (prependFT bs xs) == prependFT (as <> bs) xs" $
>         \(as :: FT.FingerTree (MeasureText w t) (Cell a))
>          (bs :: FT.FingerTree (MeasureText w t) (Cell a))
>          (xs :: Buffer w t a) ->
>           claimEqual
>             (prependFT as (prependFT bs xs))
>             (prependFT (as <> bs) xs)
> 
>     , testKreb
>         "appendFT as (appendFT bs xs) == appendFT (bs <> as) xs" $
>         \(as :: FT.FingerTree (MeasureText w t) (Cell a))
>          (bs :: FT.FingerTree (MeasureText w t) (Cell a))
>          (xs :: Buffer w t a) ->
>           claimEqual
>             (appendFT as (appendFT bs xs))
>             (appendFT (bs <> as) xs)
> 
>     , testKreb
>         "prependFT as (appendFT bs xs) == appendFT bs (prependFT as xs)" $
>         \(as :: FT.FingerTree (MeasureText w t) (Cell a))
>          (bs :: FT.FingerTree (MeasureText w t) (Cell a))
>          (xs :: Buffer w t a) ->
>           claimEqual
>             (prependFT as (appendFT bs xs))
>             (appendFT bs (prependFT as xs))
> 
>     , testKreb
>         "prependFT mempty xs == xs" $
>         \(xs :: Buffer w t a) ->
>           claimEqual
>             (xs)
>             (prependFT mempty xs)
> 
>     , testKreb
>         "appendFT mempty xs == xs" $
>         \(xs :: Buffer w t a) ->
>           claimEqual
>             (xs)
>             (appendFT mempty xs)
> 
>     , testKreb
>         "takeFirstScreenLine is a cat factorization" $
>         \(as :: Buffer w t a) ->
>           let bs = movePointToStart $ clearMark as in
>           claimAny
>             [ claimFalse (hasFullScreenLine bs)
>             , let (u, Just x) = takeFirstScreenLine bs
>               in claimEqual
>                 (bs)
>                 (movePointToStart (prependFT u x))
>             ]
> 
>     , testKreb
>         "hasFullScreenLine empty == False" $
>         claimFalse (hasFullScreenLine (empty :: Buffer w t a))
> 
>     , testKreb
>         "takeScreenLines length bound" $
>         \(NonNegative k :: NonNegative Int)
>          (as :: Buffer w t a) ->
>           claimLEQ
>             (length $ fst $ takeScreenLines k as)
>             (k)
>     ]






> {-

Generators
==========

Generates valid input/output pairs for `renderBuffer`.

> genRenderBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int
>   -> Seeded (Int, Int, Buffer w t a, [Maybe Int], [[a]])
> genRenderBuffer pw pt _ (NonNegative k) = do
>   let
>     w = toWidth pw
>     t = toTab pt
> 
>     brokenLine :: Int -> Seeded [(Maybe Int, [a])]
>     brokenLine n = do
>       u <- randIn (k*w, (k+1)*w - 2)
>       let char = fromChar <$> randIn ('a','z')
>       cs <- vectOf u char
>       let
>         labels = Just n : repeat Nothing
>         chunks = breakLinesAtWidth t w (cs ++ [fromChar '\n'])
>       return $ zip labels chunks
> 
>   NonNegative top <- arb
>   Positive height <- arb
>   NonNegative pad <- arb
> 
>   (labelss, chunkss) <- (unzip . concat) <$>
>     mapM brokenLine [0..(top+height+pad)]
> 
>   let
>     labels = take height $ drop top labelss
>     chunks = take height $ drop top chunkss
> 
>     buf :: Buffer w t a
>     buf = Buf.makeListBuffer $ concat chunkss
> 
>   return (top, height, buf, labels, chunks)
















Rendering
---------

> test_Buffer_renderBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_renderBuffer pw pt pa =
>   testGroup "Rendering"
>     [ testKreb "Prepared 0" $
>         prop_Buffer_renderBuffer_prepared_0 pw pt pa
>     , testKreb "Prepared 1" $
>         prop_Buffer_renderBuffer_prepared_1 pw pt pa
>     , testKreb "Prepared 2" $
>         prop_Buffer_renderBuffer_prepared_2 pw pt pa
>     ]

> prop_Buffer_renderBuffer_prepared_0
>    , prop_Buffer_renderBuffer_prepared_1
>    , prop_Buffer_renderBuffer_prepared_2
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Check
> 
> prop_Buffer_renderBuffer_prepared_0 pw pt pa =
>   forEach (genRenderBuffer pw pt pa (NonNegative 0)) prune $
>     \(top, height, buf, labels, lines) ->
>       claimEqual
>         (labels, lines)
>         (fixRender $ renderBuffer defaultBufferRenderSettings id top height buf)
> 
> prop_Buffer_renderBuffer_prepared_1 pw pt pa =
>   forEach (genRenderBuffer pw pt pa (NonNegative 1)) prune $
>     \(top, height, buf, labels, lines) ->
>       claimEqual
>         (labels, lines)
>         (fixRender $ renderBuffer defaultBufferRenderSettings id top height buf)
> 
> prop_Buffer_renderBuffer_prepared_2 pw pt pa =
>   forEach (genRenderBuffer pw pt pa (NonNegative 2)) prune $
>     \(top, height, buf, labels, lines) ->
>       claimEqual
>         (labels, lines)
>         (fixRender $ renderBuffer defaultBufferRenderSettings id top height buf)






Take Line
---------



> prop_Buffer_takeLines_concat
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int
>   -> FT.FingerTree (MeasureText w t) (Cell a)
>   -> Check
> prop_Buffer_takeLines_concat _ _ _ (NonNegative k) xs =
>   check $
>     let
>       zs = xs <> FT.fromList [eof]
>       (uss,vs) = takeLines k zs
>     in claimEqual zs (mconcat uss <> vs)

> prop_Buffer_takeLine_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> [Cell Char] -> ([Cell Char], [Cell Char])
>   -> Check
> prop_Buffer_takeLine_examples _ _ _ xs (as, bs) =
>   let
>     zs :: FT.FingerTree (MeasureText w t) (Cell a)
>     zs = FT.fromList $ map (fmap fromChar) xs
>   in case takeLine zs of
>     Nothing -> error "prop_Buffer_takeLine_examples: panic"
>     Just (us,vs) ->
>       (claimEqual us (FT.fromList $ map (fmap fromChar) as)) .&&.
>       (claimEqual vs (FT.fromList $ map (fmap fromChar) bs))

> test_Buffer_takeLine_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arb a, Prune a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_takeLine_examples pw pt pa =
>   testGroup "takeLine examples"
>     [ testKreb "#1" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', eof ]
>           ( [ cell 'a', eof ]
>           , []
>           )
> 
>     , testKreb "#2" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell '\n', eof ]
>           ( [ cell '\n' ]
>           , [ eof ]
>           )
> 
>     , testKreb "#3" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ eof ]
>           )
> 
>     , testKreb "#4" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', cell 'b', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ cell 'b', eof ]
>           )
> 
>     , testKreb "#5" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', cell '\n', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ cell '\n', eof ]
>           )
> 
>     , testKreb "#6" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell '\n', cell '\n', eof ]
>           ( [ cell '\n' ]
>           , [ cell '\n', eof ]
>           )
>     ]

> prop_Buffer_takeLines_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int
>   -> [Cell Char] -> ([[Cell Char]], [Cell Char])
>   -> Check
> prop_Buffer_takeLines_examples _ _ _
>   (NonNegative h) xs (uss, vs) =
>   let
>     f :: [Cell Char]
>       -> FT.FingerTree (MeasureText w t) (Cell a)
>     f zs = FT.fromList $ map (fmap fromChar) zs
> 
>     (css, ds) = takeLines h (f xs)
>   in claimAll
>     [ check (length css <= h)
>     , claimEqual css (map f uss)
>     , claimEqual ds (f vs)
>     ]

> test_Buffer_takeLines_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arb a, Prune a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_takeLines_examples pw pt pa =
>   testGroup "takeLines examples"
>     [ testKreb "#1" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 1)
>           [ cell 'a', eof ]
>           ( [ [ cell 'a', eof ]
>             ]
>           , []
>           )
> 
>     , testKreb "#2" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 1)
>           [ cell '\n', eof ]
>           ( [ [ cell '\n' ]
>             ]
>           , [ eof ]
>           )
> 
>     , testKreb "#3" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 2)
>           [ cell '\n', cell 'a', cell '\n', eof ]
>           ( [ [ cell '\n' ]
>             , [ cell 'a', cell '\n' ]
>             ]
>           , [ eof ]
>           )
> 
>     , testKreb "#4" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 2)
>           [ cell '\n', cell '\n', eof ]
>           ( [ [ cell '\n' ]
>             , [ cell '\n' ]
>             ]
>           , [ eof ]
>           )
>     ]

> test_Buffer_takeLines_wrap_examples :: TestTree
> test_Buffer_takeLines_wrap_examples =
>   testGroup "takeLines (wrapping)"
>     [ testKreb "#1" $
>         prop_Buffer_takeLines_examples nat3 nat1 pChar
>           (NonNegative 2)
>           [ cell 'a', cell 'a', cell 'a'
>           , eof ]
>           ( [ [ cell 'a', cell 'a', cell 'a' ]
>             , [ eof ]
>             ]
>           , []
>           )
> 
>     , testKreb "#2" $
>         prop_Buffer_takeLines_examples nat3 nat1 pChar
>           (NonNegative 5)
>           [ cell 'a', cell 'a', cell 'a'
>           , cell 'a', cell 'a', cell 'a'
>           , cell 'a', cell 'a', cell 'a'
>           , cell 'a', cell 'a', cell 'a'
>           , eof ]
>           ( [ [ cell 'a', cell 'a', cell 'a' ]
>             , [ cell 'a', cell 'a', cell 'a' ]
>             , [ cell 'a', cell 'a', cell 'a' ]
>             , [ cell 'a', cell 'a', cell 'a' ]
>             , [ eof ]
>             ]
>           , []
>           )
>     ]

> prop_Buffer_getLineNumbers_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> [Cell Char]
>   -> [[Cell Char]] -> [([Cell Char], Maybe Int)]
>   -> Check
> prop_Buffer_getLineNumbers_examples _ _ _
>   m xs uss =
>   let
>     f :: [Cell Char]
>       -> FT.FingerTree (MeasureText w t) (Cell a)
>     f zs = FT.fromList $ map (fmap fromChar) zs
> 
>     css = getLineNumbers (value $ f m) (map f xs)
>   in claimEqual css (map (\(us, k) -> (f us, k)) uss)
> 
> test_Buffer_getLineNumbers_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arb a, Prune a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_getLineNumbers_examples pw pt pa =
>   testGroup "getLineNumbers examples"
>     [ testKreb "#1" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a' ]
>           ]
>           [ ( [ cell 'a' ], Just 0 )
>           ]
> 
>     , testKreb "#2" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           ]
> 
>     , testKreb "#3" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell '\n' ], Just 1 )
>           ]
> 
>     , testKreb "#4" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell 'a' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testKreb "#5" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a', cell '\n' ]
>           , [ cell 'a' ]
>           ]
>           [ ( [ cell 'a', cell '\n' ], Just 0 )
>           , ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testKreb "#6" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           [ cell 'a', cell '\n' ]
>           [ [ cell 'a' ]
>           ]
>           [ ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testKreb "#7" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           [ cell 'a', cell '\n' ]
>           [ [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 1 )
>           ]
> 
>     , testKreb "#8" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a', cell '\n' ]
>           , [ cell 'a', cell '\n' ]
>           ]
>           [ ( [ cell 'a', cell '\n' ], Just 0 )
>           , ( [ cell 'a', cell '\n' ], Just 1 )
>           ]
> 
>     , testKreb "#9" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell 'a', cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell 'a', cell '\n' ], Just 1 )
>           ]
> 
>     , testKreb "#10" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ eof ]
>           ]
>           [ ( [ eof ], Just 0 )
>           ]
> 
>     , testKreb "#11" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ eof ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ eof ], Just 1 )
>           ]
>     ]







> {-

> These are very old tests; the value of converting them to the current
> test library is debatable but i am too big a coward to simply delete
> them so here we are.

> {- genBufferFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => [Positive Int]
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genBufferFixed1 ks = do
>   let
>     w = toWidth (Proxy :: Proxy w)

>     ln
>       :: (Int, Positive Int)
>       -> Gen ([Maybe Int], [[a]], [a])
>     ln (i, Positive k) = do
>       as <- map fromChar
>         <$> vectorOf k (choose ('a','z'))
>       let
>         (lbs, lns) = unzip $ zip
>           (Just i : repeat Nothing)
>           (chunksOf w as)
>       return (lbs, lns, as)

>   (labelss, liness, css) <-
>     unzip3 <$> (sequence $ map ln $ zip [0..] ks)

>   return
>     ( concat labelss
>     , concat liness
>     , makeListBuffer $ intercalate [fromChar '\n'] css
>     )

> genRenderWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Positive Int
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderWrapFixed1 (Positive h) = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (w+1, w*h)
>   genBufferFixed1 [Positive len] -}













> genBufferFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => [Positive Int]
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genBufferFixed1 ks = do
>   let
>     w = toWidth (Proxy :: Proxy w)

>     ln
>       :: (Int, Positive Int)
>       -> Gen ([Maybe Int], [[a]], [a])
>     ln (i, Positive k) = do
>       as <- map fromChar
>         <$> vectorOf k (choose ('a','z'))
>       let
>         (lbs, lns) = unzip $ zip
>           (Just i : repeat Nothing)
>           (chunksOf w as)
>       return (lbs, lns, as)

>   (labelss, liness, css) <-
>     unzip3 <$> (sequence $ map ln $ zip [0..] ks)

>   return
>     ( concat labelss
>     , concat liness
>     , makeListBuffer $ intercalate [fromChar '\n'] css
>     )

> chunksOf
>   :: Int -> [a] -> [[a]]
> chunksOf k = unfoldr f
>   where
>     f :: [a] -> Maybe ([a], [a])
>     f [] = Nothing
>     f xs = Just $ splitAt k xs








> {-






-- Rendering

> prop_renderBuffer_one_line_no_wrap
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Positive Int -> Check
> prop_renderBuffer_one_line_no_wrap _ _ _ (Positive h) =
>   forAll genRenderNoWrapFixed1 $
>     \(lb, as, buf :: Buffer w t a) ->
>       (lb, as) === fixRender $ renderBuffer defaultBufferRenderSettings id 0 h buf

-- Generate a buffer with a single nonempty, non-wrapping logical line
-- consisting only of width 1 characters.

> genRenderNoWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderNoWrapFixed1 = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (1,w)
>   genBufferFixed1 [Positive len]



> prop_renderBuffer_one_line_wrap
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Positive Int -> Check
> prop_renderBuffer_one_line_wrap _ _ _ (Positive h) =
>   (h > 1) ==>
>   forAll (genRenderWrapFixed1 (Positive h)) $
>     \(lb, as, buf :: Buffer w t a) ->
>       (lb, as) === fixRender $ renderBuffer defaultBufferRenderSettings id 0 h buf

> genRenderWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arb a, Prune a, Eq a, Show a, IsChar a )
>   => Positive Int
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderWrapFixed1 (Positive h) = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (w+1, w*h)
>   genBufferFixed1 [Positive len]


> -}


> -}

> -}
