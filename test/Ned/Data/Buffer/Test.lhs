---
title: Ned.Data.Buffer.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
* [Tests](#tests)
    * [Navigation properties](#navigation-properties)
    * [Test Suite](#test-suite)



Introduction
============

> {-# LANGUAGE
>     UndecidableInstances
>   , ScopedTypeVariables
>   , FlexibleContexts
> #-}
> 
> module Ned.Data.Buffer.Test (
>     test_Buffer
> ) where
> 
> import Data.Proxy
> import Data.List
> import System.Environment
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Struct.FingerTree
> import Kreb.Struct.FingerTreeZip
> import Kreb.Reflect.Nat
> import Ned.Data.ScreenOffset
> import Ned.Data.MeasureText
> import Ned.Data.Buffer
> import Ned.Data.Glyph
> 
> import Kreb.Check
> 
> import Kreb.Struct.FingerTree.Test
> import Kreb.Struct.FingerTreeZip.Test
> import Ned.Data.ScreenOffset.Test
> import Ned.Data.MeasureText.Test
> import Ned.Data.Glyph.Test



Generators
==========

> instance
>   ( Arbitrary a, IsChar a
>   ) => Arbitrary (Cell a)
>   where
>     arbitrary = cell <$> arbitrary

> instance
>   ( IsWidth w, IsTab t, IsChar a, Eq a
>   , Arbitrary a, Valued (MeasureText w t) a
>   ) => Arbitrary (Buffer w t a)
>   where
>     arbitrary = mkTapeFocus
>       <$> arbitrary
>       <*> arbitrary
>       <*> arbitrary
> 
>     shrink buf =
>       case unBufferFocus buf of
>         Nothing -> []
>         Just (as, x, bs) -> mkEmptyBuffer :
>           [ mkBufferFocus'' as' x bs'
>             | as' <- shrink as, bs' <- shrink bs ]

Generates valid input/output pairs for `renderBuffer`.

> genRenderBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int
>   -> Gen (Int, Int, Buffer w t a, [Maybe Int], [[a]])
> genRenderBuffer pw pt _ (NonNegative k) = do
>   let
>     w = toWidth pw
>     t = toTab pt
> 
>     brokenLine :: Int -> Gen [(Maybe Int, [a])]
>     brokenLine n = do
>       u <- choose (k*w, (k+1)*w - 2)
>       let char = fromChar <$> choose ('a','z')
>       cs <- vectorOf u char
>       let
>         labels = Just n : repeat Nothing
>         chunks = breakLinesAtWidth t w (cs ++ [fromChar '\n'])
>       return $ zip labels chunks
> 
>   NonNegative top <- arbitrary
>   Positive height <- arbitrary
>   NonNegative pad <- arbitrary
> 
>   (labelss, chunkss) <- (unzip . concat) <$>
>     mapM brokenLine [0..(top+height+pad)]
> 
>   let
>     labels = take height $ drop top labelss
>     chunks = take height $ drop top chunkss
> 
>     buf :: Buffer w t a
>     buf = mkTape $ concat chunkss
> 
>   return (top, height, buf, labels, chunks)

> {- genBufferFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
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
>     , mkBuffer $ intercalate [fromChar '\n'] css
>     )

> genRenderWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Positive Int
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderWrapFixed1 (Positive h) = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (w+1, w*h)
>   genBufferFixed1 [Positive len] -}



Tests
=====

> test_Buffer_properties
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_properties pw pt pa =
>   testGroup "Buffer"
>     [ test_Buffer_basics pw pt pa
>     , test_Buffer_nav pw pt pa
>     , test_Buffer_split pw pt pa
>     , localOption (QuickCheckTests 500) $
>         test_Buffer_renderBuffer pw pt pa
>     ]



Basics
------

> test_Buffer_basics
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_basics pw pt pa =
>   testGroup "Basics"
>     [ test_Buffer_validate pw pt pa
>     , test_Buffer_debug pw pt pa
>     , test_Buffer_atLineCol_monotone pw pt pa
>     , localOption (QuickCheckTests 1000) $
>         test_Buffer_atScreenCoords_monotone pw pt pa
>     , localOption (QuickCheckTests 1000) $
>         test_Buffer_atScreenLine_monotone pw pt pa
>     ]

> prop_Buffer_validate
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> Property
> 
> prop_Buffer_validate _ _ _ buf =
>   property $ validateBuffer buf
> 
> test_Buffer_validate
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_validate pw pt pa =
>   testProperty "Validate" $
>     prop_Buffer_validate pw pt pa

> prop_Buffer_debug
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> [(Cell Char, MeasureText w t)]
>   -> Property
> 
> prop_Buffer_debug _ _ _ buf zs =
>   let ws = map (\(c, m) -> (fmap fromChar c, m)) zs in
>   ws === toListDebugBuffer buf
> 
> test_Buffer_debug
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_debug pw pt pa =
>   testGroup "Debug examples"
>     [ testCases
>       (uncurry $ prop_Buffer_debug pw pt pa)
>       [ ( "#1"
>         , ( charBuffer pw pt
>               "a"
>           , [ ( cell 'a'
>               , mText 1 1 (0,0) (0,1) False False
>                   ( mkNoNewlines [] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             , ( eof
>               , mText 1 1 (0,1) (0,1) True False
>                   ( mkNoNewlines [(1,Fixed1)] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             ]
>           )
>         )
> 
>       , ( "#2"
>         , ( charBuffer pw pt
>               "abc\n"
>           , [ ( cell 'a'
>               , mText 1 1 (0,0) (0,1) False False
>                   ( mkNoNewlines [] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             , ( cell 'b'
>               , mText 2 2 (0,1) (0,1) False False
>                   ( mkNoNewlines [(1,Fixed1)] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             , ( cell 'c'
>               , mText 3 3 (0,2) (0,1) False False
>                   ( mkNoNewlines [(2,Fixed1)] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             , ( cell '\n'
>               , mText 4 4 (0,3) (1,0) False True
>                   ( mkNoNewlines [(3,Fixed1)] )
>                   ( mkWithNewlines [] 1 [] ))
>             , ( eof
>               , mText 4 4 (1,0) (0,1) True False
>                   ( mkWithNewlines [(3,Fixed1)] 1 [] )
>                   ( mkNoNewlines [(1,Fixed1)] ))
>             ]
>           )
>         )
>       ]
>     ]

> prop_Buffer_atLineCol_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> LineCol
>   -> Property
> 
> prop_Buffer_atLineCol_monotone _ _ _ buf w =
>   property $
>     and $ dropWhile (== False) $
>       map (atLineCol w . snd) $
>       toListDebugBuffer buf
> 
> test_Buffer_atLineCol_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_atLineCol_monotone pw pt pa =
>   testProperty "atLineCol is monotone on buffers" $
>     prop_Buffer_atLineCol_monotone pw pt pa

> prop_Buffer_atScreenCoords_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> (NonNegative Int, NonNegative Int)
>   -> Property
> 
> prop_Buffer_atScreenCoords_monotone _ _ _ buf (NonNegative u, NonNegative v) =
>   property $
>     and $ dropWhile (== False) $
>       map (atScreenCoords (u,v) . snd) $
>       toListDebugBuffer buf
> 
> test_Buffer_atScreenCoords_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_atScreenCoords_monotone pw pt pa =
>   testProperty "atScreenCoords is monotone on buffers" $
>     prop_Buffer_atScreenCoords_monotone pw pt pa

> prop_Buffer_atScreenLine_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> NonNegative Int
>   -> Property
> 
> prop_Buffer_atScreenLine_monotone _ _ _
>   buf (NonNegative u) =
>   property $
>     and $ dropWhile (== False) $
>       map (atScreenLine u . snd) $
>       toListDebugBuffer buf
> 
> test_Buffer_atScreenLine_monotone
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_atScreenLine_monotone pw pt pa =
>   testProperty "atScreenLine is monotone on buffers" $
>     prop_Buffer_atScreenLine_monotone pw pt pa



Navigation properties
---------------------

> test_Buffer_nav
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_nav pw pt pa =
>   testGroup "Basic navigation"
>     [ testProperty "initMove; getBufferHeadLineCol" $
>         prop_Buffer_getBufferHeadLineCol_initMove pw pt pa
>     , testProperty "bufferHeadLineCol; splitBufferAtLineCol" $
>         prop_Buffer_getBufferHeadLineCol_splitBufferAtLineCol pw pt pa
>     , testProperty "bufferHeadLineCol headMoveAfter" $
>         cprop_Buffer_getBufferHeadLineCol_headMoveAfter pw pt pa
>     ]

Steps:

    1. Move the read head to the beginning
    2. Get the read head line and column

Expect:

    1. We're at line 0, column 0

> prop_Buffer_getBufferHeadLineCol_initMove
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, Show a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a -> Property
> prop_Buffer_getBufferHeadLineCol_initMove _ _ _ xs =
>   mempty === getBufferHeadLineCol (initMove xs)

Steps:

    1. Get the read head line and column
    2. Move the read head to that line and column

Expect:

    1. The buffer did not change

> prop_Buffer_getBufferHeadLineCol_splitBufferAtLineCol
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a -> Property
> prop_Buffer_getBufferHeadLineCol_splitBufferAtLineCol _ _ _ xs =
>   xs === splitBufferAtLineCol (getBufferHeadLineCol xs) xs

Preconditions:

    1. Buffer is not empty and head is not at the end

Steps:

    1. Get the read head line and column
    2. Move right one step and get the read head position

Expect:

    1. New line and column is old line and column <> head value

> prop_Buffer_getBufferHeadLineCol_headMoveAfter
>    , cprop_Buffer_getBufferHeadLineCol_headMoveAfter
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a, IsChar a, Eq a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a
>   -> Property
> 
> prop_Buffer_getBufferHeadLineCol_headMoveAfter _ _ _ buf =
>   if (isEmpty buf) || (isAtLast buf)
>     then property True
>     else
>       let
>         Just a = headRead buf
>         m = value a :: MeasureText w t
>       in
>         getBufferHeadLineCol (headMoveR buf)
>          === (getBufferHeadLineCol buf) <> (logicalOffset m)
> 
> cprop_Buffer_getBufferHeadLineCol_headMoveAfter pw pt pa buf =
>   cover 30 (not $ isEmpty buf) "buf not empty" $
>   cover 30 (not $ isAtLast buf) "head not at last" $
>   prop_Buffer_getBufferHeadLineCol_headMoveAfter pw pt pa buf



Splitting properties
--------------------

> test_Buffer_split
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_split pw pt pa =
>   testGroup "Splitting"
>     [ test_Buffer_splitBufferAtScreenLine_examples pw pt pa
>     , test_Buffer_atScreenLine_examples pw pt pa
>     , test_Buffer_splitBufferAtScreenCoords_examples pw pt pa
>     , localOption (QuickCheckTests 1000)
>         $ test_Buffer_splitBufferAtLineCol_focus pw pt pa
>     , localOption (QuickCheckTests 1000)
>         $ test_Buffer_splitBufferAtScreenCoords_focus pw pt pa
>     ]

Examples for checking our intuition about screen line splitting:

> prop_Buffer_splitBufferAtScreenLine_examples
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int -> Buffer w t a -> Maybe (Buffer w t a)
>   -> Property
> prop_Buffer_splitBufferAtScreenLine_examples _ _ _
>   (NonNegative k) buf1 buf2 =
>   buf2 === splitBufferAtScreenLine k buf1
> 
> test_Buffer_splitBufferAtScreenLine_examples
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_splitBufferAtScreenLine_examples pw pt pa =
>   testGroup "Examples"
>     [ testCases
>       (uncurry3 $ prop_Buffer_splitBufferAtScreenLine_examples pw pt pa)
>       [ ( "Line 0"
>         , ( NonNegative 0
>           , charBufferFocus pw pt pa
>               "abc\nde" 'f' "\nghi"
>           , Just $ charBufferFocus pw pt pa
>               "" 'a' "bc\ndef\nghi"
>           )
>         )
> 
>       , ( "Line 1"
>         , ( NonNegative 1
>           , charBufferFocus pw pt pa
>               "abc\nde\nf" 'g' "h"
>           , Just $ charBufferFocus pw pt pa
>               "abc\n" 'd' "e\nfgh"
>           )
>         )
> 
>       , ( "Line 2"
>         , ( NonNegative 2
>           , charBufferFocus pw pt pa
>               "abc\nde\nf" 'g' "h"
>           , Just $ charBufferFocus pw pt pa
>               "abc\nde\n" 'f' "gh"
>           )
>         )
> 
>       , ( "Past end (char)"
>         , ( NonNegative 1
>           , defBufferFocus'' pw pt
>               [] (cell 'a') [eof]
>           , Nothing
>           )
>         )
> 
>       , ( "Past end (newline)"
>         , ( NonNegative 1
>           , defBufferFocus'' pw pt
>               [] (cell '\n') [eof]
>           , Just $ defBufferFocus'' pw pt
>               [cell '\n'] eof []
>           )
>         )
> 
>       , ( "At end after newline"
>         , ( NonNegative 1
>           , defBufferFocus'' pw pt
>               [cell 'a'] (cell '\n') [eof]
>           , Just $ defBufferFocus'' pw pt
>               [cell 'a', cell '\n'] (eof) []
>           )
>         )
> 
>       , ( "Past end (char)"
>         , ( NonNegative 2
>           , defBufferFocus'' pw pt
>               [] (cell 'a') [eof]
>           , Nothing
>           )
>         )
> 
>       , ( "Past end (newline)"
>         , ( NonNegative 2
>           , defBufferFocus'' pw pt
>               [] (cell '\n') [eof]
>           , Nothing
>           )
>         )
> 
>       , ( "Focus off EOF (1)"
>         , ( NonNegative 1
>           , defBufferFocus'' pw pt
>               [cell '\n'] (cell '\n') [eof]
>           , Just $ defBufferFocus'' pw pt
>               [cell '\n'] (cell '\n') [eof]
>           )
>         )
> 
>       , ( "Focus on EOF (1)"
>         , ( NonNegative 1
>           , defBufferFocus'' pw pt
>               [cell '\n', cell '\n'] (eof) []
>           , Just $ defBufferFocus'' pw pt
>               [cell '\n'] (cell '\n') [eof]
>           )
>         )
> 
>       , ( "Focus off EOF (2)"
>         , ( NonNegative 2
>           , defBufferFocus'' pw pt
>               [cell '\n', cell '\n'] (cell '\n') [eof]
>           , Just $ defBufferFocus'' pw pt
>               [cell '\n', cell '\n'] (cell '\n') [eof]
>           )
>         )
> 
>       , ( "Focus on EOF (2)"
>         , ( NonNegative 2
>           , defBufferFocus'' pw pt
>               [cell '\n', cell '\n', cell '\n'] (eof) []
>           , Just $ defBufferFocus'' pw pt
>               [cell '\n', cell '\n'] (cell '\n') [eof]
>           )
>         )
>       ]
>     ]

> prop_Buffer_atScreenLine_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int -> Buffer w t a -> [(Cell a, Bool)]
>   -> Property
> prop_Buffer_atScreenLine_examples _ _ _
>   (NonNegative k) xs ys =
>   let
>     zs = map (\(a,b) -> (a, atScreenLine k b))
>           $ toListDebugBuffer xs
>   in zs === ys

> test_Buffer_atScreenLine_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_atScreenLine_examples pw pt pa =
>   testGroup "atScreenLine examples"
>     [ testProperty "#1" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 0)
>           ( defBufferFocus'' pw pt
>             [ cell 'a' ] (eof) []
>           )
>           [ ( cell 'a', True )
>           , ( eof,      True )
>           ]
> 
>     , testProperty "#2" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 0)
>           ( defBufferFocus'' pw pt
>             [ cell '\n' ] (eof) []
>           )
>           [ ( cell '\n', True )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#3" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 0)
>           ( defBufferFocus'' pw pt
>             [ cell 'a', cell '\n' ] (eof) []
>           )
>           [ ( cell 'a',  True )
>           , ( cell '\n', True )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#4" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell 'a' ] (eof) []
>           )
>           [ ( cell 'a', False )
>           , ( eof,      False )
>           ]
> 
>     , testProperty "#5" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell '\n' ] (eof) []
>           )
>           [ ( cell '\n', False )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#6" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell 'a', cell '\n' ] (eof) []
>           )
>           [ ( cell 'a',  False )
>           , ( cell '\n', False )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#7" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell 'a', cell '\n', cell 'a' ] (eof) []
>           )
>           [ ( cell 'a',  False )
>           , ( cell '\n', False )
>           , ( cell 'a',  True )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#8" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell 'a', cell '\n', cell '\n' ] (eof) []
>           )
>           [ ( cell 'a',  False )
>           , ( cell '\n', False )
>           , ( cell '\n', True )
>           , ( eof,       True )
>           ]
> 
>     , testProperty "#9" $
>         prop_Buffer_atScreenLine_examples pw pt pa
>           (NonNegative 1)
>           ( defBufferFocus'' pw pt
>             [ cell '\n', cell '\n', cell '\n' ] (eof) []
>           )
>           [ ( cell '\n', False )
>           , ( cell '\n', True )
>           , ( cell '\n', True )
>           , ( eof,       True )
>           ]
>     ]

Examples for checking our intuition about screen coordinate splitting:

> prop_Buffer_splitBufferAtScreenCoords_examples
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> (NonNegative Int, NonNegative Int)
>   -> Buffer w t a -> Buffer w t a
>   -> Property
> prop_Buffer_splitBufferAtScreenCoords_examples _ _ _
>   (NonNegative u, NonNegative v) buf1 buf2 =
>   buf2 === splitBufferAtScreenCoords (u,v) buf1
> 
> test_Buffer_splitBufferAtScreenCoords_examples
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_splitBufferAtScreenCoords_examples pw pt pa =
>   testGroup "Examples"
>     [ testCases
>       (uncurry3 $ prop_Buffer_splitBufferAtScreenCoords_examples pw pt pa)
>       [ ( "(0,0)"
>         , ( (NonNegative 0, NonNegative 0)
>           , charBufferFocus pw pt pa
>               "abc\nde" 'f' "\nghi"
>           , charBufferFocus pw pt pa
>               "" 'a' "bc\ndef\nghi"
>           )
>         )
> 
>       , ( "(1,1)"
>         , ( (NonNegative 1, NonNegative 1)
>           , charBufferFocus pw pt pa
>               "abc\nde\nf" 'g' "h"
>           , charBufferFocus pw pt pa
>               "abc\nd" 'e' "\nfgh"
>           )
>         )
> 
>       , ( "Coords DNE"
>         , ( (NonNegative 5, NonNegative 1)
>           , charBufferFocus pw pt pa
>               "abc\nde\nf" 'g' "h"
>           , charBufferFocus pw pt pa
>               "abc\nde" '\n' "fgh"
>           )
>         )
>       ]
>     ]

> prop_Buffer_splitBufferAtLineCol_focus
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Buffer w t a -> a -> Buffer w t a
>   -> Property
> 
> prop_Buffer_splitBufferAtLineCol_focus _ _ _ as x bs =
>   let
>     m = value as <> value x :: MeasureText w t
>     z = mkBufferFocus' as x bs
>   in
>     z === (splitBufferAtLineCol (logicalCoords m) z)
> 
> test_Buffer_splitBufferAtLineCol_focus
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_splitBufferAtLineCol_focus pw pt pa =
>   testProperty "splitBufferAtLineCol (prepared focus)" $
>     prop_Buffer_splitBufferAtLineCol_focus pw pt pa

> prop_Buffer_splitBufferAtScreenCoords_focus
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> [a] -> a -> [a]
>   -> Property
> 
> prop_Buffer_splitBufferAtScreenCoords_focus pw pt _ as x bs =
>   let
>     w = Buffer $ mkTapeFTZ $ map (cell . fromChar . toChar) as :: Buffer w t a
>     m = value w :: MeasureText w t
>     pos = applyScreenOffset (screenCoords m <> screenOffset m) (0,0)
>     z1 = defBufferFocus pw pt as x bs
>     z2 = splitBufferAtScreenCoords pos z1
>   in
>     if validateBuffer z2
>       then z1 === z2
>       else error "z2 does not validate!"
> 
> test_Buffer_splitBufferAtScreenCoords_focus
>   :: ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Show a, Eq a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_splitBufferAtScreenCoords_focus pw pt pa =
>   testProperty "splitBufferAtScreenCoords (prepared focus)" $
>     prop_Buffer_splitBufferAtScreenCoords_focus pw pt pa



Examples
--------

> prop_Buffer_validateBuffer_examples
>   :: ( IsWidth w, IsTab t )
>   => Buffer w t Char
>   -> Property
> 
> prop_Buffer_validateBuffer_examples x =
>   property $ validateBuffer x
> 
> test_Buffer_validateBuffer_examples :: TestTree
> test_Buffer_validateBuffer_examples =
>   testGroup "Validate"
>     [ testProperty "#1" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "" 'a' "aaaaaaaaaaaaaaaaaa"
> 
>     , testProperty "#2" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "a" 'a' "aaaaaaaaaaaaaaaaa"
> 
>     , testProperty "#3" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aa" 'a' "aaaaaaaaaaaaaaaa"
> 
>     , testProperty "#4" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aaa" 'a' "aaaaaaaaaaaaaaa"
> 
>     , testProperty "#5" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aaaa" 'a' "aaaaaaaaaaaaaa"
> 
>     , testProperty "#6" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aaaaa" 'a' "aaaaaaaaaaaaa"
> 
>     , testProperty "#7" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aaaaaa" 'a' "aaaaaaaaaaaa"
> 
>     , testProperty "#8" $
>         prop_Buffer_validateBuffer_examples $
>           defBufferFocus nat3 nat1
>             "aaaaaaa" 'a' "aaaaaaaaaaa"
>     ]

> prop_Buffer_splitAtScreenCoords_examples
>   :: ( IsWidth w, IsTab t )
>   => ((Int, Int), Buffer w t Char, Buffer w t Char)
>   -> Property
> prop_Buffer_splitAtScreenCoords_examples (pos, buf1, buf2) =
>   buf2 === splitBufferAtScreenCoords pos buf1
> 
> test_Buffer_splitAtScreenCoords_examples :: TestTree
> test_Buffer_splitAtScreenCoords_examples =
>   testGroup "splitAtScreenCoords examples"
>     [ testProperty "#1" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (0,0)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "" 'a' "aaaaaaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#2" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (1,0)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "a" 'a' "aaaaaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#3" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (2,0)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aa" 'a' "aaaaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#4" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (3,0)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aa" 'a' "aaaaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#5" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (0,1)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aaa" 'a' "aaaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#6" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (1,1)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aaaa" 'a' "aaaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#7" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (2,1)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aaaaa" 'a' "aaaaaaaaaaaaa"
>           )
> 
>     , testProperty "#8" $
>         prop_Buffer_splitAtScreenCoords_examples
>           ( (0,2)
>           , defBuffer nat3 nat1 "aaaaaaaaaaaaaaaaaaa"
>           , defBufferFocus nat3 nat1
>               "aaaaaa" 'a' "aaaaaaaaaaaa"
>           )
>     ]

> prop_Buffer_screenCoords_value_examples
>   :: forall w t
>    . ( IsWidth w, IsTab t )
>   => (Buffer w t Char, (Int, Int))
>   -> Property
> prop_Buffer_screenCoords_value_examples (buf1, pos) =
>   let offset = screenCoords (value buf1 :: MeasureText w t)
>   in pos === (applyScreenOffset offset (0,0))
> 
> test_Buffer_screenCoords_value_examples :: TestTree
> test_Buffer_screenCoords_value_examples =
>   testGroup "Buffer value examples"
>     [ testProperty "#1" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "a"
>           , (1,0)
>           )
> 
>     , testProperty "#2" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aa"
>           , (2,0)
>           )
> 
>     , testProperty "#3" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaa"
>           , (0,1)
>           )
> 
>     , testProperty "#4" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaaa"
>           , (1,1)
>           )
> 
>     , testProperty "#5" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaaaa"
>           , (2,1)
>           )
> 
>     , testProperty "#6" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaaaaa"
>           , (0,2)
>           )
> 
>     , testProperty "#7" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaaaaaa"
>           , (1,2)
>           )
> 
>     , testProperty "#8" $
>         prop_Buffer_screenCoords_value_examples
>           ( defBuffer nat3 nat1 "aaaaaaaa"
>           , (2,2)
>           )
>     ]



Rendering
---------

> test_Buffer_renderBuffer
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_renderBuffer pw pt pa =
>   testGroup "Rendering"
>     [ testProperty "Prepared 0" $
>         prop_Buffer_renderBuffer_prepared_0 pw pt pa
>     , testProperty "Prepared 1" $
>         prop_Buffer_renderBuffer_prepared_1 pw pt pa
>     , testProperty "Prepared 2" $
>         prop_Buffer_renderBuffer_prepared_2 pw pt pa
>     ]

> prop_Buffer_renderBuffer_prepared_0
>    , prop_Buffer_renderBuffer_prepared_1
>    , prop_Buffer_renderBuffer_prepared_2
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Property
> 
> prop_Buffer_renderBuffer_prepared_0 pw pt pa =
>   forAll (genRenderBuffer pw pt pa (NonNegative 0)) $
>     \(top, height, buf, labels, lines) ->
>       (labels, lines)
>         === renderBuffer defaultBufferRenderSettings top height buf
> 
> prop_Buffer_renderBuffer_prepared_1 pw pt pa =
>   forAll (genRenderBuffer pw pt pa (NonNegative 1)) $
>     \(top, height, buf, labels, lines) ->
>       (labels, lines)
>         === renderBuffer defaultBufferRenderSettings top height buf
> 
> prop_Buffer_renderBuffer_prepared_2 pw pt pa =
>   forAll (genRenderBuffer pw pt pa (NonNegative 2)) $
>     \(top, height, buf, labels, lines) ->
>       (labels, lines)
>         === renderBuffer defaultBufferRenderSettings top height buf



Take Line
---------

> test_Buffer_takeLine
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arbitrary a )
>   => Proxy w -> Proxy t -> Proxy a -> TestTree
> test_Buffer_takeLine pw pt pa =
>   testGroup "Take Line"
>     [ testProperty "takeLine concat"
>         $ prop_Buffer_takeLine_concat pw pt pa
>     , testProperty "takeLines concat"
>         $ prop_Buffer_takeLines_concat pw pt pa
>     , test_Buffer_takeLine_examples pw pt pa
>     , test_Buffer_takeLines_examples pw pt pa
>     , test_Buffer_getLineNumbers_examples pw pt pa
>     ]

> prop_Buffer_takeLine_concat
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> FingerTree (MeasureText w t) (Cell a)
>   -> Property
> prop_Buffer_takeLine_concat _ _ _ xs =
>   property $
>     let zs = xs <> fromListFT [eof] in
>     case takeLine zs of
>       Nothing -> xs === mempty
>       Just (as,bs) -> zs === (as <> bs)

> prop_Buffer_takeLines_concat
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> NonNegative Int
>   -> FingerTree (MeasureText w t) (Cell a)
>   -> Property
> prop_Buffer_takeLines_concat _ _ _ (NonNegative k) xs =
>   property $
>     let
>       zs = xs <> fromListFT [eof]
>       (uss,vs) = takeLines k zs
>     in zs === (mconcat uss <> vs)

> prop_Buffer_takeLine_examples
>   :: forall w t a
>    . ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> [Cell Char] -> ([Cell Char], [Cell Char])
>   -> Property
> prop_Buffer_takeLine_examples _ _ _ xs (as, bs) =
>   let
>     zs :: FingerTree (MeasureText w t) (Cell a)
>     zs = fromListFT $ map (fmap fromChar) xs
>   in case takeLine zs of
>     Nothing -> error "prop_Buffer_takeLine_examples: panic"
>     Just (us,vs) ->
>       (us === (fromListFT $ map (fmap fromChar) as)) .&&.
>       (vs === (fromListFT $ map (fmap fromChar) bs))

> test_Buffer_takeLine_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arbitrary a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_takeLine_examples pw pt pa =
>   testGroup "takeLine examples"
>     [ testProperty "#1" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', eof ]
>           ( [ cell 'a', eof ]
>           , []
>           )
> 
>     , testProperty "#2" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell '\n', eof ]
>           ( [ cell '\n' ]
>           , [ eof ]
>           )
> 
>     , testProperty "#3" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ eof ]
>           )
> 
>     , testProperty "#4" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', cell 'b', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ cell 'b', eof ]
>           )
> 
>     , testProperty "#5" $
>         prop_Buffer_takeLine_examples pw pt pa
>           [ cell 'a', cell '\n', cell '\n', eof ]
>           ( [ cell 'a', cell '\n' ]
>           , [ cell '\n', eof ]
>           )
> 
>     , testProperty "#6" $
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
>   -> Property
> prop_Buffer_takeLines_examples _ _ _
>   (NonNegative h) xs (uss, vs) =
>   let
>     f :: [Cell Char]
>       -> FingerTree (MeasureText w t) (Cell a)
>     f zs = fromListFT $ map (fmap fromChar) zs
> 
>     (css, ds) = takeLines h (f xs)
>   in conjoin
>     [ property (length css <= h)
>     , css === map f uss
>     , ds === f vs
>     ]

> test_Buffer_takeLines_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arbitrary a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_takeLines_examples pw pt pa =
>   testGroup "takeLines examples"
>     [ testProperty "#1" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 1)
>           [ cell 'a', eof ]
>           ( [ [ cell 'a', eof ]
>             ]
>           , []
>           )
> 
>     , testProperty "#2" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 1)
>           [ cell '\n', eof ]
>           ( [ [ cell '\n' ]
>             ]
>           , [ eof ]
>           )
> 
>     , testProperty "#3" $
>         prop_Buffer_takeLines_examples pw pt pa
>           (NonNegative 2)
>           [ cell '\n', cell 'a', cell '\n', eof ]
>           ( [ [ cell '\n' ]
>             , [ cell 'a', cell '\n' ]
>             ]
>           , [ eof ]
>           )
> 
>     , testProperty "#4" $
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
>     [ testProperty "#1" $
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
>     , testProperty "#2" $
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
>   -> Property
> prop_Buffer_getLineNumbers_examples _ _ _
>   m xs uss =
>   let
>     f :: [Cell Char]
>       -> FingerTree (MeasureText w t) (Cell a)
>     f zs = fromListFT $ map (fmap fromChar) zs
> 
>     css = getLineNumbers (value $ f m) (map f xs)
>   in css === map (\(us, k) -> (f us, k)) uss
> 
> test_Buffer_getLineNumbers_examples
>   :: ( IsWidth w, IsTab t, IsChar a, Eq a
>      , Valued (MeasureText w t) a, Show a, Arbitrary a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> TestTree
> test_Buffer_getLineNumbers_examples pw pt pa =
>   testGroup "getLineNumbers examples"
>     [ testProperty "#1" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a' ]
>           ]
>           [ ( [ cell 'a' ], Just 0 )
>           ]
> 
>     , testProperty "#2" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           ]
> 
>     , testProperty "#3" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell '\n' ], Just 1 )
>           ]
> 
>     , testProperty "#4" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell 'a' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testProperty "#5" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a', cell '\n' ]
>           , [ cell 'a' ]
>           ]
>           [ ( [ cell 'a', cell '\n' ], Just 0 )
>           , ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testProperty "#6" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           [ cell 'a', cell '\n' ]
>           [ [ cell 'a' ]
>           ]
>           [ ( [ cell 'a' ], Just 1 )
>           ]
> 
>     , testProperty "#7" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           [ cell 'a', cell '\n' ]
>           [ [ cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 1 )
>           ]
> 
>     , testProperty "#8" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell 'a', cell '\n' ]
>           , [ cell 'a', cell '\n' ]
>           ]
>           [ ( [ cell 'a', cell '\n' ], Just 0 )
>           , ( [ cell 'a', cell '\n' ], Just 1 )
>           ]
> 
>     , testProperty "#9" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ cell 'a', cell '\n' ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ cell 'a', cell '\n' ], Just 1 )
>           ]
> 
>     , testProperty "#10" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ eof ]
>           ]
>           [ ( [ eof ], Just 0 )
>           ]
> 
>     , testProperty "#11" $
>         prop_Buffer_getLineNumbers_examples pw pt pa
>           []
>           [ [ cell '\n' ]
>           , [ eof ]
>           ]
>           [ ( [ cell '\n' ], Just 0 )
>           , ( [ eof ], Just 1 )
>           ]
>     ]



Test Suite
----------

> test_Buffer :: TestTree
> test_Buffer =
>   testGroup "Buffer properties"
>     [ testGroup "Tape"
>       [ test_Tape pChar (pBuffer nat30 nat8)
>       , test_Tape pChar (pBuffer nat30 nat4)
>       , test_Tape pChar (pBuffer nat15 nat2)
>       , test_Tape pChar (pBuffer nat8 nat2)
> 
>       , test_Tape pGlyph (pBuffer nat30 nat8)
>       , test_Tape pGlyph (pBuffer nat30 nat4)
>       , test_Tape pGlyph (pBuffer nat15 nat2)
>       , test_Tape pGlyph (pBuffer nat8 nat2)
>       ]
> 
>     , testGroup "Buffer"
>       [ test_Buffer_properties nat30 nat8 pChar
>       , test_Buffer_properties nat23 nat4 pChar
>       , test_Buffer_properties nat18 nat3 pChar
>       , test_Buffer_properties nat15 nat4 pChar
>       , test_Buffer_properties nat8 nat2 pChar
> 
>       , test_Buffer_properties nat30 nat8 pGlyph
>       , test_Buffer_properties nat23 nat4 pGlyph
>       , test_Buffer_properties nat18 nat3 pGlyph
>       , test_Buffer_properties nat15 nat4 pGlyph
>       , test_Buffer_properties nat8 nat2 pGlyph
> 
>       , test_Buffer_validateBuffer_examples
>       , test_Buffer_splitAtScreenCoords_examples
>       , test_Buffer_screenCoords_value_examples
>       ]
> 
>     , testGroup "Take Lines"
>       [ test_Buffer_takeLine nat5 nat2 pChar
>       , test_Buffer_takeLines_wrap_examples
>       ]
>     ]









> pGlyph :: Proxy Glyph
> pGlyph = Proxy

> pBuffer :: Proxy w -> Proxy t -> Proxy (Buffer w t)
> pBuffer _ _ = Proxy



> genBufferFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
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
>     , mkBuffer $ intercalate [fromChar '\n'] css
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
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Positive Int -> Property
> prop_renderBuffer_one_line_no_wrap _ _ _ (Positive h) =
>   forAll genRenderNoWrapFixed1 $
>     \(lb, as, buf :: Buffer w t a) ->
>       (lb, as) === renderBuffer defaultBufferRenderSettings 0 h buf

-- Generate a buffer with a single nonempty, non-wrapping logical line
-- consisting only of width 1 characters.

> genRenderNoWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderNoWrapFixed1 = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (1,w)
>   genBufferFixed1 [Positive len]



> prop_renderBuffer_one_line_wrap
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Positive Int -> Property
> prop_renderBuffer_one_line_wrap _ _ _ (Positive h) =
>   (h > 1) ==>
>   forAll (genRenderWrapFixed1 (Positive h)) $
>     \(lb, as, buf :: Buffer w t a) ->
>       (lb, as) === renderBuffer defaultBufferRenderSettings 0 h buf

> genRenderWrapFixed1
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Positive Int
>   -> Gen ([Maybe Int], [[a]], Buffer w t a)
> genRenderWrapFixed1 (Positive h) = do
>   let w = toWidth (Proxy :: Proxy w)
>   len <- choose (w+1, w*h)
>   genBufferFixed1 [Positive len]





-- Splitting

> prop_splitBufferAtScreenLine_one_line_wrap
>   :: forall w t a
>    . ( IsWidth w, IsTab t, Valued (MeasureText w t) a
>      , Arbitrary a, Eq a, Show a, IsChar a )
>   => Proxy w -> Proxy t -> Proxy a
>   -> Positive Int -> Property
> prop_splitBufferAtScreenLine_one_line_wrap _ _ _ (Positive h) =
>   (h > 1) ==>
>   forAll (genRenderWrapFixed1 (Positive h)) $
>     \(lb, a:as, buf :: Buffer w t a) ->
>       case unBufferFocus (splitBufferAtScreenLine 1 buf) of
>         Nothing -> error "prop_splitBufferAtScreenLine_one_line_wrap"
>         Just (us, x, vs) -> (a, concat as) === (us ++ [x], vs)

> -}
