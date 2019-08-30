---
title: Kreb.Text.TextBox.Test
---



Contents
--------

* [Introduction](#introduction)
* [Generators](#generators)
* [Tests](#tests)
    * [Test suite](#test-suite)



Introduction
============

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts
> #-}

> module Kreb.Text.TextBox.Test (
>     test_TextBox
> ) where
> 
> import Data.Proxy
> import Data.List
> 
> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck
> 
> import Kreb.Text
> 
> import Kreb.Text.Glyph.Test



Generators
==========

> data TextBox_ = TextBox_
>   { _actions :: [TextBoxAction]
>   , _x :: Positive Int
>   , _y :: Positive Int
>   , _t :: Positive Int
>   } deriving (Eq, Show)
> 
> instance Arbitrary TextBox_ where
>   arbitrary = TextBox_
>     <$> arbitrary
>     <*> arbitrary
>     <*> arbitrary
>     <*> arbitrary
> 
>   shrink a = do
>     acts <- shrink $ _actions a
>     x <- shrink $ _x a
>     y <- shrink $ _y a
>     t <- shrink $ _t a
>     return $ TextBox_ acts x y t
> 
> mk :: TextBox_ -> TextBox
> mk (TextBox_ acts x' y' t') =
>   let
>     Positive x = x'
>     Positive y = y'
>     Positive t = t'
>   in mkTextBox (x,y) t acts

> instance Arbitrary TextBoxAction where
>   arbitrary = oneof
>     [ TextBoxInsert <$> arbitrary
>     , TextBoxInsertMany <$> arbitrary
>     , return TextBoxBackspace
>     , return TextBoxCursorDown
>     , return TextBoxCursorUp
>     , return TextBoxCursorRight
>     , return TextBoxCursorLeft
>     , do
>         Positive w <- arbitrary
>         Positive h <- arbitrary
>         return $ TextBoxResize (w,h)
>     ]
> 
>   shrink x = case x of
>     TextBoxInsertMany cs ->
>       map TextBoxInsertMany $ shrink cs
>     _ -> []
> 
> instance Arbitrary TextBox where
>   arbitrary = do
>     Positive x <- arbitrary
>     Positive y <- arbitrary
>     Positive t <- arbitrary
>     mkTextBox (x,y) t <$> arbitrary

> newtype Width_ = Width_
>   { unWidth_ :: Int
>   } deriving (Eq, Show)
> 
> instance Arbitrary Width_ where
>   arbitrary =
>     Width_ <$> choose (3,30)
> 
>   shrink (Width_ w) =
>     Width_ <$> shrink w

> newtype Height_ = Height_
>   { unHeight_ :: Int
>   } deriving (Eq, Show)
> 
> instance Arbitrary Height_ where
>   arbitrary =
>     Height_ <$> choose (1,50)
> 
>   shrink (Height_ h) =
>     Height_ <$> shrink h



Test Helpers
============

> cursorIs
>   :: TextBox -> (Int, Int) -> Property
> cursorIs box xy =
>   let z = getTextBoxCursor box in
>   if xy == z
>     then property True
>     else error $ concat
>       [ "expected cursor ", show z, " "
>       , "to equal ", show xy
>       ]

> heightIs
>   :: TextBox -> Int -> Property
> heightIs box h =
>   h === (textboxHeight box)

> widthIs
>   :: TextBox -> Int -> Property
> widthIs box w =
>   w === getTextBoxWidth box

> focusLineColIs
>   :: TextBox -> LineCol -> Property
> focusLineColIs box x =
>   x === getTextBoxFocusLineCol box

> -- The relative cursor position should remain in the box
> -- (0,0) - (w-1,h-1)
> cursorInBounds
>   :: TextBox -> Property
> cursorInBounds box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
>     h = textboxHeight box
>     w = getTextBoxWidth box
>   in property $ if x < 0
>     then error $ concat
>       [ "expected (x = ", show x , ") < 0" ]
>     else if w <= x
>       then error $ concat
>         [ "expected (x = ", show x
>         , ") >= (w = ", show w, ")" ]
>       else if y < 0
>         then error $ concat
>           [ "expected (y = ", show y
>           , ") < 0" ]
>         else if h <= y
>           then error $ concat
>             [ "expected (y = ", show y , ") >= (h = "
>             , show h, ")" ]
>           else True

> -- The absolute cursor position should coincide with
> -- that of some character in the buffer.
> cursorOnCell
>   :: TextBox -> Property
> cursorOnCell box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
> 
>     offsets :: [(Int, Int)]
>     offsets =
>       querySizedBuffer
>         ( map (\x -> applyScreenOffset x (0,0)) .
>           map (screenCoords . snd) .
>           toListDebugBuffer
>         ) $ textboxBuffer box
>   in property $
>     if elem (x, y+l) offsets
>       then True
>       else error $ show (x,y+l) ++ " not in " ++ show offsets

> takeBy :: Int -> [a] -> [[a]]
> takeBy k xs =
>   let (as,bs) = splitAt k xs in
>   if null bs
>     then [as]
>     else as : takeBy k bs

> hasCoherentCursor
>   :: TextBox -> Property
> hasCoherentCursor box =
>   let
>     l = getTextBoxOffset box
>     h = getTextBoxHeight box
>     (u,v) = getTextBoxCursor box
>     (x,y) = getTextBoxFocusScreenCoords box
>   in conjoin
>     [ if x == u
>         then property True
>         else error $ concat
>           [ "expected x = ", show x, " "
>           , "to equal u = ", show u
>           ]
>     , if y == l+v
>         then property True
>         else error $ concat
>           [ "expected y = ", show y, " "
>           , "to equal l+v = ", show l, "+", show v
>           ]
>     , if (l <= y) && (y < l+h)
>         then property True
>         else error $ concat
>           [ "expected y = ", show y, " "
>           , "in the range [", show l
>           , "...", show (l+h), ")"
>           ]
>     ]



Tests
=====

Cursor
------

> test_TextBox_cursor :: TestTree
> test_TextBox_cursor =
>   testProperty "Cursor properties"
>     prop_TextBox_cursor

> prop_TextBox_cursor
>   :: TextBox_
>   -> Property
> prop_TextBox_cursor box_ =
>   let box = mk box_ in
>   conjoin
>     [ hasCoherentCursor box
>     , cursorInBounds box
>     , cursorOnCell box
>     ]



Insert One Character
--------------------

> test_TextBox_insert_one_character :: TestTree
> test_TextBox_insert_one_character =
>   testProperty "Insert One Character"
>     prop_TextBox_insert_one_char

> make_TextBox_insert_one_char
>   :: (Width_, Height_)
>   -> Positive Int
>   -> Ascii
>   -> DebugTextBox
> make_TextBox_insert_one_char
>   (Width_ x, Height_ y) (Positive tab) (Ascii c) =
>   debugTextBox $ mkTextBox (x,y) tab $
>     [ TextBoxInsert c
>     ]
> 
> prop_TextBox_insert_one_char
>   :: (Width_, Height_)
>   -> Positive Int
>   -> Ascii
>   -> Property
> prop_TextBox_insert_one_char
>   dim tab x@(Ascii c) =
>   let
>     (_, Height_ h) = dim
>     DebugTextBox labels lines box =
>       make_TextBox_insert_one_char dim tab x
>   in conjoin
>     [ labels === [Just 0]
>     , lines === [[c]]
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box $
>         case (toChar c) of
>           '\n' -> (0,1)
>           _    -> (1,0)
>     ]



Insert many 'a's
----------------

> test_TextBox_insert_many_as :: TestTree
> test_TextBox_insert_many_as =
>   testProperty "Insert Some 'a's"
>     prop_TextBox_insert_many_as

> make_TextBox_insert_many_as
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> DebugTextBox
> make_TextBox_insert_many_as
>   (Width_ x, Height_ y) (Positive tab) (Positive k) =
>   debugTextBox $ mkTextBox (x,y) tab $
>     [ TextBoxInsertMany (replicate k (fromChar 'a'))
>     ]
> 
> prop_TextBox_insert_many_as
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> Property
> prop_TextBox_insert_many_as dim tab k =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     (q,r) = quotRem u w
>     l = (q - h) + if r == 0 then 1 else 1
>     DebugTextBox labels lines box =
>       make_TextBox_insert_many_as dim tab k
>   in conjoin
>     [ heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box
>         (r, min q (h-1))
>     , labels === (take h $ drop l $
>         (Just 0) : [Nothing | i <- [1..q]])
>     , lines === (take h $ drop l $
>         (replicate (quot u w) (replicate w (fromChar 'a')) ++
>         [ replicate (rem u w) (fromChar 'a') ]))
>     ]



Insert only newlines
--------------------

> test_TextBox_insert_some_newlines :: TestTree
> test_TextBox_insert_some_newlines =
>   testProperty "Insert some newlines"
>     prop_TextBox_insert_some_newlines

> make_TextBox_insert_some_newlines
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> DebugTextBox
> make_TextBox_insert_some_newlines
>   (Width_ x, Height_ y) (Positive tab) (Positive k) =
>   debugTextBox $ mkTextBox (x,y) tab $
>     [ TextBoxInsertMany (replicate k (fromChar '\n'))
>     ]
> 
> prop_TextBox_insert_some_newlines
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> Property
> prop_TextBox_insert_some_newlines dim tab k =
>   let
>     (_, Height_ h) = dim
>     Positive u = k
>     z = if u >= h then 1 else 0
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_newlines dim tab k
>   in conjoin
>     [ labels ===
>         map Just [(max (u-h+z) 0)..u]
>     , lines ===
>         ((replicate (min u (h-1)) [fromChar '\n']) ++ [[]])
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (0, min u (h-1))
>     ]



Insert some, then left
----------------------

> test_TextBox_insert_some_then_left :: TestTree
> test_TextBox_insert_some_then_left =
>   testProperty "Insert some chars then cursor left"
>     prop_TextBox_insert_some_then_left

> make_TextBox_insert_some_then_left
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> DebugTextBox
> make_TextBox_insert_some_then_left
>   (Width_ x, Height_ y) (Positive tab) (Positive k) =
>   debugTextBox $ mkTextBox (x,y) tab $
>     [ TextBoxInsertMany (replicate k (fromChar 'a'))
>     ] ++ replicate (k-1) (TextBoxCursorLeft)
> 
> prop_TextBox_insert_some_then_left
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int
>   -> Property
> prop_TextBox_insert_some_then_left dim tab k =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     (q,r) = quotRem u w
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_then_left dim tab k
>   in conjoin
>     [ labels === take h
>         (Just 0 : (replicate q Nothing))
>     , lines === take h
>         (replicate (quot u w) (replicate w (fromChar 'a')) ++
>         [ replicate (rem u w) (fromChar 'a') ])
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (1, 0)
>     ]



Insert some, then backspace
---------------------------

> test_TextBox_insert_some_then_backspace :: TestTree
> test_TextBox_insert_some_then_backspace =
>   testProperty "Insert some chars then backspace"
>     prop_TextBox_insert_some_then_backspace

> make_TextBox_insert_some_then_backspace
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int -> NonNegative Int
>   -> DebugTextBox
> make_TextBox_insert_some_then_backspace
>   (Width_ x, Height_ y) (Positive tab)
>   (Positive k) (NonNegative b) =
>   debugTextBox $ mkTextBox (x,y) tab $
>     [ TextBoxInsertMany (replicate k (fromChar 'a'))
>     ] ++ replicate (k+b) (TextBoxBackspace)
> 
> prop_TextBox_insert_some_then_backspace
>   :: (Width_, Height_) -> Positive Int
>   -> Positive Int -> NonNegative Int
>   -> Property
> prop_TextBox_insert_some_then_backspace dim tab k b =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_then_backspace dim tab k b
>   in conjoin
>     [ labels === [Just 0]
>     , lines === [[]]
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (0, 0)
>     ]



Insert no characters
--------------------

> test_TextBox_insert_no_chars :: TestTree
> test_TextBox_insert_no_chars =
>   testGroup "Insert no chars"
>     [ test_TextBox_insert_no_chars_action TextBoxBackspace
>     , test_TextBox_insert_no_chars_action TextBoxCursorDown
>     , test_TextBox_insert_no_chars_action TextBoxCursorUp
>     , test_TextBox_insert_no_chars_action TextBoxCursorRight
>     , test_TextBox_insert_no_chars_action TextBoxCursorLeft
>     ]

> make_TextBox_insert_no_chars
>   :: TextBoxAction
>   -> (Positive Int, Positive Int) -> Positive Int
>   -> DebugTextBox
> make_TextBox_insert_no_chars
>   act (Positive x, Positive y) (Positive tab) =
>   debugTextBox $ mkTextBox (x,y) tab [ act ]
> 
> prop_TextBox_insert_no_chars
>   :: TextBoxAction
>   -> (Positive Int, Positive Int) -> Positive Int
>   -> Property
> prop_TextBox_insert_no_chars act dim tab =
>   let
>     DebugTextBox labels lines box =
>       make_TextBox_insert_no_chars act dim tab
>   in conjoin
>     [ labels === [Just 0]
>     , lines === [[]]
>     , hasCoherentCursor box
>     , cursorIs box (0, 0)
>     ]
> 
> test_TextBox_insert_no_chars_action
>   :: TextBoxAction
>   -> TestTree
> test_TextBox_insert_no_chars_action act =
>   testProperty ("Insert no chars (" ++ show act ++ ")") $
>     prop_TextBox_insert_no_chars act



Resize
------

> test_TextBox_resize :: TestTree
> test_TextBox_resize =
>   testProperty "Resize"
>     prop_TextBox_resize

> prop_TextBox_resize
>   :: TextBox_
>   -> (Width_, Positive Int)
>   -> Property
> prop_TextBox_resize box dim =
>   let
>     box1 = mk box
>     (Width_ u, Positive v) = dim
>     box2 = alterTextBox [TextBoxResize (u,v)] box1
>   in conjoin
>     [ heightIs box2 v
>     , widthIs box2 u
>     , (getTextBoxFocusLineCol box1)
>         ===
>       (getTextBoxFocusLineCol box2)
>     , hasCoherentCursor box2
>     , cursorInBounds box2
>     ]



Action examples
---------------

> prop_TextBox_action_examples
>   :: ( (Int, Int)
>      , Int
>      , [TextBoxAction] )
>   -> ( [Maybe Int]
>      , [[Char]]
>      , (Int, Int) )
>   -> Property
> prop_TextBox_action_examples
>   (dim, tab, acts) (labels', lines', cursor) =
>   let
>     DebugTextBox labels lines box =
>       debugTextBox $ mkTextBox dim tab acts
>   in conjoin
>     [ labels === labels'
>     , lines === (map (map fromChar) lines')
>     , textboxCursor box === cursor
>     , hasCoherentCursor box
>     ]

> test_TextBox_action_examples :: TestTree
> test_TextBox_action_examples =
>   testGroup "Action Examples"
>     [ testProperty "#1" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             ]
>           )
>           ( [Just 0]
>           , [ "a" ]
>           , (1,0)
>           )
> 
>     , testProperty "#2" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorLeft
>             ]
>           )
>           ( [Just 0]
>           , [ "a" ]
>           , (0,0)
>           )
> 
>     , testProperty "#3" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorLeft
>             , TextBoxInsert (mkGlyph 'b')
>             ]
>           )
>           ( [Just 0]
>           , [ "ba" ]
>           , (1,0)
>           )
> 
>     , testProperty "#4" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorUp
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n", "a" ]
>           , (0,0)
>           )
> 
>     , testProperty "#5" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxCursorUp
>             , TextBoxCursorRight
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#6" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxInsert (mkGlyph '\n')
>             , TextBoxCursorUp
>             , TextBoxCursorRight
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "ab\n", "" ]
>           , (1,0)
>           )
> 
>     , testProperty "#7" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxInsert (mkGlyph 'c')
>             , TextBoxCursorDown
>             ]
>           )
>           ( [Just 0, Nothing]
>           , [ "abc", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#8" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#9" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 1, Just 2]
>           , [ "\n", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#10" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 2, Just 3]
>           , [ "\n", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#11" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxInsert (mkGlyph 'c')
>             , TextBoxInsert (mkGlyph 'd')
>             , TextBoxInsert (mkGlyph 'e')
>             , TextBoxInsert (mkGlyph 'f')
>             , TextBoxCursorDown
>             ]
>           )
>           ( [ Nothing ]
>           , [ "" ]
>           , (0,0)
>           )
> 
>     , testProperty "#12" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxInsert (mkGlyph 'c')
>             , TextBoxInsert (mkGlyph 'd')
>             , TextBoxInsert (mkGlyph 'e')
>             , TextBoxInsert (mkGlyph 'f')
>             , TextBoxInsert (mkGlyph 'g')
>             , TextBoxInsert (mkGlyph 'h')
>             , TextBoxInsert (mkGlyph 'i')
>             , TextBoxInsert (mkGlyph 'j')
>             , TextBoxInsert (mkGlyph 'k')
>             , TextBoxInsert (mkGlyph 'l')
>             , TextBoxCursorDown
>             ]
>           )
>           ( [ Nothing, Nothing ]
>           , [ "jkl", "" ]
>           , (0,1)
>           )
> 
>     , testProperty "#13" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , []
>           )
>           ( [ Just 0 ]
>           , [ "" ]
>           , (0,0)
>           )
> 
>     , testProperty "#14" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxBackspace
>             ]
>           )
>           ( [Just 0]
>           , [ "" ]
>           , (0,0)
>           )
> 
>     , testProperty "#15" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxBackspace
>             , TextBoxBackspace
>             ]
>           )
>           ( [Just 0]
>           , [ "" ]
>           , (0,0)
>           )
> 
>     , testProperty "#16" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxBackspace
>             ]
>           )
>           ( [Just 0]
>           , [ "b" ]
>           , (0,0)
>           )
> 
> 
>     , testProperty "#17" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsertMany
>               [ mkGlyph 'a', mkGlyph 'b', mkGlyph 'c'
>               , mkGlyph 'd', mkGlyph 'e', mkGlyph 'f' ]
>             , TextBoxCursorUp
>             , TextBoxResize (3,1)
>             ]
>           )
>           ( [Nothing]
>           , [ "def" ]
>           , (0,0)
>           )
>     ]





Test suite
----------

> test_TextBox :: TestTree
> test_TextBox =
>   testGroup "TextBox"
>     [ testGroup "Fixed Actions"
>       [ test_TextBox_insert_one_character
>       , localOption (QuickCheckTests 1000)
>           $ test_TextBox_insert_many_as
>       , test_TextBox_insert_some_newlines
>       , test_TextBox_insert_no_chars
>       , localOption (QuickCheckTests 1000)
>           $ test_TextBox_insert_some_then_left
>       , localOption (QuickCheckTests 1000)
>           $ test_TextBox_insert_some_then_backspace
>       , localOption (QuickCheckTests 100)
>           $ test_TextBox_resize
>       , test_TextBox_action_examples
>       ]
>     , localOption (QuickCheckTests 100)
>         $ test_TextBox_cursor
>     ]
