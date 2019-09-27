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
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Text
> 
> import Kreb.Text.Glyph.Test




Test suite
----------

> test_TextBox :: TestTree
> test_TextBox =
>   testGroup "TextBox"
>     [ testGroup "Fixed Actions"
>       [ test_TextBox_insert_one_character
>       , localOption (KrebCheckTests 1000)
>           $ test_TextBox_insert_many_as
>       , test_TextBox_insert_some_newlines
>       , test_TextBox_insert_no_chars
>       , localOption (KrebCheckTests 1000)
>           $ test_TextBox_insert_some_then_left
>       , localOption (KrebCheckTests 1000)
>           $ test_TextBox_insert_some_then_backspace
>       , localOption (KrebCheckTests 100)
>           $ test_TextBox_resize
>       , test_TextBox_action_examples
>       ]
> 
>     , localOption (KrebCheckTests 100)
>         $ test_TextBox_cursor
>     ]





Generators
==========

> data TextBox_ = TextBox_
>   { _actions :: [TextBoxAction]
>   , _x :: Positive Int
>   , _y :: Positive Int
>   , _t :: Positive Int
>   } deriving (Eq, Show)
> 
> instance Arb TextBox_ where
>   arb = TextBox_
>     <$> arb
>     <*> arb
>     <*> arb
>     <*> arb
> 
> instance Prune TextBox_ where
>   prune a = do
>     acts <- prune $ _actions a
>     x <- prune $ _x a
>     y <- prune $ _y a
>     t <- prune $ _t a
>     return $ TextBox_ acts x y t
> 
> mk :: TextBox_ -> TextBox
> mk (TextBox_ acts x' y' t') =
>   let
>     Positive x = x'
>     Positive y = y'
>     Positive t = t'
>   in mkTextBox (x,y) t acts
> 
> newtype Width_ = Width_
>   { unWidth_ :: Int
>   } deriving (Eq, Show)
> 
> instance Arb Width_ where
>   arb =
>     Width_ <$> randIn (3,30)
> 
> instance Prune Width_ where
>   prune (Width_ w) =
>     if w <= 3
>       then []
>       else fmap Width_ $ filter (>= 3) $ prune w

> newtype Height_ = Height_
>   { unHeight_ :: Int
>   } deriving (Eq, Show)
> 
> instance Arb Height_ where
>   arb =
>     Height_ <$> randIn (1,50)
> 
> instance Prune Height_ where
>   prune (Height_ h) =
>     if h <= 1
>       then []
>       else fmap Height_ $ filter (>= 1) $ prune h



Test Helpers
============

> cursorIs
>   :: TextBox -> (Int, Int) -> Check
> cursorIs box xy =
>   let z = getTextBoxCursor box in
>   if xy == z
>     then accept
>     else reject $ concat
>       [ "expected cursor ", show z, " "
>       , "to equal ", show xy
>       ]

> heightIs
>   :: TextBox -> Int -> Check
> heightIs box h =
>   claimEqual h (textboxHeight box)

> widthIs
>   :: TextBox -> Int -> Check
> widthIs box w =
>   claimEqual w (getTextBoxWidth box)

> focusLineColIs
>   :: TextBox -> LineCol -> Check
> focusLineColIs box x =
>   claimEqual x (getTextBoxFocusLineCol box)

> -- The relative cursor position should remain in the box
> -- (0,0) - (w-1,h-1)
> cursorInBounds
>   :: TextBox -> Check
> cursorInBounds box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
>     h = textboxHeight box
>     w = getTextBoxWidth box
>   in if x < 0
>     then reject $ concat
>       [ "expected (x = ", show x , ") < 0" ]
>     else if w <= x
>       then reject $ concat
>         [ "expected (x = ", show x
>         , ") >= (w = ", show w, ")" ]
>       else if y < 0
>         then reject $ concat
>           [ "expected (y = ", show y
>           , ") < 0" ]
>         else if h <= y
>           then reject $ concat
>             [ "expected (y = ", show y , ") >= (h = "
>             , show h, ")" ]
>           else accept

> -- The absolute cursor position should coincide with
> -- that of some character in the buffer.
> cursorOnCell
>   :: TextBox -> Check
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
>   in if elem (x, y+l) offsets
>     then accept
>     else reject $ show (x,y+l) ++ " not in " ++ show offsets

> takeBy :: Int -> [a] -> [[a]]
> takeBy k xs =
>   let (as,bs) = splitAt k xs in
>   if null bs
>     then [as]
>     else as : takeBy k bs

> padQuad :: (Int, Int) -> a -> [[a]] -> [[a]]
> padQuad (w,h) a xss =
>   pad h (replicate w a) $
>     map (\xs -> pad w a xs) xss
>   where
>     pad :: Int -> b -> [b] -> [b]
>     pad k c zs = if k <= 0
>       then zs
>       else case zs of
>         [] -> replicate k c
>         w:ws -> w : pad (k-1) c ws

> hasCoherentCursor
>   :: TextBox -> Check
> hasCoherentCursor box =
>   let
>     l = getTextBoxOffset box
>     h = getTextBoxHeight box
>     (u,v) = getTextBoxCursor box
>     (x,y) = getTextBoxFocusScreenCoords box
>   in checkAll
>     [ if x == u
>         then accept
>         else reject $ concat
>           [ "expected x = ", show x, " "
>           , "to equal u = ", show u
>           ]
>     , if y == l+v
>         then accept
>         else reject $ concat
>           [ "expected y = ", show y, " "
>           , "to equal l+v = ", show l, "+", show v
>           ]
>     , if (l <= y) && (y < l+h)
>         then accept
>         else reject $ concat
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
>   testKreb "Cursor properties"
>     prop_TextBox_cursor

> prop_TextBox_cursor
>   :: TextBox_
>   -> Check
> prop_TextBox_cursor box_ =
>   let box = mk box_ in
>   checkAll
>     [ hasCoherentCursor box
>     , cursorInBounds box
>     , cursorOnCell box
>     ]



Insert One Character
--------------------

> test_TextBox_insert_one_character :: TestTree
> test_TextBox_insert_one_character =
>   testKreb "Insert One Character"
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
>   -> Check
> prop_TextBox_insert_one_char
>   dim tab@(Positive t) x@(Ascii c) =
>   let
>     (Width_ w, Height_ h) = dim
>     DebugTextBox labels lines box =
>       make_TextBox_insert_one_char dim tab x
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ claimEqual labels $
>         padL $ case (h, toChar c) of
>           (1, '\n') -> [Just 1]
>           (_, '\n') -> [Just 0, Just 1]
>           (1, '\t') -> if t >= w then [Nothing] else [Just 0]
>           (_, '\t') -> [Just 0]
>           _ -> [Just 0]
>     , claimEqual (map (map fst) lines) $
>         padC $ case (h, toChar c) of
>           (1, '\n') -> [[]]
>           (_, '\n') -> [[c], []]
>           (1, '\t') -> if t >= w then [[]] else [[c]]
>           (_, '\t') -> [[c]]
>           _ -> [[c]]
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box $
>         case (h, toChar c) of
>           (1, '\n') -> (0,0)
>           (_, '\n') -> (0,1)
>           (1, '\t') -> if t >= w then (0,0) else (t,0)
>           (_, '\t') -> if t >= w then (0,1) else (t,0)
>           _    -> (1,0)
>     ]



Insert many 'a's
----------------

> test_TextBox_insert_many_as :: TestTree
> test_TextBox_insert_many_as =
>   testKreb "Insert Some 'a's"
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
>   -> Check
> prop_TextBox_insert_many_as dim tab k =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     (q,r) = quotRem u w
>     l = (q - h) + if r == 0 then 1 else 1
>     DebugTextBox labels lines box =
>       make_TextBox_insert_many_as dim tab k
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box
>         (r, min q (h-1))
>     , claimEqual labels $ padL $ (take h $ drop l $
>         (Just 0) : [Nothing | i <- [1..q]])
>     , claimEqual (map (map fst) lines) $ padC $ (take h $ drop l $
>         (replicate (quot u w) (replicate w (fromChar 'a')) ++
>         [ replicate (rem u w) (fromChar 'a') ]))
>     ]



Insert only newlines
--------------------

> test_TextBox_insert_some_newlines :: TestTree
> test_TextBox_insert_some_newlines =
>   testKreb "Insert some newlines"
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
>   -> Check
> prop_TextBox_insert_some_newlines dim tab k =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     z = if u >= h then 1 else 0
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_newlines dim tab k
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ claimEqual labels $ padL
>         (map Just [(max (u-h+z) 0)..u])
>     , claimEqual (map (map fst) lines) $ padC
>         ((replicate (min u (h-1)) [fromChar '\n']) ++ [[]])
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (0, min u (h-1))
>     ]



Insert some, then left
----------------------

> test_TextBox_insert_some_then_left :: TestTree
> test_TextBox_insert_some_then_left =
>   testKreb "Insert some chars then cursor left"
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
>   -> Check
> prop_TextBox_insert_some_then_left dim tab k =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     (q,r) = quotRem u w
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_then_left dim tab k
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ claimEqual labels $ padL (take h
>         (Just 0 : (replicate q Nothing)))
>     , claimEqual (map (map fst) lines) $ padC (take h
>         (replicate (quot u w) (replicate w (fromChar 'a')) ++
>         [ replicate (rem u w) (fromChar 'a') ]))
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (1, 0)
>     ]



Insert some, then backspace
---------------------------

> test_TextBox_insert_some_then_backspace :: TestTree
> test_TextBox_insert_some_then_backspace =
>   testKreb "Insert some chars then backspace"
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
>   -> Check
> prop_TextBox_insert_some_then_backspace dim tab k b =
>   let
>     (Width_ w, Height_ h) = dim
>     Positive u = k
>     DebugTextBox labels lines box =
>       make_TextBox_insert_some_then_backspace dim tab k b
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ claimEqual labels $ padL [Just 0]
>     , claimEqual (map (map fst) lines) $ padC [[]]
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
>   -> Check
> prop_TextBox_insert_no_chars act dim tab =
>   let
>     (Positive w, Positive h) = dim
>     DebugTextBox labels lines box =
>       make_TextBox_insert_no_chars act dim tab
>     padC = padQuad (w,h) (mkGlyph ' ')
>     padL xs = take h $ xs ++ repeat Nothing
>   in checkAll
>     [ claimEqual labels $ padL [Just 0]
>     , claimEqual (map (map fst) lines) $ padC [[]]
>     , hasCoherentCursor box
>     , cursorIs box (0, 0)
>     ]
> 
> test_TextBox_insert_no_chars_action
>   :: TextBoxAction
>   -> TestTree
> test_TextBox_insert_no_chars_action act =
>   testKreb ("Insert no chars (" ++ show act ++ ")") $
>     prop_TextBox_insert_no_chars act



Resize
------

> test_TextBox_resize :: TestTree
> test_TextBox_resize =
>   testKreb "Resize"
>     prop_TextBox_resize

> prop_TextBox_resize
>   :: TextBox_
>   -> (Width_, Positive Int)
>   -> Check
> prop_TextBox_resize box dim =
>   let
>     box1 = mk box
>     (Width_ u, Positive v) = dim
>     box2 = alterTextBox [TextBoxResize (u,v)] box1
>   in checkAll
>     [ heightIs box2 v
>     , widthIs box2 u
>     , claimEqual
>         (getTextBoxFocusLineCol box1)
>         (getTextBoxFocusLineCol box2)
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
>   -> Check
> prop_TextBox_action_examples
>   (dim, tab, acts) (labels', lines', cursor) =
>   let
>     DebugTextBox labels lines box =
>       debugTextBox $ mkTextBox dim tab acts
>   in checkAll
>     [ claimEqual labels labels'
>     , claimEqual (map (map fst) lines) (map (map fromChar) lines')
>     , claimEqual (textboxCursor box) cursor
>     , hasCoherentCursor box
>     ]

> test_TextBox_action_examples :: TestTree
> test_TextBox_action_examples =
>   testGroup "Action Examples"
>     [ testKreb "#1" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             ]
>           )
>           ( [Just 0]
>           , [ "a  " ]
>           , (1,0)
>           )
> 
>     , testKreb "#2" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorLeft
>             ]
>           )
>           ( [Just 0]
>           , [ "a  " ]
>           , (0,0)
>           )
> 
>     , testKreb "#3" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorLeft
>             , TextBoxInsert (mkGlyph 'b')
>             ]
>           )
>           ( [Just 0]
>           , [ "ba " ]
>           , (1,0)
>           )
> 
>     , testKreb "#4" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph 'a')
>             , TextBoxCursorUp
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n  ", "a  " ]
>           , (0,0)
>           )
> 
>     , testKreb "#5" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxCursorUp
>             , TextBoxCursorRight
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#6" $
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
>           , [ "ab\n", "   " ]
>           , (1,0)
>           )
> 
>     , testKreb "#7" $
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
>           , [ "abc", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#8" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 0, Just 1]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#9" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 1, Just 2]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#10" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             , TextBoxInsert (mkGlyph '\n')
>             ]
>           )
>           ( [Just 2, Just 3]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#11" $
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
>           , [ "   " ]
>           , (0,0)
>           )
> 
>     , testKreb "#12" $
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
>           , [ "jkl", "   " ]
>           , (0,1)
>           )
> 
>     , testKreb "#13" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , []
>           )
>           ( [ Just 0, Nothing ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , testKreb "#14" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxBackspace
>             ]
>           )
>           ( [ Just 0, Nothing ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , testKreb "#15" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxBackspace
>             , TextBoxBackspace
>             ]
>           )
>           ( [ Just 0, Nothing ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , testKreb "#16" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (mkGlyph 'a')
>             , TextBoxInsert (mkGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxBackspace
>             ]
>           )
>           ( [ Just 0, Nothing ]
>           , [ "b  ", "   " ]
>           , (0,0)
>           )
> 
> 
>     , testKreb "#17" $
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
>           ( [ Nothing ]
>           , [ "def" ]
>           , (0,0)
>           )
>     ]
