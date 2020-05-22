---
title: Text boxes -- Testing
---


Introduction
============

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , FlexibleContexts, DataKinds, OverloadedStrings
> #-}

> module Kreb.Text.TextBox.Test (
>     test_TextBox
> ) where
> 
> import Data.Proxy
> import Data.List
> import Control.Monad
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>), reflow)
> import Kreb.Prop
> import Kreb.Effect
> import Kreb.Text
> import Kreb.Control

> import Kreb.Text.TextBox.Test.Unit



> test_TextBox :: TestTree
> test_TextBox = testGroup "TextBox"
>   [ test_TextBox_unit
>   , testGroup "Property Tests"
>     [ testGroup "Fixed Actions"
>       [ test_TextBox_insert_one_character
>       , test_TextBox_insert_many_as
>       , test_TextBox_insert_many_newlines
>       ]
>     ]
>   ]



Generators
==========

> data TextBox_ = TextBox_
>   { _actions :: [TextBoxAction]
>   , _x :: Positive Int
>   , _y :: Positive Int
>   , _t :: Positive Int
>   }

> mkMockTextBox
>   :: ( Monad m )
>   => Chaos -> MockWorld ev
>   -> TextBoxFX (Mock ev m) -> EventId
>   -> TextBox_ -> m (MockWorld ev, TextBox)
> mkMockTextBox chi world fx eId (TextBox_ acts x' y' t') =
>   let
>     Positive x = x'
>     Positive y = y'
>     Positive t = t'
>   in runMock chi world $ mkTextBox fx eId (x,y) t acts

> data Dim_ = Dim_ Int Int Int
>   deriving (Eq, Show)

> instance Fmt.Display Dim_ where
>   display (Dim_ x y t) =
>     "Dim_" <+> display x
>       <+> display y <+> display t

> instance Arb Dim_ where
>   arb = do
>     w <- randIn (3,100)
>     h <- randIn (1,100)
>     t <- randIn (1,min 1 (w-1))
>     return $ Dim_ w h t

> instance Prune Dim_ where
>   prune (Dim_ w h t) =
>     if (w == 3) && (h == 1) && (t == 1)
>       then []
>       else do
>         w' <- [w, max (w-1) 3, max (div w 2) 3, 3]
>         h' <- [h, max (h-1) 1, max (div h 2) 1, 1]
>         t' <- [t, max (t-1) 1, max (div t 2) 1, 1]
>         guard $ not $ (w' == w) && (h' == h) && (t' == t)
>         guard $ t' < w'
>         return $ Dim_ w' h' t'





Test Helpers
============

> cursorIs
>   :: TextBox -> (Int, Int) -> Check
> cursorIs box xy =
>   let z = textboxCursor box in
>   if xy == z
>     then accept
>     else reject $ reflow $ concat
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
>   claimEqual w (textboxWidth box)

> focusLineColIs
>   :: TextBox -> LineCol -> Check
> focusLineColIs box x =
>   claimEqual x (textboxFocusPointLineCol box)

> -- The relative cursor position should remain in the box
> -- (0,0) - (w-1,h-1)
> cursorInBounds
>   :: TextBox -> Check
> cursorInBounds box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
>     h = textboxHeight box
>     w = textboxWidth box
>   in if x < 0
>     then reject $ reflow $ concat
>       [ "expected (x = ", show x , ") < 0" ]
>     else if w <= x
>       then reject $ reflow $ concat
>         [ "expected (x = ", show x
>         , ") >= (w = ", show w, ")" ]
>       else if y < 0
>         then reject $ reflow $ concat
>           [ "expected (y = ", show y
>           , ") < 0" ]
>         else if h <= y
>           then reject $ reflow $ concat
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
>           toAnnotatedList
>         ) $ textboxBuffer box
>   in if elem (x, y+l) offsets
>     then accept
>     else reject $ reflow $ show (x,y+l) ++ " not in " ++ show offsets

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

> annotateCol :: [[Char]] -> [[(Char, Int)]]
> annotateCol css = map (\cs -> zip cs [0..]) css

> hasCoherentCursor
>   :: TextBox -> Check
> hasCoherentCursor box =
>   let
>     l = textboxOffset box
>     h = textboxHeight box
>     (u,v) = textboxCursor box
>     (x,y) = textboxFocusPointScreenOffset box
>   in explain (reflow "Cursor is inside visible window:") $ claimAll
>     [ if x == u
>         then accept
>         else reject $ reflow $ concat
>           [ "expected x = ", show x, " "
>           , "to equal u = ", show u
>           ]
>     , if y == l+v
>         then accept
>         else reject $ reflow $ concat
>           [ "expected y = ", show y, " "
>           , "to equal l+v = ", show l, "+", show v
>           ]
>     , if (l <= y) && (y < l+h)
>         then accept
>         else reject $ reflow $ concat
>           [ "expected y = ", show y, " "
>           , "in the range [", show l
>           , "...", show (l+h), ")"
>           ]
>     ]





Tests
=====

Insert One Character
--------------------

> test_TextBox_insert_one_character :: TestTree
> test_TextBox_insert_one_character =
>   krebProp "Insert One Character"
>     prop_TextBox_insert_one_char

> make_TextBox_insert_one_char
>   :: Dim_
>   -> Char
>   -> DebugTextBox
> make_TextBox_insert_one_char (Dim_ x y t) c =
>   let
>     acts =
>       [ TextBoxInsert c
>       ]
>     box = TextBox_ acts (Positive x) (Positive y) (Positive t)
>     Identity (w,b) = mkMockTextBox
>       (Chaos 0) (initMockWorld []) fxMock (EventId 0 "") box
>   in debugTextBox b
> 
> prop_TextBox_insert_one_char
>   :: Dim_
>   -> Char
>   -> Check
> prop_TextBox_insert_one_char
>   dim@(Dim_ w h t) c =
>   let
>     DebugTextBox labels lines box =
>       make_TextBox_insert_one_char dim c
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ explain (reflow "Line labels:") $
>         claimEqual labels $ annotateCol $
>           padL $ case (h, c) of
>             (1, '\n') -> ["1 "]
>             (_, '\n') -> ["0 ", "1 "]
>             (1, '\t') -> if t >= w then ["  "] else ["0 "]
>             (_, '\t') -> ["0 "]
>             _ -> ["0 "]
>     , claimEqual (map (concatMap fst) lines) $
>         case (h, c) of
>           (1, '\n') -> [[]]
>           (_, '\n') -> [[c], []]
>           (1, '\t') -> if t >= w then [[]] else [[c]]
>           (_, '\t') -> [[c]]
>           _ -> [[c]]
>     , explain (reflow "Box height:") $
>         heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box $
>         case (h, c) of
>           (1, '\n') -> (0,0)
>           (_, '\n') -> (0,1)
>           (1, '\t') -> if t >= w then (0,0) else (t,0)
>           (_, '\t') -> if t >= w then (0,1) else (t,0)
>           _         -> (1,0)
>     ]



Insert many 'a's
----------------

> test_TextBox_insert_many_as :: TestTree
> test_TextBox_insert_many_as =
>   krebProp "Insert Many 'a's"
>     prop_TextBox_insert_many_as

> make_TextBox_insert_many_as
>   :: Dim_
>   -> Between 1 100 Int
>   -> DebugTextBox
> make_TextBox_insert_many_as (Dim_ x y t) (Between k) =
>   let
>     acts =
>       [ TextBoxInsertMany (replicate k 'a')
>       ]
>     box = TextBox_ acts (Positive x) (Positive y) (Positive t)
>     Identity (w,b) = mkMockTextBox
>       (Chaos 0) (initMockWorld []) fxMock (EventId 0 "") box
>   in debugTextBox b
> 
> prop_TextBox_insert_many_as
>   :: Dim_
>   -> Between 1 100 Int
>   -> Check
> prop_TextBox_insert_many_as dim@(Dim_ w h tab) k =
>   let
>     Between u = k
>     (q,r) = quotRem u w
>     l = max 0 $ (q - h) + 1
>     DebugTextBox labels lines box =
>       make_TextBox_insert_many_as dim k
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box
>         (r, min q (h-1))
>     , explain (reflow "Line labels:") $
>         claimEqual labels $ annotateCol $ padL $ (take h $ drop l $
>           ("0 ") : [ "  " | i <- [1..q]])
>     , explain (reflow "Line contents:") $
>         claimEqual (map (concatMap fst) lines) $ (take h $ drop l $
>           (replicate q (replicate w 'a') ++
>           [ replicate r 'a' ]))
>     ]



Insert many newlines
--------------------

> test_TextBox_insert_many_newlines :: TestTree
> test_TextBox_insert_many_newlines =
>   krebProp "Insert some newlines"
>     prop_TextBox_insert_many_newlines

> make_TextBox_insert_many_newlines
>   :: Dim_
>   -> Between 1 100 Int
>   -> DebugTextBox
> make_TextBox_insert_many_newlines (Dim_ x y t) (Between k) =
>   let
>     acts =
>       [ TextBoxInsertMany (replicate k '\n')
>       ]
>     box = TextBox_ acts (Positive x) (Positive y) (Positive t)
>     Identity (w,b) = mkMockTextBox
>       (Chaos 0) (initMockWorld []) fxMock (EventId 0 "") box
>   in debugTextBox b
> 
> prop_TextBox_insert_many_newlines
>   :: Dim_
>   -> Between 1 100 Int
>   -> Check
> prop_TextBox_insert_many_newlines dim k =
>   let
>     Dim_ w h tab = dim
>     Between u = k
>     z = if u >= h then 1 else 0
>     DebugTextBox labels lines box =
>       make_TextBox_insert_many_newlines dim k
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ claimEqual labels $ annotateCol $ padL
>         (map (\k -> show k ++ " ") [(max (u-h+z) 0)..u])
>     , claimEqual (map (concatMap fst) lines) $
>         ((replicate (min u (h-1)) "\n") ++ [[]])
>     , heightIs box h
>     , hasCoherentCursor box
>     , cursorIs box (0, min u (h-1))
>     ]








> {-


Test suite
----------

> test_TextBox :: TestTree
> test_TextBox =
>   testGroup "TextBox"
>     [ testGroup "Fixed Actions"

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













Tests
=====


> test_TextBox_cursor :: TestTree
> test_TextBox_cursor =
>   krebProp "Cursor properties"
>     prop_TextBox_cursor

> prop_TextBox_cursor
>   :: TextBox_
>   -> Check
> prop_TextBox_cursor box_ =
>   let box = mk box_ in
>   claimAll
>     [ hasCoherentCursor box
>     , cursorInBounds box
>     , cursorOnCell box
>     ]















Insert some, then left
----------------------

> test_TextBox_insert_some_then_left :: TestTree
> test_TextBox_insert_some_then_left =
>   krebProp "Insert some chars then cursor left"
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
>     padC = padQuad (w,h) (plainGlyph ' ')
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ claimEqual labels $ annotateCol $ padL (take h
>         ("0 " : (replicate q "  ")))
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
>   krebProp "Insert some chars then backspace"
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
>     padC = padQuad (w,h) (plainGlyph ' ')
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ claimEqual labels $ annotateCol $ padL ["0 "]
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
>     padC = padQuad (w,h) (plainGlyph ' ')
>     padL xs = take h $ xs ++ repeat "  "
>   in claimAll
>     [ claimEqual labels $ annotateCol $ padL ["0 "]
>     , claimEqual (map (map fst) lines) $ padC [[]]
>     , hasCoherentCursor box
>     , cursorIs box (0, 0)
>     ]
> 
> test_TextBox_insert_no_chars_action
>   :: TextBoxAction
>   -> TestTree
> test_TextBox_insert_no_chars_action act =
>   krebProp ("Insert no chars (" ++ show act ++ ")") $
>     prop_TextBox_insert_no_chars act



Resize
------

> test_TextBox_resize :: TestTree
> test_TextBox_resize =
>   krebProp "Resize"
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
>   in claimAll
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
>   -> ( [[Char]]
>      , [[Char]]
>      , (Int, Int) )
>   -> Check
> prop_TextBox_action_examples
>   (dim, tab, acts) (labels', lines', cursor) =
>   let
>     DebugTextBox labels lines box =
>       debugTextBox $ mkTextBox dim tab acts
>   in claimAll
>     [ claimEqual labels $ annotateCol labels'
>     , claimEqual (map (map fst) lines) (map (map fromChar) lines')
>     , claimEqual (textboxCursor box) cursor
>     , hasCoherentCursor box
>     ]

> test_TextBox_action_examples :: TestTree
> test_TextBox_action_examples =
>   testGroup "Action Examples"
>     [ krebProp "#1" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             ]
>           )
>           ( ["0 "]
>           , [ "a  " ]
>           , (1,0)
>           )
> 
>     , krebProp "#2" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxCursorLeft
>             ]
>           )
>           ( ["0 "]
>           , [ "a  " ]
>           , (0,0)
>           )
> 
>     , krebProp "#3" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxCursorLeft
>             , TextBoxInsert (plainGlyph 'b')
>             ]
>           )
>           ( ["0 "]
>           , [ "ba " ]
>           , (1,0)
>           )
> 
>     , krebProp "#4" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph '\n')
>             , TextBoxInsert (plainGlyph 'a')
>             , TextBoxCursorUp
>             ]
>           )
>           ( ["0 ", "1 "]
>           , [ "\n  ", "a  " ]
>           , (0,0)
>           )
> 
>     , krebProp "#5" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph '\n')
>             , TextBoxCursorUp
>             , TextBoxCursorRight
>             ]
>           )
>           ( ["0 ", "1 "]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#6" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxInsert (plainGlyph '\n')
>             , TextBoxCursorUp
>             , TextBoxCursorRight
>             ]
>           )
>           ( ["0 ", "1 "]
>           , [ "ab\n", "   " ]
>           , (1,0)
>           )
> 
>     , krebProp "#7" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxInsert (plainGlyph 'c')
>             , TextBoxCursorDown
>             ]
>           )
>           ( ["0 ", "  "]
>           , [ "abc", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#8" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph '\n')
>             ]
>           )
>           ( ["0 ", "1 "]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#9" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph '\n')
>             , TextBoxInsert (plainGlyph '\n')
>             ]
>           )
>           ( ["1 ", "2 "]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#10" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph '\n')
>             , TextBoxInsert (plainGlyph '\n')
>             , TextBoxInsert (plainGlyph '\n')
>             ]
>           )
>           ( ["2 ", "3 "]
>           , [ "\n  ", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#11" $
>         prop_TextBox_action_examples
>           ( (3,1)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxInsert (plainGlyph 'c')
>             , TextBoxInsert (plainGlyph 'd')
>             , TextBoxInsert (plainGlyph 'e')
>             , TextBoxInsert (plainGlyph 'f')
>             , TextBoxCursorDown
>             ]
>           )
>           ( [ "  " ]
>           , [ "   " ]
>           , (0,0)
>           )
> 
>     , krebProp "#12" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxInsert (plainGlyph 'c')
>             , TextBoxInsert (plainGlyph 'd')
>             , TextBoxInsert (plainGlyph 'e')
>             , TextBoxInsert (plainGlyph 'f')
>             , TextBoxInsert (plainGlyph 'g')
>             , TextBoxInsert (plainGlyph 'h')
>             , TextBoxInsert (plainGlyph 'i')
>             , TextBoxInsert (plainGlyph 'j')
>             , TextBoxInsert (plainGlyph 'k')
>             , TextBoxInsert (plainGlyph 'l')
>             , TextBoxCursorDown
>             ]
>           )
>           ( [ "  ", "  " ]
>           , [ "jkl", "   " ]
>           , (0,1)
>           )
> 
>     , krebProp "#13" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , []
>           )
>           ( [ "0 ", "  " ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , krebProp "#14" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxBackspace
>             ]
>           )
>           ( [ "0 ", "  " ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , krebProp "#15" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxBackspace
>             , TextBoxBackspace
>             ]
>           )
>           ( [ "0 ", "  " ]
>           , [ "   ", "   " ]
>           , (0,0)
>           )
> 
>     , krebProp "#16" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsert (plainGlyph 'a')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxBackspace
>             ]
>           )
>           ( [ "0 ", "  " ]
>           , [ "b  ", "   " ]
>           , (0,0)
>           )
> 
> 
>     , krebProp "#17" $
>         prop_TextBox_action_examples
>           ( (3,2)
>           , 1
>           , [ TextBoxInsertMany
>               [ plainGlyph 'a', plainGlyph 'b', plainGlyph 'c'
>               , plainGlyph 'd', plainGlyph 'e', plainGlyph 'f' ]
>             , TextBoxCursorUp
>             , TextBoxResize (3,1)
>             ]
>           )
>           ( [ "  " ]
>           , [ "def" ]
>           , (0,0)
>           )
> 
>     , krebProp "#18" $
>         prop_TextBox_action_examples
>           ( (8,2)
>           , 4
>           , [ TextBoxInsert (plainGlyph '\t')
>             , TextBoxInsert (plainGlyph 'b')
>             , TextBoxCursorLeft
>             , TextBoxCursorLeft
>             , TextBoxInsert (plainGlyph 'a')
>             ]
>           )
>           ( [ "0 ", "  " ]
>           , [ "a\tb     "
>             , "        " ]
>           , (1,0)
>           )
>     ]









 > instance Arb (TextBox_ m) where
 >   arb = TextBox_
 >     <$> arb
 >     <*> arb
 >     <*> arb
 >     <*> arb
 > 
 > instance Prune (TextBox_ m) where
 >   prune a = do
 >     acts <- prune $ _actions a
 >     x <- prune $ _x a
 >     y <- prune $ _y a
 >     t <- prune $ _t a
 >     return $ TextBox_ acts x y t

> 
> mk :: TextBox_ m -> TextBox
> mk (TextBox_ acts x' y' t') =
>   let
>     Positive x = x'
>     Positive y = y'
>     Positive t = t'
>   in mkTextBox (x,y) t acts
> 


> -}
