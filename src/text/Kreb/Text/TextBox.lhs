---
title: Text Boxes
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Queries](#queries): Getting information out of a text box
* [Rendering](#rendering): Representing the viewable region
* [Mutation](#mutation): Altering the text box
:::



::: frontmatter

> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE RecordWildCards #-}
> 
> module Kreb.Text.TextBox (
>     TextBox(..)
>   , emptyTextBox
>   , stringTextBox
> 
>   , getTextBoxWidth
>   , getTextBoxHeight
>   , getTextBoxTabStop
>   , getTextBoxBytes
>   , getTextBoxLineCol
>   , getTextBoxScreenCoords




>   , mkTextBox
>   , textboxLabelWidth
> 
>   -- * Mutation
>   , alterTextBox
>   , TextBoxAction(..)
> 
>   -- * Queries
>   , getTextBoxCursor
>   , getTextBoxOffset
>   , getTextBoxBuffer
>   , getTextBoxString

>   , getTextBoxFocusLineCol
>   , getTextBoxFocusScreenCoords

>   , setTextBoxOffset
>   , setTextBoxCursor

>   , editTextBoxBuffer

>   , textboxNearestScreenCoords

>   , renderTextBox

>   , DebugTextBox(..)
>   , debugTextBox
>   , writeDebugTextBoxActions
> ) where

> import Data.List (unlines)

> import Kreb.Check
> import Kreb.Control
> import Kreb.Text.MeasureText
> import Kreb.Text.ScreenOffset
> import Kreb.Text.Pigment
> import Kreb.Text.Buffer
> import Kreb.Text.Glyph
> import Kreb.Text.Cell
> import Kreb.Text.SizedBuffer

:::



Introduction
------------

There's a pretty big gap between a buffer data structure and a usable text editor. In a way the concrete data structure is the easy part, because it is all algorithms and data layout, while the gritty details of a usable text editor involve mostly _policy_ and _compromise_ -- it's more politics than engineering. We can still build up to the usable editor in layers, though, so that the policy choices are made at properly separate levels and as naturally as possible -- the code that interprets keystrokes can and should be independent of the code that draws to the screen, even though both of these activities are almost 100% policy decisions.

In this module we'll build the bottom layer of the editor cake. It may seem odd to call this the bottom layer since it depends on a ton of code already. But here for the first time a recognizable _editor API_ will start to emerge. Specifically, this module represents the basic text buffer data structure _as exposed to the editor_ as well as the operations that can be performed with it.

To that end, we need to draw some boundaries around what exactly we're trying to do here.

The `TextBox` structure should represent the following as generically as possible:

  1. A sized buffer of text
  2. A pointer to the _persistent source_ of the text, if one exists (e.g. the filename)
  3. A representation of the _screen view_ into the text
  4. A distinguished _cursor position_ measured in screen coordinates

We represent this information with the following type.

> data TextBox = TextBox
>   -- The sized buffer
>   { textboxBuffer     :: SizedBuffer (Glyph Char)
> 
>   -- The persistent source
>   , textboxSource     :: Maybe FilePath
>   , textboxHasChanged :: Bool
> 
>   -- The view
>   , textboxOffset     :: Int -- ^ The top screen line index
>   , textboxHeight     :: Int -- ^ Height in cells
>   , textboxLabelBase  :: Int -- ^ Numeric base of line labels
> 
>   -- The cursor
>   , textboxCursor     :: (Int, Int) -- ^ Relative cursor position
>   } deriving (Eq, Show)

The `Eq` and `Show` instances are just for testing. Under normal use we won't care about checking text boxes for equality, and the derived show instance is practically useless.

Definitely not useless are the functions for constructing text boxes. First we have the empty box:

> emptyTextBox
>   :: (Int, Int) -- ^ Screen dimensions: (Width, Height)
>   -> Int        -- ^ Tab width
>   -> TextBox
> emptyTextBox (w,h) t =
>   if (w <= 0) || (h <= 0) || (t <= 0) || (w < t)
>     then error "emptyTextBox: panic (invalid dimensions)"
>     else TextBox
>       { textboxHeight     = h
>       , textboxCursor     = (0,0)
>       , textboxBuffer     = emptySizedBuffer w t
>       , textboxOffset     = 0
>       , textboxLabelBase  = 10
>       , textboxSource     = Nothing
>       , textboxHasChanged = False
>       }

And we can build a text box out of an arbitrary string:

> stringTextBox
>   :: (Int, Int) -- ^ Screen dimensions: (Width, Height)
>   -> Int        -- ^ Tab width
>   -> String
>   -> TextBox
> stringTextBox (w,h) t str =
>   if (w <= 0) || (h <= 0) || (t <= 0) || (w < t)
>     then error "emptyTextBox: panic (invalid dimensions)"
>     else TextBox
>       { textboxHeight     = h
>       , textboxCursor     = (0,0)
>       , textboxBuffer     = makeSizedBuffer w t $ map plainGlyph str
>       , textboxOffset     = 0
>       , textboxLabelBase  = 10
>       , textboxSource     = Nothing
>       , textboxHasChanged = False
>       }



Queries
-------

Text boxes have lots of properties, and we'd like to know what those properties are.

We can extract the screen width of the box:

> getTextBoxWidth
>   :: TextBox -> Int
> getTextBoxWidth box =
>   querySizedBuffer getBufferWidth
>     $ textboxBuffer box

And an example for fun:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxWidth x
> -- :}
> -- 8

:::

We can extract the view height of the box:

> getTextBoxHeight
>   :: TextBox -> Int
> getTextBoxHeight = textboxHeight

And an example for fun:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxHeight x
> -- :}
> -- 3

:::

We can extract the tab stop width of the box:

> getTextBoxTabStop
>   :: TextBox -> Int
> getTextBoxTabStop box =
>   querySizedBuffer getBufferTabStop
>     $ textboxBuffer box

And an example for fun:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxTabStop x
> -- :}
> -- 2

:::

We can extract the number of bytes in the box:

> getTextBoxBytes
>   :: TextBox -> Int
> getTextBoxBytes box =
>   querySizedBuffer getBufferByteCount
>     $ textboxBuffer box

::: doctest

And an example for fun:

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxBytes x
> -- :}
> -- 12

:::

We can extract the logical size (line and column) of the box:

> getTextBoxLineCol
>   :: TextBox -> LineCol
> getTextBoxLineCol box =
>   querySizedBuffer getBufferLineCol
>     $ textboxBuffer box

::: doctest

And some examples for fun:

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxLineCol x
> -- :}
> -- l0c11
> --
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello there,\nworld!"
> -- in getTextBoxLineCol x
> -- :}
> -- l1c5

:::

We can extract the dimensions of the box in screen space:

> getTextBoxScreenCoords
>   :: TextBox -> (Int, Int)
> getTextBoxScreenCoords box =
>   querySizedBuffer getBufferScreenCoords
>     $ textboxBuffer box

And some examples for fun:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello world!"
> -- in getTextBoxScreenCoords x
> -- :}
> -- (4,1)
> --
> -- >>> :{
> -- let
> --   x = stringTextBox (8,3) 2
> --     "hello there,\nworld!"
> -- in getTextBoxScreenCoords x
> -- :}
> -- (6,2)

:::







> getTextBoxString
>   :: TextBox -> String
> getTextBoxString box =
>   querySizedBuffer (map toChar . toList)
>     $ textboxBuffer box
> 

> 
> getTextBoxCursor
>   :: TextBox -> (Int, Int)
> getTextBoxCursor = textboxCursor
> 
> getTextBoxOffset
>   :: TextBox -> Int
> getTextBoxOffset = textboxOffset
> 
> getTextBoxBuffer
>   :: TextBox -> SizedBuffer (Glyph Char)
> getTextBoxBuffer = textboxBuffer

> getTextBoxHasChanged
>   :: TextBox -> Bool
> getTextBoxHasChanged = textboxHasChanged

> getTextBoxFocusLineCol
>   :: TextBox -> LineCol
> getTextBoxFocusLineCol box =
>   querySizedBuffer getPointLineCol
>     $ textboxBuffer box
> 
> getTextBoxFocusScreenCoords
>   :: TextBox -> (Int, Int)
> getTextBoxFocusScreenCoords box =
>   querySizedBuffer getPointScreenCoords
>     $ textboxBuffer box
















> editTextBoxBuffer
>   :: (SizedBuffer (Glyph Char) -> SizedBuffer (Glyph Char))
>   -> TextBox -> TextBox
> editTextBoxBuffer f box =
>   box { textboxBuffer = f (textboxBuffer box) }



> setTextBoxHeight
>   :: Int -> TextBox -> TextBox
> setTextBoxHeight h box =
>   box { textboxHeight = h }

> setTextBoxOffset
>   :: Int -> TextBox -> TextBox
> setTextBoxOffset k box =
>   box { textboxOffset = k }

> setTextBoxCursor
>   :: (Int, Int) -> TextBox -> TextBox
> setTextBoxCursor pos box =
>   box { textboxCursor = pos }


> highlightRegion
>   :: Glyph Char -> Glyph Char
> highlightRegion z = case z of
>   Glyph c f _ -> Glyph c dullBlack vividWhite


> renderTextBox
>   :: TextBox
>   -> ([[(Glyph Char, Int)]], Int, [[(Glyph Char, Int)]], (Int, Int), (Int, Int))
> renderTextBox box@TextBox{..} =
>   let
>     w = getTextBoxWidth box
>     (labels', lines') =
>       querySizedBuffer
>         (renderScreenLinesWithRegion highlightRegion textboxOffset textboxHeight) $
>       textboxBuffer
> 
>     firstMaybe :: [Maybe a] -> Maybe a
>     firstMaybe xs = case xs of
>       [] -> Nothing
>       Nothing : ys -> firstMaybe ys
>       Just a : _ -> Just a
> 
>     labelW :: Int
>     labelW = case firstMaybe labels' of
>       Nothing -> 1
>       Just k -> 1 + length (show k)
> 
>     showLabel :: Maybe Int -> [(Glyph Char, Int)]
>     showLabel z = case z of
>       Nothing -> [(fromChar ' ', 0)]
>       Just k -> zip ((map fromChar (show k)) ++ [fromChar ' ']) [0..]
>   in
>     ( take textboxHeight $ (map showLabel labels') ++ repeat []
>     , labelW
>     , map (\ln -> take w $ ln ++ repeat (fromChar ' ', 1)) $
>         take textboxHeight $ lines' ++ repeat []
>     , (w, textboxHeight)
>     , textboxCursor
>     )

> -- | Width in character cells of the line labels for
> -- a @TextBox@.
> textboxLabelWidth :: TextBox -> Int
> textboxLabelWidth box =
>   let m = textboxOffset box in
>   1 + dlog (textboxLabelBase box) m


> mkTextBox
>   :: (Int, Int) -> Int
>   -> [TextBoxAction]
>   -> TextBox
> mkTextBox (x,y) tab acts =
>   alterTextBox acts $ emptyTextBox (x,y) tab




> -- | Digital logarithm; number of digits needed to express
> -- positive @k@ in base @b@.
> dlog :: Int -> Int -> Int
> dlog b k =
>   if (b <= 1) || (k < 0)
>     then error "dlog: bad argument"
>     else dlog' k
>   where
>     dlog' m = if m < b
>       then 1
>       else 1 + (dlog' $ div m b)


> textboxNearestScreenCoords
>   :: TextBox -> (Int, Int) -> (Int, Int)
> textboxNearestScreenCoords box coords =
>   querySizedBuffer
>     (seekScreenCoords coords)
>     (textboxBuffer box)




> data TextBoxAction
>   -- Text Manipulation
>   = TextBoxInsert (Glyph Char)
>   | TextBoxInsertMany [Glyph Char]
>   | TextBoxBackspace
> 
>   -- Navigation
>   | TextBoxCursorDown
>   | TextBoxCursorUp
>   | TextBoxCursorRight
>   | TextBoxCursorLeft
> 
>   | TextBoxLineStart
>   | TextBoxLineEnd
>   | TextBoxPageUp
>   | TextBoxPageDown
>   | TextBoxToTop
>   | TextBoxToBottom
> 
>   | TextBoxLeaveMark
>   | TextBoxClearMark
> 
>   -- 
>   | TextBoxResize (Int, Int)

>   | TextBoxLoad FilePath String
>   | TextBoxClear
>   deriving (Eq, Show)

> instance Arb TextBoxAction where
>   arb = selectFrom
>     [ TextBoxInsert <$> arb
>     , TextBoxInsertMany <$> arb
>     , return TextBoxBackspace
>     , return TextBoxCursorDown
>     , return TextBoxCursorUp
>     , return TextBoxCursorRight
>     , return TextBoxCursorLeft
>     , return TextBoxLeaveMark
>     , return TextBoxClearMark
>     , do
>         Positive w <- arb
>         Positive h <- arb
>         return $ TextBoxResize (w,h)
>     ]
> 
> instance Prune TextBoxAction where
>   prune x = case x of
>     TextBoxInsertMany cs ->
>       map TextBoxInsertMany $ prune cs
>     _ -> []

> instance Arb TextBox where
>   arb = do
>     Positive x <- arb
>     Positive y <- arb
>     Positive t <- arb
>     mkTextBox (x,y) t <$> arb




> alterTextBox
>   :: [TextBoxAction]
>   -> TextBox -> TextBox
> alterTextBox acts box =
>   foldl (flip alterTextBoxPrimitive) box acts



> alterTextBoxPrimitive
>   :: TextBoxAction
>   -> TextBox -> TextBox
> alterTextBoxPrimitive act = case act of
>   TextBoxInsert c      -> textBoxInsert c
>   TextBoxInsertMany cs -> textBoxInsertMany cs
>   TextBoxBackspace     -> textBoxBackspace
>   TextBoxCursorDown    -> textboxCursorDown
>   TextBoxCursorUp      -> textboxCursorUp
>   TextBoxCursorRight   -> textboxCursorRight
>   TextBoxCursorLeft    -> textboxCursorLeft
>   TextBoxResize dim    -> textboxResize dim
>   TextBoxLoad path str -> textboxLoad path str
>   TextBoxLeaveMark     -> textboxLeaveMark
>   TextBoxClearMark     -> textboxClearMark
>   TextBoxClear         -> textboxClear





-- ================= --
-- Primitive Actions --
-- ================= --

> textboxClear
>   :: TextBox -> TextBox
> textboxClear box = box
>   { textboxBuffer = emptySizedBuffer
>       (getTextBoxWidth box) (getTextBoxHeight box)
>   , textboxOffset = 0
>   , textboxCursor = (0,0)
>   , textboxSource = Nothing
>   , textboxHasChanged = False
>   }


> textboxLoad
>   :: FilePath -> String -> TextBox -> TextBox
> textboxLoad path str box = box
>   { textboxBuffer = makeSizedBuffer
>       (getTextBoxWidth box) (getTextBoxHeight box) (map fromChar str)
>   , textboxOffset = 0
>   , textboxCursor = (0,0)
>   , textboxSource = Just path
>   , textboxHasChanged = False
>   }


> textboxLeaveMark
>   :: TextBox -> TextBox
> textboxLeaveMark box =
>   let
>     buf =
>       alterSizedBuffer (leaveMark) $
>       textboxBuffer box
>   in box
>     { textboxBuffer = buf
>     }

> textboxClearMark
>   :: TextBox -> TextBox
> textboxClearMark box =
>   let
>     buf =
>       alterSizedBuffer (clearMark) $
>       textboxBuffer box
>   in box
>     { textboxBuffer = buf
>     }


> textBoxInsert
>   :: Glyph Char
>   -> TextBox -> TextBox
> textBoxInsert c box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
>     h = textboxHeight box
> 
>     buf =
>       alterSizedBuffer (insertPointLeft c) $
>       alterSizedBuffer (movePointToScreenCoords (x, y+l)) $
>       textboxBuffer box
> 
>     (u,v) =
>       querySizedBuffer getPointScreenCoords $
>       buf
> 
>     (v', l') = if v >= l+h
>       then (h-1, l+1)
>       else (v-l, l)
>   in box
>     { textboxBuffer = buf
>     , textboxCursor = (u,v')
>     , textboxOffset = l'
>     , textboxHasChanged = True
>     }

> textBoxInsertMany
>   :: [Glyph Char]
>   -> TextBox -> TextBox
> textBoxInsertMany cs box =
>   foldl (flip textBoxInsert) box cs

> textBoxBackspace
>   :: TextBox -> TextBox
> textBoxBackspace box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
> 
>     buf =
>       alterSizedBuffer deletePointLeft $
>       alterSizedBuffer (movePointToScreenCoords (x, y+l)) $
>       textboxBuffer box
>     
>     (u,v) =
>       querySizedBuffer getPointScreenCoords $
>       buf
> 
>     (v', l') =
>       if (v < l) && (l > 0)
>         then (0, l-1)
>         else (v-l, l)
> 
>   in if (x,y+l) == (0,0)
>     then box
>     else box
>       { textboxCursor = (u, v')
>       , textboxOffset = l'
>       , textboxBuffer = buf
>       , textboxHasChanged = True
>       }



> textboxCursorDown
>   :: TextBox -> TextBox
> textboxCursorDown box =
>   localSt box $ do
>     (x,y) <- readSt getTextBoxCursor
> 
>     l <- readSt getTextBoxOffset
>     h <- readSt getTextBoxHeight
> 
>     editSt $ editTextBoxBuffer $ alterSizedBuffer $
>       movePointToScreenCoords (x,y+l+1)
> 
>     (u,v) <- readSt $
>       querySizedBuffer getPointScreenCoords
>         . getTextBoxBuffer
> 
>     (_,q) <- readSt $
>       querySizedBuffer getBufferScreenCoords
>         . getTextBoxBuffer
> 
>     let
>       (v', l') =
>         if v >= l+h
>           then if v < q
>             then (h-1, l+1)
>             else (h-1, l)
>           else (v-l, l)
> 
>     editSt $ setTextBoxOffset l'
>     editSt $ setTextBoxCursor (u, v')

> textboxCursorUp
>   :: TextBox -> TextBox
> textboxCursorUp box =
>   localSt box $ do
>     (x,y) <- readSt getTextBoxCursor
>     l <- readSt getTextBoxOffset
> 
>     editSt $ editTextBoxBuffer $ alterSizedBuffer $
>       movePointToScreenCoords (x,y+l-1)
> 
>     (u,v) <- readSt $
>       querySizedBuffer getPointScreenCoords
>         . getTextBoxBuffer
> 
>     let
>       (v', l') =
>         if (v < l) && (l > 0)
>           then (0, l-1)
>           else (v-l, l)
> 
>     editSt $ setTextBoxOffset l'
>     editSt $ setTextBoxCursor (u, v')

> textboxCursorLeft
>   :: TextBox -> TextBox
> textboxCursorLeft box =
>   let
>     l = textboxOffset box
> 
>     buf =
>       alterSizedBuffer (movePointLeft) $
>         textboxBuffer box
>     
>     (u,v) =
>       querySizedBuffer getPointScreenCoords $
>       buf
> 
>     (v', l') =
>       if (v < l) && (l > 0)
>         then (0, l-1)
>         else (v-l, l)
>   in box
>     { textboxCursor = (u, v')
>     , textboxOffset = l'
>     , textboxBuffer = buf
>     }

> textboxCursorRight
>   :: TextBox -> TextBox
> textboxCursorRight box =
>   localSt box $ do
>     editSt $ editTextBoxBuffer $
>       alterSizedBuffer (movePointRight)
> 
>     (x,y) <- readSt getTextBoxCursor
> 
>     l <- readSt getTextBoxOffset
>     h <- readSt getTextBoxHeight
> 
>     (u,v) <- readSt $
>       querySizedBuffer getPointScreenCoords
>         . getTextBoxBuffer
> 
>     let
>       (v', l') =
>         if (v >= l+h)
>           then (h-1, l+1)
>           else (v-l, l)
> 
>     editSt $ setTextBoxCursor (u, v')
>     editSt $ setTextBoxOffset l'

Resizing should not change the logical coordinates of the focus.

> textboxResize
>   :: (Int, Int) -> TextBox -> TextBox
> textboxResize (w, h) box =
>   localSt box $ do
>     pos <- readSt $
>       querySizedBuffer getPointLineCol
>         . getTextBoxBuffer
> 
>     (_,v3) <- readSt getTextBoxCursor
> 
>     t <- readSt $
>       querySizedBuffer getBufferTabStop
>         . getTextBoxBuffer
> 
>     editSt $ editTextBoxBuffer $
>       alterSizedBuffer (movePointToLineCol pos)
>         . shapeSizedBuffer w t
> 
>     (u2,v2) <- readSt $
>       querySizedBuffer getPointScreenCoords
>         . getTextBoxBuffer
> 
>     let
>       l2 =
>         let z = max 0 (v2 - v3) in
>         if v2 >= z + h
>           then v2 - h + 1
>           else z
> 
>     editSt $ setTextBoxHeight h
>     editSt $ setTextBoxOffset l2
>     editSt $ setTextBoxCursor (u2, v2-l2)






> data DebugTextBox = DebugTextBox
>   { _labels :: [[(Glyph Char, Int)]]
>   , _lines  :: [[(Glyph Char, Int)]]
>   , _box    :: TextBox
>   }

> instance Show DebugTextBox where
>   show DebugTextBox{..} = unlines
>     [ "== DebugTextBox =="
>     , "Line Labels:"
>     , show _labels
>     , ""
>     , "Rendered Lines:"
>     , show _lines
>     , ""
>     , "TextBox State:"
>     , show _box
>     , ""
>     , "Buffer State: UNIMPLEMENTED"
>     ]

> debugTextBox
>   :: TextBox
>   -> DebugTextBox
> debugTextBox box =
>   let (lb,_, ln, _, _) = renderTextBox box
>   in DebugTextBox lb ln box

> debugTextBoxActions
>   :: (Int, Int) -> Int -> [TextBoxAction]
>   -> (DebugTextBox, [(TextBoxAction, Maybe DebugTextBox)])
> debugTextBoxActions dim tab acts =
>   let
>     box = mkTextBox dim tab []
>     f x as = case as of
>       [] -> []
>       b:bs ->
>         let
>           y = alterTextBoxPrimitive b x
>           z = if x == y
>             then Nothing
>             else Just $ debugTextBox y
>         in (b, z) : f y bs
>   in (debugTextBox box, f box acts)

> printDebugTextBoxActions
>   :: (DebugTextBox, [(TextBoxAction, Maybe DebugTextBox)]) -> String
> printDebugTextBoxActions (init, steps) = unlines
>   [ "=== START ==="
>   , show init
>   , ""
>   , p $ zip steps [1..]
>   ]
>   where
>     p xs = case xs of
>       [] -> ""
>       ((a,b),k):ys -> unlines
>         [ "=== THEN (" ++ show k ++ ") ==="
>         , show a
>         , ""
>         , case b of
>             Nothing -> ">>> No Change <<<"
>             Just z -> show z
>         , ""
>         , p ys
>         ]

> writeDebugTextBoxActions
>   :: FilePath
>   -> (Int, Int) -> Int -> [TextBoxAction]
>   -> IO ()
> writeDebugTextBoxActions path dim tab acts =
>   writeFile path
>     $ printDebugTextBoxActions
>     $ debugTextBoxActions dim tab acts
