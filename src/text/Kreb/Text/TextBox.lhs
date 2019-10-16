---
title: Kreb.Text.TextBox
---



Contents
--------



Introduction
============

> {-# LANGUAGE
>     FlexibleContexts
>   , RecordWildCards
> #-}

> module Kreb.Text.TextBox (
>     TextBox(..)
>   , initTextBox
>   , mkTextBox
>   , textboxLabelWidth
> 
>   -- * Mutation
>   , alterTextBox
>   , TextBoxAction(..)
> 
>   -- * Queries
>   , getTextBoxWidth
>   , getTextBoxHeight
>   , getTextBoxTabStop
>   , getTextBoxBytes
>   , getTextBoxLineCol
>   , getTextBoxScreenCoords
>   , getTextBoxCursor
>   , getTextBoxOffset
>   , getTextBoxBuffer
>   , getTextBoxString

>   , getTextBoxFocusLineCol
>   , getTextBoxFocusScreenCoords

>   , setTextBoxHeight
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
> import Kreb.Text.Buffer
> import Kreb.Text.Glyph
> import Kreb.Text.Cell



A @TextBox@ is a rectangular array of cells that acts as a view into a @Buffer@, together with a distinguished cursor position that is used to interact with the buffer.

> data TextBox = TextBox
>   { textboxHeight     :: Int        -- ^ Height in cells
>   , textboxCursor     :: (Int, Int) -- ^ Relative cursor position
>   , textboxOffset     :: Int        -- ^ The top screen line index
>   , textboxBuffer     :: SizedBuffer Glyph
>   , textboxLabelBase  :: Int
>   , textboxSource     :: Maybe FilePath
>   , textboxHasChanged :: Bool
>   } deriving (Eq, Show)



> initTextBox
>   :: (Int, Int) -- ^ (Width, Height)
>   -> Int        -- ^ Tab
>   -> TextBox
> initTextBox (w,h) t = TextBox
>   { textboxHeight     = h
>   , textboxCursor     = (0,0)
>   , textboxBuffer     = emptySizedBuffer w t
>   , textboxOffset     = 0
>   , textboxLabelBase  = 10
>   , textboxSource     = Nothing
>   , textboxHasChanged = False
>   }



Queries
=======

> getTextBoxWidth
>   :: TextBox -> Int
> getTextBoxWidth box =
>   querySizedBuffer getBufferWidth
>     $ textboxBuffer box
> 
> getTextBoxTabStop
>   :: TextBox -> Int
> getTextBoxTabStop box =
>   querySizedBuffer getBufferTabStop
>     $ textboxBuffer box
> 
> getTextBoxBytes
>   :: TextBox -> Int
> getTextBoxBytes box =
>   querySizedBuffer getBufferBytes
>     $ textboxBuffer box
> 
> getTextBoxLineCol
>   :: TextBox -> LineCol
> getTextBoxLineCol box =
>   querySizedBuffer getBufferLineCol
>     $ textboxBuffer box
> 
> getTextBoxScreenCoords
>   :: TextBox -> (Int, Int)
> getTextBoxScreenCoords box =
>   querySizedBuffer getBufferScreenCoords
>     $ textboxBuffer box
> 
> getTextBoxString
>   :: TextBox -> String
> getTextBoxString box =
>   querySizedBuffer getBufferString
>     $ textboxBuffer box
> 
> getTextBoxHeight
>   :: TextBox -> Int
> getTextBoxHeight = textboxHeight
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
>   :: TextBox -> SizedBuffer Glyph
> getTextBoxBuffer = textboxBuffer

> getTextBoxHasChanged
>   :: TextBox -> Bool
> getTextBoxHasChanged = textboxHasChanged

> getTextBoxFocusLineCol
>   :: TextBox -> LineCol
> getTextBoxFocusLineCol box =
>   querySizedBuffer getBufferHeadLineCol
>     $ textboxBuffer box
> 
> getTextBoxFocusScreenCoords
>   :: TextBox -> (Int, Int)
> getTextBoxFocusScreenCoords box =
>   querySizedBuffer getBufferHeadScreenCoords
>     $ textboxBuffer box



> editTextBoxBuffer
>   :: (SizedBuffer Glyph -> SizedBuffer Glyph)
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
>   :: Cell Glyph -> Cell Glyph
> highlightRegion z = case z of
>   EOF -> Cell (Glyph ' ' (RuneColor HueBlack BrightnessDull) (RuneColor HueWhite BrightnessVivid))
>   Cell (Glyph c f _) -> Cell (Glyph c (RuneColor HueBlack BrightnessDull) (RuneColor HueWhite BrightnessVivid))


> renderTextBox
>   :: BufferRenderSettings
>   -> TextBox
>   -> ([[(Glyph, Int)]], Int, [[(Glyph, Int)]], (Int, Int), (Int, Int))
> renderTextBox opts box@TextBox{..} =
>   let
>     w = getTextBoxWidth box
>     (labels', lines') =
>       querySizedBuffer
>         (renderBuffer defaultBufferRenderSettings highlightRegion textboxOffset textboxHeight) $
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
>     showLabel :: Maybe Int -> [(Glyph, Int)]
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
>   alterTextBox acts $ initTextBox (x,y) tab




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
>   = TextBoxInsert Glyph
>   | TextBoxInsertMany [Glyph]
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
>   TextBoxInsert c ->
>     textBoxInsert c
> 
>   TextBoxInsertMany cs ->
>     textBoxInsertMany cs
> 
>   TextBoxBackspace ->
>     textBoxBackspace
> 
>   TextBoxCursorDown ->
>     textboxCursorDown
> 
>   TextBoxCursorUp ->
>     textboxCursorUp
> 
>   TextBoxCursorRight ->
>     textboxCursorRight
> 
>   TextBoxCursorLeft ->
>     textboxCursorLeft
> 
>   TextBoxResize dim ->
>     textboxResize dim
> 
>   TextBoxLoad path str ->
>     textboxLoad path str
> 
>   TextBoxLeaveMark ->
>     textboxLeaveMark
> 
>   TextBoxClearMark ->
>     textboxClearMark
> 
>   TextBoxClear ->
>     textboxClear





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
>       (getTextBoxWidth box) (getTextBoxHeight box) str
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
>       alterSizedBuffer (leaveMarkBuffer) $
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
>   :: Glyph
>   -> TextBox -> TextBox
> textBoxInsert c box =
>   let
>     (x,y) = textboxCursor box
>     l = textboxOffset box
>     h = textboxHeight box
> 
>     buf =
>       alterSizedBuffer (insertPointLeftBuffer c) $
>       alterSizedBuffer (splitBufferAtScreenCoords (x, y+l)) $
>       textboxBuffer box
> 
>     (u,v) =
>       querySizedBuffer getBufferHeadScreenCoords $
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
>   :: [Glyph]
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
>       alterSizedBuffer deletePointLeftBuffer $
>       alterSizedBuffer (splitBufferAtScreenCoords (x, y+l)) $
>       textboxBuffer box
>     
>     (u,v) =
>       querySizedBuffer getBufferHeadScreenCoords $
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
>       splitBufferAtScreenCoords (x,y+l+1)
> 
>     (u,v) <- readSt $
>       querySizedBuffer getBufferHeadScreenCoords
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
>       splitBufferAtScreenCoords (x,y+l-1)
> 
>     (u,v) <- readSt $
>       querySizedBuffer getBufferHeadScreenCoords
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
>       querySizedBuffer getBufferHeadScreenCoords $
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
>       querySizedBuffer getBufferHeadScreenCoords
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
>       querySizedBuffer getBufferHeadLineCol
>         . getTextBoxBuffer
> 
>     (_,v3) <- readSt getTextBoxCursor
> 
>     t <- readSt $
>       querySizedBuffer getBufferTabStop
>         . getTextBoxBuffer
> 
>     editSt $ editTextBoxBuffer $
>       alterSizedBuffer (splitBufferAtLineCol pos)
>         . shapeSizedBuffer w t
> 
>     (u2,v2) <- readSt $
>       querySizedBuffer getBufferHeadScreenCoords
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
>   { _labels :: [[(Glyph, Int)]]
>   , _lines  :: [[(Glyph, Int)]]
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
>     , "Buffer State:"
>     , querySizedBuffer
>         (show . toListDebugBuffer)
>         (textboxBuffer _box)
>     ]

> debugTextBox
>   :: TextBox
>   -> DebugTextBox
> debugTextBox box =
>   let (lb,_, ln, _, _) = renderTextBox defaultBufferRenderSettings box
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
