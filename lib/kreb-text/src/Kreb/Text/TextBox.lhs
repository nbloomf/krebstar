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

> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE RecordWildCards #-}

> module Kreb.Text.TextBox (
>     TextBox(..)
>   , emptyTextBox
>   , stringTextBox
>   , getTextBoxString

>   , textboxWidth
>   , textboxFocusPointScreenCoords
>   , textboxFocusPointScreenOffset
>   , textboxFocusPointLineCol

>   , alterTextBoxM

>   , mkTextBox

>   , renderTextBox
>   , textboxLabelWidth
>   , DebugTextBox(..)
>   , debugTextBox

>   , RenderedTextBox(..)
>   , process

>   , evalEditT

>   , getHeight
>   , setHeight

>   , textboxInsert
>   , textboxInsertMany
>   , textboxBackspace
>   , textboxCursorLeft
>   , textboxCursorRight
>   , textboxCursorUp
>   , textboxCursorDown
>   , textboxResize
>   , textboxClear

>   , TextBoxFX(..)
>   , fxIO, fxMock

>   , expectedTextBoxDebug
>   , showTextBoxDebug
> ) where

> import System.IO (Handle)

> import Kreb.Arith
> import Kreb.Effect
> import Kreb.Control (Identity(..), concatM, LiftIO)

> import Kreb.Text.ScreenOffset
> import Kreb.Text.MeasureText
> import Kreb.Text.Pigment
> import Kreb.Text.Grapheme
> import Kreb.Text.BufferOp
> import Kreb.Text.Buffer
> import Kreb.Text.SizedBuffer

> import Kreb.Text.TextBox.EditsT
> import Kreb.Text.TextBox.Action

:::





Introduction
------------

There's a pretty big gap between a buffer data structure and a usable text editor. In a way the concrete data structure is the easy part, because it is all algorithms and data layout, while the gritty details of a usable text editor involve mostly _policy_ and _compromise_ -- it's more politics than engineering. We can still build up to the usable editor in layers, though, so that the policy choices are made at properly separate levels and as naturally as possible. For instance, the code that interprets keystrokes can and should be independent of the code that draws to the screen, even though both of these activities are almost 100% policy decisions.

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
>   { textboxBuffer     :: SizedBuffer
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
> 
>   -- The flags
>   , textboxDragMode   :: Bool
>   }

> deriving instance Eq TextBox
> deriving instance Show TextBox

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
>       , textboxDragMode   = False
>       }

And we can build a text box out of an arbitrary string:

> stringTextBox
>   :: EventId
>   -> (Int, Int) -- ^ Screen dimensions: (Width, Height)
>   -> Int        -- ^ Tab width
>   -> String
>   -> TextBox
> stringTextBox eId (w,h) t str =
>   if (w <= 0) || (h <= 0) || (t <= 0) || (w < t)
>     then error "emptyTextBox: panic (invalid dimensions)"
>     else TextBox
>       { textboxHeight     = h
>       , textboxCursor     = (0,0)
>       , textboxBuffer     = makeSizedBuffer eId w t str
>       , textboxOffset     = 0
>       , textboxLabelBase  = 10
>       , textboxSource     = Nothing
>       , textboxHasChanged = False
>       , textboxDragMode   = False
>       }

> getTextBoxString
>   :: TextBox -> String
> getTextBoxString =
>   querySizedBuffer toString . textboxBuffer

> textboxWidth
>   :: TextBox -> Int
> textboxWidth =
>   querySizedBuffer getBufferWidth . textboxBuffer

> textboxFocusPointScreenCoords
>   :: TextBox -> (Int, Int)
> textboxFocusPointScreenCoords =
>   querySizedBuffer getFocusPointScreenCoords . textboxBuffer

> textboxFocusPointScreenOffset
>   :: TextBox -> (Int, Int)
> textboxFocusPointScreenOffset =
>   querySizedBuffer getFocusPointScreenOffset . textboxBuffer

> textboxFocusPointLineCol
>   :: TextBox -> LineCol
> textboxFocusPointLineCol =
>   querySizedBuffer getFocusPointLineCol . textboxBuffer

> printTextBox_test
>   :: TextBox -> IO ()
> printTextBox_test box = do
>   let h = textboxHeight box
>   let l = textboxOffset box
>   putStr "> Cursor: " >> print (textboxCursor box)
>   putStrLn "> Lines:"
>   querySizedBuffer
>     (printRenderedBufferWith printGraphemePlain_test
>       . renderScreenLines l h)
>     (textboxBuffer box)

> showTextBoxDebug
>   :: TextBox -> ((Int, Int), [String])
> showTextBoxDebug box =
>   let
>     h = textboxHeight box
>     l = textboxOffset box
>     c = textboxCursor box
>     lns = querySizedBuffer
>       (showRenderedBufferDebug . renderScreenLines l h)
>       (textboxBuffer box)
>   in (c,lns)

> expectedTextBoxDebug
>   :: (Int, Int) -> [String] -> ((Int, Int),[String])
> expectedTextBoxDebug c ls = (c, ls)



Editing
-------

> type EditT m a = EditsT TextBox m a

> evalEditT
>   :: ( Monad m )
>   => TextBox -> EditT (Mock ev m) a -> m (a, TextBox, MockWorld ev)
> evalEditT box edits = do
>   (u,(v,w)) <- runMock (Chaos 0) (initMockWorld [])
>     $ evalEditsT logWriterMock (EventId 0 "") box edits
>   return (v,w,u)

> queryBuffer
>   :: forall m u
>    . ( Monad m )
>   => (forall w t d
>        . ( IsWidth w, IsTab t, IsBase d )
>       => Buffer w t d -> u)
>   -> EditT m u
> queryBuffer f = query
>   (querySizedBuffer f . textboxBuffer)
> 
> alterBuffer
>   :: forall m
>    . ( Monad m )
>   => (forall w t d
>        . ( IsWidth w, IsTab t, IsBase d )
>       => Buffer w t d -> Buffer w t d)
>   -> EditT m ()
> alterBuffer f = edit $ \st ->
>   st { textboxBuffer = alterSizedBuffer f $ textboxBuffer st }
> 
> alterBufferWithOp
>   :: forall m u
>    . ( Monad m )
>   => (forall w t d
>        . ( IsWidth w, IsTab t, IsBase d )
>       => Buffer w t d -> (Buffer w t d, BufferOp d))
>   -> EditT m BaseBufferOp
> alterBufferWithOp f = editWithOutput $ \st ->
>   let
>     (buf, u) = alterSizedBufferOp f (textboxBuffer st)
>   in return (st {
>     textboxBuffer = buf
>   }, u)

> mutateSizedBuffer
>   :: ( Monad m )
>   => (SizedBuffer -> SizedBuffer)
>   -> EditT m ()
> mutateSizedBuffer f = alterTextBox $ \box ->
>   let buf = textboxBuffer box
>   in box { textboxBuffer = f buf }

> queryTextBox
>   :: ( Monad m )
>   => (TextBox -> u)
>   -> EditT m u
> queryTextBox = query
> 
> alterTextBox
>   :: ( Monad m )
>   => (TextBox -> TextBox)
>   -> EditT m ()
> alterTextBox = edit



> getHeight :: ( Monad m ) => EditT m Int
> getHeight = queryTextBox textboxHeight
> 
> setHeight
>   :: ( Monad m ) => Int -> EditT m ()
> setHeight h = alterTextBox $ \box ->
>   box {
>     textboxHeight = h
>   }



> getWidth :: ( Monad m ) => EditT m Int
> getWidth = queryTextBox textboxWidth



> getCursor
>   :: ( Monad m )
>   => EditT m (Int, Int)
> getCursor = queryTextBox textboxCursor
> 
> setCursor
>   :: ( Monad m )
>   => (Int, Int) -> EditT m ()
> setCursor pos = alterTextBox $ \box ->
>   box { textboxCursor = pos }

> getOffset :: ( Monad m ) => EditT m Int
> getOffset = queryTextBox textboxOffset
> 
> setOffset
>   :: ( Monad m ) => Int -> EditT m ()
> setOffset k = alterTextBox $ \box ->
>   box { textboxOffset = k }

> getHasChanged :: ( Monad m ) => EditT m Bool
> getHasChanged = queryTextBox textboxHasChanged
> 
> setHasChanged
>   :: ( Monad m ) => Bool -> EditT m ()
> setHasChanged hasChanged = alterTextBox $ \box ->
>   box { textboxHasChanged = hasChanged }

> setBuffer
>   :: ( Monad m ) => SizedBuffer -> EditT m ()
> setBuffer buf = alterTextBox $ \box ->
>   box { textboxBuffer = buf }

> getSource :: ( Monad m ) => EditT m (Maybe FilePath)
> getSource = queryTextBox textboxSource
> 
> setSource
>   :: ( Monad m ) => FilePath -> EditT m ()
> setSource path = alterTextBox $ \box ->
>   box { textboxSource = Just path }
> 
> unsetSource
>   :: ( Monad m ) => EditT m ()
> unsetSource = alterTextBox $ \box ->
>   box { textboxSource = Nothing }





Textbox Actions
---------------

> textboxInsert
>   :: ( Monad m )
>   => Char
>   -> EditT m ()
> textboxInsert c = do
>   eId <- askEventId
>   (x,y) <- getCursor
>   l <- getOffset
>   h <- getHeight
>   w <- getWidth
> 
>   alterBuffer (moveFocusRegionToScreenOffset (x, y+l))
>   delta <- alterBufferWithOp (insertAtFocusPointLeft eId c)
>   writeLog "ins" Debug_ (showDebugBaseBufferOp delta)
>   (u,v) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     (v', l') = if v >= l+h
>       then (h-1, l+1)
>       else (v-l, l)
> 
>   setCursor (u,v')
>   setOffset l'
>   setHasChanged True





> textboxInsertMany
>   :: ( Monad m )
>   => [ Char ]
>   -> EditT m ()
> textboxInsertMany =
>   mapM_ textboxInsert





> textboxBackspace
>   :: ( Monad m )
>   => EditT m ()
> textboxBackspace = do
>   eId <- askEventId
>   (x,y) <- getCursor
>   l <- getOffset
> 
>   alterBuffer (moveFocusRegionToScreenOffset (x, y+l))
>   delta <- alterBufferWithOp deleteAtFocusPointLeft
>   writeLog "bksp" Debug_ (showDebugBaseBufferOp delta)
>   (u,v) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     (v', l') =
>       if (v < l) && (l > 0)
>         then (0, l-1)
>         else (v-l, l)
> 
>   setCursor (u,v')
>   setOffset l'
>   setHasChanged True





> textboxCursorLeft
>   :: ( Monad m )
>   => EditT m ()
> textboxCursorLeft = do
>   l <- getOffset
> 
>   alterBuffer (clearCursorsExceptFocus . moveFocusPointLeft)
>   (u,v) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     (v', l') =
>       if (v < l) && (l > 0)
>         then (0, l-1)
>         else (v-l, l)
> 
>   setCursor (u, v')
>   setOffset l'





> textboxCursorRight
>   :: ( Monad m )
>   => EditT m ()
> textboxCursorRight = do
>   l <- getOffset
>   h <- getHeight
> 
>   alterBuffer (clearCursorsExceptFocus . moveFocusPointRight)
>   (u,v) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     (v', l') =
>       if (v >= l+h)
>         then (h-1, l+1)
>         else (v-l, l)
> 
>   setCursor (u, v')
>   setOffset l'





> textboxCursorUp
>   :: ( Monad m )
>   => EditT m ()
> textboxCursorUp = do
>   (x,y) <- getCursor
>   l <- getOffset
> 
>   alterBuffer (moveFocusRegionToScreenOffset (x, y+l-1))
>   (u,v) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     (v', l') =
>       if (v < l) && (l > 0)
>         then (0, l-1)
>         else (v-l, l)
> 
>   setOffset l'
>   setCursor (u, v')





> textboxCursorDown
>   :: ( Monad m )
>   => EditT m ()
> textboxCursorDown = do
>   (x,y) <- getCursor
>   l <- getOffset
>   h <- getHeight
> 
>   alterBuffer (moveFocusRegionToScreenOffset (x, y+l+1))
>   (u,v) <- queryBuffer getFocusPointScreenOffset
>   (_,q) <- queryBuffer getBufferScreenCoords
> 
>   let
>     (v', l') =
>       if v >= l+h
>         then if v < q
>           then (h-1, l+1)
>           else (h-1, l)
>         else (v-l, l)
> 
>   setOffset l'
>   setCursor (u, v')





> textboxResize
>   :: ( Monad m )
>   => (Int, Int)
>   -> EditT m ()
> textboxResize (w, h) = do
>   pos <- queryBuffer getFocusPointLineColOffset
>   (_,v3) <- getCursor
>   t <- queryBuffer getBufferTabStop
> 
>   mutateSizedBuffer (shapeSizedBuffer w t)
> 
>   alterBuffer (moveFocusRegionToLineColOffset pos)
>   (u2,v2) <- queryBuffer getFocusPointScreenOffset
> 
>   let
>     l2 =
>       let z = max 0 (v2 - v3) in
>       if v2 >= z + h
>         then v2 - h + 1
>         else z
> 
>   setHeight h
>   setOffset l2
>   setCursor (u2, v2 - l2)





> textboxClear
>   :: ( Monad m )
>   => EditT m ()
> textboxClear = do
>   w <- getWidth
>   h <- getHeight
>   setBuffer (emptySizedBuffer w h)
>   setOffset 0
>   setCursor (0,0)
>   unsetSource
>   setHasChanged False
















> data TextBoxFX m = TextBoxFX
>   { logWriter       :: LogWriter m
>   , fileWriter      :: FileWriter m
>   , fileReader      :: FileReader m
>   , clipboardWriter :: ClipboardWriter m
>   , clipboardReader :: ClipboardReader m
>   }

> fxIO :: ( LiftIO m ) => Handle -> TextBoxFX m
> fxIO logHandle = TextBoxFX
>   { logWriter       = logWriterIO logHandle
>   , fileWriter      = fileWriterIO
>   , fileReader      = fileReaderIO
>   , clipboardWriter = clipboardWriterIO
>   , clipboardReader = clipboardReaderIO
>   }

> fxMock :: (Monad m) => TextBoxFX (Mock ev m)
> fxMock = TextBoxFX
>   { logWriter       = logWriterMock
>   , fileWriter      = fileWriterMock
>   , fileReader      = fileReaderMock
>   , clipboardWriter = clipboardWriterMock
>   , clipboardReader = clipboardReaderMock
>   }



> alterTextBoxM
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> [TextBoxAction]
>   -> TextBox -> m TextBox
> alterTextBoxM fx eId acts =
>   concatM (map (alterTextBoxPrimitive fx eId) acts)



> alterTextBoxPrimitive
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> TextBoxAction
>   -> TextBox -> m TextBox
> alterTextBoxPrimitive fx eId act box =
>   runEditsT (logWriter fx) eId box $ case act of
>     TextBoxInsert c            -> textboxInsert c
>     TextBoxInsertMany cs       -> textboxInsertMany cs
>     TextBoxBackspace           -> textboxBackspace
> 
>     TextBoxCursorDown          -> textboxCursorDown
>     TextBoxCursorUp            -> textboxCursorUp
>     TextBoxCursorRight         -> textboxCursorRight
>     TextBoxCursorLeft          -> textboxCursorLeft
> 
>     TextBoxResize dim          -> textboxResize dim
>     TextBoxClear               -> textboxClear

>     _ -> return ()

 >   TextBoxCursorTo pos        -> textboxCursorTo pos
 > 
 >   TextBoxLeaveMark           -> textboxLeaveMark
 >   TextBoxClearMark           -> textboxClearMark
 > 
 >   TextBoxCursorDrag pos      -> textboxCursorDrag pos
 >   TextBoxCancelDrag          -> textboxCancelDrag
 > 
 >   TextBoxDeleteRegion        -> textboxDeleteRegion
 >   TextBoxClipRegion writer   -> textboxClipRegion writer
 >   TextBoxPasteRegion reader  -> textboxPasteRegion reader
 > 
 >   TextBoxLoad force reader   -> textboxLoad force reader
 >   TextBoxSave writer         -> textboxSave writer
 >   TextBoxSetSource path      -> textboxSetSource path
 > 












> -- | Width in character cells of the line labels for
> -- a @TextBox@.
> textboxLabelWidth :: TextBox -> Int
> textboxLabelWidth box =
>   let m = textboxOffset box in
>   1 + dlog (textboxLabelBase box) m

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


> data RenderedTextBox a = RT
>   { rtLineLabels :: [[(Char, Int)]]
>   , rtLabelWidth :: Int
>   , rtContents   :: [[(a, Int)]]
>   , rtDimensions :: (Int, Int)
>   , rtCursorPos  :: (Int, Int)
>   } deriving (Eq, Show)

> type PrintGrapheme s a
>    = forall w t d
>    . s -> Pigment -> Pigment -> Char
>   -> Maybe LogicalLineNumber
>   -> ScreenLineNumber
>   -> ScreenColumnNumber
>   -> RenderedWidth
>   -> RenderedHeight
>   -> TabStopWidth
>   -> IsInsideRegion
>   -> IsLineStart
>   -> IsLineEnd
>   -> (a, s)

> process
>   :: forall w t d s a
>    . ( IsWidth w, IsTab t, IsBase d )
>   => PrintGrapheme s a
>   -> ProcRenderedBufferEntry s (Grapheme w t d) a
> process f ll ln cn wid ht tab ins ls le s g =
>   let (c, fg, bg) = getColorfulGraphemeChar g
>   in f s fg bg c ll ln cn wid ht tab ins ls le

> processPlain
>   :: forall w t d s
>    . ( IsWidth w, IsTab t, IsBase d )
>   => ProcRenderedBufferEntry s (Grapheme w t d) String
> processPlain _ _ _ _ _ _ _ _ _ s g = ([getGraphemeChar g], s)


> renderTextBox
>   :: s
>   -> (forall w t d. (IsWidth w, IsTab t, IsBase d)
>       => ProcRenderedBufferEntry s (Grapheme w t d) a)
>   -> TextBox
>   -> RenderedTextBox a
> renderTextBox s proc box@TextBox{..} =
>   let
>     w = querySizedBuffer getBufferWidth textboxBuffer
> 
>     (theLabels, theLines) =
>       querySizedBuffer
>         (getRenderedBufferLines
>           . fst . mungeRenderedBuffer s proc
>           . renderScreenLines textboxOffset textboxHeight)
>         textboxBuffer
> 
>     rtLineLabels =
>       let
>         showLabel :: Maybe Int -> [(Char, Int)]
>         showLabel z = case z of
>           Nothing -> take (1 + rtLabelWidth) $ zip (repeat ' ') [0..]
>           Just k -> zip ((show k) ++ " ") [0..]
>       in take textboxHeight $ (map showLabel theLabels)
>         ++ repeat (zip (replicate (1 + rtLabelWidth) ' ') [0..])
> 
>     rtLabelWidth =
>       let
>         lastJust :: [Maybe Int] -> Maybe Int
>         lastJust xs = case filter (== Nothing) $ reverse xs of
>           [] -> Nothing
>           u:_ -> u
>       in case lastJust theLabels of
>         Nothing -> 1
>         Just k -> 1 + length (show k)
> 
>     rtContents =
>       map (take w)
>         $ take textboxHeight theLines
> 
>     rtDimensions = (w, textboxHeight)
> 
>     rtCursorPos = textboxCursor
> 
>   in RT {..}



> data DebugTextBox = DebugTextBox
>   { _labels :: [[(Char, Int)]]
>   , _lines  :: [[(String, Int)]]
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
>   let RT lb _ ln _ _ = renderTextBox () processPlain box
>   in DebugTextBox lb ln box



> mkTextBox
>   :: ( Monad m )
>   => TextBoxFX m -> EventId -> (Int, Int) -> Int
>   -> [TextBoxAction]
>   -> m TextBox
> mkTextBox fx eId (x,y) tab acts =
>   alterTextBoxM fx eId acts $ emptyTextBox (x,y) tab










> {-

> 

>   , mkTextBox
>   , textboxLabelWidth
> 
>   -- * Mutation
>   , alterTextBoxM
>   , TextBoxAction(..)
> 
>   -- * Queries
>   , getTextBoxString

>   , renderTextBox

>   , DebugTextBox(..)
>   , debugTextBox
> ) where

> import Data.List (unlines)

> import Kreb.Prop
> import Kreb.Effect
> import Kreb.Control
> import Kreb.Arith
> import Kreb.Text.ScreenOffset
> import Kreb.Text.Pigment
> import Kreb.Text.Grapheme
> import Kreb.Text.SizedBuffer







Queries
-------

> 






> putEditT
>   :: ( Monad m )
>   => EditSt
>   -> EditT m ()
> putEditT = EditT . lift . put








> getWidth
>   :: ( Monad m ) => EditT m Int
> getWidth = queryBuffer getBufferWidth



















> getDragMode :: ( Monad m ) => EditT m Bool
> getDragMode = queryTextBox textboxDragMode
> 
> setDragMode
>   :: ( Monad m ) => Bool -> EditT m ()
> setDragMode dragMode = alterTextBox $ \box ->
>   box {
>     textboxDragMode = dragMode
>   }




























> textboxLeaveMark
>   :: ( Monad m )
>   => EditT m ()
> textboxLeaveMark =
>   alterBuffer leaveMark

> textboxClearMark
>   :: ( Monad m )
>   => EditT m ()
> textboxClearMark =
>   alterBuffer clearMark



> textboxCursorDrag
>   :: ( Monad m )
>   => (Int, Int)
>   -> EditT m ()
> textboxCursorDrag (x,y) = do
>   l <- getOffset
> 
>   alterBuffer (movePointToScreenCoords (x, y+l))
>   (u,v) <- queryBuffer getFocusScreenCoords
> 
>   -- TODO: use this to scroll
>   let (u', v') = (u, v)
>   let l' = l
> 
>   q <- getDragMode
> 
>   if q
>     then return ()
>     else alterBuffer (leaveMark . clearMark)
> 
>   setCursor (u', v' - l')
>   setOffset l'
>   setDragMode True



> textboxCancelDrag
>   :: ( Monad m )
>   => EditT m ()
> textboxCancelDrag = do
>   p <- queryBuffer isCoincident
> 
>   if not p
>     then return ()
>     else alterBuffer clearMark
> 
>   setDragMode False

> textboxDeleteRegion
>   :: ( Monad m )
>   => EditT m ()
> textboxDeleteRegion = do
>   eId <- askEventId
>   p <- queryBuffer hasMark

>   let
>     cut b = case cutRegion eId b of
>       Nothing -> (b, [])
>       Just (u,v,delta) -> (v, u)
> 
>   if not p
>     then return ()
>     else do
>       l <- getOffset
>       str <- alterBuffer' cut
>       (u,v) <- queryBuffer getFocusScreenCoords
>       setCursor (u, v - l)

> textboxClipRegion
>   :: ( Monad m )
>   => ClipboardWriter m
>   -> EditT m ()
> textboxClipRegion writer = do
>   region <- queryBuffer copyRegion 
>   case region of
>     Nothing ->
>       return ()
>     Just str -> do
>       lift $ writeClipboardWith writer $ map toChar str
>       return ()

> textboxPasteRegion
>   :: ( Monad m )
>   => ClipboardReader m
>   -> EditT m ()
> textboxPasteRegion reader = do
>   eId <- askEventId
>   z <- lift $ readClipboardWith reader
>   case z of
>     Left err -> error "textboxPasteRegion: panic"
>     Right str -> do
>       _ <- alterBuffer' (insertRegion eId str)
>       return ()



> textboxLoad
>   :: ( Monad m )
>   => Bool -> FileReader m
>   -> EditT m ()
> textboxLoad force reader = do
>   eId <- askEventId
>   p <- getHasChanged
>   if p
>     then error "unsaved changes"
>     else do
>       result <- getSource
>       case result of
>         Nothing -> error "textboxLoad no path"
>         Just path -> do
>           read <- lift $ readFileWith reader path
>           case read of
>             Left err -> error "read error"
>             Right content -> do
>               w <- getWidth
>               h <- getHeight
>               setBuffer (makeSizedBuffer eId w h content)
>               setOffset 0
>               setCursor (0,0)
>               setSource path
>               setHasChanged False

> textboxSave
>   :: ( Monad m )
>   => FileWriter m
>   -> EditT m ()
> textboxSave writer = do
>   return ()

> textboxSetSource
>   :: ( Monad m )
>   => FilePath
>   -> EditT m ()
> textboxSetSource path = do
>   return ()







> textboxCursorTo
>   :: ( Monad m )
>   => (Int, Int)
>   -> EditT m ()
> textboxCursorTo pos =
>   return ()












> setTextBoxCursor
>   :: (Int, Int) -> TextBox -> TextBox
> setTextBoxCursor pos box =
>   box { textboxCursor = pos }








> textboxNearestScreenCoords
>   :: TextBox -> (Int, Int) -> (Int, Int)
> textboxNearestScreenCoords box coords =
>   querySizedBuffer
>     (seekScreenCoords coords)
>     (textboxBuffer box)








 > instance Arb TextBox where
 >   arb = do
 >     Positive x <- arb
 >     Positive y <- arb
 >     Positive t <- arb
 >     eId <- arb
 >     mkTextBox eId (x,y) t <$> arb








-- ================= --
-- Primitive Actions --
-- ================= --



Move the cursor; if no mark is set, then leave the mark



Resizing should not change the logical coordinates of the focus.





 > debugTextBoxActions
 >   :: EventId -> (Int, Int) -> Int -> [TextBoxAction]
 >   -> (DebugTextBox, [(TextBoxAction, Maybe DebugTextBox)])
 > debugTextBoxActions eId dim tab acts =
 >   let
 >     box = mkTextBox eId dim tab []
 >     f x as = case as of
 >       [] -> []
 >       b:bs ->
 >         let
 >           y = alterTextBoxPrimitive eId b x
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
 >   :: EventId -> FilePath
 >   -> (Int, Int) -> Int -> [TextBoxAction]
 >   -> IO ()
 > writeDebugTextBoxActions eId path dim tab acts =
 >   writeFile path
 >     $ printDebugTextBoxActions
 >     $ debugTextBoxActions eId dim tab acts

> -}
