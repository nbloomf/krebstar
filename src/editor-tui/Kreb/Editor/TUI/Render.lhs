> module Kreb.Editor.TUI.Render (
>     imageAppState
> ) where

> import Graphics.Vty

> import Kreb.Text
> import Kreb.Editor.Core



> data RenderedPanel = RenderedPanel
>   { lineLabels :: ([[Glyph String]], (Int, Int))
>   , textLines  :: ([[Glyph String]], (Int, Int), (Int, Int))
>   , histLines  :: ([[Glyph String]], (Int, Int))
>   , cmdLines   :: ([[Glyph String]], (Int, Int), (Int, Int))
>   , statusLine :: ([[Glyph String]], (Int, Int))
>   } deriving (Eq, Show)

> updateRenderedPanel
>   :: GlyphRenderSettings
>   -> EditorMode
>   -> Int
>   -> Panel
>   -> RenderedPanel
> updateRenderedPanel settings mode tab panel =
>   let
>     textL = textboxOffset $ textBox panel

>     (labels, labW, text, tDim, tCursor) =
>       renderTextBox (textBox panel)

>     (_, _, cmd, cDim, cCursor) =
>       renderTextBox (cmdBox panel)

>     (_, _, hist, hDim, _) =
>       renderTextBox (histBox panel)
> 
>     (_, _, stat, sDim, _) =
>       renderTextBox (statusBox panel)
> 
>     m = case mode of
>       InsertMode -> map plainRune "INS "
>       CommandMode -> map plainRune "CMD "
>       NormalMode -> map plainRune "NOR "
> 
>     cropAndRender :: (Int, Int) -> [[(Glyph Char, Int)]] -> [[Glyph String]]
>     cropAndRender (w, h) gss =
>       take h $ (++ repeat (repeat $ plainRune ' ')) $
>         map (take w . concatMap (renderGlyph settings tab)) gss
> 
>   in RenderedPanel
>       { lineLabels = (cropAndRender (labW, snd tDim) labels, (labW, snd tDim))
>       , textLines  = (cropAndRender tDim text, tDim, tCursor)
>       , histLines  = (cropAndRender hDim hist, hDim)
>       , statusLine = (fmap (m ++) $ cropAndRender sDim stat, sDim)
>       , cmdLines   = (cropAndRender cDim cmd, cDim, cCursor)
>       }




> imageAppState
>   :: ( Monad m ) => AppState m -> Picture
> imageAppState st =
>   case getActiveTab $ tabbedBuffers st of
>     Nothing -> Picture (Cursor 0 0) [char defAttr ' '] (ClearBackground)
>     Just panel -> imageRenderedPanel (editorMode st) $ updateRenderedPanel (glyphRenderSettings st) (editorMode st) (tabWidth st) panel


> drawCell :: Glyph String -> Image
> drawCell (Glyph c fore back) =
>   string (withForeColor (withBackColor defAttr (getColor back)) (getColor fore)) c

> getColor :: Pigment -> Color
> getColor p =
>   let (r,g,b) = pigmentToRGB24 p
>   in rgbColor r g b

> imageRenderedPanel
>   :: EditorMode -> RenderedPanel -> Picture
> imageRenderedPanel mode rp =
>   let
>     (labelPaneLines, (labW, _)) = lineLabels rp
>     (textPaneLines, (textW, textH), (tcX, tcY)) = textLines rp
>     (histPaneLines, (histW, histH)) = histLines rp
>     (cmdPaneLines, (cmdW, cmdH), (ccX, ccY)) = cmdLines rp
>     (statPaneLine, (statW, statH)) = statusLine rp
> 
>     labelSep = replicate textH [dimRune '│']
>     histSep = concat
>       [ replicate histH [dimRune '│']
>       , [[dimRune '├']]
>       , replicate cmdH [dimRune '│']
>       ]
>     statusSep  = concat
>       [ replicate labW (dimRune '═')
>       , [dimRune '╧']
>       , replicate textW (dimRune '═')
>       , [dimRune '╧']
>       , replicate histW (dimRune '═')
>       ]
>     cmdSep = replicate histW (dimRune '─')
> 
>     vsep = vertCat $ map (horizCat . map drawCell) $ labelSep
>     ssep = vertCat $ map (horizCat . map drawCell) $ histSep
>     hsep = horizCat $ map drawCell $ cmdSep
>     tsep = horizCat $ map drawCell $ statusSep
> 
>     labels = vertCat $ map (horizCat . map drawCell) labelPaneLines
>     text = vertCat $ map (horizCat . map drawCell) textPaneLines
>     cmd  = vertCat $ map (horizCat . map drawCell) cmdPaneLines
>     hist = vertCat $ map (horizCat . map drawCell) histPaneLines
>     stat = vertCat $ map (horizCat . map drawCell) statPaneLine
> 
>     mainPane = vertCat
>       [ horizCat [ labels, vsep, text, ssep, vertCat [ hist, hsep, cmd ] ]
>       , tsep, stat
>       ]
> 
>     cursorPane = horizCat
>       [ pad 5 5 5 5 (drawCell phantomCursorRune)
>       ]
> 
>     cursor = case mode of
>       CommandMode -> Cursor (labW + 1 + textW + 1 + ccX) (histH + 1)
>       InsertMode -> Cursor (labW + 1 + tcX) (tcY)
>       NormalMode -> NoCursor
>   in Picture
>     cursor
>     [mainPane]
>     (ClearBackground)
