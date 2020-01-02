> module Kreb.Editor.TUI.Render (
>     renderState
> ) where

> import Graphics.Vty

> import Kreb.Text
> import Kreb.Editor.Core

> import Kreb.Editor.TUI.Data.Layout

> blankPicture :: Picture
> blankPicture =
>   Picture (Cursor 0 0) [char defAttr ' '] (ClearBackground)

> renderState
>   :: ( Monad m )
>   => EditorMode -> AppState m -> (Picture, Layout)
> renderState mode st =
>   case getActiveTab $ tabbedBuffers st of
>     Nothing -> (blankPicture, emptyLayout)
>     Just panel ->

>       let

>         settings = glyphRenderSettings st
>         tab = tabWidth st
> 
>         textL = textboxOffset $ textBox panel

>         (labels, labW, text, tDim@(textW, textH), tCursor) =
>           renderTextBox (textBox panel)

>         (_, _, cmd, cDim@(cmdW, cmdH), cCursor) =
>           renderTextBox (cmdBox panel)

>         (_, _, hist, hDim@(histW, histH), _) =
>           renderTextBox (histBox panel)
> 
>         (_, _, stat, sDim@(statW, statH), _) =
>           renderTextBox (statusBox panel)

>         m = case mode of
>           InsertMode -> map plainRune "INS "
>           CommandMode -> map plainRune "CMD "
>           NormalMode -> map plainRune "NOR "

>         cropAndRender :: (Int, Int) -> [[(Glyph Char, Int)]] -> [[Glyph String]]
>         cropAndRender (w, h) gss =
>           take h $ (++ repeat (repeat $ plainRune ' ')) $
>             map (take w . concatMap (renderGlyph settings tab)) gss

>         labelPaneLines = cropAndRender (labW, snd tDim) labels
>         (textPaneLines, (tcX', tcY')) = (cropAndRender tDim text, tCursor)
>         histPaneLines = cropAndRender hDim hist
>         (cmdPaneLines, (ccX', ccY')) = (cropAndRender cDim cmd, cCursor)
>         statPaneLine = fmap (m ++) $ cropAndRender sDim stat
>           
>         labelSep = replicate textH [dimRune '│']
>         histSep = concat
>           [ replicate histH [dimRune '│']
>           , [[dimRune '├']]
>           , replicate cmdH [dimRune '│']
>           ]
>         statusSep  = concat
>           [ replicate labW (dimRune '═')
>           , [dimRune '╧']
>           , replicate textW (dimRune '═')
>           , [dimRune '╧']
>           , replicate histW (dimRune '═')
>           ]

>         cmdSep = replicate histW (dimRune '─')
> 
>         vsep = vertCat $ map (horizCat . map drawCell) $ labelSep
>         ssep = vertCat $ map (horizCat . map drawCell) $ histSep
>         hsep = horizCat $ map drawCell $ cmdSep
>         tsep = horizCat $ map drawCell $ statusSep

>         labels' = vertCat $ map (horizCat . map drawCell) labelPaneLines
>         text' = vertCat $ map (horizCat . map drawCell) textPaneLines
>         cmd'  = vertCat $ map (horizCat . map drawCell) cmdPaneLines
>         hist' = vertCat $ map (horizCat . map drawCell) histPaneLines
>         stat' = vertCat $ map (horizCat . map drawCell) statPaneLine

>         mainPane = vertCat
>           [ horizCat [ labels', vsep, text', ssep, vertCat [ hist', hsep, cmd' ] ]
>           , tsep, stat'
>           ]
> 
>         cursorPane = horizCat
>           [ pad 5 5 5 5 (drawCell phantomCursorRune)
>           ]
> 
>         cursor = case mode of
>           CommandMode -> Cursor (labW + 1 + textW + 1 + ccX') (histH + 1)
>           InsertMode -> Cursor (labW + 1 + tcX') (tcY')
>           NormalMode -> NoCursor

>         picture = Picture cursor [mainPane] (ClearBackground)

>         layout = Layout
>           { _textRect   = Rect (labW+1) 0 textW textH
>           , _cmdRect    = Rect (labW+1+textW+1) (histH+1) cmdW cmdH
>           , _histRect   = Rect (labW+1+textW+1) 0 histW histH
>           , _statusRect = Rect 0 (textH+1) statW statH
>           }

>       in (picture, layout)






> drawCell :: Glyph String -> Image
> drawCell (Glyph c fore back) =
>   string (withForeColor (withBackColor defAttr (getColor back)) (getColor fore)) c

> getColor :: Pigment -> Color
> getColor p =
>   let (r,g,b) = pigmentToRGB24 p
>   in rgbColor r g b

