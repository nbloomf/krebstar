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

> data RSt = RSt

> renderGrapheme
>   :: RSt -> Pigment -> Pigment -> Char
>   -> Maybe LogicalLineNumber
>   -> ScreenLineNumber
>   -> ScreenColumnNumber
>   -> RenderedWidth
>   -> RenderedHeight
>   -> TabStopWidth
>   -> IsInsideRegion
>   -> IsLineStart
>   -> IsLineEnd
>   -> (Image, RSt)
> renderGrapheme s fg bg c _ _ col _ _ tab _ _ _ =
>   case c of
>     '\n' ->
>       ( string (withForeColor (withBackColor defAttr black) white) [' ']
>       , s
>       )
>     '\t' ->
>       let w = tab - rem col tab in
>       ( string (withForeColor (withBackColor defAttr black) white) (replicate w ' ')
>       , s
>       )
>     _ ->
>       ( string (withForeColor (withBackColor defAttr (getColor bg)) (getColor fg)) [c]
>       , s
>       )

> getColor :: Pigment -> Color
> getColor p =
>   let (r,g,b) = pigmentToRGB24 p
>   in rgbColor r g b

> dimChar :: Char -> Image
> dimChar c = string (defAttr `withForeColor` white) [c]

> plainChar :: Char -> Image
> plainChar c = string defAttr [c]



> renderState
>   :: ( Monad m )
>   => EditorMode -> AppState r m -> (Picture, Layout)
> renderState mode st =
>   case getActiveTab $ tabbedBuffers st of
>     Nothing -> (blankPicture, emptyLayout)
>     Just panel ->
>       let
>         tab = tabWidth st
> 
>         textL = textboxOffset $ textBox panel

>         RT labels labW text tDim@(textW, textH) tCursor =
>           renderTextBox RSt (process renderGrapheme) (textBox panel)
>         RT _ _ cmd cDim@(cmdW, cmdH) cCursor =
>           renderTextBox RSt (process renderGrapheme) (cmdBox panel)
>         RT _ _ hist hDim@(histW, histH) _ =
>           renderTextBox RSt (process renderGrapheme) (histBox panel)
>         RT _ _ stat sDim@(statW, statH) _ =
>           renderTextBox RSt (process renderGrapheme) (statusBox panel)

>         m = case mode of
>           InsertMode  -> horizCat $ map dimChar "INS "
>           CommandMode -> horizCat $ map dimChar "CMD "
>           NormalMode  -> horizCat $ map dimChar "NOR "

>         cropAndRender :: (Int, Int) -> [[(Image, Int)]] -> Image
>         cropAndRender (w, h) gss =
>           resizeHeight h $ vertCat $ map (resizeWidth w . horizCat)
>             $ map (map fst) gss

>         labelPaneLines = cropAndRender (labW, snd tDim) $ map (map (\(c,k) -> (dimChar c, k))) labels
>         (textPaneLines, (tcX', tcY')) = (cropAndRender tDim text, tCursor)
>         histPaneLines = cropAndRender hDim hist
>         (cmdPaneLines, (ccX', ccY')) = (cropAndRender cDim cmd, cCursor)
>         statPaneLine = horizJoin m $ cropAndRender sDim stat
>           
>         labelSep = replicate textH [dimChar '│']
>         histSep = concat
>           [ replicate histH [dimChar '│']
>           , [ [ dimChar '├' ] ]
>           , replicate cmdH [dimChar '│']
>           ]
>         statusSep  = concat
>           [ replicate labW (dimChar '═')
>           , [ dimChar '╧' ]
>           , replicate textW (dimChar '═')
>           , [ dimChar '╧' ]
>           , replicate histW (dimChar '═')
>           ]

>         cmdSep = replicate histW (dimChar '─')
> 
>         vsep = vertCat $ map horizCat labelSep
>         ssep = vertCat $ map horizCat histSep
>         hsep = horizCat cmdSep
>         tsep = horizCat statusSep

>         -- lay out the "windows"
>         mainPane = vertCat
>           [ horizCat
>             [ labelPaneLines
>             , vsep
>             , textPaneLines
>             , ssep
>             , vertCat
>               [ histPaneLines
>               , hsep
>               , cmdPaneLines
>               ]
>             ]
>           , tsep
>           , statPaneLine
>           ]
> 
>         -- calculate the screen cursor coordinates
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








