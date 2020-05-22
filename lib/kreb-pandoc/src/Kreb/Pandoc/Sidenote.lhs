> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Pandoc.Sidenote where

> import           Data.List           (intercalate)
> import qualified Data.Text as T (Text, append, pack)

> import           Control.Monad.State

> import qualified Text.Pandoc as P
> import qualified Text.Pandoc.Walk as P (walk, walkM)

Adapted from https://github.com/jez/pandoc-sidenote


> getFirstStr :: [P.Inline] -> Maybe T.Text
> getFirstStr x = case x of
>   [] -> Nothing
>   (P.Str text):_ -> Just $ T.pack text
>   _:ls -> getFirstStr ls

> newline :: [P.Inline]
> newline = [P.LineBreak, P.LineBreak]

-- Extract inlines from blocks

> coerceToInline :: [P.Block] -> [P.Inline]
> coerceToInline = concatMap deBlock . P.walk deNote
>   where
>     deNote (P.Note _) = P.Str ""
>     deNote x        = x
> 
>     deBlock :: P.Block -> [P.Inline]
>     deBlock (P.Plain     ls    ) = ls
>     -- Simulate paragraphs with double LineBreak
>     deBlock (P.Para      ls    ) = ls ++ newline
>     -- See extension: line_blocks
>     deBlock (P.LineBlock lss   ) = intercalate [P.LineBreak] lss ++ newline
>     -- Pretend RawBlock is RawInline (might not work!)
>     -- Consider: raw <div> now inside RawInline... what happens?
>     deBlock (P.RawBlock fmt str) = [P.RawInline fmt str]
>     -- lists, blockquotes, headers, hrs, and tables are all omitted
>     -- Think they shouldn't be? I'm open to sensible PR's.
>     deBlock _                  = []

> filterInline :: P.Inline -> State Int P.Inline
> filterInline (P.Note blocks) = do
>   -- Generate a unique number for the 'for=' attribute
>   i <- getThenIncr

>   -- Note has a [P.Block], but Span needs [Inline]
>   let content  = coerceToInline blocks

>   -- The '{-}' symbol differentiates between margin note and side note
>   let nonu     = getFirstStr content == Just "{-}"
>   let content' = if nonu then tail content else content

>   let labelCls = "margin-toggle" ++
>                  (if nonu then "" else " sidenote-number")
>   let labelSym = if nonu then "&#8853;" else ""
>   let labelHTML = mconcat
>          [ "<label for=\"sn-"
>          , (show i)
>          , "\" class=\""
>          , labelCls
>          , "\">"
>          , labelSym
>          , "</label>"
>          ]
>   let label = P.RawInline (P.Format "html") labelHTML

>   let inputHTML = mconcat
>         [ "<input type=\"checkbox\" id=\"sn-"
>         , (show i)
>         , "\" "
>         , "class=\"margin-toggle\"/>"
>         ]
>   let input             = P.RawInline (P.Format "html") inputHTML

>   let (ident, _, attrs) = P.nullAttr
>   let noteTypeCls       = if nonu then "marginnote" else "sidenote"
>   let note              = P.Span (ident, [noteTypeCls], attrs) content'

>   return $ P.Span P.nullAttr [label, input, note]
> filterInline inline = return inline

> getThenIncr :: State Int Int
> getThenIncr = do
>   i <- get
>   put (i + 1)
>   return i

> usingSideNotes :: P.Pandoc -> P.Pandoc
> usingSideNotes (P.Pandoc meta blocks) =
>   P.Pandoc meta (evalState (P.walkM filterInline blocks) 0)

> convertSidenotes
>   :: ( P.PandocMonad m )
>   => P.Pandoc -> m P.Pandoc
> convertSidenotes = return . usingSideNotes
