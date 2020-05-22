---
title: A Document Algebra
author: nbloomf
---

::: frontmatter

> module Kreb.Format.Combinator (
>     emptyDoc, char, space, spaces, (<+>), annotate
>   , enclose, parens, braces, bracks, squote, dquote, surround
>   , lineS, lineE, softlineS, softlineE, hardline
>   , nest, align, hang, indent
>   , group, flatAlt
>   , hcat, hsep, vcat, vsep, fillCat, fillSep, cat, sep
>   , text, string, reflow
>   , withColumn, withPageWidth, withNesting, width, fill, fillBreak
>   , ifColumn
>   , encloseSep, brackList, braceList, parenList, punctuate
> ) where
> 
> import           Data.String
> import qualified Data.Text as T
> import qualified Data.List as L
> 
> import Kreb.Format.Util
> import Kreb.Format.Doc

:::



Introduction
------------

We've defined a type `Doc` of abstract annotated document layouts and the machinery needed to efficiently render them. In this module we define a more convenient combinator library for constructing `Doc`s. This is also a lightly edited copy of the corresponding code from the [prettyprinter](http://hackage.haskell.org/package/prettyprinter) library.



Primitives
----------

The simplest document has nothing in it.

> emptyDoc
>   :: Doc a
> emptyDoc = Empty

Note that `emptyDoc` is a document with width 0 and height 1. This may be surprising if we expect it to have height 0, for instance when vertically concatenating documents.

The next simplest document has a single character in it. Note that we capture newlines here as a special case to maintain the invariant on `Doc`.

> char :: Char -> Doc a
> char c = case c of
>   '\n' -> Line
>   _    -> Char c

We can also define spaces as documents. Note that these are _content_ whitespace, rather than _layout_ whitespace.

> space :: Doc a
> space = Char ' '
> 
> spaces :: Int -> Doc a
> spaces n = case compare n 1 of
>   LT -> Empty
>   EQ -> Char ' '
>   GT -> Text n (textSpaces n)

Recall that `Doc` is a (semantic!) monoid, and that (<>) is horizontal concatenation.

> (<+>) :: Doc a -> Doc a -> Doc a
> x <+> y = x <> space <> y
> infixr 6 <+> -- same as <>

We can use this to assemble some basic examples.

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   emptyDoc
> -- :}
> -- 
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a'
> -- :}
> -- a
> -- 
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> char 'b'
> -- :}
> -- ab
> -- 
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> emptyDoc <> char 'b'
> -- :}
> -- ab
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> space <> char 'b'
> -- :}
> -- a b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> spaces 4 <> char 'b'
> -- :}
> -- a    b

:::

With just the tools on hand we can define some common convenience functions. `enclose` wraps its third argument between the first two using `<>`.

> enclose
>   :: Doc a -- left side
>   -> Doc a -- right side
>   -> Doc a -- contents
>   -> Doc a
> enclose lhs rhs x =
>   lhs <> x <> rhs

A common example of this pattern is wrapping text 

> parens :: Doc a -> Doc a
> parens = enclose (char '(') (char ')')
> 
> braces :: Doc a -> Doc a
> braces = enclose (char '{') (char '}')
> 
> bracks :: Doc a -> Doc a
> bracks = enclose (char '[') (char ']')
> 
> squote :: Doc a -> Doc a
> squote = enclose (char '\'') (char '\'')
> 
> dquote :: Doc a -> Doc a
> dquote = enclose (char '"') (char '"')

`surround` is an argument-reordering of `enclose`, which is handy for treating "concatenate around" as a binary operation on documents.

> surround
>   :: Doc a -- contents
>   -> Doc a -- left side
>   -> Doc a -- right side
>   -> Doc a
> surround x lhs rhs = lhs <> x <> rhs

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   parens (char 'a' <> space <> char 'b')
> -- :}
> -- (a b)
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   squote $ bracks (char 'a' <> space <> char 'b')
> -- :}
> -- '[a b]'
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   foldr1 (surround (char '.')) $ map char "abcd"
> -- :}
> -- a.b.c.d

:::

Next up we have several different kinds of line break. These behave differently when grouped or flattened.

`lineS` advances to the next line and indents to the current nesting level, but devolves to a single space if the line break is undone by `group`.

> lineS :: Doc a
> lineS = FlatAlt Line (Char ' ')

`lineE` is almost identical to `lineS`, but devolves to `emptyDoc` rather than a single space when grouped.

> lineE :: Doc a
> lineE = FlatAlt Line emptyDoc

`softlineS` inserts a single space if the resulting layout fits on the page, and otherwise inserts a line break.

> softlineS :: Doc a
> softlineS = Union (Char ' ') Line

`softlineE` is almost identical to `softlineS`, but attempts to insert `emptyDoc` rather than a single space.

> softlineE :: Doc a
> softlineE = Union emptyDoc Line

`hardline` is always laid out as a line break, even when grouped or when there's plenty of space remaining on the current line.

> hardline :: Doc a
> hardline = Line

Showing examples of these will be difficult until we've defined the combinators that distinguish them. But here's an example for `hardline`:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> hardline <> char 'b'
> -- :}
> -- a
> -- b

:::

We might as well define `annotate` here too; it is only relevant when we need our documents to target a backend that handles specific annotation types.

> annotate :: a -> Doc a -> Doc a
> annotate = Annotated



Indentation
-----------

There are a few different ways to handle indentation. The primitive `Doc` constructor behind these is `Nest`, but they behave differently with respect to the current column position.

The most blunt instrument for indentation is `nest`. This combinator takes a _change_ in the nesting level and applies it to a subdocument. Notably, the change argument can be negative.

> nest
>   :: Int -- Change of nesting level
>   -> Doc a -> Doc a
> nest k doc = if k == 0
>   then doc
>   else Nest k doc

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2 (char 'b')
> -- :}
> -- ab
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2 (hardline <> char 'b')
> -- :}
> -- a
> --   b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2 (lineS <> char 'b')
> -- :}
> -- a
> --   b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2 (softlineS <> char 'b')
> -- :}
> -- a b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2
> --     (lineS <> char 'b' <> nest (-1) (lineS <> char 'c'))
> -- :}
> -- a
> --   b
> --  c
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> nest 2 (lineS <> char 'b' <> lineS <> char 'c')
> -- :}
> -- a
> --   b
> --   c

:::

`align` lays out its argument with the nesting level set to the "current" column. This allows for dynamically setting the indentation level.

> align
>   :: Doc a -> Doc a
> align doc = WithColumn $ \k ->
>   WithNesting $ \i -> nest (k - i) doc

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> align (char 'b')
> -- :}
> -- ab
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> align (hardline <> char 'b')
> -- :}
> -- a
> --  b

:::

`hang` takes a _change_ in indentation and lays out its argument with the nesting level set to the current column plus the change. As with `nest`, negative values are allowed.

> hang
>   :: Int -- Change of nesting level relative to the current column
>   -> Doc a -> Doc a
> hang i doc = align (nest i doc)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> hang 2 (char 'b')
> -- :}
> -- ab
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> hang 2 (hardline <> char 'b')
> -- :}
> -- a
> --    b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> hang 2 (hardline <> char 'b' <> hardline <> char 'c')
> -- :}
> -- a
> --    b
> --    c

:::

`indent` takes a number and indents its subdocument by that many spaces, from the current cursor position.

> indent
>   :: Int -- Number of spaces to increase indentation by
>   -> Doc a -> Doc a
> indent i doc = hang i (spaces i <> doc)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> indent 2 (char 'b')
> -- :}
> -- a  b
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'a' <> indent 2 (hardline <> char 'b')
> -- :}
> -- a
> --    b

:::



Grouping
--------

The combinators we've defined so far are not reactive to the space available -- time to fix that. The `group` combinator tries to lay out its argument on a single line by removing line breaks. If the result doesn't fit, it falls back to the original layout.

> group :: Doc a -> Doc a
> group x = case x of
>   Union _ _ -> x
>   FlatAlt a b ->
>     case flattenAttempt b of
>       Flattened b' -> Union b' a
>       AlreadyFlat  -> Union b a
>       NeverFlat    -> a
>   _ ->
>     case flattenAttempt x of
>       Flattened x' -> Union x' x
>       AlreadyFlat  -> x
>       NeverFlat    -> x

Note that `group` works by constructing a union of two documents, which is expensive for layout. If we can avoid constructing the `Union` we can save some effort. As an optimization, `group` first attempts to determine whether its argument _can_ be flattened. If not then there's no need to construct the union. This helps to avoid exponential behavior for some documents. `flattenAttempt` is the helper function that decides this, and it returns a `FlattenResult`.

> data FlattenResult a
>   = Flattened a -- an equivalent document likely
>                 -- to be flatter than the input.
>   | AlreadyFlat -- signals that the input was
>                 -- already flat, e.g. a 'Text'.
>   | NeverFlat   -- signals that the input couldn't
>                 -- be flattened.
> 
> instance Functor FlattenResult where
>   fmap f x = case x of
>     Flattened a -> Flattened (f a)
>     AlreadyFlat -> AlreadyFlat
>     NeverFlat   -> NeverFlat

Now `flattenAttempt` tries to determine whether a document can be flattened. Using the invariants of `Union` it always takes the first option, and always takes the second option of a `FlatAlt`.

The output is `Flattened` when the document represents more than one layout that can be flattened, `AlreadyFlat` if it contains only one layout, and `NeverFlat` if it cannot be flattened because it contains a `Line` or `Fail`.

> flattenAttempt
>   :: Doc a -> FlattenResult (Doc a)
> flattenAttempt doc = case doc of
>   Fail     -> NeverFlat
>   Empty    -> AlreadyFlat
>   Line     -> NeverFlat
>   Char _   -> AlreadyFlat
>   Text _ _ -> AlreadyFlat
>   Nest i x -> fmap (Nest i) (flattenAttempt x)
> 
>   Cat x y -> case (flattenAttempt x, flattenAttempt y) of
>     (NeverFlat    ,  _          ) -> NeverFlat
>     (_            , NeverFlat   ) -> NeverFlat
>     (Flattened x' , Flattened y') -> Flattened (Cat x' y')
>     (Flattened x' , AlreadyFlat ) -> Flattened (Cat x' y)
>     (AlreadyFlat  , Flattened y') -> Flattened (Cat x y')
>     (AlreadyFlat  , AlreadyFlat ) -> AlreadyFlat
> 
>   FlatAlt _ y     -> Flattened (flatten y)
>   Union x _       -> Flattened x
> 
>   WithColumn f    -> Flattened (WithColumn (flatten . f))
>   WithNesting f   -> Flattened (WithNesting (flatten . f))
>   WithPageWidth f -> Flattened (WithPageWidth (flatten . f))
>   Annotated a x   -> fmap (Annotated a) (flattenAttempt x)
> 
>   where
>     -- Flatten, but don’t report whether anything changes.
>     flatten :: Doc a -> Doc a
>     flatten doc = case doc of
>       Fail     -> doc
>       Empty    -> doc
>       Line     -> Fail
>       Char _   -> doc
>       Text _ _ -> doc
>       Nest i x -> Nest i (flatten x)
> 
>       Cat x y     -> Cat (flatten x) (flatten y)
>       FlatAlt _ y -> flatten y
>       Union x _   -> flatten x
> 
>       WithColumn f    -> WithColumn (flatten . f)
>       WithPageWidth f -> WithPageWidth (flatten . f)
>       WithNesting f   -> WithNesting (flatten . f)
>       Annotated ann x -> Annotated ann (flatten x)

To make the effects of `group` available in our documents we also define `flatAlt`: this combinator renders its first argument by default, but falls back to the second when it is subject to a `group` operator. It's up to us to make sure the two options are semantically equivalent. Also, note that the layout algorithm depends on `group` shrinking the width of its input, and `flatAlt` does not enforce this. So remember to make the second argument of `flatAlt` narrower than the first.

> flatAlt
>   :: Doc a -- default
>   -> Doc a -- group fallback
>   -> Doc a
> flatAlt = FlatAlt

`group` is a subtle and important combinator; it will be easier to give good examples when we have a few more primitives available. But we can see some examples using `flatAlt`.

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   flatAlt (char 'a') (char 'b')
> -- :}
> -- a
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   group (flatAlt (char 'a') (char 'b'))
> -- :}
> -- b

:::



Many Flavors of Concatenation
-----------------------------

We start with a generic concatenation function, parameterized on a binary concat function.

> concatWith
>   :: ( Foldable t )
>   => (Doc a -> Doc a -> Doc a)
>   -> t (Doc a) -> Doc a
> concatWith f ds = if null ds
>   then emptyDoc
>   else foldr1 f ds

`hcat` concatenates documents horizontally with nothing between them; this is identical to `mconcat` on documents.

> hcat
>   :: [Doc a] -> Doc a
> hcat = concatWith (<>)

An example:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   hcat [char 'a', char 'b', char 'c']
> -- :}
> -- abc

:::

`hsep` concatenates documents horizontally with a space between them, using `<+>`. Note that it will not introduce line breaks, and if used carelessly can result in documents that will not fit on the line.

> hsep
>   :: [Doc a] -> Doc a
> hsep = concatWith (<+>)

An example:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   hsep [char 'a', char 'b', char 'c']
> -- :}
> -- a b c

:::

`vcat` vertically concatenates documents by interspersing `lineE` between them. Inside a `group`, the line breaks are removed (and replaced with nothing).

> vcat
>   :: [Doc a] -> Doc a
> vcat = concatWith (\x y -> x <> lineE <> y)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   vcat [char 'a', char 'b', char 'c']
> -- :}
> -- a
> -- b
> -- c
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   group $ vcat [char 'a', char 'b', char 'c']
> -- :}
> -- abc

:::

`vsep` vertically concatenates documents by interspersing `lineS` between them. Inside a `group`, the line breaks are replaced by spaces.

> vsep
>   :: [Doc a] -> Doc a
> vsep = concatWith (\x y -> x <> lineS <> y)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc pretty $
> --   vsep [char 'a', char 'b', char 'c']
> -- :}
> -- a
> -- b
> -- c
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   char 'd' <+>
> --     (align $ vsep [char 'a', char 'b', char 'c'])
> -- :}
> -- d a
> --   b
> --   c
> --
> -- $
> -- >>> :{
> -- printDoc pretty $
> --   group $ vsep [char 'a', char 'b', char 'c']
> -- :}
> -- a b c

:::

Finally, we can use `vsep` to define primitives for `String` and `Text`. Note how `text` preserves the `Doc` constructors' invariants. `reflow` is a helper function for turning paragraphs into documents.

> text :: T.Text -> Doc a
> text = vsep . map f . T.splitOn "\n"
>   where
>     -- invariant: x contains no '\n's
>     f x = case T.uncons x of
>       Nothing -> Empty
>       Just (c,cs) -> if T.null cs
>         then Char c
>         else Text (T.length x) x
> 
> string :: String -> Doc a
> string = text . T.pack
> 
> reflow
>   :: String -> Doc a
> reflow = fillSep . map string . L.words
> 
> instance IsString (Doc a) where
>   fromString = string

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   string "this is a chunk of text too wide to fit"
> -- :}
> -- this is a chunk of text too wide to fit

:::

`fillCat` concatenates documents horizontally with `<>` as long as doing so fits on the line, and otherwise inserts a line break before continuing to concatenate with `<>`. This is analogous to word wrapping in a text editor except that adjacent documents are not separated by anything.

> fillCat
>   :: [Doc a] -> Doc a
> fillCat = concatWith (\x y -> x <> softlineE <> y)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   fillCat
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- thisisachunkof
> -- texttoowideto
> -- fit
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   group $ fillCat
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- thisisachunkof
> -- texttoowideto
> -- fit
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   string "howdy:" <+> fillCat
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- howdy: thisisa
> -- chunkoftexttoo
> -- widetofit

:::

`fillSep` is analogous to `fillCat`, but concatenates with a space between unless inserting a line break.

> fillSep
>   :: [Doc a] -> Doc a
> fillSep = concatWith (\x y -> x <> softlineS <> y)

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   fillSep
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- this is a chunk
> -- of text too
> -- wide to fit
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   group $ fillSep
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- this is a chunk
> -- of text too
> -- wide to fit
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   string "howdy:" <+> fillSep
> --     [ string "this", string "is", string "a"
> --     , string "chunk", string "of", string "text"
> --     , string "too", string "wide", string "to", string "fit" ]
> -- :}
> -- howdy: this is
> -- a chunk of text
> -- too wide to fit

:::

`cat` attempts to lay out a list of documents separated by nothing, but if this does not fit, separates them _all_ with newlines.

> cat
>   :: [Doc a] -> Doc a
> cat = group . vcat

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   cat [ string "this", string "fits" ]
> -- :}
> -- thisfits
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   cat
> --     [ string "this", string "does"
> --     , string "not", string "fit" ]
> -- :}
> -- this
> -- does
> -- not
> -- fit

:::

`sep` is analogous to `cat`, but attempts to separate its arguments with spaces before falling back to newlines.

> sep
>   :: [Doc a] -> Doc a
> sep = group . vsep

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   sep [ string "this", string "fits" ]
> -- :}
> -- this fits
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   sep
> --     [ string "this", string "does"
> --     , string "not", string "fit" ]
> -- :}
> -- this
> -- does
> -- not
> -- fit

:::



Reactive Combinators
--------------------

We have three primitive _reactive_ combinators, allowing us to describe layouts that depend on the current column number, the page width, and the nesting level.

> withColumn
>   :: (Int -> Doc a) -> Doc a
> withColumn = WithColumn
> 
> withPageWidth
>   :: (PageWidth -> Doc a) -> Doc a
> withPageWidth = WithPageWidth
> 
> withNesting
>   :: (Int -> Doc a) -> Doc a
> withNesting = WithNesting

One application of these is the _width_ combinator, which lays out one document and makes its width (in columns) available to another.

> width
>   :: Doc ann -> (Int -> Doc ann) -> Doc ann
> width doc f =
>   withColumn (\colStart ->
>     doc <> withColumn (\colEnd ->
>       f (colEnd - colStart)))

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   width (string "xxxxx")
> --     (\k -> string "<- width:" <+> string (show k))
> -- :}
> -- xxxxx<- width: 5
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   width (string "xxxxx" <> indent 2 (string "yy"))
> --     (\k -> string "<- width:" <+> string (show k))
> -- :}
> -- xxxxx  yy<- width: 9
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 10 1.0) $
> --   string "zzz" <+> align
> --     (width (string "xxxxx" <> hardline <> string "yy")
> --       (\k -> string "<- width:" <+> string (show k)))
> -- :}
> -- zzz xxxxx
> --     yy<- width: 2

:::

The `fill` combinator pads a document on the right to a given length using spaces.

> fill
>   :: Int -> Doc a -> Doc a
> fill n doc = width doc (\w -> spaces (n - w))

An example:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   align (vcat $ map (\(a, b) -> fill 4 a <> char ':' <+> b)
> --     [ (string "a", string "A"), (string "foo", string "FOO")
> --     , (string "munge", string "MUNGE") ])
> -- :}
> -- a   : A
> -- foo : FOO
> -- munge: MUNGE

:::

`fillBreak` is similar to fill, but inserts a line break an adjusts the nesting level if the input document is wider than the padding width.

> fillBreak
>   :: Int -> Doc a -> Doc a
> fillBreak f x = width x (\w ->
>   if w > f
>     then nest f lineE
>     else spaces (f - w))

An example:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   align (vcat $ map (\(a, b) -> fillBreak 4 a <> char ':' <+> b)
> --     [ (string "a", string "A"), (string "foo", string "FOO")
> --     , (string "munge", string "MUNGE") ])
> -- :}
> -- a   : A
> -- foo : FOO
> -- munge
> --     : MUNGE

:::

`ifColumn` lets us selectively alter a document if the starting column satisfies some predicate.

> ifColumn
>   :: (Int -> Bool) -> (Doc a -> Doc a) -> Doc a -> Doc a
> ifColumn p f doc =
>   withColumn $ \c -> if p c
>     then f doc
>     else doc

::: doctest

> -- $
> -- >>> :{
> --   let
> --     doc = ifColumn (== 0) ((string "foo") <+>) (string "bar")
> --   in printDoc (prettyWith 15 1.0) $
> --     align $ vcat
> --       [ doc
> --       , string ">>>>>" <+> doc
> --       ]
> -- :}
> -- foo bar
> -- >>>>> bar

:::



Lists
-----

Here we define some helper functions for defining lists. The basic pattern used here is `encloseSep`, which intersperses a separator document between items in a list, and wraps the result between two delimiters. The use of `cat` here means the resulting document will be laid out on one line if it fits, and with one item per line (with separators at the beginning) otherwise.

> encloseSep
>   :: Doc a   -- left delimiter
>   -> Doc a   -- right delimiter
>   -> Doc a   -- separator
>   -> [Doc a] -- input documents
>   -> Doc a
> encloseSep lhs rhs sep ds = case ds of
>   []  -> lhs <> rhs
>   [d] -> lhs <> d <> rhs
>   _   -> cat (zipWith (<>) (lhs : repeat sep) ds) <> rhs

We also define some common delimited list patterns.

> brackList :: [Doc ann] -> Doc ann
> brackList = group . encloseSep
>   (flatAlt (text "[ ") (text "["))
>   (flatAlt (text " ]") (text "]"))
>   (text ", ")
> 
> braceList :: [Doc ann] -> Doc ann
> braceList = group . encloseSep
>   (flatAlt (text "{ ") (text "{"))
>   (flatAlt (text " }") (text "}"))
>   (text ", ")
> 
> parenList :: [Doc ann] -> Doc ann
> parenList = group . encloseSep
>   (flatAlt (text "( ") (text "("))
>   (flatAlt (text " )") (text ")"))
>   (text ", ")

Some examples:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   parenList [string "foo", string "barre", string "mung"]
> -- :}
> -- ( foo
> -- , barre
> -- , mung )
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   string "things:" <+>
> --     braceList [string "foo", string "barre", string "mung"]
> -- :}
> -- things: { foo
> -- , barre
> -- , mung }
> --
> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   string "things:" <+>
> --     align (brackList [string "foo", string "barre", string "mung"])
> -- :}
> -- things: [ foo
> --         , barre
> --         , mung ]

:::

`punctuate` concatenates a separator to the right of each item in a list except for the last one.

> punctuate
>     :: Doc ann -- ^ Punctuation, e.g. 'comma'
>     -> [Doc ann]
>     -> [Doc ann]
> punctuate p = go
>   where
>     go []     = []
>     go [d]    = [d]
>     go (d:ds) = (d <> p) : go ds

An example:

::: doctest

> -- $
> -- >>> :{
> -- printDoc (prettyWith 15 1.0) $
> --   hsep $ punctuate (char ',')
> --     [string "foo", string "barre", string "mung"]
> -- :}
> -- foo, barre, mung

:::
