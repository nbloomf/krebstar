---
title: Abstract Documents
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE TypeFamilies, TypeApplications, BangPatterns #-}
> 
> module Kreb.Format.Doc (
>     Doc(..), isEmptyDoc, fuseDoc
>   , alterAnnotations, unAnnotate, mergeAnnotationsWith
>   , DocS(..), renderDocSA, renderDocS, showDocS, hPrintDocS, printDocS
>   , alterAnnotationsS, unAnnotateS
>   , LayoutOptions(..), PageWidth(..), FittingPredicate(..)
>   , layoutWadlerLeijen, layoutPretty, layoutSmart, layoutCompact
>   , Layout(..), layout, pretty, prettyWith, smart, smartWith, hPrintDoc, printDoc
>   , defaultLayoutOptions, defaultPageWidth, boundedLayout, unboundedLayout
>   , prettyString, prettyStringWith
>   , DocST(..), renderDocSTA, renderDocST, hPrintDocST, printDocST
>   , alterAnnotationsST, unAnnotateST
> ) where
> 
> import Control.Monad.Fail
> import Control.Applicative
> import System.IO
> 
> import Kreb.Control
> 
> import Kreb.Format.Util

:::



Introduction
------------

It will be handy later to have a _pretty printer_ available -- this is a function that lays out tree-like data as ascii text in a way that tries to preserve the structure visually without consuming too much vertical space. If that sounds impossibly vague, it is. :)

Pretty printing has been used as a playground for functional programming techniques for a long, time, and there are many Haskell libraries implementing Wadler/Leijen style pretty printers. The code here started as a wholesale copy of the [prettyprinter](http://hackage.haskell.org/package/prettyprinter) library, which is itself a fork of earlier work. I've made some (very minor) style changes and tried to add prose, but the essence of this code is not mine.

We're avoiding external dependencies as much as possible, but here we really do need to use `Data.Text` from the standard library for efficiency. Code from that library will always be prefixed.

> import qualified Data.Text              as T
> import qualified Data.Text.IO           as T
> import qualified Data.Text.Lazy         as TL
> import qualified Data.Text.Lazy.Builder as TLB



A Doc Type
----------

The basic strategy behind pretty printing is that we define an "abstract document", which includes both the concrete content we want displayed, as well as _optional_ layouts -- this representation is of a _set_ of possible concrete layous. The abstract document is then serialized to a specific instance by a layout algorithm, which attempts to choose the "prettiest" layout for a given page width.

Abstract documents are represented here by the `Doc` type. It has several constructors, which we try to explain.

> data Doc a
>   = Fail  -- document which cannot be laid out.
>   | Empty -- document with no width (but height 1!)

It may seem strange to have a document which cannot be laid out -- this case is used to prune optional layouts which are too wide to fit in the available space.

>   | Line              -- a literal '\n'
>   | Char Char         -- not '\n'
>   | Text !Int !T.Text -- at least two characters, no '\n's, with cached length

Here we have documents containing literal text, split into three constructors. Note the invariants on the `Char` and `Text` constructors; it will be important that we maintain these.

>   | Nest !Int (Doc a)

The `Nest` constructor indents its document argument by a given amount -- which may be negative.

>   | Cat (Doc a) (Doc a)

The `Cat` constructor concatenates two documents by abutting the first character of the second against the last character of the first.

>   | FlatAlt (Doc a) (Doc a)
>   | Union   (Doc a) (Doc a) -- first lines of LHS > first lines of RHS, used for group

`FlatAlt` and `Union` are used to implement the `group` combinator, which we will see later. In `Union x y`, the first line of `x` should be longer than the first line of `y`. In `FlatAlt x y`, `x` should be taller and narrower than `y`.

>   | WithColumn    (Int -> Doc a)       -- react on column position
>   | WithPageWidth (PageWidth -> Doc a) -- react on doc width
>   | WithNesting   (Int -> Doc a)       -- react on indentation level

The `With*` constructors allow us to build documents whose layout depends on layout-time information.

>   | Annotated a (Doc a)

The `Annotated` constructor allows documents to be annotated with metadata to be interpreted by the renderer, like color.

Semantically, `Cat` and `Empty` make `Doc a` a monoid; however, this instance is not strictly lawful.

> instance Semigroup (Doc a) where
>   (<>) = Cat
> 
> instance Monoid (Doc a) where
>   mempty = Empty
>   mappend = (<>)

Later we will need to detect when a document is empty.

> isEmptyDoc
>   :: Doc a -> Bool
> isEmptyDoc d = case d of
>   Empty -> True
>   _     -> False

`Doc` behaves like a container over the annotation type; we define some helper functions here for manipulating these. First is a kind of mapping function over annotations. If `f` is a function taking annotations of type `a` to _lists_ of annotations of type `b`, then `alterannotations f doc` replaces every annotation in `doc` with a nested tower of annotations.

> alterAnnotations
>   :: (a -> [b]) -> Doc a -> Doc b
> alterAnnotations h = go
>   where
>     go = \doc -> case doc of
>       -- base cases
>       Fail     -> Fail
>       Empty    -> Empty
>       Char c   -> Char c
>       Text l t -> Text l t
>       Line     -> Line
> 
>       -- recursive cases
>       FlatAlt x y     -> FlatAlt (go x) (go y)
>       Cat x y         -> Cat (go x) (go y)
>       Nest i x        -> Nest i (go x)
>       Union x y       -> Union (go x) (go y)
>       WithColumn f    -> WithColumn (go . f)
>       WithPageWidth f -> WithPageWidth (go . f)
>       WithNesting f   -> WithNesting (go . f)
>       Annotated ann x -> foldr Annotated (go x) (h ann)

`alterAnnotations` has two useful specializations. First we can use it to define a functor instance for `Doc`:

> instance Functor Doc where
>   fmap f = alterAnnotations (return . f)

Second, we can remove annotations entirely, turning a `Doc a` into a `Doc b` for any type `b`.

> unAnnotate :: Doc a -> Doc b
> unAnnotate = alterAnnotations (const [])

Note that `alterAnnotations` will generally expand a `Doc` along its annotations. `mergeAnnotationsWith` collapses nested annotations to a single node with a merging function.

> mergeAnnotationsWith
>   :: (a -> a -> a) -> Doc a -> Doc a
> mergeAnnotationsWith h = go
>   where
>     go = \doc -> case doc of
>       -- base cases
>       Fail     -> Fail
>       Empty    -> Empty
>       Char c   -> Char c
>       Text l t -> Text l t
>       Line     -> Line
> 
>       -- recursive cases
>       FlatAlt x y     -> FlatAlt (go x) (go y)
>       Cat x y         -> Cat (go x) (go y)
>       Nest i x        -> Nest i (go x)
>       Union x y       -> Union (go x) (go y)
>       WithColumn f    -> WithColumn (go . f)
>       WithPageWidth f -> WithPageWidth (go . f)
>       WithNesting f   -> WithNesting (go . f)
>       Annotated an1 x -> case x of
>         Annotated an2 y -> go (Annotated (h an1 an2) y)
>         _               -> Annotated an1 (go x)

Manipulating the annotations on a `Doc` should be done with care, since it requires traversing the entire structure (even if most of it is never needed). If possible it is better to work with the annotations on a `DocS` after layout.

Another optimization we can perform is _fusion_. When a `Doc` is serialized to a `DocS`, every node in the `Doc` tree (along the optimal layout) appears in the `DocS` stream -- even when this is otherwise unnecessary, as when several `Text` nodes are directly `Cat`ted together. As a rule of thumb, `fuseDoc` should be used on concatenations of lots of small strings that are used many times. The fused `Doc` is identical (that is, represents the same set of layouts) as its unfused counterpart.

> data FusionDepth = Shallow | Deep
>   deriving (Eq, Ord, Show)

`fuseDoc` supports two kinds of fusion. `Shallow` does not go deep into nested documents, focusing on concatenating text nodes. `Deep` fusion recurses into all parts of the document, including any reactive subdocuments. Deep fusion is very expensive and should only be used if profiling shows it is an improvement over shallow fusion.

> fuseDoc :: FusionDepth -> Doc a -> Doc a
> fuseDoc depth = go
>   where
>     go doc = case doc of
>       Cat Empty x                   -> go x
>       Cat x Empty                   -> go x
> 
>       Cat (Char c1) (Char c2)       -> Text 2 (T.singleton c1 <> T.singleton c2)
>       Cat (Text lt t) (Char c)      -> Text (lt+1) (T.snoc t c)
>       Cat (Char c) (Text lt t)      -> Text (1+lt) (T.cons c t)
>       Cat (Text l1 t1) (Text l2 t2) -> Text (l1+l2) (t1 <> t2)
> 
>       Cat x@Char{} (Cat y@Char{} z) -> go (Cat (go (Cat x y)) z)
>       Cat x@Text{} (Cat y@Char{} z) -> go (Cat (go (Cat x y)) z)
>       Cat x@Char{} (Cat y@Text{} z) -> go (Cat (go (Cat x y)) z)
>       Cat x@Text{} (Cat y@Text{} z) -> go (Cat (go (Cat x y)) z)
> 
>       Cat (Cat x y@Char{}) z -> go (Cat x (go (Cat y z)))
>       Cat (Cat x y@Text{}) z -> go (Cat x (go (Cat y z)))
> 
>       Cat x y -> Cat (go x) (go y)
> 
>       Nest i (Nest j x) -> let !fused = Nest (i+j) x
>                              in go fused
>       Nest _ x@Empty{} -> x
>       Nest _ x@Text{}  -> x
>       Nest _ x@Char{}  -> x
>       Nest 0 x         -> go x
>       Nest i x         -> Nest i (go x)
> 
>       Annotated ann x -> Annotated ann (go x)
> 
>       FlatAlt x1 x2 -> FlatAlt (go x1) (go x2)
>       Union x1 x2   -> Union (go x1) (go x2)
> 
>       other | depth == Shallow -> other
> 
>       WithColumn f    -> WithColumn (go . f)
>       WithPageWidth f -> WithPageWidth (go . f)
>       WithNesting f   -> WithNesting (go . f)
> 
>       other -> other



Document Streams
----------------

The `Doc` type is useful for describing documents with optional layouts. But for rendering, it will be useful to have an abstract serialized format our layout engine can target, rather than building a string directly. This is what the `DocS` type is for.

> data DocS a
>   = SFail
>   | SEmpty
>   | SChar Char        (DocS a)
>   | SText !Int T.Text (DocS a) -- cached length
>   | SLine !Int        (DocS a) -- indentation of the next line
>   | SAnnPush a        (DocS a)
>   | SAnnPop           (DocS a)
>   deriving (Eq, Ord, Show)

Note that `DocS` is essentially a stack of document fragments. The analog of `alterAnnotations` for `DocS` has a slightly different signature; rather than mapping to a list of new annotations, we can optionally remove the annotation.

> alterAnnotationsS
>   :: (a -> Maybe b) -> DocS a -> DocS b
> alterAnnotationsS f = go []
>   where
>     go st docs = case docs of
>       SFail             -> SFail
>       SEmpty            -> SEmpty
>       SChar c rest      -> SChar c (go st rest)
>       SText l t rest    -> SText l t (go st rest)
>       SLine l rest      -> SLine l (go st rest)
> 
>       -- we carry a stack indicating which pops to ignore
>       SAnnPush a rest -> case f a of
>         Nothing -> go (Toss:st) rest
>         Just b  -> SAnnPush b (go (Keep:st) rest)
>       SAnnPop rest -> case st of
>         []       -> error "alterAnnotationsS: unpaired pop"
>         Keep:st' -> SAnnPop (go st' rest)
>         Toss:st' -> go st' rest
> 
> data AnnotationAction = Toss | Keep

Again, `alterAnnotationsS` has two useful specializations. First we can map over the annotations:

> instance Functor DocS where
>   fmap f = alterAnnotationsS (return . f)

Second, we can remove annotations entirely, turning a `DocS a` into a `DocS b` for any type `b`.

> unAnnotateS :: DocS a -> DocS b
> unAnnotateS = alterAnnotationsS (const Nothing)

The `renderDocSA` function maps this stack to a monoid (in practice either `Text` or `String`, but using only the monoid interface makes rendering more flexible) under an applicative functor. Monoid concatenation is right associative, as `a <> (b <> c)`; with lazy evaluation this means we can extract prefixes of the rendered result without computing the entire output.

Note that the accumulator function `go` maintains a stack of annotations.

> renderDocSA
>   :: ( Applicative f, Monoid t )
>   => (T.Text -> f t) -- Render plain Text
>   -> (a -> f t)      -- Render an opening annotation
>   -> (a -> f t)      -- Render a closing annotation
>   -> DocS a
>   -> f t
> renderDocSA text push pop = go []
>   where
>     (<++>) = liftA2 mappend
> 
>     go st docS = case docS of
>       SFail -> error "renderDocSA: uncaught SFail"
>       SEmpty -> case st of
>         [] -> pure mempty
>         _  -> error "renderDocSA: input not fully consumed"
>       SChar c rest -> text (T.singleton c) <++> go st rest
>       SText _ t rest -> text t <++> go st rest
>       SLine i rest -> text (T.singleton '\n') <++> text (textSpaces i) <++> go st rest
>       SAnnPush a rest -> push a <++> go (a:st) rest
>       SAnnPop rest -> case st of
>         a:as -> pop a <++> go as rest
>         [] -> error "renderDocSA: unpaired pop"

`renderDocS` simply specializes `renderDocSA` to the identity functor.

> renderDocS
>   :: ( Monoid t )
>   => (T.Text -> t) -- Render plain Text
>   -> (a -> t)      -- Render an opening annotation
>   -> (a -> t)      -- Render a closing annotation
>   -> DocS a
>   -> t
> renderDocS text push pop = unIdentity .
>   renderDocSA (Identity . text) (Identity . push) (Identity . pop)

This is also a good place to convert document streams into `ShowS`s. Recall that `ShowS = String -> String` is an optimization over the usual `show` class function, which allows us to build up strings as difference lists. This is potentially more efficient than nested concatenation.

> showDocS :: DocS a -> ShowS
> showDocS docS = case docS of
>   SFail        -> error "showDocS: uncaught SFail"
>   SEmpty       -> id
>   SChar c x    -> showChar c . showDocS x
>   SText _l t x -> showString (T.unpack t) . showDocS x
>   SLine i x    -> showString ('\n' : replicate i ' ') . showDocS x
>   SAnnPush _ x -> showDocS x
>   SAnnPop x    -> showDocS x

We also define a helper to print a `DocS` directly to a file handle (or stdout), ignoring any annotations.

> hPrintDocS
>   :: Handle -> DocS a -> IO ()
> hPrintDocS h docs = go docs
>   where
>     go :: DocS a -> IO ()
>     go doc = case doc of
>       SFail              -> error "hRenderDocIO: uncaught SFail"
>       SEmpty             -> pure ()
>       SChar c rest       -> hPutChar h c >>  go rest
>       SText _ t rest     -> T.hPutStr h t >> go rest
>       SLine n rest       -> hPutChar h '\n' >> T.hPutStr h (textSpaces n) >> go rest
>       SAnnPush _ann rest -> go rest
>       SAnnPop rest       -> go rest
> 
> printDocS :: DocS a -> IO ()
> printDocS = hPrintDocS stdout

We can also give `Foldable` and `Traversable` instances for `DocS`, treating it as a list of annotations.

> instance Foldable DocS where
>   foldMap f = go
>     where
>       go docs = case docs of
>         SFail             -> mempty
>         SEmpty            -> mempty
>         SChar _ rest      -> go rest
>         SText _ _ rest    -> go rest
>         SLine _ rest      -> go rest
>         SAnnPush ann rest -> mappend (f ann) (go rest)
>         SAnnPop rest      -> go rest
> 
> instance Traversable DocS where
>   traverse f = go
>     where
>       go docs = case docs of
>         SFail             -> pure SFail
>         SEmpty            -> pure SEmpty
>         SChar c rest      -> SChar c   <$> go rest
>         SText l t rest    -> SText l t <$> go rest
>         SLine i rest      -> SLine i   <$> go rest
>         SAnnPush ann rest -> SAnnPush  <$> f ann <*> go rest
>         SAnnPop rest      -> SAnnPop   <$> go rest



The Wadler/Leijen Layout Algorithm
----------------------------------

Conversion from `Doc` to `DocS`, called _layout_, amounts to collapsing all the optional documents using some notion of "best fit". Here this is done using a single function, `layoutWadlerLeijen`, which is parameterized over two inputs.

`LayoutOptions` specifies how wide the page is in columns, and, if the page is bounded, the "ribbon width" -- the fraction of each line that can be occupied by non-indentation characters.

> newtype LayoutOptions = LOpts
>   { layoutPageWidth :: PageWidth
>   } deriving (Eq, Ord, Show)
> 
> data PageWidth
>   = AvailablePerLine Int Double
>   | Unbounded
>   deriving (Eq, Ord, Show)

A `FittingPredicate` decides whether a given `DocS` can be rendered within a particular combination of

  - page width,
  - minimum indentation, and
  - maximum width of the first rendered line.

> newtype FittingPredicate a = FP
>   (PageWidth -> Int -> Int -> DocS a -> Bool)

Extracting the fitting predicate as a parameter allows `layoutWadlerLeijen` to be very flexible. Using different fitting predicates allows us to tweak the tradeoffs layout must make among compactness, readability, and efficiency without changing the core algorithm.

> layoutWadlerLeijen
>   :: forall a
>    . FittingPredicate a -> LayoutOptions
>   -> Doc a -> DocS a
> layoutWadlerLeijen pred opts doc =
>   best 0 0 (Cons 0 doc Nil)
>   where
>     FP fits = pred
>     LOpts { layoutPageWidth = pWidth } = opts
> 
>     -- best lays out the documents in a layout pipeline "optimally".
>     -- its parameters must satisfy the following invariants:
>     --   * current column >= current nesting level
>     --   * current column - current indentaion = number of chars inserted in line
>     best
>       :: Int              -- Current nesting level
>       -> Int              -- Current column, i.e. "where the cursor is"
>       -> LayoutPipeline a -- Documents remaining to be handled (in order)
>       -> DocS a
>     best !_ !_ Nil           = SEmpty
>     best nl cc (UndoAnn ds)  = SAnnPop (best nl cc ds)
>     best nl cc (Cons i d ds) = case d of
>       Fail            -> SFail
>       Empty           -> best nl cc ds
>       Char c          -> let !cc' = cc+1 in SChar c (best nl cc' ds)
>       Text l t        -> let !cc' = cc+l in SText l t (best nl cc' ds)
>       Line            -> SLine i (best i i ds)
>       FlatAlt x _     -> best nl cc (Cons i x ds)
>       Cat x y         -> best nl cc (Cons i x (Cons i y ds))
>       Nest j x        -> let !ij = i+j in best nl cc (Cons ij x ds)
>       Union x y       -> let
>                            x' = best nl cc (Cons i x ds)
>                            y' = best nl cc (Cons i y ds)
>                          in selectNicer nl cc x' y'
>       -- this is where we pass layout parameters to the reactive constructors
>       WithColumn f    -> best nl cc (Cons i (f cc) ds)
>       WithPageWidth f -> best nl cc (Cons i (f pWidth) ds)
>       WithNesting f   -> best nl cc (Cons i (f i) ds)
>       Annotated ann x -> SAnnPush ann (best nl cc (Cons i x (UndoAnn ds)))
> 
>     -- selectNicer returns choice A if it fits, otherwise choice B.
>     -- The fit of choice B is /not/ checked! It is the user's responsibility
>     -- to provide an alternative that can fit the page even when choice A doesn't.
>     selectNicer
>       :: Int    -- Current nesting level
>       -> Int    -- Current column
>       -> DocS a -- Choice A
>       -> DocS a -- Choice B; should be less wide than choice A.
>       -> DocS a -- Choice A if it fits, otherwise B.
>     selectNicer lineIndent currentColumn x y = case pWidth of
>         AvailablePerLine lineLength ribbonFraction
>           | fits pWidth minNestingLevel availableWidth x -> x
>           where
>             minNestingLevel = case initialIndentation y of
>               Just i -> min i currentColumn
>               Nothing -> currentColumn
>             availableWidth = min columnsLeftInLine columnsLeftInRibbon
>               where
>                 columnsLeftInLine = lineLength - currentColumn
>                 columnsLeftInRibbon = lineIndent + ribbonWidth - currentColumn
>                 ribbonWidth =
>                   (max 0 . min lineLength . round)
>                     (fromIntegral lineLength * ribbonFraction)
>         Unbounded
>           | not (failsOnFirstLine x) -> x
>         _ -> y
> 
>     initialIndentation :: DocS a -> Maybe Int
>     initialIndentation sds = case sds of
>       SLine i _    -> Just i
>       SAnnPush _ s -> initialIndentation s
>       SAnnPop s    -> initialIndentation s
>       _            -> Nothing
> 
>     failsOnFirstLine :: DocS a -> Bool
>     failsOnFirstLine = go
>       where
>         go sds = case sds of
>           SFail        -> True
>           SEmpty       -> False
>           SChar _ s    -> go s
>           SText _ _ s  -> go s
>           SLine _ _    -> False
>           SAnnPush _ s -> go s
>           SAnnPop s    -> go s

`layoutWadlerLeijen` uses an intermediate data type, `LayoutPipeline`, to keep a stack of documents waiting to be processed. (This type is only used here.)

> -- List of nesting level/document pairs yet to be laid out.
> data LayoutPipeline a
>   = Nil
>   | Cons !Int (Doc a) (LayoutPipeline a)
>   | UndoAnn (LayoutPipeline a)

From here, we can define some concrete layout algorithms by defining an appropriate fitting predicate. The first, `layoutPretty`, has one `DocS` element of lookahead -- that is, it commits to rendering as long as the next element in the stream fits. This is reasonably efficient, but may give less pretty results.

> layoutPretty
>     :: LayoutOptions
>     -> Doc a
>     -> DocS a
> layoutPretty = layoutWadlerLeijen
>     (FP (\_ _ maxWidth docS -> fits maxWidth docS))
>   where
>     fits
>       :: Int -- Width in which to fit the first line
>       -> DocS a
>       -> Bool
>     fits w doc = if w < 0
>       then False
>       else case doc of
>         SFail        -> False
>         SEmpty       -> True
>         SChar _ x    -> fits (w - 1) x
>         SText l _t x -> fits (w - l) x
>         SLine _ _    -> True
>         SAnnPush _ x -> fits w x
>         SAnnPop x    -> fits w x

The next concrete algorithm, `layoutSmart`, looks further ahead than `layoutPretty`. Rather than some fixed amount of lookahead, it scans the document until it reaches a line with equal or lesser indentation before checking fit. This is useful for layouts with semantically significant indentation.

> layoutSmart
>   :: LayoutOptions
>   -> Doc ann
>   -> DocS ann
> layoutSmart = layoutWadlerLeijen (FP fits)
>   where
>     fits
>       :: PageWidth
>       -> Int -- Minimum nesting level to fit in
>       -> Int -- Width in which to fit the first line
>       -> DocS ann
>       -> Bool
>     fits pw m = fits'
>       where
>         fits' w docS = if w < 0
>           then False
>           else case docS of
>             SFail        -> False
>             SEmpty       -> True
>             SChar _ x    -> fits' (w-1) x
>             SText k _ x  -> fits' (w-k) x
>             SLine i x    -> case pw of
>               AvailablePerLine c _ -> if m < i
>                 then fits' (c - i) x
>                 else False
>               Unbounded -> False
>             SAnnPush _ x -> fits' w x
>             SAnnPop x    -> fits' w x

Finally, `layoutCompact` ignores indentation and doesn't use `layoutWadlerLeijen`. It is very fast, but not pretty at all; this may be useful for producing output to be consumed by another program.

> layoutCompact :: Doc a -> DocS a
> layoutCompact doc = scan 0 [doc]
>   where
>     scan _ [] = SEmpty
>     scan !col (d:ds) = case d of
>       Fail            -> SFail
>       Empty           -> scan col ds
>       Char c          -> SChar c (scan (col+1) ds)
>       Text l t        -> let !col' = col+l in SText l t (scan col' ds)
>       FlatAlt x _     -> scan col (x:ds)
>       Line            -> SLine 0 (scan 0 ds)
>       Cat x y         -> scan col (x:y:ds)
>       Nest _ x        -> scan col (x:ds)
>       Union _ y       -> scan col (y:ds)
>       WithColumn f    -> scan col (f col:ds)
>       WithPageWidth f -> scan col (f Unbounded : ds)
>       WithNesting f   -> scan col (f 0 : ds)
>       Annotated _ x   -> scan col (x:ds)

For convenience we bundle all three layout algorithms behind a single function taking a `Layout` argument.

> data Layout
>   = Pretty LayoutOptions
>   | Smart LayoutOptions
>   | Compact
>   deriving (Eq, Show)
> 
> layout
>   :: Layout -> Doc a -> DocS a
> layout l doc = case l of
>   Pretty opts -> layoutPretty opts doc
>   Smart opts  -> layoutSmart opts doc
>   Compact     -> layoutCompact doc
> 
> pretty :: Layout
> pretty = Pretty defaultLayoutOptions
> 
> prettyWith :: Int -> Double -> Layout
> prettyWith n r = Pretty $ LOpts
>   { layoutPageWidth = AvailablePerLine n r }
> 
> smart :: Layout
> smart = Smart defaultLayoutOptions
> 
> smartWith :: Int -> Double -> Layout
> smartWith n r = Smart $ LOpts
>   { layoutPageWidth = AvailablePerLine n r }
> 
> defaultLayoutOptions :: LayoutOptions
> defaultLayoutOptions = LOpts
>   { layoutPageWidth = defaultPageWidth }
> 
> defaultPageWidth :: PageWidth
> defaultPageWidth = AvailablePerLine 80 1
> 
> boundedLayout :: Int -> Double -> LayoutOptions
> boundedLayout w r = LOpts $ AvailablePerLine w r
> 
> unboundedLayout :: LayoutOptions
> unboundedLayout = LOpts Unbounded

We also define helper functions for printing a laid out `Doc` directly to a file handle (or stdout), ignoring any annotations.

> hPrintDoc
>   :: Handle -> Layout -> Doc a -> IO ()
> hPrintDoc h l = hPrintDocS h . layout l
> 
> printDoc
>   :: Layout -> Doc a -> IO ()
> printDoc = hPrintDoc stdout

Finally, we provide some utility functions for simply pretty printing a `Doc` as a string.

> prettyStringWith
>   :: LayoutOptions -> Doc a -> String
> prettyStringWith opts doc =
>   (showDocS $ layout (Pretty opts) doc) ""
> 
> prettyString
>   :: Doc a -> String
> prettyString = prettyStringWith defaultLayoutOptions



Document Trees
--------------

`DocS` is great for serialized output, however it's less great if our target type is tree-shaped, because we have to scan to the next pop to find the end of an annotation group. The `DocST` type represents a specific document layout, like `DocS`, but is tree-shaped so that annotations are explicitly delimited.

> data DocST a
>   = STEmpty
>   | STChar Char
>   | STText !Int T.Text -- cached length
>   | STLine !Int        -- indentation of the next line
>   | STAnn a  (DocST a)
>   | STConcat [DocST a] -- horizontal concatenation of multiple documents
>   deriving (Eq, Ord, Show)

The analog of `alterAnnotations` for `DocST` restores the flexibility we lost with `DocS`, allowing us to replace a single annotation with a list of annotations.

> alterAnnotationsST
>   :: (a -> [b]) -> DocST a -> DocST b
> alterAnnotationsST re = f
>   where
>     f = \sdt -> case sdt of
>       STEmpty        -> STEmpty
>       STChar c       -> STChar c
>       STText l t     -> STText l t
>       STLine i       -> STLine i
>       STConcat xs    -> STConcat (map f xs)
>       STAnn ann rest -> Prelude.foldr STAnn (f rest) (re ann)

Again we have two interesting specializations. A functor instance:

> instance Functor DocST where
>   fmap f = alterAnnotationsST (return . f)

And the ability to remove annotations altogether.

> unAnnotateST
>   :: DocST a -> DocST b
> unAnnotateST = alterAnnotationsST (const [])

The `renderDocST` function maps the tree of document fragments to a monoid (in practice either `Text` or `String`, but we only use the monoid interface here)

> renderDocSTA
>   :: ( Applicative f, Monoid t )
>   => (T.Text -> f t)   -- Render plain Text
>   -> (a -> f t -> f t) -- Modify an element with an annotation
>   -> DocST a
>   -> f t
> renderDocSTA text ann = go
>   where
>     go = \sdt -> case sdt of
>       STEmpty      -> pure mempty
>       STChar c     -> text (T.singleton c)
>       STText _ t   -> text t
>       STLine i     -> text (T.cons '\n' (textSpaces i))
>       STAnn a rest -> ann a (go rest)
>       STConcat xs  -> fmap mconcat (traverse go xs)

`renderDocST` specializes `renderDocSTA` to the identity functor.

> renderDocST
>   :: ( Monoid t )
>   => (T.Text -> t) -- Render plain Text
>   -> (a -> t -> t) -- Modify an element with an annotation
>   -> DocST a
>   -> t
> renderDocST text ann = unIdentity .
>   renderDocSTA (Identity . text) (\a (Identity x) -> Identity (ann a x))

We also define helper functions for printing a `DocST` directly to a file handle (or stdout), ignoring any annotations.

> hPrintDocST
>   :: Handle -> DocST a -> IO ()
> hPrintDocST h = go
>   where
>     go :: DocST a -> IO ()
>     go docst = case docst of
>       STEmpty     -> return ()
>       STChar c    -> hPutChar h c
>       STText _ t  -> T.hPutStr h t
>       STLine i    -> hPutChar h '\n' >> T.hPutStr h (textSpaces i)
>       STAnn _ x   -> go x
>       STConcat xs -> mapM_ go xs
> 
> printDocST :: DocST a -> IO ()
> printDocST = hPrintDocST stdout

We also have `Foldable` and `Traversable` instances for `DocST`.

> instance Foldable DocST where
>   foldMap f = go
>     where
>       go = \sdt -> case sdt of
>         STEmpty        -> mempty
>         STChar _       -> mempty
>         STText _ _     -> mempty
>         STLine _       -> mempty
>         STAnn ann rest -> mappend (f ann) (go rest)
>         STConcat xs    -> mconcat (map go xs)
> 
> instance Traversable DocST where
>   traverse f = go
>     where
>       go = \sdt -> case sdt of
>         STEmpty        -> pure STEmpty
>         STChar c       -> pure (STChar c)
>         STText l t     -> pure (STText l t)
>         STLine i       -> pure (STLine i)
>         STAnn ann rest -> STAnn <$> f ann <*> go rest
>         STConcat xs    -> STConcat <$> traverse go xs

Converting a stream into a tree structure is called _parsing_. We can use our `StreamT` monad transformer over `Maybe` to model parsers, which have access to a stream of tokens and may fail.

> type Parser s a = StreamT s Maybe a
> 
> runParser
>   :: (IsStream s) => Parser s a -> s -> Maybe (a, s)
> runParser = unStreamT

To parse a `DocS` we first give it an `IsStream` instance. Here `StreamValue` is the token type, and `advance` .

> instance IsStream (DocS a) where
>   data StreamValue (DocS a)
>     = TokEndOfStream
>     | TokEmpty
>     | TokChar Char
>     | TokText !Int T.Text
>     | TokLine Int
>     | TokAnnPush a
>     | TokAnnPop
> 
>   advance :: DocS a -> (StreamValue (DocS a), DocS a)
>   advance x = case x of
>     SFail         -> error "advance: uncaught SFail"
>     SEmpty        -> (TokEndOfStream, SEmpty)
>     SChar c xs    -> (TokChar c,      xs)
>     SText l t xs  -> (TokText l t,    xs)
>     SLine i xs    -> (TokLine i,      xs)
>     SAnnPush a xs -> (TokAnnPush a,   xs)
>     SAnnPop xs    -> (TokAnnPop,      xs)

Parsing the document stream is straightforward:

> parseDocS :: forall a. Parser (DocS a) (DocST a)
> parseDocS = fmap wrap (many chunk)
>   where
>     wrap :: [DocST a] -> DocST a
>     wrap z = case z of
>       []  -> STEmpty
>       [x] -> x
>       xs  -> STConcat xs
> 
>     chunk :: Parser (DocS a) (DocST a)
>     chunk = do
>       tok <- next
>       case tok of
>         TokEndOfStream -> pure STEmpty
>         TokEmpty       -> pure STEmpty
>         TokChar c      -> pure (STChar c)
>         TokText l t    -> pure (STText l t)
>         TokLine i      -> pure (STLine i)
>         TokAnnPop      -> empty
>         TokAnnPush a   -> do
>           contents <- parseDocS
>           x <- next @(DocS a)
>           case x of
>             TokAnnPop -> return (STAnn a contents)
>             _ -> error "parseDocS: unbalanced TokAnnPop"

Now to actually convert a `DocS` to a `DocST` we just run the parser.

> toTree :: DocS a -> DocST a
> toTree sdoc = case runParser parseDocS sdoc of
>   Nothing ->
>     error "toTree: DocST conversion failed"
>   Just (x, rest) -> case rest of
>     SEmpty -> x
>     _      -> error "toTree: input not fully consumed"
