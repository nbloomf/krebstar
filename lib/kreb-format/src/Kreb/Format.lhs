> module Kreb.Format (
>     module Kreb.Format.Doc
>   , module Kreb.Format.Combinator
>   , module Kreb.Format.Display
> ) where

> import Kreb.Format.Doc
> import Kreb.Format.Combinator
> import Kreb.Format.Display







 > softHyphen = flatAlt mempty "-"
 > softline   = flatAlt space line



> {-







-- | >>> pretty ("hello\nworld")
-- hello
-- world
--
-- This instance uses the 'Pretty' 'Text' instance, and uses the same newline to
-- 'line' conversion.
instance IsString (Doc ann) where
    fromString = pretty . T.pack





-- | Concatenate all documents element-wise with a binary function.
--
-- @
-- 'concatWith' _ [] = 'mempty'
-- 'concatWith' (**) [x,y,z] = x ** y ** z
-- @
--
-- Multiple convenience definitions based on 'concatWith' are alredy predefined,
-- for example
--
-- @
-- 'hsep'    = 'concatWith' ('<+>')
-- 'fillSep' = 'concatWith' (\\x y -> x '<>' 'softline' '<>' y)
-- @
--
-- This is also useful to define customized joiners,
--
-- >>> concatWith (surround dot) ["Data", "Text", "Prettyprint", "Doc"]
-- Data.Text.Prettyprint.Doc






-- $
-- prop> \(NonNegative n) -> length (show (spaces n)) == n
--
-- >>> case spaces 1 of Char ' ' -> True; _ -> False
-- True
--
-- >>> case spaces 0 of Empty -> True; _ -> False
-- True
--
-- prop> \(Positive n) -> case (spaces (-n)) of Empty -> True; _ -> False



-- | @('plural' n one many)@ is @one@ if @n@ is @1@, and @many@ otherwise. A
-- typical use case is  adding a plural "s".
--
-- >>> let things = [True]
-- >>> let amount = length things
-- >>> pretty things <+> "has" <+> pretty amount <+> plural "entry" "entries" amount
-- [True] has 1 entry
plural
    :: (Num amount, Eq amount)
    => doc -- ^ @1@ case
    -> doc -- ^ other cases
    -> amount
    -> doc
plural one multiple n
    | n == 1    = one
    | otherwise = multiple




-- | Remove all annotations.
--
-- Although 'unAnnotate' is idempotent with respect to rendering,
--
-- @
-- 'unAnnotate' . 'unAnnotate' = 'unAnnotate'
-- @
--
-- it should not be used without caution, for each invocation traverses the
-- entire contained document. If possible, it is preferrable to unannotate after
-- producing the layout by using 'unAnnotateS'.


-- | Change the annotation of a 'Doc'ument.
--
-- Useful in particular to embed documents with one form of annotation in a more
-- generlly annotated document.
--
-- Since this traverses the entire @'Doc'@ tree, including parts that are not
-- rendered due to other layouts fitting better, it is preferrable to reannotate
-- after producing the layout by using @'reAnnotateS'@.
--
-- Since @'reAnnotate'@ has the right type and satisfies @'reAnnotate id = id'@,
-- it is used to define the @'Functor'@ instance of @'Doc'@.
reAnnotate :: (ann -> ann') -> Doc ann -> Doc ann'
reAnnotate re = alterAnnotations (pure . re)




-- $
-- >>> let doc = "lorem" <+> annotate () "ipsum" <+> "dolor"
-- >>> let re () = ["FOO", "BAR"]
-- >>> layoutPretty defaultLayoutOptions (alterAnnotations re doc)
-- SText 5 "lorem" (SChar ' ' (SAnnPush "FOO" (SAnnPush "BAR" (SText 5 "ipsum" (SAnnPop (SAnnPop (SChar ' ' (SText 5 "dolor" SEmpty))))))))






-- | Remove all trailing space characters.
--
-- This has some performance impact, because it does an entire additional pass
-- over the 'DocS'.
--
-- No trimming will be done inside annotations, which are considered to contain
-- no (trimmable) whitespace, since the annotation might actually be /about/ the
-- whitespace, for example a renderer that colors the background of trailing
-- whitespace, as e.g. @git diff@ can be configured to do.
removeTrailingWhitespace :: DocS ann -> DocS ann
removeTrailingWhitespace = go (RecordedWhitespace [] 0)
  where
    commitWhitespace
        :: [Int] -- Withheld lines
        -> Int -- Withheld spaces
        -> DocS ann
        -> DocS ann
    commitWhitespace is !n sds = case is of
        []      -> case n of
                       0 -> sds
                       1 -> SChar ' ' sds
                       _ -> SText n (textSpaces n) sds
        (i:is') -> let !end = SLine (i + n) sds
                   in prependEmptyLines is' end

    prependEmptyLines :: [Int] -> DocS ann -> DocS ann
    prependEmptyLines is sds0 = foldr (\_ sds -> SLine 0 sds) sds0 is

    go :: WhitespaceStrippingState -> DocS ann -> DocS ann
    -- We do not strip whitespace inside annotated documents, since it might
    -- actually be relevant there.
    go annLevel@(AnnotationLevel annLvl) = \sds -> case sds of
        SFail             -> SFail
        SEmpty            -> SEmpty
        SChar c rest      -> SChar c (go annLevel rest)
        SText l text rest -> SText l text (go annLevel rest)
        SLine i rest      -> SLine i (go annLevel rest)
        SAnnPush ann rest -> let !annLvl' = annLvl+1
                             in SAnnPush ann (go (AnnotationLevel annLvl') rest)
        SAnnPop rest
            | annLvl > 1  -> let !annLvl' = annLvl-1
                             in SAnnPop (go (AnnotationLevel annLvl') rest)
            | otherwise   -> SAnnPop (go (RecordedWhitespace [] 0) rest)
    -- Record all spaces/lines encountered, and once proper text starts again,
    -- release only the necessary ones.
    go (RecordedWhitespace withheldLines withheldSpaces) = \sds -> case sds of
        SFail -> SFail
        SEmpty -> prependEmptyLines withheldLines SEmpty
        SChar c rest
            | c == ' ' -> go (RecordedWhitespace withheldLines (withheldSpaces+1)) rest
            | otherwise -> commitWhitespace
                               withheldLines
                               withheldSpaces
                               (SChar c (go (RecordedWhitespace [] 0) rest))
        SText textLength text rest ->
            let stripped = T.dropWhileEnd (== ' ') text
                strippedLength = T.length stripped
                trailingLength = textLength - strippedLength
                isOnlySpace = strippedLength == 0
            in if isOnlySpace
                then go (RecordedWhitespace withheldLines (withheldSpaces + textLength)) rest
                else commitWhitespace
                        withheldLines
                        withheldSpaces
                        (SText strippedLength
                               stripped
                               (go (RecordedWhitespace [] trailingLength) rest))
        SLine i rest -> go (RecordedWhitespace (i:withheldLines) 0) rest
        SAnnPush ann rest -> commitWhitespace
                                 withheldLines
                                 withheldSpaces
                                 (SAnnPush ann (go (AnnotationLevel 1) rest))
        SAnnPop _ -> error "Tried skipping spaces in unannotated data! Please report this as a bug in 'prettyprinter'."

data WhitespaceStrippingState
    = AnnotationLevel !Int
    | RecordedWhitespace [Int] !Int
      -- ^ [Newline with indentation i] Spaces
  deriving Typeable



-- $
-- >>> import qualified Data.Text.IO as T
-- >>> doc = "lorem" <> hardline <> hardline <> pretty "ipsum"
-- >>> go = T.putStrLn . renderStrict . removeTrailingWhitespace . layoutPretty defaultLayoutOptions
-- >>> go doc
-- lorem
-- <BLANKLINE>
-- ipsum





-- $ Test to avoid surprising behaviour
-- >>> Unbounded > AvailablePerLine maxBound 1
-- True






-- | @('show' doc)@ prettyprints document @doc@ with 'defaultLayoutOptions',
-- ignoring all annotations.

instance Show (Doc ann) where
    showsPrec _ doc = renderShowS (layoutPretty defaultLayoutOptions doc)





> -}




-- | Automatically converts all newlines to @'line'@.
--
-- >>> pretty ("hello\nworld" :: Text)
-- hello
-- world
--
-- Note that  @'line'@ can be undone by @'group'@:
--
-- >>> group (pretty ("hello\nworld" :: Text))
-- hello world
--
-- Manually use @'hardline'@ if you /definitely/ want newlines.


-- | (lazy 'Text' instance, identical to the strict version)
instance Pretty Lazy.Text where pretty = pretty . Lazy.toStrict





{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}


-- | Conversion of the linked-list-like 'SimpleDocStream' to a tree-like
-- 'SimpleDocTree'.
module Data.Text.Prettyprint.Doc.Render.Util.SimpleDocTree (

    -- * Type and conversion
    SimpleDocTree(..),
    treeForm,



    -- * Common use case shortcut definitions
    renderSimplyDecorated,
    renderSimplyDecoratedA,
) where



import           Control.Applicative
import           Data.Text           (Text)
import qualified Data.Text           as T
import           Data.Typeable       (Typeable)
import           GHC.Generics

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Internal
import Data.Text.Prettyprint.Doc.Render.Util.Panic



import Data.Monoid (Monoid (..))
import Data.Foldable    (Foldable (..))
import Data.Traversable (Traversable (..))


-- $setup
--
-- (Definitions for the doctests)
--
-- >>> import Data.Text.Prettyprint.Doc hiding ((<>))
-- >>> import qualified Data.Text.IO as T



-- | Simplest possible tree-based renderer.
--
-- For example, here is a document annotated with @()@, and the behaviour is to
-- surround annotated regions with »>>>« and »<<<«:
--
-- >>> let doc = "hello" <+> annotate () "world" <> "!"
-- >>> let stdoc = treeForm (layoutPretty defaultLayoutOptions doc)
-- >>> T.putStrLn (renderSimplyDecorated id (\() x -> ">>>" <> x <> "<<<") stdoc)
-- hello >>>world<<<!

{-# INLINE renderSimplyDecoratedA #-}





-- $
--
-- >>> :set -XOverloadedStrings
-- >>> treeForm (layoutPretty defaultLayoutOptions ("lorem" <+> "ipsum" <+> annotate True ("TRUE" <+> annotate False "FALSE") <+> "dolor"))
-- STConcat [STText 5 "lorem",STChar ' ',STText 5 "ipsum",STChar ' ',STAnn True (STConcat [STText 4 "TRUE",STChar ' ',STAnn False (STText 5 "FALSE")]),STChar ' ',STText 5 "dolor"]









