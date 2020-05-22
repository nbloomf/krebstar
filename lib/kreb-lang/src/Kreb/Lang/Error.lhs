> {-# LANGUAGE OverloadedStrings #-}

> module Kreb.Lang.Error where

> import Prelude hiding (Word)

> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, align, squote, (<+>), string, indent, vsep)

> import Kreb.Lang.Expr
> import Kreb.Lang.Type



> data ReplError
>   = RTE RuntimeError
>   | SE SourceError
>   | TE Err
>   deriving (Eq, Show)

> instance Fmt.Display ReplError where
>   display x = case x of
>     RTE y -> display y
>     SE y  -> display y
>     TE y  -> display y

> data SourceError
>   = WordAlreadyDefined Atom
>   | WordAlreadyTyped Atom
>   | SignatureMismatch Scheme Scheme
>   | DuplicateDataConst [Atom]
>   | ExtraneousVars [V Type]
>   deriving (Eq, Show)

> instance Fmt.Display SourceError where
>   display x = case x of
>     WordAlreadyDefined atom ->
>       squote (display atom) <+> "is already defined."
>     WordAlreadyTyped atom ->
>       squote (display atom) <+> "already has a type."
>     SignatureMismatch sch1 sch2 -> align $ vsep
>       [ "Signature mismatch: expected type scheme"
>       , indent 2 $ display sch1
>       , "to match"
>       , indent 2 $ display sch2
>       ]

> data RuntimeError
>   = WordNotDefined Atom
>   | UndefinedBuiltIn BuiltIn
>   | BuiltInUnrecognized String
>   | StackHeadMismatch
>   | EmptyStack
>   | ExpectedQuote
>   | TypePanic String
>   deriving (Eq, Show)

> instance Fmt.Display RuntimeError where
>   display z = case z of
>     WordNotDefined x ->
>       squote (display x) <+> "is not defined."
>     BuiltInUnrecognized str ->
>       "Built in" <+> string str <+> "is not recognized."
>     StackHeadMismatch ->
>       "Type mismatch at stack head."
>     EmptyStack ->
>       "The stack is unexpectedly empty."
>     ExpectedQuote ->
>       "Expected a quotation."
>     TypePanic str ->
>       "Type panic:" <+> string str
