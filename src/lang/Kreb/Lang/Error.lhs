> module Kreb.Lang.Error where

> import Prelude hiding (Word)

> import Kreb.Lang.Expr
> import Kreb.Lang.Type



> data ReplError
>   = RTE RuntimeError
>   | SE SourceError
>   | TE Err
>   deriving (Eq, Show)

> data SourceError
>   = WordAlreadyDefined Atom
>   | WordAlreadyTyped Atom
>   | SignatureMismatch Scheme Scheme
>   | DuplicateDataConst [Atom]
>   | ExtraneousVars [V Type]
>   deriving (Eq, Show)

> data RuntimeError
>   = WordNotDefined Atom
>   | UndefinedBuiltIn BuiltIn
>   | BuiltInUnrecognized String
>   | StackHeadMismatch
>   | EmptyStack
>   | ExpectedQuote
>   | TypePanic String
>   deriving (Eq, Show)
