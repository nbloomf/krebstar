> module Lang.Data.Error where

> import Prelude hiding (Word)

> import Lang.Data.Expr
> import Lang.Data.Type



> data ReplError
>   = RTE RuntimeError
>   | SE SourceError
>   | TE Err
>   deriving (Eq, Show)

> data SourceError
>   = WordAlreadyDefined Atom
>   | WordAlreadyTyped Atom
>   | SignatureMismatch Arrow Arrow
>   | DuplicateDataConst [Atom]
>   | ExtraneousVars
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
