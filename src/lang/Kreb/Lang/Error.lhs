> module Kreb.Lang.Error where

> import Prelude hiding (Word)

> import Kreb.Format
> import Kreb.Lang.Expr
> import Kreb.Lang.Type



> data ReplError
>   = RTE RuntimeError
>   | SE SourceError
>   | TE Err
>   deriving (Eq, Show)

> instance DisplayNeat ReplError where
>   displayNeat x = case x of
>     RTE y -> displayNeat y
>     SE y -> displayNeat y
>     TE y -> displayNeat y

> data SourceError
>   = WordAlreadyDefined Atom
>   | WordAlreadyTyped Atom
>   | SignatureMismatch Scheme Scheme
>   | DuplicateDataConst [Atom]
>   | ExtraneousVars [V Type]
>   deriving (Eq, Show)

> instance DisplayNeat SourceError where
>   displayNeat x = case x of
>     WordAlreadyDefined atom -> concat
>       [ "\'", displayNeat atom, "\' is already defined.\n" ]
>     WordAlreadyTyped atom -> concat
>       [ "\'", displayNeat atom, "\' already has a type.\n" ]
>     SignatureMismatch sch1 sch2 -> concat
>       [ "Signature mismatch: expected type scheme\n"
>       , displayNeat sch1, "\n"
>       , "to match\n"
>       , displayNeat sch2 ]

> data RuntimeError
>   = WordNotDefined Atom
>   | UndefinedBuiltIn BuiltIn
>   | BuiltInUnrecognized String
>   | StackHeadMismatch
>   | EmptyStack
>   | ExpectedQuote
>   | TypePanic String
>   deriving (Eq, Show)

> instance DisplayNeat RuntimeError where
>   displayNeat z = case z of
>     WordNotDefined x -> concat
>       [ "\'", displayNeat x, "\' is not defined." ]
