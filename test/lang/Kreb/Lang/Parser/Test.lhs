> module Kreb.Lang.Parser.Test (
>     test_Parser
> ) where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Data.Proxy

> import Test.Tasty

> import Kreb.Check

> import Kreb.Lang.Loc
> import Kreb.Lang.Type
> import Kreb.Lang.Expr
> import Kreb.Lang.Lexer
> import Kreb.Lang.Module
> import Kreb.Lang.Parser



> test_Parser :: TestTree
> test_Parser =
>   testGroup "Parsing"
>     [ test_Parse_examples
>     ]

> prop_Parse_examples
>   :: (String, Module)
>   -> Check
> prop_Parse_examples (str, ast) =
>   case runParser pModule str of
>     Left err -> reject $ show err
>     Right x -> claimEqual x ast

> prop_ParsePhrase_examples
>   :: (String, Phrase)
>   -> Check
> prop_ParsePhrase_examples (str, ast) =
>   case runParser pPhrase str of
>     Left err -> reject $ show err
>     Right x -> claimEqual x ast

> test_Parse_examples :: TestTree
> test_Parse_examples =
>   testGroup "Parsing Modules"
>     [ testKrebCases "Parsing"
>       prop_Parse_examples
>       [ ( "#1"
>         , ( "@define foo :: S -> S == #id\n@end"
>           , Module
>               [ Definition
>                   (Atom "foo")
>                   (Then (BuiltIn BuiltIn_Id) Silence)
>                   (ForAll
>                     (Vars [V "S"] []) $ Arrow
>                     (Stack (V "S") [])
>                     (Stack (V "S") []))]
>           )
>         )
>       ]
> 
>     , testKrebCases "Parse"
>       prop_ParsePhrase_examples
>       [ ( "#1"
>         , ( "2"
>           , Then (BuiltIn (BuiltIn_Int 2)) Silence
>           )
>         )
>       ]
>     ]
