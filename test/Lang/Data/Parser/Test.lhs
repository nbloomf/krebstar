> module Lang.Data.Parser.Test (
>     test_Parser
> ) where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Data.Proxy

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Lang.Data.Loc
> import Lang.Data.Type
> import Lang.Data.Expr
> import Lang.Data.Lexer
> import Lang.Data.Module
> import Lang.Data.Parser

> import Ned.Data.FingerTree.Test

> test_Parser :: TestTree
> test_Parser =
>   testGroup "Parsing"
>     [ test_Parse_examples
>     ]

> prop_Parse_examples
>   :: (String, Module)
>   -> Property
> prop_Parse_examples (str, ast) =
>   case runParser pModule str of
>     Left err -> error $ show err
>     Right x -> x === ast

> prop_ParsePhrase_examples
>   :: (String, Phrase)
>   -> Property
> prop_ParsePhrase_examples (str, ast) =
>   case runParser pPhrase str of
>     Left err -> error $ show err
>     Right x -> x === ast

> test_Parse_examples :: TestTree
> test_Parse_examples =
>   testGroup "Parsing Modules"
>     [ testCases prop_Parse_examples
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
>     , testCases prop_ParsePhrase_examples
>       [ ( "#1"
>         , ( "2"
>           , Then (BuiltIn (BuiltIn_Int 2)) Silence
>           )
>         )
>       ]
>     ]
