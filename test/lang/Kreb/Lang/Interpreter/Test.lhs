> module Kreb.Lang.Interpreter.Test (
>     test_Interpreter
> ) where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Data.Proxy

> import Test.Tasty

> import Kreb.Lang.Type
> import Kreb.Lang.Expr
> import Kreb.Lang.Interpreter
> import Kreb.Lang.Value
> import Kreb.Lang.Error
> import Kreb.Lang.Module
> import Kreb.Lang.Runtime

> import Kreb.Check



> test_Interpreter :: TestTree
> test_Interpreter =
>   testGroup "Interpreter"
>     [ test_InterpretAll_examples
>     ]

> data Id a = Id a
>   deriving (Eq, Show)
> 
> instance Functor Id where
>   fmap f (Id a) = Id (f a)
> 
> instance Applicative Id where
>   pure = Id
>   (Id f) <*> (Id x) = Id (f x)
> 
> instance Monad Id where
>   return = Id
>   (Id x) >>= f = f x

> prop_InterpretAll_examples
>   :: ([Command], Either ReplError DataStack)
>   -> Check
> prop_InterpretAll_examples (cs, result) =
>   let Id actual = runRuntime (interpretAll cs) (initRuntimeState (const Nothing) (const Nothing)) in
>   case (result, actual) of
>     (Left err1, Left err2) -> claimEqual err1 err2
>     (Right st1, Right (st2, env2)) -> checkAll
>       [ claimEqual st1 (_rtStack env2) ]



> test_InterpretAll_examples :: TestTree
> test_InterpretAll_examples =
>   localOption (KrebCheckTests 1) $
>   testGroup "interpretAll"
>     [ testKrebCases "interpretAll"
>       prop_InterpretAll_examples
>       [ ( "(empty)"
>         , ( []
>           , Right Empty
>           )
>         )
> 
>       , ( "2"
>         , ( [ Query $ Then (BuiltIn $ BuiltIn_Int 2) Silence ]
>           , Right $
>               Cons Empty (V_Prim (Prim_Int 2))
>           )
>         )
> 
>       , ( "2 3"
>         , ( [ Query $ Then (BuiltIn $ BuiltIn_Int 2) (Then (BuiltIn $ BuiltIn_Int 3) Silence) ]
>           , Right $
>               Cons (Cons Empty (V_Prim (Prim_Int 2))) (V_Prim (Prim_Int 3))
>           )
>         )
>       ]
>     ]
