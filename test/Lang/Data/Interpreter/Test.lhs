> module Lang.Data.Interpreter.Test (
>     test_Interpreter
> ) where

> import Prelude hiding (Word)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import Data.Proxy

> import Test.QuickCheck
> import Test.Tasty
> import Test.Tasty.QuickCheck

> import Lang.Data.Type
> import Lang.Data.Expr
> import Lang.Data.Interpreter
> import Lang.Data.Value
> import Lang.Data.Error
> import Lang.Data.Module
> import Lang.Data.Runtime

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
>   -> Property
> prop_InterpretAll_examples (cs, result) =
>   let Id actual = runRuntime (interpretAll cs) (initRuntimeState (const Nothing) (const Nothing)) in
>   case (result, actual) of
>     (Left err1, Left err2) -> err1 === err2
>     (Right st1, Right (st2, env2)) -> conjoin
>       [ st1 === (_rtStack env2) ]



> test_InterpretAll_examples :: TestTree
> test_InterpretAll_examples =
>   localOption (QuickCheckTests 1) $
>   testGroup "interpretAll"
>     [ testCases prop_InterpretAll_examples
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






