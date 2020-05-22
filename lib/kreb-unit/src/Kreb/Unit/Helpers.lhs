---
title: Test Helpers and Patterns
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE OverloadedStrings #-}
> 
> module Kreb.Unit.Helpers where
> 
> import Control.Exception
> import Data.Typeable
> 
> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format
> 
> import Kreb.Unit.Declare

:::



Introduction
------------

Here we define a stable of common unit test patterns. For instance, declaring on the value of a boolean.

> declareTrue :: Bool -> Declaration
> declareTrue p = case p of
>   True  -> return ()
>   False -> raiseMsgIO "Expected True but got False."
> 
> declareFalse :: Bool -> Declaration
> declareFalse p = case p of
>   False -> return ()
>   True  -> raiseMsgIO "Expected False but got True."

For example:

::: doctest

> -- $
> -- >>> :{
> --   testUnit $ declareTrue False
> -- :}
> -- >=> failure!
> -- Expected True but got False.

:::

Another common pattern is to assert that two values are equal, or not equal, or in a specific order.

> declareEqualTo
>   :: ( Eq a, Fmt.Display a ) => a -> a -> Declaration
> declareEqualTo u v = if u == v
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be equal to"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]
> 
> declareNotEqualTo
>   :: ( Eq a, Fmt.Display a ) => a -> a -> Declaration
> declareNotEqualTo u v = if u /= v
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be not equal to"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]
> 
> declareLessThan
>   :: ( Ord a, Fmt.Display a ) => a -> a -> Declaration
> declareLessThan u v = if v < u
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be strictly less than"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]
> 
> declareLessThanOrEqualTo
>   :: ( Ord a, Fmt.Display a ) => a -> a -> Declaration
> declareLessThanOrEqualTo u v = if v <= u
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be less than or equal to"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]
> 
> declareGreaterThan
>   :: ( Ord a, Fmt.Display a ) => a -> a -> Declaration
> declareGreaterThan u v = if v > u
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be strictly greater than"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]
> 
> declareGreaterThanOrEqualTo
>   :: ( Ord a, Fmt.Display a ) => a -> a -> Declaration
> declareGreaterThanOrEqualTo u v = if v >= u
>   then return ()
>   else raiseDocIO $ sep
>     [ "Expected"
>     , ifColumn (== 0) (indent 2) $ display v
>     , "to be greater than or equal to"
>     , ifColumn (== 0) (indent 2) $ display u
>     ]

Some examples:

::: doctest

> -- $
> -- >>> :{
> --   testUnit $ declareEqualTo 2 3
> -- :}
> -- >=> failure!
> -- Expected 3 to be equal to 2
> --
> -- $
> -- >>> :{
> --   testUnit $ declareEqualTo
> --     [1,2,3,4,5,6,7,8,9,10]
> --     [1,2,3,4,5,6,7,8,9,10,11]
> -- :}
> -- >=> failure!
> -- Expected
> --   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
> -- to be equal to
> --   [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

:::



Checking for Errors
-------------------

There are times when we want to test that some code throws a specific runtime exception. Since `Declaration`s are just `IO ()`, we can simply catch these.

> declareRaises
>   :: forall e a t
>    . ( Exception e, Fmt.Display t )
>   => (e -> Maybe t) -> IO a -> Declaration
> declareRaises p act = do
>   result <- try act
>   case result of
>     Left e -> case p e of
>       Nothing -> return ()
>       Just msg -> raiseIO msg
>     Right _ -> raiseMsgIO $ concat
>       [ "Expected to catch an exception of type "
>       , show $ typeRep (Proxy :: Proxy e), "."
>       ]

For instance:

::: doctest

> -- $
> -- >>> :{
> --   let
> --     f :: ArithException -> Maybe (Doc ())
> --     f e = if e == Overflow
> --       then Nothing
> --       else Just $
> --         reflow "Expected overflow but caught" <+> string (show e)
> --   in testUnit $ declareRaises f $
> --     throw DivideByZero
> -- :}
> -- >=> failure!
> -- Expected overflow but caught divide by zero
> --
> -- $
> -- >>> :{
> --   let
> --     f :: ArithException -> Maybe (Doc ())
> --     f e = if e == Overflow
> --       then Nothing
> --       else Just $
> --         reflow "Expected overflow but caught" <+> string (show e)
> --   in testUnit $ declareRaises f $
> --     throw Overflow
> -- :}
> -- >=> ok.

:::

One possible disadvantage of `declareRaises` (depending on what you want) is that it only catches exceptions of a specific type. `declareRaisesSomeException` checks that _any_ exception is thrown.

> declareRaisesSomeException
>   :: IO a -> Declaration
> declareRaisesSomeException act =
>   let
>     err :: SomeException -> Maybe ()
>     err = const Nothing
>   in declareRaises err act

For example:

::: doctest

> -- $
> -- >>> :{
> --   testUnit $ declareRaisesSomeException $
> --     throw DivideByZero
> -- :}
> -- >=> ok.
> --
> -- $
> -- >>> :{
> --   testUnit $ declareRaisesSomeException $
> --     return ()
> -- :}
> -- >=> failure!
> -- Expected to catch an exception of type SomeException.

:::



Parameterized Unit Tests
------------------------

In OO languages, a common unit testing pattern is the use of _data providers_ to run the same (parameterized) unit test on multiple inputs, making the test more succinct and more flexible by factoring out "unnecessary" details. We could do this by simply `mapM_`-ing a function with signature `a -> Declaration` over a list of inputs. However, a nice quality of life improvement over the naive strategy is to have the parameter test report, if it fails, _which_ of the inputs it fails on. This is a tidy application of the `State` monad. 

> withInputs
>   :: forall f a
>    . ( Traversable f
>      , Fmt.Display a )
>   => f a -> (a -> IO ()) -> IO ()
> withInputs xs act =
>   let
>     act' :: (Int, a) -> IO ()
>     act' (k,a) = do
>       prependPrettyException
>         (string $ "test case #" ++ show k ++ ":")
>         $ act a
> 
>     getIndex :: a -> State Int (Int, a)
>     getIndex a = do
>       k <- get
>       mutate (+ (1 :: Int))
>       return (k, a)
>
>     xs' :: f (Int, a)
>     xs' = runState 1 $ mapM getIndex xs
>   in sequence_ $ fmap act' xs'

For example:

::: doctest

> -- $
> -- >>> :{
> --   let
> --     datums :: [Int]
> --     datums = [ 1, 2, 3, 4, 5 ]
> --   in testUnit $ withInputs datums $ \k ->
> --     declareLessThan 4 k
> -- :}
> -- >=> failure!
> -- test case #4:
> --   Expected 4 to be strictly less than 4

:::

`withInputs` can apparently only handle parameterized tests with one argument -- but of course with some uncurrying it can handle any number of arguments (at least, as many as GHC can store in a tuple). We give some helpers for this too.

> withInputs2
>   :: ( Traversable f
>      , Fmt.Display a1, Fmt.Display a2 )
>   => f (a1,a2)
>   -> (a1 -> a2 -> IO ())
>   -> IO ()
> withInputs2 xs act = withInputs xs (uncurry act)
> 
> withInputs3
>   :: ( Traversable f
>      , Fmt.Display a1, Fmt.Display a2, Fmt.Display a3 )
>   => f (a1,a2,a3)
>   -> (a1 -> a2 -> a3 -> IO ())
>   -> IO ()
> withInputs3 xs act = withInputs xs (uncurry3 act)
> 
> withInputs4
>   :: ( Traversable f
>      , Fmt.Display a1, Fmt.Display a2, Fmt.Display a3, Fmt.Display a4 )
>   => f (a1,a2,a3,a4)
>   -> (a1 -> a2 -> a3 -> a4 -> IO ())
>   -> IO ()
> withInputs4 xs act = withInputs xs (uncurry4 act)
