---
title: Dirt Cheap Unit Tests
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE OverloadedStrings #-}
> 
> module Kreb.Unit.Declare where
> 
> import Control.Applicative
> import Control.Exception
> import Data.Typeable
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (align, fillSep, hang, string, indent, display, (<+>), lineS, nest, sep)

:::



Introduction
------------

What is the simplest possible interface for a unit testing library you can think of? What is the essence of _unit test_? Bearing in mind that we're working with an extremely strict type system, and would like to be able to do IO in our tests if needed.

Well the test will have to be `IO` _something_, and presumably the something needs to allow us to signal whether the test succeeded, along with any diagnostic info. But what, exactly? We can't predict in advance all the possible needs the consumers of a unit testing framework will have.

Fortunately there is another way. Haskell (or rather GHC, although that distinction is not very big) has a powerful and type safe exception mechanism built in, whereby any code -- pure or not -- can throw an exception, which can be caught in `IO`. Moreover the interface for throwing exceptions is standardized and very flexible such that consumers can provide their own exception types.

So this is the compromise: rather than trying to anticipate all the different ways we might want to define a unit test and its failure, we will just say that:

1. A unit test is an `IO` action,
2. A unit test _fails_ if it throws an exception, and
3. A unit test _succeeds_ if it does not throw an exception.

Since the return type of the action plays no role here, it can be trivial. Thus we define our type of unit tests, called `Declaration`s, like so.

> type Declaration = IO ()

To actually "run" the test we evaluate the action and catch any thrown exceptions.

> testDeclaration
>   :: Declaration -> IO (Maybe String)
> testDeclaration dec = do
>   result <- try dec
>   return $ case result of
>     Right () -> Nothing
>     Left (err :: SomeException) -> Just (show err)

Lastly we write a helper function to print the test result to the console.

> testUnit :: Declaration -> IO ()
> testUnit dec = do
>   result <- testDeclaration dec
>   case result of
>     Nothing -> do
>       putStrLn ">=> ok."
>     Just ex -> do
>       putStrLn ">=> failure!"
>       putStrLn ex

To run a unit test we just pass it to `testUnit`.

::: doctest

> -- $
> -- >>> :{
> --   testUnit $ return ()
> -- :}
> -- >=> ok.
> --
> -- $
> -- >>> :{
> --   testUnit $ throw DivideByZero
> -- :}
> -- >=> failure!
> -- divide by zero

:::



Prettier Exceptions
-------------------

That's that, right? We already have the bones of a simple unit testing library. There's an awful lot it doesn't do for us, like generating nice reports, but the essence is there.

As a first quality of life upgrade, it would be nice if our unit testing library integrated with our pretty printing library to give formatted output. We can do this with a custom exception type.

> data PrettyException
>   = PE (Maybe Fmt.LayoutOptions) (Fmt.Doc ())
> 
> instance Show PrettyException where
>   show (PE opts msg) = case opts of
>     Nothing -> Fmt.renderString msg
>     Just lo -> Fmt.renderStringWith (Fmt.Smart lo) msg
> 
> instance Exception PrettyException

Now we can throw `PrettyException`s in our declarations, and when the resulting errors get `show`ed in `testDeclaration` they will be pretty printed first. We don't even need to expose the type outside of this module; it's enough to provide some simple wrappers around `throw` and `throwIO`, which we call `raise*`. The `*Msg` variants are convenience functions for when we just want to throw a string.

> raiseDoc
>   :: Fmt.Doc a -> b
> raiseDoc doc =
>   let err = PE Nothing (Fmt.unAnnotate doc)
>   in throw err
> 
> raise
>   :: ( Fmt.Display t ) => t -> a
> raise = raiseDoc . Fmt.display
> 
> raiseMsg
>   :: String -> a
> raiseMsg = raiseDoc . Fmt.reflow
> 
> raiseDocIO
>   :: Fmt.Doc a -> IO b
> raiseDocIO doc =
>   let err = PE Nothing (Fmt.unAnnotate doc)
>   in throwIO err
> 
> raiseIO
>   :: ( Fmt.Display t ) => t -> IO a
> raiseIO = raiseDocIO . Fmt.display
> 
> raiseMsgIO
>   :: String -> IO a
> raiseMsgIO = raiseDocIO . Fmt.reflow

We're bootstrapping a test library from scratch -- but what tests the testers? We use a separate doctest tool to interleave some simple tests with the source code. These serve as executable examples and, because they are machine checked, a basic test.

::: doctest

> -- $
> -- >>> :{
> --   testUnit $ raiseMsg "bork bork bork"
> -- :}
> -- >=> failure!
> -- bork bork bork

:::

For funsies, we also provide helpers for catching and re-throwing pretty exceptions with adjustments.

> adjustPrettyException
>   :: (PrettyException -> PrettyException)
>   -> IO a -> IO a
> adjustPrettyException f act = do
>   result <- try act
>   case result of
>     Right a  -> return a
>     Left err -> throw (f err)
> 
> prependPrettyException
>   :: ( Fmt.Display t )
>   => t -> IO a -> IO a
> prependPrettyException t act =
>   let
>     f (PE opts doc) =
>       PE opts (display t <> lineS <> indent 2 doc)
>   in adjustPrettyException f act

For example:

::: doctest

> -- $
> -- >>> :{
> --   testUnit $
> --     prependPrettyException (string "in unit:") $
> --       raiseMsg "bork bork bork"
> -- :}
> -- >=> failure!
> -- in unit:
> --   bork bork bork

:::
