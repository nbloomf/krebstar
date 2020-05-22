---
title: Test Outcomes
author: nbloomf
---

::: frontmatter

> module Kreb.Prop.Check where
> 
> import System.Random (newStdGen, StdGen)
> import Data.List (find)
> 
> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format
> 
> import Kreb.Prop.Sample
> import Kreb.Prop.Arb

:::



Introduction
------------

We now have a monad for expressing values drawn from a distribution and a class of types which can be randomly generated. Recall that the goal is to have a concept of "thing that takes an arbitrary input and either succeeds or fails" -- and with pruning taken into account, the arbitrary input is not just a value, but a tree of values, with entries getting smaller toward the leaves.

To get there, next we will define (1) a type describing the outcome of a test and (2) a class of things that can be property tested.



Outcomes
--------

A specific test case can have one of three outcomes:

- _Discard_ means the input was somehow invalid. This outcome must be accompanied by a human-readable reason for the invalidation.
- _Accept_ means the input was valid and the test succeeded; this is boring.
- _Reject_ means the input was valid and the test failed. This outcome must be accompanied by a human-readable reason for the failure, as well as a representation of the input that caused the failure.

> data Outcome
>   = Accept
>   | Discard Reason
>   | Reject Reason [ShowArg]
> 
> type Reason = Fmt.Doc ()
> type ShowArg = Fmt.Doc ()

We'll need some helper functions for adding an argument to `Reject` outcomes.

> prependArg :: ShowArg -> Outcome -> Outcome
> prependArg arg z = case z of
>   Accept -> Accept
>   Discard msg -> Discard msg
>   Reject msg args -> Reject msg (arg:args)

Each generated test case has an implicit tree of pruned values. When a property fails, we generate enough of this tree to find a minimal failing test case. To keep track of these, and their corresponding failures, the outcome of a _property_ test will actually be a tree of `Outcomes`. N-ary trees like this are commonly called _rose trees_.

> data Rose a
>   = Rose a [Rose a]
>   deriving (Eq, Show)
> 
> instance Functor Rose where
>   fmap f (Rose a as) =
>     Rose (f a) (map (fmap f) as)
> 
> unfurl
>   :: (a -> [a]) -> a -> Rose a
> unfurl f x =
>   Rose x (map (unfurl f) (f x))
> 
> joinRose :: Rose (Rose a) -> Rose a
> joinRose (Rose (Rose x xs) ys) =
>   Rose x (map joinRose ys ++ xs)
> 
> isFailure :: Rose Outcome -> Bool
> isFailure (Rose z _) = case z of
>   Reject _ _ -> True
>   _          -> False

`minimizeFailure` is a helper function which traverses a list of outcome trees looking for a minimal `Reject`. It keeps track of the number of successful prunes, as well as the number of unsuccessful prunes after the last successful prune.

> minimizeFailure
>   :: [Rose Outcome] -> Int
>   -> (Int, Int, Reason, [ShowArg])
>   -> (Int, Int, Reason, [ShowArg])
> minimizeFailure next m (prunes, pruneAttempts, msg, args) =
>   case find isFailure $ take m next of
>     Nothing -> (prunes, pruneAttempts + length next, msg, args)
>     Just (Rose (Reject msg0 args0) ts) ->
>       minimizeFailure ts m (1 + prunes, 0, msg0, args0)

Now a `Check` is a computation in the `Sample` monad which produces a tree of outcomes -- this is the type of our property tests. The test runner will take one of these, generate the tree of outcomes, and minimize failure if it exists.

> newtype Check = Check
>   { unCheck :: Sample (Rose Outcome) }

The `explain` combinator is a useful escape hatch for annotating test failures with extra information.

> explain
>   :: Fmt.Doc () -> Check -> Check
> explain ann (Check x) =
>   Check $ fmap (fmap f) x
>   where
>     f :: Outcome -> Outcome
>     f o = case o of
>       Accept -> Accept
>       Discard msg -> Discard (fillSep [ann, msg])
>       Reject msg args -> Reject (fillSep [ann, msg]) args



Checkable Types
---------------

Under the hood our property tests work with the `Check` type. But in practice it is useful to abstract that detail behind a class of types which can be converted into `Check`s.

> class Checkable t where
>   check :: t -> Check
> 
> instance Checkable Check where
>   check = id

Of course `Check` itself is trivially checkable, as is `Outcome`.

> instance Checkable Outcome where
>   check x = Check $ return $ Rose x []
> 
> accept :: Check
> accept = check Accept
> 
> discard :: Reason -> Check
> discard msg = check $ Discard msg
> 
> reject :: Reason -> Check
> reject msg = check $ Reject msg []

We can now start to define some simple

> claimTrue
>   :: Bool -> Check
> claimTrue p =
>   if p
>     then accept
>     else reject $ reflow "Expected True but got False."
> 
> claimFalse
>   :: Bool -> Check
> claimFalse p =
>   if p
>     then reject $ reflow "Expected False but got True."
>     else accept

A common test pattern is to assert that two values are equal or in order; we provide helpers for this.

> claimEqual
>   :: ( Eq a, Fmt.Display a )
>   => a -> a -> Check
> claimEqual x y =
>   if x == y
>     then accept
>     else reject $ sep
>       [ string "expecting"
>       , ifColumn (== 0) (indent 2) $ display x
>       , reflow "to equal"
>       , ifColumn (== 0) (indent 2) $ display y
>       ]
> 
> claimLT
>   :: ( Ord a, Fmt.Display a )
>   => a -> a -> Check
> claimLT x y =
>   if x < y
>     then accept
>     else reject $ sep
>       [ string "expecting"
>       , ifColumn (== 0) (indent 2) $ display x
>       , reflow "to be less than"
>       , ifColumn (== 0) (indent 2) $ display y
>       ]
> 
> claimLEQ
>   :: ( Ord a, Fmt.Display a )
>   => a -> a -> Check
> claimLEQ x y =
>   if x <= y
>     then accept
>     else reject $ sep
>       [ string "expecting"
>       , ifColumn (== 0) (indent 2) $ display x
>       , reflow "to be less than or equal to"
>       , ifColumn (== 0) (indent 2) $ display y
>       ]

> claimEqualIn
>   :: forall ctx a
>    . ( EqIn ctx a, Fmt.Display a, Fmt.Display ctx )
>   => ctx -> a -> a -> Check
> claimEqualIn ctx x y =
>   if eqIn ctx x y
>     then accept
>     else reject $ sep
>       [ string "expecting"
>       , ifColumn (== 0) (indent 2) $ display x
>       , reflow "to equal"
>       , ifColumn (== 0) (indent 2) $ display y
>       , reflow "within the context"
>       , ifColumn (== 0) (indent 2) $ display ctx
>       ]

We can also define _and_ and _or_ on the level of `Check`s, as well as n-ary equivalents. Note that these are short-circuiting from the left.

> (.&&.)
>   :: ( Checkable check1, Checkable check2 )
>   => check1 -> check2 -> Check
> u .&&. v =
>   let
>     Check x = check u
>     Check y = check v
>   in Check $ do
>     Rose a _ <- x
>     case a of
>       Accept -> y
>       _      -> x
> 
> claimAll
>   :: ( Checkable check )
>   => [check] -> Check
> claimAll = foldr (.&&.) accept
> 
> (.||.) :: Check -> Check -> Check
> (Check x) .||. (Check y) = Check $ do
>   Rose a _ <- x
>   case a of
>     Accept -> x
>     _      -> y
> 
> claimAny :: [Check] -> Check
> claimAny = foldr (.||.) (reject $ reflow "No accepted alternatives")

The `provisio` combinator allows us to conditionally discard a check with an explanation. This is useful when writing a more specific test case generator is not feasible.

> provisio
>   :: ( Fmt.Display t, Checkable check )
>   => [(t, Bool)] -> check -> Check
> provisio ps ch =
>   case fst <$> find (not . snd) ps of
>     Nothing -> check ch
>     Just t  -> discard $ display t

While we're here we can define a `Checkable` instance for booleans, although it's not very interesting.

> instance Checkable Bool where
>   check p = Check $ return $
>     case p of
>       True ->
>         Rose Accept []
>       False ->
>         Rose (Reject (reflow "False =/= True") []) []

Checking outcomes and booleans is fine and all, but we can't actually write _property_ tests with them alone -- at least not very interesting ones. What we need is a `Checkable` instance for functions that generates arbitrary inputs. And here it is:

> instance
>   ( Fmt.Display a, Arb a, Prune a, Checkable check
>   ) => Checkable (a -> check)
>   where
>     check = forEach arb prune
> 
> forEach
>   :: ( Fmt.Display a, Checkable check )
>   => Sample a -> (a -> [a]) -> (a -> check) -> Check
> forEach gen sh f = Check $ do
>   arg <- gen
>   results sh arg f
> 
> results
>   :: forall a t
>    . ( Fmt.Display a, Checkable t )
>   => (a -> [a]) -> a -> (a -> t) -> Sample (Rose Outcome)
> results f arg ch = do
>   G s <- next
>   env <- ask
>   let
>     result :: a -> Rose Outcome
>     result x = fmap (prependArg $ display x) $
>       runSample s env (unCheck $ check $ ch x)
>   return $ joinRose $ fmap result $ unfurl f arg

The complexity is in `results`. Note that a function of type `a -> b -> check` is implicitly a function of type `a -> (b -> check)`; this means our `Checkable` instance for functions handles functions with any number of arguments. (Because, recall, every haskell function is really a function of one argument that might return another function.)
