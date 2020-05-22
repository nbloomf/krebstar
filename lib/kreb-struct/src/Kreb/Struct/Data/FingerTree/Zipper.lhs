---
title: One-Pointed Lists
---

::: contents
* [The Derivative of a List](#the-derivative-of-a-list): Introduction to the problem
* [Constructors](#constructors): Building examples
* [Class Instances](#class-instances): Code for free
* [Queries](#queries): Extracting values from a one-pointed list
* [Navigation](#navigation): Moving around the list
* [Mutation](#mutation): Changing, adding, and removing list items
* [Measurement](#measurement): Working with value annotations
* [As Finger Trees](#as-finger-trees): Converting to and from
* [Concatenation](#concatenation): Combining one-pointed lists
* [Testing and Debugging](#testing-and-debugging): Test helpers
:::



::: frontmatter

> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE StandaloneDeriving #-}
> {-# LANGUAGE OverloadedStrings #-}
> 
> module Kreb.Struct.Data.FingerTree.Zipper where
> 
> import Data.List (unwords)
> import Data.Foldable
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Format (display, (<+>))
> import           Kreb.Prop
> import           Kreb.Control
> import           Kreb.Control.Constrained

> import           Kreb.Struct.Class
> import qualified Kreb.Struct.Data.FingerTree as FT

:::



The Derivative of a List
------------------------

Algebraic types are a powerful tool for modeling data, but they do have some tradeoffs. One of the most painful is a general lack of _random access_. As an example consider the standard implementation of cons-lists.

::: example
``` haskell
data List a = Nil | Cons a (List a)
```
:::

Suppose we have a specific nonempty list of `a`s constructed via nested applications of `Cons`. There's exactly one entry in this list that we're guaranteed to have constant-effort access to: the first one. To get an arbitrary element of the list requires destructuring -- that is, peeling of list elements starting at the head. On average this will take $n/2$ destructurings where $n$ is the length of the list. This is okay for some applications, but there are times when we really need efficient random access.

Full-on random access lists are achievable in Haskell using, for instance, the `Array` interface, but this comes at a cost: we lose the benefits of algebraic types. If we're willing to settle for something between the extremes of "constant access only at the head" and "constant random access" there's another solution, called a _zipper_, that looks something like this.

::: example
``` haskell
data Zipper a = Nada | Zipper (List a) a (List a)
```
:::

Note first of all that `Zipper` is defined in terms of `List`. We can interpret this definition as follows: a `Zipper` is either empty, or it has a special value `a` together with a list of values to the left and a list of values to the right. Collectively these are equivalent to a single list except that the special value acts like a _pointer_ to some position in the middle of the list. Moreover, we can move this pointer to the left or right with constant effort.

::: example
``` haskell
moveLeft :: Zipper a -> Zipper a
moveLeft z = case z of
  Nada -> Nada
  Zipper as x bs -> case as of
    [] -> Zipper [] x bs
    u:us -> Zipper us u (x:bs)
```
:::

So `Zipper` is not quite as nice as a random access array, but we've traded the expensive access pattern of `List` for a kind of constant-effort _read head_ that can move about the list. The major advantage of `Zipper` is that it is an algebraic type, making it easier to reason about.

Here's the big idea: the shift from `List` to `Zipper` is just one example of a more general pattern for constructing navigable types like this. The general pattern applies to any polynomial type -- that is, built out of constants, type variables, products, and sums -- and this pattern is based on calculus. The elevator pitch is that if we have an algebraic type given by a polynomial functor, we can treat the definition like a high school algebra style implicit function definition. Then the _derivative_ of this function is again a polynomial functor and represents _one-hole contexts_ over the original type.

In this module we will develop such a type from scratch, called `OnePointedList`. Specifically, we'll define something analogous to `Zipper`, but using `FingerTree` as the underlying list representation rather than naive cons-lists. The definition is pretty simple: a `OnePointedList` is either `Vacant`, or has a special value called the _point_ together with finger trees of values to the left and to the right of the point.

> data FingerTreeZipper a where
>   Empty
>     :: ( Valued a )
>     => FingerTreeZipper a
>   NonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a
>     -> FingerTreeZipper a
> 
> deriving instance Eq a   => Eq (FingerTreeZipper a)
> deriving instance Show a => Show (FingerTreeZipper a)

> data NonEmptyFingerTreeZipper a where
>   Point
>     :: ( Valued a )
>     => FT.FingerTree a
>     -> a
>     -> FT.FingerTree a
>     -> NonEmptyFingerTreeZipper a
> 
> deriving instance Eq a   => Eq (NonEmptyFingerTreeZipper a)
> deriving instance Show a => Show (NonEmptyFingerTreeZipper a)

(Note that, since this is defined in terms of `FingerTree`, we have an extra value type parameter `m`.) `OnePointedList` combines the benefits of `Zipper` with the efficient splitting and deque operations of `FingerTree`. Recall that we have amortized constant time access to both ends of a finger tree, so `OnePointedList` gives us fast access to the beginning and end of the list in addition to the read head.



Constructors
------------

As usual, we'll build up a theory of `OnePointedList`s by defining queries and operations on them. First though we need some constructors. Two natural candidates are `empty` and `singleton`, which construct lists with zero and one item, respectively.

> fromTupFingerTreeZipper
>   :: ( Valued a )
>   => Maybe ([a],a,[a])
>   -> FingerTreeZipper a
> fromTupFingerTreeZipper x = case x of
>   Nothing -> Empty
>   Just z -> NonEmpty $ fromTupNonEmptyFingerTreeZipper z
> 
> toTupFingerTreeZipper
>   :: ( Valued a )
>   => FingerTreeZipper a
>   -> Maybe ([a],a,[a])
> toTupFingerTreeZipper x = case x of
>   Empty -> Nothing
>   NonEmpty z -> Just $ toTupNonEmptyFingerTreeZipper z
> 
> fromTupNonEmptyFingerTreeZipper
>   :: ( Valued a )
>   => ([a],a,[a])
>   -> NonEmptyFingerTreeZipper a
> fromTupNonEmptyFingerTreeZipper (as,x,bs) =
>   Point (fromList as) x (fromList bs)
> 
> toTupNonEmptyFingerTreeZipper
>   :: ( Valued a )
>   => NonEmptyFingerTreeZipper a
>   -> ([a],a,[a])
> toTupNonEmptyFingerTreeZipper (Point as x bs) =
>   (toList as, x, toList bs)

> instance Container FingerTreeZipper where
>   type ContainerConstraint FingerTreeZipper = Valued
> 
> instance Container NonEmptyFingerTreeZipper where
>   type ContainerConstraint NonEmptyFingerTreeZipper = Valued

> instance Subset NonEmptyFingerTreeZipper where
>   type SupersetOf NonEmptyFingerTreeZipper = FingerTreeZipper
> 
>   inject
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> FingerTreeZipper a
>   inject = NonEmpty
> 
>   restrict
>     :: ( Valued a )
>     => FingerTreeZipper a -> Maybe (NonEmptyFingerTreeZipper a)
>   restrict x = case x of
>     Empty -> Nothing
>     NonEmpty w -> Just w

> instance NonEmpty NonEmptyFingerTreeZipper where
>   empty
>     :: ( Valued a )
>     => FingerTreeZipper a
>   empty = Empty
> 
>   isEmpty
>     :: ( Valued a )
>     => FingerTreeZipper a -> Bool
>   isEmpty x = case x of
>     Empty -> True
>     NonEmpty _ -> False



> instance Singleton FingerTreeZipper where
>   singleton
>     :: ( Valued a )
>     => a -> FingerTreeZipper a
>   singleton = NonEmpty . singleton
> 
>   isSingleton
>     :: ( Valued a )
>     => FingerTreeZipper a -> Bool
>   isSingleton w = case w of
>     Empty -> False
>     NonEmpty x -> isSingleton x



> instance Singleton NonEmptyFingerTreeZipper where
>   singleton
>     :: ( Valued a )
>     => a -> NonEmptyFingerTreeZipper a
>   singleton a =
>     Point mempty a mempty
> 
>   isSingleton
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> Bool
>   isSingleton (Point as _ bs) =
>     (isEmpty as) && (isEmpty bs)
> 
> instance SubsetSingleton NonEmptyFingerTreeZipper
> instance NonEmptySingleton NonEmptyFingerTreeZipper


> instance LinearZipper FingerTreeZipper where
>   readPointer
>     :: ( Valued a )
>     => FingerTreeZipper a -> Maybe a
>   readPointer x = case x of
>     Empty -> Nothing
>     NonEmpty w -> readPointer w
> 
>   alterPointer
>     :: ( Valued a )
>     => (a -> a) -> FingerTreeZipper a -> FingerTreeZipper a
>   alterPointer f w = case w of
>     Empty -> Empty
>     NonEmpty u -> NonEmpty $ alterPointer f u
> 
>   alterPointerM
>     :: ( Valued a, Monad m )
>     => (a -> m a) -> FingerTreeZipper a -> m (FingerTreeZipper a)
>   alterPointerM f w = case w of
>     Empty -> return Empty
>     NonEmpty u -> fmap NonEmpty $ alterPointerM f u
> 
>   isAtStart
>     :: ( Valued a )
>     => FingerTreeZipper a -> Bool
>   isAtStart x = case x of
>     Empty -> False
>     NonEmpty w -> isAtStart w
> 
>   isAtEnd
>     :: ( Valued a )
>     => FingerTreeZipper a -> Bool
>   isAtEnd x = case x of
>     Empty -> False
>     NonEmpty w -> isAtEnd w
> 
>   moveTowardStart
>     :: ( Valued a )
>     => FingerTreeZipper a -> FingerTreeZipper a
>   moveTowardStart x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty $ moveTowardStart w
> 
>   moveTowardEnd
>     :: ( Valued a )
>     => FingerTreeZipper a -> FingerTreeZipper a
>   moveTowardEnd x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty $ moveTowardEnd w
> 
>   moveToStart 
>     :: ( Valued a )
>     => FingerTreeZipper a -> FingerTreeZipper a
>   moveToStart x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty $ moveToStart w
> 
>   moveToEnd
>     :: ( Valued a )
>     => FingerTreeZipper a -> FingerTreeZipper a
>   moveToEnd x = case x of
>     Empty -> Empty
>     NonEmpty w -> NonEmpty $ moveToEnd w


> instance LinearZipper NonEmptyFingerTreeZipper where
>   readPointer
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> Maybe a
>   readPointer (Point _ a _) = Just a
> 
>   alterPointer
>     :: ( Valued a )
>     => (a -> a) -> NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper a
>   alterPointer f (Point as x bs) =
>     Point as (f x) bs
> 
>   alterPointerM
>     :: ( Valued a, Monad m )
>     => (a -> m a) -> NonEmptyFingerTreeZipper a -> m (NonEmptyFingerTreeZipper a)
>   alterPointerM f (Point as x bs) =
>     f x >>= \z -> return (Point as z bs)
> 
>   isAtStart
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> Bool
>   isAtStart (Point as _ _) = isEmpty as
> 
>   isAtEnd
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> Bool
>   isAtEnd (Point _ _ bs) = isEmpty bs
> 
>   moveTowardStart
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper a
>   moveTowardStart (Point as x bs) = case unsnoc as of
>     Nothing -> Point mempty x bs
>     Just (a, as') -> Point as' a (cons x bs)
> 
>   moveTowardEnd
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper a
>   moveTowardEnd (Point as x bs) = case uncons bs of
>     Nothing -> Point as x mempty
>     Just (b, bs') -> Point (snoc x as) b bs'
> 
>   moveToStart 
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper a
>   moveToStart (Point as x bs) = case uncons as of
>     Nothing -> Point mempty x bs
>     Just (a, as') -> Point mempty a (as' <> cons x bs)
> 
>   moveToEnd
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper a
>   moveToEnd (Point as x bs) = case unsnoc bs of
>     Nothing -> Point as x mempty
>     Just (b, bs') -> Point (as <> cons x bs') b mempty





> {-


It's also natural to convert lists to `OnePointedList`s; we'll do this by interpreting the head of the list as the point, with the tail marching off to the right.

> fromList
>   :: ( Valued a )
>   => [a] -> OnePointedList a
> fromList xs = case xs of
>   [] -> Vacant
>   x:xs -> Point mempty x (FT.fromList xs)

Finally, we also provide a constructor which converts lists to `OnePointedList`s, but also allows us to specify where the point is. This is especially useful for testing.

> makePoint
>   :: ( Valued a )
>   => [a] -> a -> [a]
>   -> OnePointedList a
> makePoint as x bs =
>   Point (FT.fromList as) x (FT.fromList bs)

> -}



Class Instances
---------------

It's not too surprising that we can also give `OnePointedList` a `Foldable` instance.

> instance Foldable NonEmptyFingerTreeZipper where
>   toList
>     :: NonEmptyFingerTreeZipper a -> [a]
>   toList (Point as x bs) = concat
>     [ toList as, [x], toList bs ]
> 
>   foldr
>     :: (a -> b -> b) -> b
>     -> NonEmptyFingerTreeZipper a -> b
>   foldr f e (Point as x bs) =
>     foldr f (f x (foldr f e bs)) as
> 
>   foldl
>     :: (b -> a -> b) -> b
>     -> NonEmptyFingerTreeZipper a -> b
>   foldl f e (Point as x bs) =
>     foldl f (f (foldl f e as) x) bs

> instance Foldable FingerTreeZipper where
>   toList
>     :: FingerTreeZipper a -> [a]
>   toList x = case x of
>     Empty -> []
>     NonEmpty w -> toList w
> 
>   foldr
>     :: (a -> b -> b) -> b
>     -> FingerTreeZipper a -> b
>   foldr f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldr f e w
> 
>   foldl
>     :: (b -> a -> b) -> b
>     -> FingerTreeZipper a -> b
>   foldl f e x = case x of
>     Empty -> e
>     NonEmpty w -> foldl f e w

Another natural class instance we'd like for `OnePointedList` is `Functor`. Unfortunately we can't write a proper instance in this case, due to the essential `Valued` constraint. We can however get pretty close. `fmapOPL` maps a function over a one-pointed list as we expect.

> instance ConstrainedFunctor FingerTreeZipper where
>   type FunctorConstraint FingerTreeZipper = Valued
> 
>   fmapC
>     :: forall a b
>      . ( Valued a, Valued b )
>     => (a -> b) -> FingerTreeZipper a -> FingerTreeZipper b
>   fmapC f w = case w of
>     Empty -> Empty
>     NonEmpty u -> NonEmpty $ fmapC f u
> 
> instance ConstrainedFunctor NonEmptyFingerTreeZipper where
>   type FunctorConstraint NonEmptyFingerTreeZipper = Valued
> 
>   fmapC
>     :: forall a b
>      . ( Valued a, Valued b )
>     => (a -> b) -> NonEmptyFingerTreeZipper a -> NonEmptyFingerTreeZipper b
>   fmapC f (Point as x bs) =
>     Point (fmapC f as) (f x) (fmapC f bs)

> instance ConstrainedTraversable FingerTreeZipper where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> FingerTreeZipper a -> f (FingerTreeZipper b)
>   traverseC f w = case w of
>     Empty -> pure Empty
>     NonEmpty u -> fmap NonEmpty $ traverseC f u
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => FingerTreeZipper (f a) -> f (FingerTreeZipper a)
>   sequenceAC w = case w of
>     Empty -> pure Empty
>     NonEmpty u -> fmap NonEmpty $ sequenceAC u
> 
> instance ConstrainedTraversable NonEmptyFingerTreeZipper where
>   traverseC
>     :: ( Applicative f, Valued a, Valued b )
>     => (a -> f b) -> NonEmptyFingerTreeZipper a -> f (NonEmptyFingerTreeZipper b)
>   traverseC f (Point as x bs) =
>     pure Point <*> traverseC f as <*> f x <*> traverseC f bs
> 
>   sequenceAC
>     :: ( Applicative f, Valued a, Valued (f a) )
>     => NonEmptyFingerTreeZipper (f a) -> f (NonEmptyFingerTreeZipper a)
>   sequenceAC (Point as x bs) =
>     pure Point <*> sequenceAC as <*> x <*> sequenceAC bs

Note that types built on top of `OnePointedList` which fix the `m` parameter can generally be given a proper `Functor` instance, which is given by `fmapOPL`.

> {-

> traverseOPL
>   :: ( Valued a, Valued b, Applicative f )
>   => (a -> f b) -> OnePointedList a -> f (OnePointedList b)
> traverseOPL f w = case w of
>   Empty -> pure Empty
>   Point as x bs -> 
>     Point <$> FT.traverseFT f as <*> f x <*> FT.traverseFT f bs


Queries
-------

Next we'll define some functions which query one-pointed lists, both by extracting elements and by computing predicates. For example, a simple but handy predicate detects when the list is `Vacant`.

We can test our intuition about how `isEmpty` behaves with some examples.

::: doctest

> -- $
> -- >>> isEmpty empty
> -- True
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = singleton 'a'
> -- in isEmpty x
> -- :}
> -- False
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = fromList []
> -- in isEmpty x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = fromList ['a','b']
> -- in isEmpty x
> -- :}
> -- False

:::

Another simple and sometimes useful predicate detects when our list has only one item in it.

We can also detect when the read head is at the beginning (init) or end (last) position of the list. This is where we break the symmetry of `Point` and declare one end of the list to be the beginning and the other side the end.

Again we check our intuition with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = singleton 'a'
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint [] 'a' ['b','c']
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (True,False)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' []
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (False,True)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d']
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (False,False)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = empty
> -- in (isAtInit x, isAtLast x)
> -- :}
> -- (False,False)



:::

Our final queries allow us to extract the item at three special indices in the list (which may not all be distinct!) -- the initial position, the last position, and the point, also called the read head. Note that `readInit` and `readLast` use `uncons` and `unsnoc` on the underlying finger tree, respectively; these functions have constant amortized complexity.

> readInit
>   :: ( Valued a )
>   => OnePointedList a -> Maybe a
> readInit w = case w of
>   Vacant -> Nothing
>   Point as x bs -> case uncons as of
>     Nothing -> Just x
>     Just (a, _) -> Just a
> 
> readLast
>   :: ( Valued a )
>   => OnePointedList a -> Maybe a
> readLast w = case w of
>   Vacant -> Nothing
>   Point as x bs -> case unsnoc bs of
>     Nothing -> Just x
>     Just (b,_) -> Just b

And some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = empty
> -- in (readInit x, readPoint x, readLast x)
> -- :}
> -- (Nothing,Nothing,Nothing)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = singleton 'a'
> -- in (readInit x, readPoint x, readLast x)
> -- :}
> -- (Just 'a',Just 'a',Just 'a')
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d','e']
> -- in (readInit x, readPoint x, readLast x)
> -- :}
> -- (Just 'a',Just 'c',Just 'e')

:::



Navigation
----------

What distinguishes a pointed list from an ordinary list is the read head, which gives constant access to a specific item in the list and crucially is _mobile_. The simplest operations for this simply move the read head to the left or right by one index.



We can test our understanding of these functions with some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, xL, xR :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d','e']
> --   xL = makePoint ['a'] 'b' ['c','d','e']
> --   xR = makePoint ['a','b','c'] 'd' ['e']
> -- in (xL == movePointLeft x, xR == movePointRight x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint [] 'a' ['b','c']
> -- in x == movePointLeft x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' []
> -- in x == movePointRight x
> -- :}
> -- True

:::

Because finger trees have amortized constant `uncons` and `unsnoc`, we can also efficiently navigate to the beginning or end of the list.



And more examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = empty
> -- in (x == moveToInit x, x == moveToLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xI, xL :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d','e']
> --   xI = makePoint [] 'a' ['b','c','d','e']
> --   xL = makePoint ['a','b','c','d'] 'e' []
> -- in (xI == moveToInit x, xL == moveToLast x)
> -- :}
> -- (True,True)

:::



Mutation
--------

Singling out the functions in this section as "mutators" is a little misleading; the navigation functions also mutate their arguments. But the state of a one-pointed list exists on two levels -- there's the elements of the list and their ordering, and separate from this the location of the read head. In some sense moving the read head around is a different kind of mutation, changing the structure of data but not the content. In this section we'll deal with changing content.

First we can insert and delete items at the beginning.



> instance Deque FingerTreeZipper where
>   pushFront
>     :: ( Valued a )
>     => a -> FingerTreeZipper a
>     -> FingerTreeZipper a
>   pushFront a x = case x of
>     Empty -> singleton a
>     NonEmpty w -> NonEmpty $ pushFront a w
> 
>   pushBack
>     :: ( Valued a )
>     => a -> FingerTreeZipper a
>     -> FingerTreeZipper a
>   pushBack a x = case x of
>     Empty -> singleton a
>     NonEmpty w -> NonEmpty $ pushBack a w
> 
>   popFront
>     :: ( Valued a )
>     => FingerTreeZipper a
>     -> Maybe (a, FingerTreeZipper a)
>   popFront x = case x of
>     Empty -> Nothing
>     NonEmpty w ->
>       let (a, y) = popFrontNonEmpty w
>       in case y of
>         Nothing -> Just (a, Empty)
>         Just z  -> Just (a, NonEmpty z)
> 
>   popBack
>     :: ( Valued a )
>     => FingerTreeZipper a
>     -> Maybe (a, FingerTreeZipper a)
>   popBack x = case x of
>     Empty -> Nothing
>     NonEmpty w ->
>       let (a, y) = popBackNonEmpty w
>       in case y of
>         Nothing -> Just (a, Empty)
>         Just z  -> Just (a, NonEmpty z)

> instance Deque NonEmptyFingerTreeZipper where
>   pushFront
>     :: ( Valued a )
>     => a -> NonEmptyFingerTreeZipper a
>     -> NonEmptyFingerTreeZipper a
>   pushFront a (Point as x bs) =
>     Point (cons a as) x bs
> 
>   pushBack
>     :: ( Valued a )
>     => a -> NonEmptyFingerTreeZipper a
>     -> NonEmptyFingerTreeZipper a
>   pushBack a (Point as x bs) =
>     Point as x (snoc a bs)
> 
>   popFront
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a
>     -> Maybe (a, NonEmptyFingerTreeZipper a)
>   popFront x =
>     let (a, y) = popFrontNonEmpty x
>     in case y of
>       Nothing -> Nothing
>       Just w  -> Just (a, w)
> 
>   popBack
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a
>     -> Maybe (a, NonEmptyFingerTreeZipper a)
>   popBack x =
>     let (a, y) = popBackNonEmpty x
>     in case y of
>       Nothing -> Nothing
>       Just w  -> Just (a, w)

> instance NonEmptyDeque NonEmptyFingerTreeZipper where
>   popFrontNonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a
>     -> (a, Maybe (NonEmptyFingerTreeZipper a))
>   popFrontNonEmpty (Point as x bs) = case uncons as of
>     Nothing -> case uncons bs of
>       Nothing       -> (x, Nothing)
>       Just (b, bs') -> (x, Just $ Point empty b bs')
>     Just (a, as')   -> (a, Just $ Point as' x bs)
> 
>   popBackNonEmpty
>     :: ( Valued a )
>     => NonEmptyFingerTreeZipper a
>     -> (a, Maybe (NonEmptyFingerTreeZipper a))
>   popBackNonEmpty (Point as x bs) = case unsnoc bs of
>     Nothing -> case unsnoc as of
>       Nothing       -> (x, Nothing)
>       Just (a, as') -> (x, Just $ Point as' a empty)
>     Just (b, bs')   -> (b, Just $ Point as x bs')


> 
> deleteInit
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a
> deleteInit w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case uncons as of
>     Just (_,as') -> Point as' x bs
>     Nothing -> case uncons bs of
>       Nothing -> Vacant
>       Just (b,bs') -> Point mempty b bs'

And some tests for our understanding:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: OnePointedList Count Char
> --   x = empty
> --   y = singleton 'a'
> -- in (y == insertInit 'a' x, x == deleteInit x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xI, xD :: OnePointedList Count Char
> --   x = makePoint ['b'] 'c' ['d','e']
> --   xI = makePoint ['a','b'] 'c' ['d','e']
> --   xD = makePoint [] 'c' ['d','e']
> -- in (xI == insertInit 'a' x, xD == deleteInit x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xI, xD :: OnePointedList Count Char
> --   x = makePoint [] 'b' ['c','d','e']
> --   xI = makePoint ['a'] 'b' ['c','d','e']
> --   xD = makePoint [] 'c' ['d','e']
> -- in (xI == insertInit 'a' x, xD == deleteInit x)
> -- :}
> -- (True,True)

:::

Similarly, we can insert and delete items at the end of the list.

> insertLast
>   :: ( Valued a )
>   => a -> OnePointedList a -> OnePointedList a
> insertLast a w = case w of
>   Vacant ->
>     Point mempty a mempty
>   Point as x bs ->
>     Point as x (snoc a bs)
> 
> deleteLast
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a
> deleteLast w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case unsnoc bs of
>     Nothing -> case unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> Point as' a mempty
>     Just (_, bs') -> Point as x bs'

And some tests for our understanding:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y :: OnePointedList Count Char
> --   x = empty
> --   y = singleton 'a'
> -- in (y == insertLast 'a' x, x == deleteLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xI, xD :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d']
> --   xI = makePoint ['a','b'] 'c' ['d','e']
> --   xD = makePoint ['a','b'] 'c' []
> -- in (xI == insertLast 'e' x, xD == deleteLast x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xI, xD :: OnePointedList Count Char
> --   x = makePoint ['a','b','c'] 'd' []
> --   xI = makePoint ['a','b','c'] 'd' ['e']
> --   xD = makePoint ['a','b'] 'c' []
> -- in (xI == insertLast 'e' x, xD == deleteLast x)
> -- :}
> -- (True,True)

:::

Insert and delete at the point is a little more complex, because we need to distinguish between operations that take effect immediately to the left or to the right of the point.

> insertPointLeft
>   :: ( Valued a )
>   => a -> OnePointedList a -> OnePointedList a
> insertPointLeft a w = case w of
>   Vacant -> Point mempty a mempty
>   Point as x bs -> Point (snoc a as) x bs
> 
> insertPointRight
>   :: ( Valued a )
>   => a -> OnePointedList a -> OnePointedList a
> insertPointRight a w = case w of
>   Vacant -> Point mempty a mempty
>   Point as x bs -> Point as x (cons a bs)
> 
> deletePointLeft
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a
> deletePointLeft w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case unsnoc as of
>     Just (a, as') -> Point as' x bs
>     Nothing -> case uncons bs of
>       Just (b, bs') -> Point mempty b bs'
>       Nothing -> Vacant
> 
> deletePointRight
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a
> deletePointRight w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case uncons bs of
>     Just (b, bs') -> Point as x bs'
>     Nothing -> case unsnoc as of
>       Just (a, as') -> Point as' a mempty
>       Nothing -> Vacant

And some tests for our understanding. Note that generally inserting and deleting at the point does not change the item at the point, except in cases where we're already at the beginning or end of the list (or the list is empty).

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, xL, xR :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d','e']
> --   xL = makePoint ['a'] 'c' ['d','e']
> --   xR = makePoint ['a','b'] 'c' ['e']
> -- in (xL == deletePointLeft x, xR == deletePointRight x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xL :: OnePointedList Count Char
> --   x = makePoint [] 'a' ['b']
> --   xL = makePoint [] 'b' []
> -- in xL == deletePointLeft x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x, xR :: OnePointedList Count Char
> --   x = makePoint ['a'] 'b' []
> --   xR = makePoint [] 'a' []
> -- in xR == deletePointRight x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x, xL, xR :: OnePointedList Count Char
> --   x = makePoint ['a'] 'b' ['c']
> --   xL = makePoint ['a','z'] 'b' ['c']
> --   xR = makePoint ['a'] 'b' ['z','c']
> -- in (xL == insertPointLeft 'z' x, xR == insertPointRight 'z' x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xL, xR :: OnePointedList Count Char
> --   x = makePoint [] 'a' ['b']
> --   xL = makePoint ['z'] 'a' ['b']
> --   xR = makePoint [] 'a' ['z','b']
> -- in (xL == insertPointLeft 'z' x, xR == insertPointRight 'z' x)
> -- :}
> -- (True,True)
> --
> -- >>> :{
> -- let
> --   x, xL, xR :: OnePointedList Count Char
> --   x = makePoint ['a'] 'b' []
> --   xL = makePoint ['a','z'] 'b' []
> --   xR = makePoint ['a'] 'b' ['z']
> -- in (xL == insertPointLeft 'z' x, xR == insertPointRight 'z' x)
> -- :}
> -- (True,True)

:::

Finally, we can alter the item at each of our three special positions. These functions act like a very restricted form of `fmap`, affecting only one item in the list.

> alterInit
>   :: ( Valued a )
>   => (a -> a)
>   -> OnePointedList a -> OnePointedList a
> alterInit f w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case uncons as of
>     Nothing -> Point mempty (f x) bs
>     Just (a, as') -> Point (cons (f a) as') x bs
> 
> alterLast
>   :: ( Valued a )
>   => (a -> a)
>   -> OnePointedList a -> OnePointedList a
> alterLast f w = case w of
>   Vacant -> Vacant
>   Point as x bs -> case unsnoc bs of
>     Nothing -> Point as (f x) mempty
>     Just (b, bs') -> Point as x (snoc (f b) bs')
> 
> alterPoint
>   :: ( Valued a )
>   => (a -> a)
>   -> OnePointedList a -> OnePointedList a
> alterPoint f w = case w of
>   Vacant ->
>     Vacant
>   Point as x bs ->
>     Point as (f x) bs

And some examples.

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, xI, xL, xH :: OnePointedList Count Char
> --   x = makePoint ['a','b'] 'c' ['d','e']
> --   xI = makePoint ['z','b'] 'c' ['d','e']
> --   xL = makePoint ['a','b'] 'c' ['d','z']
> --   xH = makePoint ['a','b'] 'z' ['d','e']
> --   f = const 'z'
> -- in (xI == alterInit f x, xL == alterLast f x, xH == alterPoint f x)
> -- :}
> -- (True,True,True)

:::

> alterPointA
>   :: ( Applicative f, Valued a )
>   => (a -> f a)
>   -> OnePointedList a -> f (OnePointedList a)
> alterPointA f w = case w of
>   Vacant ->
>     pure Vacant
>   Point as x bs ->
>     Point <$> pure as <*> f x <*> pure bs



Measurement
-----------

Since `OnePointedList` is defined in terms of finger trees, it depends essentially on the `Valued` class. It will be handy to define some functions for working with values. First of all, we have a class instance for one-pointed lists.

> instance
>   ( Valued a
>   ) => Valued (OnePointedList a)
>   where
>     type Value (OnePointedList a) = FT.Value a
> 
>     value :: OnePointedList a -> FT.Value a
>     value w = case w of
>       Vacant -> mempty
>       Point as x bs -> mconcat
>         [ FT.value as, FT.value x, FT.value bs ]



As Finger Trees
---------------

Recall that one of the killer features of finger trees is a powerful _splitting_ function that takes advantage of the value annotation type to efficiently break the tree at a given position. The output of this splitting has the same shape as a one-pointed list.

> split
>   :: ( Valued a )
>   => (FT.Value a -> Bool)
>   -> FT.FingerTree a
>   -> OnePointedList a
> split p xs = case FT.splitL p xs of
>     FT.NotFound -> Vacant
>     FT.Found as x bs -> Point as x bs

Analogously, we may sometimes need to turn a pointed list back into a finger tree without a distinguished point. The function for doing this is called _integrate_ -- the "inverse" of differentiation.

> integrate
>   :: ( Valued a )
>   => OnePointedList a -> FT.FingerTree a
> integrate w = case w of
>   Vacant -> mempty
>   Point as x bs -> as <> (cons x bs)

> -}


Concatenation
-------------

It will also be useful to have utilities for combining one-pointed lists together sort of like concatenation. I say 'sort of' because pointed lists because there's not a unique way to do this; we have to decide what to do with the point. To this end we provide two biased versions of concat: _prepend_, which preserves the point of the right argument, and _append_, which preserves the left.

> {-

> prepend
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a -> OnePointedList a
> prepend as bs = case bs of
>   Vacant ->
>     moveToLast as
>   Point us x vs ->
>     Point (integrate as <> us) x vs
> 
> append
>   :: ( Valued a )
>   => OnePointedList a -> OnePointedList a -> OnePointedList a
> append as bs = case bs of
>   Vacant ->
>     moveToInit as
>   Point us x vs ->
>     Point us x (vs <> integrate as)

> -}

And some examples:

::: doctest

> -- $
> -- >>> :{
> -- let
> --   x, y, z :: OnePointedList Count Char
> --   x = makePoint ['a'] 'b' ['c']
> --   y = makePoint ['d'] 'e' ['f']
> --   z = makePoint ['d','e','f','a'] 'b' ['c']
> -- in z == prepend y x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x, y, z :: OnePointedList Count Char
> --   x = makePoint ['a'] 'b' ['c']
> --   y = makePoint ['d'] 'e' ['f']
> --   z = makePoint ['a'] 'b' ['c','d','e','f']
> -- in z == append y x
> -- :}
> -- True

:::



Testing and Debugging
---------------------

In order to test this module with our property testing framework we'll need some class instances. These handle generating and shrinking one-pointed lists.

> instance
>   ( Fmt.Display a, Valued a
>   ) => Fmt.Display (FingerTreeZipper a)
>   where
>     display x = case x of
>       Empty      -> "Empty"
>       NonEmpty z -> "NonEmpty" <+> display z
> 
> instance
>   ( Arb a, Valued a
>   ) => Arb (FingerTreeZipper a)
>   where
>     arb = do
>       k <- size
>       if k <= 0
>         then pure Empty
>         else pickFrom2
>           ( pure Empty
>           , NonEmpty <$> arb
>           )
> 
> instance
>   ( Prune a, Valued a
>   ) => Prune (FingerTreeZipper a)
>   where
>     prune w = case w of
>       Empty -> []
>       NonEmpty u -> Empty : (map NonEmpty $ prune u)
> 
> instance
>   ( CoArb a, Valued a
>   ) => CoArb (FingerTreeZipper a)
>   where
>     coarb x = case x of
>       Empty      -> twiddle 0
>       NonEmpty z -> twiddle 1 . coarb z
> 
> instance
>   ( MakeTo a, Valued a
>   ) => MakeTo (FingerTreeZipper a)
>   where
>     makeTo = makeToExtendWith makeTo
>       toTupFingerTreeZipper
>       fromTupFingerTreeZipper

> instance
>   ( Fmt.Display a, Valued a
>   ) => Fmt.Display (NonEmptyFingerTreeZipper a)
>   where
>     display (Point as x bs) =
>       "Point" <+> display as <+> display x <+> display bs
> 
> instance
>   ( Arb a, Valued a
>   ) => Arb (NonEmptyFingerTreeZipper a)
>   where
>     arb = Point <$> arb <*> arb <*> arb
> 
> instance
>   ( Prune a, Valued a
>   ) => Prune (NonEmptyFingerTreeZipper a)
>   where
>     prune (Point as x bs) = concat 
>       [ [ Point as' x bs | as' <- prune as ]
>       , [ Point as x' bs | x' <- prune x ]
>       , [ Point as x bs' | bs' <- prune bs ]
>       ]
> instance
>   ( CoArb a, Valued a
>   ) => CoArb (NonEmptyFingerTreeZipper a)
>   where
>     coarb (Point as x bs) = coarb (as, x, bs)
> 
> instance
>   ( MakeTo a, Valued a
>   ) => MakeTo (NonEmptyFingerTreeZipper a)
>   where
>     makeTo = makeToExtendWith makeTo
>       toTupNonEmptyFingerTreeZipper
>       fromTupNonEmptyFingerTreeZipper
