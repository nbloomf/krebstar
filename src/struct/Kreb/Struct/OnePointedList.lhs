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

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE InstanceSigs #-}
> 
> module Kreb.Struct.OnePointedList where
> 
> import Data.List (unwords)
> import Data.Foldable
> 
> import           Kreb.Check
> import           Kreb.Struct.Valued
> import qualified Kreb.Struct.FingerTree as FT

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

> data OnePointedList m a
>   = Vacant
>   | Point (FT.FingerTree m a, a, FT.FingerTree m a)
>   deriving (Eq, Show)

(Note that, since this is defined in terms of `FingerTree`, we have an extra value type parameter `m`.) `OnePointedList` combines the benefits of `Zipper` with the efficient splitting and deque operations of `FingerTree`. Recall that we have amortized constant time access to both ends of a finger tree, so `OnePointedList` gives us fast access to the beginning and end of the list in addition to the read head.



Constructors
------------

As usual, we'll build up a theory of `OnePointedList`s by defining queries and operations on them. First though we need some constructors. Two natural candidates are `empty` and `singleton`, which construct lists with zero and one item, respectively.

> empty
>   :: OnePointedList m a
> empty = Vacant
> 
> singleton
>   :: ( Valued m a )
>   => a -> OnePointedList m a
> singleton x =
>   Point (mempty, x, mempty)

It's also natural to convert lists to `OnePointedList`s; we'll do this by interpreting the head of the list as the point, with the tail marching off to the right.

> makeFromList
>   :: ( Valued m a )
>   => [a] -> OnePointedList m a
> makeFromList xs = case xs of
>   [] -> Vacant
>   x:xs -> Point (mempty, x, FT.fromList xs)

Finally, we also provide a constructor which converts lists to `OnePointedList`s, but also allows us to specify where the point is. This is especially useful for testing.

> makePoint
>   :: ( Valued m a )
>   => [a] -> a -> [a]
>   -> OnePointedList m a
> makePoint as x bs =
>   Point (FT.fromList as, x, FT.fromList bs)



Class Instances
---------------

It's not too surprising that we can also give `OnePointedList` a `Foldable` instance.

> instance Foldable (OnePointedList m) where
>   toList
>     :: OnePointedList m a -> [a]
>   toList w = case w of
>     Vacant -> []
>     Point (as,x,bs) -> concat
>       [ toList as, [x], toList bs ]
> 
>   foldr
>     :: (a -> b -> b) -> b
>     -> OnePointedList m a -> b
>   foldr f e w = case w of
>     Vacant -> e
>     Point (as, x, bs) ->
>       foldr f (f x (foldr f e bs)) as
> 
>   foldl
>     :: (b -> a -> b) -> b
>     -> OnePointedList m a -> b
>   foldl f e w = case w of
>     Vacant -> e
>     Point (as, x, bs) ->
>       foldl f (f (foldl f e as) x) bs

Another natural class instance we'd like for `OnePointedList` is `Functor`. Unfortunately we can't write a proper instance in this case, due to the essential `Valued` constraint. We can however get pretty close. `fmapList` maps a function over a one-pointed list as we expect.

> fmapList
>   :: forall m1 m2 a1 a2
>    . ( Valued m1 a1, Valued m2 a2 )
>   => (a1 -> a2) -> OnePointedList m1 a1 -> OnePointedList m2 a2
> fmapList f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) ->
>     Point (FT.fmapFT f as, f x, FT.fmapFT f bs)

Note that types built on top of `OnePointedList` which fix the `m` parameter can generally be given a proper `Functor` instance, which is given by `fmapList`.



Queries
-------

Next we'll define some functions which query one-pointed lists, both by extracting elements and by computing predicates. For example, a simple but handy predicate detects when the list is `Vacant`.

> isEmpty
>   :: OnePointedList m a -> Bool
> isEmpty w = case w of
>   Vacant -> True
>   _ -> False

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
> --   x = makeFromList []
> -- in isEmpty x
> -- :}
> -- True
> --
> -- >>> :{
> -- let
> --   x :: OnePointedList Count Char
> --   x = makeFromList ['a','b']
> -- in isEmpty x
> -- :}
> -- False

:::

Another simple and sometimes useful predicate detects when our list has only one item in it.

> isSingleton
>   :: ( Valued m a )
>   => OnePointedList m a -> Bool
> isSingleton w = case w of
>   Vacant -> False
>   Point (as,_,bs) -> (FT.isEmpty as) && (FT.isEmpty bs)

We can also detect when the read head is at the beginning (init) or end (last) position of the list. This is where we break the symmetry of `Point` and declare one end of the list to be the beginning and the other side the end.

> isAtInit
>   :: ( Valued m a )
>   => OnePointedList m a -> Bool
> isAtInit w = case w of
>   Vacant -> False
>   Point (as,_,_) -> FT.isEmpty as
> 
> isAtLast
>   :: ( Valued m a )
>   => OnePointedList m a -> Bool
> isAtLast w = case w of
>   Vacant -> False
>   Point (_,_,bs) -> FT.isEmpty bs

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
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe a
> readInit w = case w of
>   Vacant -> Nothing
>   Point (as, x, bs) -> case FT.uncons as of
>     Nothing -> Just x
>     Just (a, _) -> Just a
> 
> readLast
>   :: ( Valued m a )
>   => OnePointedList m a -> Maybe a
> readLast w = case w of
>   Vacant -> Nothing
>   Point (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> Just x
>     Just (b,_) -> Just b
> 
> readPoint
>   :: OnePointedList m a -> Maybe a
> readPoint w = case w of
>   Vacant -> Nothing
>   Point (_,x,_) -> Just x

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

> movePointLeft
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> movePointLeft w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.unsnoc as of
>     Nothing ->
>       Point (mempty, x, bs)
>     Just (a, as') ->
>       Point (as', a, FT.cons x bs)
> 
> movePointRight
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> movePointRight w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.uncons bs of
>     Nothing ->
>       Point (as, x, mempty)
>     Just (b, bs') ->
>       Point (FT.snoc x as, b, bs')

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

> moveToInit  
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> moveToInit w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.uncons as of
>     Nothing ->
>       Point (mempty, x, bs)
>     Just (a, as') ->
>       Point (mempty, a, as' <> (FT.cons x bs))
> 
> moveToLast
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> moveToLast w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.unsnoc bs of
>     Nothing ->
>       Point (as, x, mempty)
>     Just (b, bs') -> 
>       Point (as <> (FT.cons x bs'), b, mempty)

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

> insertInit
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertInit a w = case w of
>   Vacant ->
>     Point (mempty, a, mempty)
>   Point (as, x, bs) ->
>     Point (FT.cons a as, x, bs)
> 
> deleteInit
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deleteInit w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.uncons as of
>     Just (_,as') -> Point (as', x, bs)
>     Nothing -> case FT.uncons bs of
>       Nothing -> Vacant
>       Just (b,bs') -> Point (mempty, b, bs')

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
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertLast a w = case w of
>   Vacant ->
>     Point (mempty, a, mempty)
>   Point (as, x, bs) ->
>     Point (as, x, FT.snoc a bs)
> 
> deleteLast
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deleteLast w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> case FT.unsnoc as of
>       Nothing -> Vacant
>       Just (a, as') -> Point (as', a, mempty)
>     Just (_, bs') -> Point (as, x, bs')

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
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertPointLeft a w = case w of
>   Vacant -> Point (mempty, a, mempty)
>   Point (as, x, bs) -> Point (FT.snoc a as, x, bs)
> 
> insertPointRight
>   :: ( Valued m a )
>   => a -> OnePointedList m a -> OnePointedList m a
> insertPointRight a w = case w of
>   Vacant -> Point (mempty, a, mempty)
>   Point (as, x, bs) -> Point (as, x, FT.cons a bs)
> 
> deletePointLeft
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deletePointLeft w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.unsnoc as of
>     Just (a,as') -> Point (as', x, bs)
>     Nothing -> case FT.uncons bs of
>       Just (b,bs') -> Point (mempty, b, bs')
>       Nothing -> Vacant
> 
> deletePointRight
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a
> deletePointRight w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.uncons bs of
>     Just (b,bs') -> Point (as, x, bs')
>     Nothing -> case FT.unsnoc as of
>       Just (a,as') -> Point (as', a, mempty)
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
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterInit f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.uncons as of
>     Nothing -> Point (mempty, f x, bs)
>     Just (a, as') -> Point (FT.cons (f a) as', x, bs)
> 
> alterLast
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterLast f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> case FT.unsnoc bs of
>     Nothing -> Point (as, f x, mempty)
>     Just (b, bs') -> Point (as, x, FT.snoc (f b) bs')
> 
> alterPoint
>   :: ( Valued m a )
>   => (a -> a)
>   -> OnePointedList m a -> OnePointedList m a
> alterPoint f w = case w of
>   Vacant -> Vacant
>   Point (as, x, bs) -> Point (as, f x, bs)

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



Measurement
-----------

Since `OnePointedList` is defined in terms of finger trees, it depends essentially on the `Valued` class. It will be handy to define some functions for working with values. First of all, we have a class instance for one-pointed lists.

> instance
>   ( Valued m a
>   ) => Valued m (OnePointedList m a)
>   where
>     value w = case w of
>       Vacant -> mempty
>       Point (as, x, bs) -> mconcat
>         [ value as, value x, value bs ]

It will also be handy to _remeasure_ a one-pointed list. Note that on the actual list items this function is the identity; its effect is on the value annotations.

> remeasure
>   :: ( Valued m1 a, Valued m2 a )
>   => OnePointedList m1 a -> OnePointedList m2 a
> remeasure w = case w of
>   Vacant -> Vacant
>   Point (as,x,bs) ->
>     Point (FT.remeasure as, x, FT.remeasure bs)



As Finger Trees
---------------

Recall that one of the killer features of finger trees is a powerful _splitting_ function that takes advantage of the value annotation type to efficiently break the tree at a given position. The output of this splitting has the same shape as a one-pointed list.

> split
>   :: ( Valued m a )
>   => (m -> Bool)
>   -> FT.FingerTree m a
>   -> OnePointedList m a
> split p xs = case FT.split p xs of
>     Nothing -> Vacant
>     Just z -> Point z

Analogously, we may sometimes need to turn a pointed list back into a finger tree without a distinguished point. The function for doing this is called _integrate_ -- the "inverse" of differentiation.

> integrate
>   :: ( Valued m a )
>   => OnePointedList m a -> FT.FingerTree m a
> integrate w = case w of
>   Vacant -> mempty
>   Point (as, x, bs) -> as <> (FT.cons x bs)



Concatenation
-------------

It will also be useful to have utilities for combining one-pointed lists together sort of like concatenation. I say 'sort of' because pointed lists because there's not a unique way to do this; we have to decide what to do with the point. To this end we provide two biased versions of concat: _prepend_, which preserves the point of the right argument, and _append_, which preserves the left.

> prepend
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a -> OnePointedList m a
> prepend as bs = case bs of
>   Vacant ->
>     moveToLast as
>   Point (us, x, vs) ->
>     Point (integrate as <> us, x, vs)
> 
> append
>   :: ( Valued m a )
>   => OnePointedList m a -> OnePointedList m a -> OnePointedList m a
> append as bs = case bs of
>   Vacant ->
>     moveToInit as
>   Point (us, x, vs) ->
>     Point (us, x, vs <> integrate as)

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
>   ( Arb a, Valued m a
>   ) => Arb (OnePointedList m a)
>   where
>     arb = do
>       ZZ k <- askSize
>       if k <= 0
>         then pure Vacant
>         else pickFrom2
>           ( pure Vacant
>           , Point <$> arb
>           )
> 
> instance
>   ( Prune a, Valued m a
>   ) => Prune (OnePointedList m a)
>   where
>     prune w = case w of
>       Vacant -> []
>       Point z -> concat 
>         [ [ Vacant ]
>         , Point <$> prune z
>         ]
