---
title: Red-Black Trees
---

::: contents
* [Introduction](#introduction): The problem we're solving
* [Types and Terms](#types-and-terms): Type-level encoding
* [Enforcing Invariants Statically](#enforcing-invariants-statically): Defining errors out of existence
* [Class Instances](#class-instances): Code for free
* [Basic API](#basic-api): But just the _very_ basics
* [Insertion](#insertion): Putting stuff in the tree
* [Deletion](#deletion): Taking stuff out of the tree
:::



::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE KindSignatures #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE GADTs #-}
> 
> module Kreb.Struct.RedBlackTree (
>     RedBlackTree()
>   , empty
>   , member
>   , toList
>   , maxElt
> 
>   , insert
>   , fromList
> 
>   , delete
> ) where
> 
> import Kreb.Check

:::



Introduction
------------

This module implements a data structure called a _red-black tree_. Structurally this is a binary tree, but the name comes from the fact that each node in the tree has an additional bit of metadata -- a color, which is traditionally either red or black. In addition, the arrangement of colors in the tree must satisfy some constraints:

1. The root and all leaves are black.
2. Both children of a red node are black.
3. All paths from a given node to any of its descendant leaves contain the same number of black nodes.

The constraints conspire to make the overall tree height-balanced enough to ensure optimal asymptotic bounds on insertion and deletion of nodes. This makes red-black trees a good structure for implementing sets -- unordered collections of elements with no duplicates -- especially where we intend for the structure to stay around and be manipulated for a long time. But crucially, any operations on the tree must maintain these constraints as invariants in order to preserve the asymptotic complexity.

Red-black trees are widely used in functional languages, but they are tricky to implement correctly. In all material respects this version is taken from code written by [Stephanie Weirich](https://github.com/sweirich/dth), who also credits Dan Licata and John Hughes, and with a deletion strategy developed by Matt Might. Notably, Weirich's code uses _dependent types_, that is, types which depend on terms, to enforce the red-black invariants statically.^[This example is also the subject of an excellent <a href="https://www.youtube.com/watch?v=n-b1PYbRUOY">talk by Weirich</a> on using dependent types in Haskell.] This accomplishes two major feats: it gets the _compiler_ to help prove that the invariants hold, and means that invariant checks at runtime (or even in tests) are unnecessary.

I will stumble around and try my best to understand and explain this code. But similar to our finger tree module, on some level here and more so than most of the other modules in this project, I'm aping someone else's insights. But that's okay! This is how we learn.



Types and Terms
---------------

The red-black tree invariants involve two kinds of metadata: the _color_ of the current node, and the _number_ of black descendants in any path to the leaves. In order to guarantee the invariants statically we need representations of this data at the type-level. Haskell's `DataKinds` extension provides an elegant way to do this. Normally, a `data` declaration defines a _type_ along with some _data constructors_ for building values of that type. But with `DataKinds` enabled, the `data` keyword also defines a _kind_ along with some _type constructors_ for building types of that kind. The classic example is the type of Peano numbers, defined like so.

> data Nat
>   = Z | S Nat

This defines a type, `Nat`, with two value constructors. But it also defines a _kind_, `Nat`, with two type constructors. In GHCi we can use `:k` to infer the kind of a type expression just like `:t` infers the type of a value expression. For example:

::: doctest

> -- $
> -- >>> :t Z
> -- Z :: Nat
> --
> -- >>> :set -XDataKinds
> -- >>> :k Z
> -- Z :: Nat

:::

This is a powerful tool for building type constraints, and we can also promote the node colors to the type level. It turns out that there's a nice implementation of the delete function on red-black trees using an intermediate form with more than two colors, so our `Color` kind looks like this:

> data Color
>   = Red
>   | Black
>   | DoubleBlack   -- only used internally
>   | NegativeBlack -- only used internally

Note that we don't need `Eq` or `Show` instances on either `Nat` or `Color` -- we're not using these as types, but as _kinds_, and values of the _type_ `Nat` or `Color` will never actually exist in our program at either compile or run time. We do, however, need a way to "reify" the `Color` kind at the value level, and the best way to do this uses a GADT.

> data ValueColor (c :: Color) where
>   R  :: ValueColor Red
>   B  :: ValueColor Black
>   BB :: ValueColor DoubleBlack
>   NB :: ValueColor NegativeBlack

Note how `Color` appears as a constraint on `c`, with the colors as types. Using a GADT here allows the constructors of `ValueColor` to have distinct monomorphic type arguments. We can give a show instance for `ValueColor` here, useful mostly for debugging.

> instance Show (ValueColor c) where  
>   show x = case x of
>     R  -> "R"
>     B  -> "B"
>     BB -> "BB"
>     NB -> "NB"

We will also need a way to express that two types of kind `Color` are equal at the type level; this can be done by lifting `ValueColor`s. First we give a data type which expresses type equality for `Color`s.

> data EqColor (a :: Color) (b :: Color) where
>   EqColor :: EqColor a a

Now the `sameColor` function takes two value-level colors and provides a type-level proof that they are equal (if they are). A value with type `EqColor c1 c2` is such a proof, and we are careful to constrain which values can exist. (The type and constructor for `EqColor` are not exposed outside this module.)

> sameColor
>   :: ValueColor (c1 :: Color)
>   -> ValueColor (c2 :: Color)
>   -> Maybe (EqColor c1 c2)
> sameColor u v = case (u,v) of
>   (R,  R)  -> Just EqColor
>   (B,  B)  -> Just EqColor
>   (BB, BB) -> Just EqColor
>   (NB, NB) -> Just EqColor
>   _        -> Nothing

What we have so far is this: a type-level representation of natural numbers, and a type-level representation of the possible node colors, as well as a mechanism for asserting the equality of type-level colors. We'll use these to encode the red-black invariants in types.



Enforcing Invariants Statically
-------------------------------

We're now prepared to define red-black trees so that invariant-violating instances are _not allowed to exist_ by the compiler -- and it turns out, it's not even that hard to do. We begin with an intermediate type, defined as a GADT, which looks almost like an ordinary binary tree if we squint at it just right.

> -- CT is short for "colorful tree"
> data CT (n :: Nat) (c :: Color) (a :: *) where
>    E
>      :: CT Z Black a
>    T
>      :: ( Valid c c1 c2 )
>      => ValueColor c
>      -> (CT n c1 a) -> a -> (CT n c2 a)
>      -> CT (Incr c n) c a

Let's pause on this for a moment. This definition says that a `CT` value has three type parameters: a `Nat`, representing the black height of the tree, a `Color`, representing the color of the root, and a `*`, for the type of the values at the nodes. There are two ways to make a `CT`.

* There's the _empty_ `CT`, with color `Black` and black height zero. This tree trivially satisfies the constraints.
* Given two trees a, value, and a color parameter, we can construct a _branch_ `CT` with the value and color at its root. But in the branching case something else is going on: our colors have to satisfy an extra `Valid` constraint, and the resulting tree has a black height which depends on the root color in some way (the `Incr` constructor). In the `Red` and `Black` case, this satisfies the red-black invariants _except_ for the requirement that the root element be black.

The `Valid` class lets us constrain the colors allowed among the children at each node: red nodes must have black children, and there is no restriction on the children of black nodes. Note also that the double-black and negative-black colors do not appear among the instances of `Valid`, which excludes these internal-only colors from leaking out to valid trees.

> class Valid (c :: Color) (c1 :: Color) (c2 :: Color)
> instance Valid Red Black Black 
> instance Valid Black c1 c2

We can also implement `Incr` as a type family -- notice the black height of the root of a `T` depends on the root color and black heights of the children. This looks suspiciously like a function at the type level, because that's what it is.

> type family Incr (c :: Color) (n :: Nat) :: Nat
> type instance Incr Black n             = S n
> type instance Incr DoubleBlack n       = S (S n)
> type instance Incr Red   n             = n
> type instance Incr NegativeBlack (S n) = n

Next, for debugging, we give a `Show` instance for `CT`.

> instance
>   ( Show a
>   ) => Show (CT n c a)
>   where
>     show x = case x of
>       E -> "E"
>       (T c l x r) -> concat
>         [ "(T ", show c, " ", show l
>         , " ", show x, " ", show r, ")"
>         ]

Finally we can hide the extra parameters to expose a red-black tree type depending only on the value type and forcing the root to be black. This is the type we expose from this module. This also enforces the black root invariant, which does not hold for bare `CT`s.

> data RedBlackTree a where
>   Root :: (CT n Black a) -> RedBlackTree a
> 
> instance
>   ( Show a
>   ) => Show (RedBlackTree a)
>   where
>     show (Root x) = show x

And that's all it takes to encode the red-black invariants at the type level; invalid trees cannot exist because they won't satisfy the type checker. Neat!

Now for the hard(er) part: implementing the useful operations. We can be confident at this point that _if_ we can write implementations for `insert` and `delete` then they will preserve the invariants, but it is not obvious that we can do this or how complicated it will be. In particular, insertion and deletion necessarily requires working with intermediate structures that violate the red-black invariants, so we'll have to deal with that.



Class Instances
---------------

Before getting to the meat of this module lets do a bit of cleanup. Note that we didn't derive equality on `RedBlackTree`s, because we _couldn't_ derive a useful equality on the inner type `CT`. To do this we'll need a kind of equality testing on `CT`s that ignores the black height parameter.

> instance
>   ( Eq a
>   ) => Eq (RedBlackTree a)
>   where
>     (Root t1) == (Root t2) = eqCT t1 t2
>       where
>         eqCT
>           :: ( Eq a )
>           => CT n1 c1 a -> CT n2 c2 a -> Bool
>         eqCT u v = case (u,v) of
>           (E, E) -> True
>           (T c1 a1 x1 b1, T c2 a2 x2 b2) ->
>             case sameColor c1 c2 of
>               Nothing -> False
>               Just _  -> (eqCT a1 a2) && (x1 == x2) && (eqCT b1 b2)
>           _ -> False

Both `CT` and `RedBlackTree` are instances of functor, however, in a straightforward way.

> instance Functor (CT n c) where
>   fmap f w = case w of
>     E -> E
>     T c l a r ->
>       T c (fmap f l) (f a) (fmap f r)
> 
> instance Functor RedBlackTree where
>   fmap f (Root x) = Root (fmap f x)



Basic API
---------

The bare minimum API we need from red-black trees is a constructor for empty trees, a way to query for membership, insertion, and deletion. The first two of these are simple to implement, but the last two are tricky to express because they involve temporarily dealing with "almost" red-black trees which violate one or more of the invariants.

Defining the empty tree is straightforward:

> empty :: RedBlackTree a
> empty = Root E

As is querying for membership.

> member
>   :: forall a
>    . ( Ord a )
>   => a -> RedBlackTree a -> Bool
> member x (Root t) = member' t
>   where
>     member'
>       :: ( Ord a )
>       => CT n c a -> Bool
>     member' w = case w of
>       E -> False
>       T _ l u r -> case compare x u of
>         LT -> member' l
>         EQ -> True
>         GT -> member' r

While we're here, we can also convert a red-black tree to a list, preserving the left-to-right order of the items in the tree.

> toList
>   :: ( Ord a )
>   => RedBlackTree a -> [a]
> toList (Root t) = accum t []
>   where
>     accum
>       :: ( Ord a )
>       => CT n c a -> [a] -> [a]
>     accum w zs = case w of
>       E -> zs
>       T _ a x b -> accum a (x : accum b zs)

Finally, since the items in the tree are ordered, we can extract the maximum element.

> getMax
>   :: ( Ord a )
>   => CT n c a -> Maybe a
> getMax w = case w of
>   E         -> Nothing
>   T _ _ x E -> Just x
>   T _ _ x r -> getMax r
> 
> maxElt
>   :: ( Ord a )
>   => RedBlackTree a -> Maybe a
> maxElt (Root x) = getMax x

Note that these operations do not involve destructuring a red-black tree in a way that may violate any invariants.



Insertion
---------

Suppose we want to insert an item into a red-black tree. The naive way to do this is to demand that the item type be ordered and walk down the tree from the root, branching at each node depending on whether the item to be inserted is greater than or less than the node item. We have to be careful here though, because we will end up violating at least one of the invariants. To see why, note that the inserted item will end up as a leaf in the tree, and (like all other leaves) the color of the new leaf must be black. But now the path from the root to this new leaf has one more black node than the others, a violation.

So during the process of insertion, what we have is _not_ a red-black tree. Fortunately though it fails in only a few possible ways; in particular, we may end up with a red root having a red child. We give an auxiliary data structure capable of holding such a tree. Note the absence of a `Valid` constraint, as well as an empty constructor (since after insertion the tree cannot be empty).

> data IR (n :: Nat) a where
>   IR
>     :: ValueColor (c :: Color)
>     -> CT n c1 a -> a -> CT n c2 a
>     -> IR (Incr c n) a

So an `IR` is almost the same as a `RedBlackTree`, except that the constraint on adjacent red nodes can be violated.

Now `insert` works something like this: first we insert into a `RedBlackTree` to return an `IR`. The possible invariant violations are known, and rebalancing corresponds to tree _rotation_.

> insert
>   :: forall a
>    . ( Ord a )
>   => a -> RedBlackTree a -> RedBlackTree a
> insert x (Root s) = blacken (ins s)
>   where
>     -- May (slightly) break the invariants
>     ins
>       :: ( Ord a )
>       => CT n c a -> IR n a
>     ins w = case w of
>       E -> IR R E x E
>       s@(T c a y b) -> case compare x y of
>         LT -> balanceL c (ins a) y b
>         EQ -> IR c a y b
>         GT -> balanceR c a y (ins b)
> 
>     blacken
>       :: IR n a -> RedBlackTree a
>     blacken (IR _ a x b) = Root (T B a x b)
> 
>     balanceL
>       :: ValueColor c -> IR n a -> a -> CT n c1 a -> IR (Incr c n) a
>     balanceL c u z d = case (c,u) of
>       (_, IR B a x b)                         -> IR c (T B a x b) z d
>       (_, IR R a@E x b@E)                     -> IR c (T R a x b) z d
>       (_, IR R a@(T B _ _ _) x b@(T B _ _ _)) -> IR c (T R a x b) z d
>       (B, IR R (T R a x b) y c)               -> IR R (T B a x b) y (T B c z d)
>       (B, IR R a x (T R b y c))               -> IR R (T B a x b) y (T B c z d)
> 
>     balanceR
>       :: ValueColor c -> CT n c1 a -> a -> IR n a -> IR (Incr c n) a
>     balanceR c a x u = case (c,u) of
>       (_, IR B b z d)                         -> IR c a x (T B b z d)
>       (_, IR R b@E z d@E)                     -> IR c a x (T R b z d)
>       (_, IR R b@(T B _ _ _) z d@(T B _ _ _)) -> IR c a x (T R b z d)
>       (B, IR R (T R b y c) z d)               -> IR R (T B a x b) y (T B c z d)
>       (B, IR R b y (T R c z d))               -> IR R (T B a x b) y (T B c z d)

With `insert` we also define a helper function for building red-black trees out of lists.

> fromList
>   :: ( Ord a )
>   => [a] -> RedBlackTree a
> fromList = foldr insert empty

And with `insert` in hand we can finally see some interesting examples!

::: doctest

> -- $
> -- >>> empty
> -- E
> --
> -- >>> insert 7 empty
> -- (T B E 7 E)
> --
> -- >>> fromList [1..5]
> -- (T B (T R (T B E 1 E) 2 (T B E 3 E)) 4 (T B E 5 E))

:::



Deletion
--------

Deleting elements from a red-black tree is much tricker than insertion, most obviously because while insertion always alters the tree at a leaf, deletion can affect an interior node. Then one of the deleted node's children is promoted, but the global invariant on black height is in danger of being violated. This code uses a strategy described by [Matt Might](http://matt.might.net/articles/red-black-delete/) which sidesteps the hard part of preserving the global invariant by introducing two new colors: _double black_, which counts as two black nodes for the purpose of computing black height, and _negative black_, which counts as negative one black nodes. Trees with nodes of these colors are not red-black trees, of course, but just like the intermediate type `IR` used for insertion, we can precisely describe how malformed intermediate trees are during deletion. Where `IR` could have a red root with a red child, we define a type, `DT`, which may have a double black node at the root or as a single leaf.

> data DT (n :: Nat) a where
>   DT
>     :: ValueColor c
>     -> CT n c1 a -> a -> CT n c2 a
>     -> DT (Incr c n) a
> 
>   -- Double-black empty tree
>   DEE
>     :: DT (S Z) a
> 
>   -- Ordinary empty tree
>   DE
>     :: DT Z a

Might's deletion strategy proceeds by attempting to _discharge_ double black nodes by rotation, and if this isn't possible, to _bubble_ the double black node further up the tree. (A double black node at the root is trivially discharged.)

> delete
>   :: forall a
>    . ( Ord a )
>   => a -> RedBlackTree a -> RedBlackTree a
> delete x (Root s) = blacken (del x s) 
>   where
>     -- May (slightly) break the invariants
>     del
>       :: ( Ord a )
>       => a -> CT n c a -> DT n a
>     del x w = case w of
>       E -> DE
>       s@(T c a y b) -> case compare x y of
>         LT -> bubbleL c (del x a) y b
>         EQ -> removeRoot s
>         GT -> bubbleR c a y (del x b)
> 
>     blacken
>       :: DT n a -> RedBlackTree a
>     blacken w = case w of
>       DE         -> Root E
>       DEE        -> Root E
>       DT _ a x b -> Root (T B a x b)
> 
>     -- Remove the root element. May leave a double
>     -- black node to preserve the black height.
>     removeRoot
>       :: ( Ord a )
>       => CT n c a -> DT n a
>     removeRoot w = case w of
>       T R E           _ E           -> DE
>       T B E           _ E           -> DEE 
>       T B E           _ (T R a x b) -> DT B a x b
>       T B (T R a x b) _ E           -> DT B a x b
>       T c l           y r           ->
>         let Just u = getMax l
>         in bubbleL c (removeMax l) u r
> 
>     -- Remove the largest element.
>     removeMax
>       :: ( Ord a )
>       => CT n c a -> DT n a
>     removeMax w = case w of
>       E         -> error "removeMax: panic"
>       T _ _ _ E -> removeRoot w
>       T c l x r -> bubbleR c l x (removeMax r)
> 
>     -- Raise a double black root in the left subtree
>     bubbleL
>       :: forall n c c1 a
>        . ValueColor c
>       -> DT n a -> a -> CT n c1 a -> DT (Incr c n) a
>     bubbleL c d x r = case (c,d) of
>       (B,  DEE)         -> dbalanceR BB E x (redden r)
>       (R,  DEE)         -> dbalanceR B  E x (redden r)
>       (NB, DEE)         -> dbalanceR R  E x (redden r)
>       (B,  DT BB a y b) -> dbalanceR BB (T B a y b) x (redden r)
>       (R,  DT BB a y b) -> dbalanceR B  (T B a y b) x (redden r)
>       (NB, DT BB a y b) -> dbalanceR R  (T B a y b) x (redden r)
>       _ -> dbalanceL c d x r
> 
>     -- Raise a double black root in the right subtree
>     bubbleR
>       :: forall n c c1 a
>        . ValueColor c
>       -> CT n c1 a -> a -> DT n a -> DT (Incr c n) a 
>     bubbleR c l x d = case (c,d) of
>       (B,  DEE)         -> dbalanceL BB (redden l) x E
>       (R,  DEE)         -> dbalanceL B  (redden l) x E
>       (NB, DEE)         -> dbalanceL R  (redden l) x E
>       (B,  DT BB a y b) -> dbalanceL BB (redden l) x (T B a y b)
>       (R,  DT BB a y b) -> dbalanceL B  (redden l) x (T B a y b)
>       (NB, DT BB a y b) -> dbalanceL R  (redden l) x (T B a y b)
>       _ -> dbalanceR c l x d
> 
>     -- Decrease the black height
>     redden
>       :: forall n c a
>        . CT (S n) c a -> DT n a
>     redden (T c a x y) = case c of
>       B  -> DT R  a x y
>       BB -> DT B  a x y
>       R  -> DT NB a x y
> 
>     dbalanceL
>       :: forall n c c1 a
>        . ValueColor c
>       -> DT n a -> a -> CT n c1 a -> DT (Incr c n) a
>     dbalanceL k dt u r = case (k,dt) of
>       (B,  DT R (T R a x b) y c)           -> DT R (T B a x b) y (T B c u r)
>       (B,  DT R a           x (T R b y c)) -> DT R (T B a x b) y (T B c u r)
>       (BB, DT R (T R a x b) y c)           -> DT B (T B a x b) y (T B c u r)
>       (BB, DT R a           x (T R b y c)) -> DT B (T B a x b) y (T B c u r)
> 
>       (BB, DT NB a@(T B _ _ _) x (T B b y c)) ->
>         case dbalanceL B (redden a) x b of
>           l@(DT R _ _ _) -> DT B (assertRed l)  y (T B c u r)
>           DT B a1 x1 y1  -> DT B (T B a1 x1 y1) y (T B c u r)
> 
>       -- Fallthrough cases
>       (_, DE)                                 -> DT k E           u r
>       (_, DT B a             x b)             -> DT k (T B a x b) u r
>       (_, DT R a@E           x b@E)           -> DT k (T R a x b) u r
>       (_, DT R a@(T B _ _ _) x b@(T B _ _ _)) -> DT k (T R a x b) u r
>       _ -> error "dbalanceL: panic"
> 
>     dbalanceR
>       :: forall n c c1 a
>        . ValueColor c
>       -> CT n c1 a -> a -> DT n a -> DT (Incr c n) a
>     dbalanceR k l u dt = case (k,dt) of
>       (B,  DT R (T R b y c) z d)           -> DT R (T B l u b) y (T B c z d)
>       (B,  DT R b           y (T R c z d)) -> DT R (T B l u b) y (T B c z d)
>       (BB, DT R (T R b y c) z d)           -> DT B (T B l u b) y (T B c z d)
>       (BB, DT R b           y (T R c z d)) -> DT B (T B l u b) y (T B c z d)
> 
>       (BB, DT NB (T B b y c) z d@(T B _ _ _)) ->
>         case (dbalanceR B c z (redden d)) of
>           r@(DT R _ _ _)  -> DT B (T B l u b) y (assertRed r)
>           (DT B a1 x1 y1) -> DT B (T B l u b) y (T B a1 x1 y1)
> 
>       -- Fallthrough cases
>       (_, DE)                                 -> DT k l u E
>       (_, DT B b             z d)             -> DT k l u (T B b z d)
>       (_, DT R b@E           z d@E)           -> DT k l u (T R b z d)
>       (_, DT R b@(T B _ _ _) z d@(T B _ _ _)) -> DT k l u (T R b z d)
>       _ -> error "dbalanceR: panic"
> 
>     assertRed
>       :: forall n c a
>        . DT n a -> CT n Red a
>     assertRed w = case w of
>       DT R (T B a y b) x (T B c z d) ->
>         T R (T B a y b) x (T B c z d)
>       _ -> error "assertRed: panic"

And we can check some examples.

::: doctest

> -- $
> -- >>> let x = fromList [1,2,3]
> -- >>> member 2 x
> -- True
> -- >>> member 2 (delete 2 x)
> -- False

:::



Testing and Debugging
---------------------

Finally, class instances for using `RedBlackTree`s with our property testing framework.

> instance
>   ( Ord a, Arb a
>   ) => Arb (RedBlackTree a)
>   where
>     arb = w
>       where
>         w
>           :: forall a
>            . ( Ord a, Arb a )
>           => Seeded (RedBlackTree a)
>         w = fmap fromList (arb :: Seeded [a])
> 
> instance
>   ( Ord a, Prune a
>   ) => Prune (RedBlackTree a)
>   where
>     prune = map fromList . prune . toList

The intermediate tree types aren't used outside this module, but for debugging, some show instances are handy.

> instance
>   ( Show a
>   ) => Show (IR n a)
>   where
>     show x = case x of
>       IR c l x r -> concat
>         [ "(IR ", show c, " ", show l
>         , " ... ", show r, ")"
>         ]
> 
> instance
>   ( Show a
>   ) => Show (DT n a)
>   where
>     show x = case x of
>       DE -> "DE"
>       DEE -> "DEE"
>       DT c l x r -> concat
>         [ "(DT ", show c, " ", show l
>         , " ... ", show r, ")"
>         ]
