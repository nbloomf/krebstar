---
title: Categories in Haskell
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.CategoryC where
> 
> import GHC.Exts (Constraint)
> import Control.Monad ((<=<), join)
> 
> import           Kreb.Control
> import qualified Kreb.Format as Fmt
> import           Kreb.Format
> import           Kreb.Prop

:::



Introduction
------------

We've avoided talking about categories so far, but no more.

A category has 5 parts:

- A class of _objects_, with an equality relation
- A class of _morphisms_, with an equality relation
- For every morphism $f$, two associated objects $\dom(f)$ and $\cod(f)$, called the _domain_ and _codomain_ of $f$. If $A$ and $B$ are objects and $f$ is a morphism such that $A = \dom(f)$ and $B = \cod(f)$, we write this as $f : A \rightarrow B$ or $A \xrightarrow{f} B$, pronounced "$f$ maps from $A$ to $B$".
- For every pair of morphisms $f : A \rightarrow B$ and $g : B \rightarrow C$, a morphism $g \circ f : A \rightarrow C$, called the _composite_ of $f$ and $g$.
- For every object $A$, a morphism $\id_A$ called the _identity_ on $A$.

Moreover these parts have to satisfy some laws:

Associativity
  : If $f : A \rightarrow B$, $g : B \rightarrow C$, and $h : C \rightarrow D$, then $h \circ (g \circ f) = (h \circ g) \circ f$.

Identity
  : If $f : A \rightarrow B$, then $f \circ \id_A = f$ and $\id_B \circ f = f$.

The first time we see an abstraction like this, it's important to ask -- what are the concrete examples that motivated it? For categories the original motivation comes from math, where it's very common to study a particular kind of "structure" (loosely defined) together with an associated class of "structure preserving functions" which together form a category. Category theory gives a common language we can use to understand otherwise very different kinds of things, but they sit a good 4 or 5 levels of abstraction above what normal people would consider _concrete_, and it takes a decent amount of mathematical maturity to appreciate the problem they solve.

But for us here there's one very concrete example of a category we can play with: Haskell types and functions. We'll call this category $\Hask$.

More generally, given any type constraint $C$, the Haskell types inhabiting $C$ and the functions among them form a category, which we'll call $\Hask_C$. More precisely, $\Hask_C$ is a _full subcategory_ of $\Hask$, which just means that if $A$ and $B$ are objects in $\Hask_C$ (and thus also $\Hask$), then all the morphisms $f : A \rightarrow B$ in $\Hask$ are also morphisms in $\Hask_C$.

We can use the machinery of Haskell's (or rather, GHC's) type system to encode $\Hask_C$. We first define two type synonyms: `Obj` for constraints, representing the object classes, and `Mor` for morphisms (since not all morphisms are strictly functions, as we'll see).

> type Obj = * -> Constraint
> type Mor = * -> * -> *

Now a given `Obj` and `Mor` form a category if they have an identity and composition, which we call `unit` and `comp`.^[I can't think of a useful way to encode $\dom$ and $\cod$ in Haskell, but fortunately we don't actually need them.]

> class CategoryC (obj :: Obj) (mor :: Mor) where
>   unit
>     :: (obj x)
>     => mor x x
>   comp
>     :: (obj x, obj y, obj z)
>     => mor y z -> mor x y -> mor x z

Strictly speaking we need this instance to be _lawful_ -- to satisfy the category laws. One hitch in checking the laws for a particular instance is that what it means for two morphisms to be _equal_ is tricky.

Let's see some examples. The empty constraint is trivially inhabited by all types, and is a category with ordinary functions.

> class    Hask a
> instance Hask a
> 
> instance CategoryC (obj :: Obj) (->) where
>   unit = id
>   comp = (.)

We can generalize this to constrained functions with a GADT.

> data Map (dom :: Obj) (cod :: Obj) a b where
>   Map
>     :: (dom a, cod b)
>     => (a -> b) -> Map dom cod a b
> 
> type MapOn obj = Map obj obj

Next we need some utility instances for `Map`:

> instance (EqIn ctx b) => EqIn (ctx,a) (Map dom cod a b) where
>   eqIn (ctx,a) (Map f) (Map g) = eqIn ctx (f a) (g a)
> 
> instance (dom a, cod b) => Fmt.Display (Map dom cod a b) where
>   display _ = string "<Map>"
> 
> instance (dom a, cod b, CoArb a, Arb b) => Arb (Map dom cod a b) where
>   arb = fmap Map arb
> 
> instance (dom a, cod b) => Prune (Map dom cod a b) where
>   prune _ = []

And now we can define a category instance for arbitrary type constraints. Using `MapOn` here also prevents this from overlapping the instance for `Hask`.

> instance
>   CategoryC (obj :: Obj) (MapOn obj)
>   where
>     unit = Map id
>     comp (Map g) (Map f) = Map (g . f)



Other Examples
--------------

In the definition of category the roles of $\dom$ and $\cod$ are symmetric, meaning we can swap them and get another structure satisfying the laws. The result is called the _opposite_ of the original category, and intuitively it's obtained by reversing all the arrows. We can encode this using the `Opp` type.

> data Opp mor a b =
>   Opp (mor b a)
> 
> instance
>   ( CategoryC obj mor
>   ) => CategoryC obj (Opp mor)
>   where
>     unit = Opp (unit @obj)
>     comp (Opp f) (Opp g) = Opp (comp @obj g f)

Given a monad `m`, functions of type `a -> m b` are called _kleisli arrows_ of `m`. The `return` function from the monad signature is a kleisli arrow, and the standard library includes functions for working with them. Anyway it's not too surprising that kleisli arrows are the morphisms of a category.

> data Kleisli (dom :: Obj) (cod :: Obj) m a b where
>   Kleisli
>     :: ( Monad m, dom a, cod b )
>     => (a -> m b)
>     -> Kleisli dom cod m a b
> 
> type KleisliOn obj = Kleisli obj obj
> 
> instance
>   ( Monad m
>   ) => CategoryC (obj :: Obj) (KleisliOn obj m)
>   where
>     unit = Kleisli return
>     comp (Kleisli g) (Kleisli f) = Kleisli (g <=< f)

There's another way to build a category out of a monad, called the _Eilenberg-Moore_ category. Given a monad `m`, a type `a` with a distinguished morphism `m a -> a` is called an _algebra_ of the monad. Every monad has at least two algebras -- one induced by `join`, on `m a`, and the constant function, on `()`.

> class
>   ( Monad m
>   ) => AlgebraC (obj :: Obj) m a
>   where
>     algebraC :: (obj a) => m a -> a
> 
> instance
>   ( Monad m
>   ) => AlgebraC (obj :: Obj) m (m a)
>   where
>     algebraC = join
> 
> instance
>   ( Monad m
>   ) => AlgebraC (obj :: Obj) m ()
>   where
>     algebraC _ = ()

It turns out that the algebras of a monad form a category. Morphisms are (ordinary) functions between algebra objects.

> data MapAlg (dom :: Obj) (cod :: Obj) m a b where
>   MapAlg
>     :: ( AlgebraC dom m a, AlgebraC cod m b )
>     => (a -> b) -> MapAlg dom cod m a b
> 
> type MapAlgOn obj = MapAlg obj obj
> 
> instance
>   ( Monad m
>   ) => CategoryC (AlgebraC (obj :: Obj) m) (MapAlgOn obj m)
>   where
>     unit = MapAlg @obj id
>     comp (MapAlg f) (MapAlg g) = MapAlg (f . g)
