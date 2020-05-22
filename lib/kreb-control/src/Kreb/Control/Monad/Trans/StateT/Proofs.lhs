---
title: Lawfulness Proofs for StateT
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
> 
> module Kreb.Control.Monad.Trans.StateT.Proofs where
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Trans.StateT

:::



Introduction
------------

One really nice consequence of referential transparency is that we can _reason_ about our haskell code. Instances of the `Monad` and `MonadTrans` type classes are required to satisfy some algebraic laws, which we can prove for `StateT`. The proofs here are written in an equational style, as a list of statements each equivalent to its neighbors, with each equivalence justified by some known (or assumed) equality.

Because we're proving things about Haskell code, we can write the proofs _in_ Haskell, and get some syntax and type validation of our steps for free.



Monad Laws
----------

[@statet-monad-lawful]()

Recall the three _monad laws_:

Left Identity Law
  : `return a >>= f` is equivalent to `f a`

Right Identity Law
  : `x >>= return` is equivalent to `x`

Associative Law
  : `(x >>= f) >>= g` is equivalent to `x >>= (\a -> f a >>= g)`

Here we show that if `m` is a monad, then `StateT s m` is also a monad. To that end suppose `m` is a monad.

First: `StateT s m` satisfies the left identity law.

::: example

> _StateT_left_identity_law
>   :: forall s m a b
>    . ( Monad m )
>   => a -> (a -> StateT s m b)
>   -> [StateT s m b]
> _StateT_left_identity_law a f =
> 
>   [ return a >>= f
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (return a) s1
>         >>= (\(b, s2) -> unStateT (f b) s2)
> 
>   -- (expand return)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (a,s)) s1
>         >>= (\(b, s2) -> unStateT (f b) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> return (a,s)) s1
>         >>= (\(b, s2) -> unStateT (f b) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return (a,s1)
>         >>= (\(b, s2) -> unStateT (f b) s2)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(b, s2) -> unStateT (f b) s2) (a, s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (f a) s1
> 
>   -- (η-reduction)
> 
>   , StateT $ (unStateT (f a))
> 
>   -- (StateT . unStateT == id)
> 
>   , f a
>   ]

:::::::::::

Second: `StateT s m` satisfies the right identity law.

::: example

> _StateT_right_identity_law
>   :: forall s m a
>    . ( Monad m )
>   => (StateT s m a)
>   -> [StateT s m a]
> _StateT_right_identity_law x =
> 
>   [ x >>= return
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT x s1
>         >>= (\(a, s2) -> unStateT (return a) s2)
> 
>   -- (expand return)
> 
>   , StateT $ \s1 ->
>       unStateT x s1
>         >>= (\(a, s2) -> unStateT (StateT $ \s -> return (a,s)) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT x s1
>         >>= (\(a, s2) -> (\s -> return (a,s)) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT x s1
>         >>= (\(a, s2) -> return (a, s2))
> 
>   -- (η-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT x s1
>         >>= return
> 
>   -- (right identity law for m)
> 
>   , StateT $ \s1 -> unStateT x s1
> 
>   -- (η-reduction)
> 
>   , StateT $ unStateT x
> 
>   -- (StateT . unStateT == id)
> 
>   , x
>   ]

:::::::::::

Third: `StateT s m` satisfies the associativity law.

::: example

> _StateT_associativity_law
>   :: forall s m a b c
>    . ( Monad m )
>   => StateT s m a -> (a -> StateT s m b) -> (b -> StateT s m c)
>   -> [StateT s m c]
> _StateT_associativity_law x f g =
> 
>   [ (x >>= f) >>= g
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (x >>= f) s1
>         >>= (\(b, s2) -> unStateT (g b) s2)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s2 ->
>         unStateT x s2 >>= (\(a, s3) -> unStateT (f a) s3)) s1
>         >>= (\(b, s2) -> unStateT (g b) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s2 ->
>         unStateT x s2 >>= (\(a, s3) -> unStateT (f a) s3)) s1
>         >>= (\(b, s2) -> unStateT (g b) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>= (\(a, s3) -> unStateT (f a) s3)
>         >>= (\(b, s2) -> unStateT (g b) s2)
> 
>   -- (associativity law in m)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>= (\(c, s4) ->
>         (\(a, s3) -> unStateT (f a) s3) (c, s4)
>           >>= (\(b, s2) -> unStateT (g b) s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>= (\(c, s4) ->
>         unStateT (f c) s4
>           >>= (\(b, s2) -> unStateT (g b) s2))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>=
>         (\(c, s4) -> (\s5 ->
>           unStateT (f c) s5 >>= (\(b, s2) -> unStateT (g b) s2)) s4)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>=
>         (\(c, s4) -> unStateT (StateT $ \s5 ->
>           unStateT (f c) s5 >>= (\(b, s2) -> unStateT (g b) s2)) s4)
> 
>   -- (condense >>=)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>=
>         (\(c, s4) -> unStateT (f c >>= g) s4)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       unStateT x s1 >>=
>         (\(c, s4) -> unStateT ((\a -> f a >>= g) c) s4)
> 
>   -- (condense >>=)
> 
>   , x >>= (\a -> f a >>= g)
>   ]

:::::::::::

That looks pretty bad, but each step is a straightforward lawful program transformation.



Monad Transformer Laws
----------------------

[@statet-monad-trans-lawful]()

Recall the two _monad transformer laws_:

Unit Law
  : `lift . return` is equivalent to `return`

Bind Law
  : `lift (x >>= f)` is equivalent to `lift x >>= (lift . f)`

Here we show that `StateT s` is a monad transformer. First: `StateT s` satisfies the unit law.

::: example

> _StateT_unit_law
>   :: forall s m a
>    . ( Monad m )
>   => a
>   -> [StateT s m a]
> _StateT_unit_law a =
> 
>   [ lift (return a)
> 
>   -- (expand lift)
> 
>   , StateT $ \s ->
>       return a
>         >>= (\b -> return (b, s))
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s ->
>       (\b -> return (b, s)) a
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       return (a, s)
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Second: `StateT s` satisfies the bind law.

::: example

> _StateT_bind_law
>   :: forall s m a b
>    . ( Monad m )
>   => m a -> (a -> m b)
>   -> [StateT s m b]
> _StateT_bind_law x f =
> 
>   [ lift (x >>= f)
> 
>   -- (expand lift)
> 
>   , StateT $ \s ->
>       (x >>= f)
>         >>= (\a -> return (a, s))
> 
>   -- (associativity law for m)
> 
>   , StateT $ \s ->
>       x
>         >>= (\c -> f c >>= (\a -> return (a, s)))
> 
>   -- (α-conversion)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\c -> f c >>= (\d -> return (d, s1)))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\c ->
>           (\(a, s2) -> f a >>= (\d -> return (d, s2))) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\c -> return (c, s1)
>           >>= (\(a, s2) -> f a >>= (\d -> return (d, s2))))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> f a >>= (\d -> return (d, s2))))
> 
>   -- (associativity law in m)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> f a >>= (\d -> return (d, s2)))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> (\s -> f a >>= (\d -> return (d, s))) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStateT (StateT $ \s -> f a >>= (\d -> return (d, s))) s2)
> 
>   -- (condense lift)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStateT (lift (f a)) s2)
> 
>   -- (definition of .)
> 
>   , StateT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStateT ((lift . f) a) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       ((\s -> x >>= (\b -> return (b, s))) s1)
>         >>= (\(a, s2) -> unStateT ((lift . f) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> x >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((lift . f) a) s2)
> 
>   -- (condense lift)
> 
>   , StateT $ \s1 ->
>       unStateT (lift x) s1
>         >>= (\(a, s2) -> unStateT ((lift . f) a) s2)
> 
>   -- (condense >>=)
> 
>   , lift x >>= (lift . f)
>   ]

:::::::::::

There's probably a shorter way to do that!



MonadReadWrite Laws
-------------------

[@statet-monad-read-write-lawful]()

There are two different `MonadReadWrite` instances for `StateT s m` that we need to verify. Recall the laws for [`MonadReadWrite`](@monad-read-write):

Put/Put Law
  : `put u1 >> put u2` is equivalent to `put u2`

Put/Get Law
  : `put u >> get` is equivalent to `put u >> return u`

Get/Put Law
  : `get >>= put` is equivalent to `return ()`

Get/Get Law
  : `get >>= (\u -> get >>= k u)` is equivalent to `get >>= (\u -> k u u)`

Here we show that if `m` is a monad then `StateT s m` satisfies the MonadReadWrite laws with state type `s`.

First: `StateT s m` satisfies the put/put law with state type `s`:

::: example

> _StateT_put_put_law_natural
>   :: forall s m
>    . ( Monad m )
>   => s -> s
>   -> [StateT s m ()]
> _StateT_put_put_law_natural u1 u2 =
> 
>   [ put u1 >> put u2
> 
>   -- (expand >>)
> 
>   , put u1 >>= (\_ -> put u2)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (put u1) s1
>         >>= (\(a, s2) -> unStateT ((\_ -> put u2) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (put u1) s1
>         >>= (\(a, s2) -> unStateT (put u2) s2)
> 
>   -- (expand put)
> 
>   , StateT $ \s1 ->
>       unStateT (put u1) s1
>         >>= (\(a, s2) -> unStateT (StateT $ \_ -> return ((), u2)) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (put u1) s1
>         >>= (\(a, s2) -> (\_ -> return ((), u2)) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (put u1) s1
>         >>= (\(a, s2) -> return ((), u2))
> 
>   -- (expand put)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \_ -> return ((), u1)) s1
>         >>= (\(a, s2) -> return ((), u2))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\_ -> return ((), u1)) s1
>         >>= (\(a, s2) -> return ((), u2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return ((), u1)
>         >>= (\(a, s2) -> return ((), u2))
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> return ((), u2)) ((), u1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return ((), u2)
> 
>   -- (α-conversion)
> 
>   , StateT $ \_ ->
>       return ((),u2)
> 
>   -- (condense put)
> 
>   , put u2
>   ]

:::::::::::

Second: `StateT s m` satisfies the put/get law with state type `s`.

::: example

> _StateT_put_get_law_natural
>   :: forall s m
>    . ( Monad m )
>   => s
>   -> [StateT s m s]
> _StateT_put_get_law_natural u =
> 
>   [ put u >> get
> 
>   -- (expand >>)
> 
>   , put u >>= (\_ -> get)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a, s2) -> unStateT ((\_ -> get) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a, s2) -> unStateT get s2)
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a, s2) -> unStateT (StateT $ \s -> return (s, s)) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a, s2) -> (\s -> return (s, s)) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a, s2) -> return (s2, s2))
> 
>   -- (expand put)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \_ -> return ((), u)) s1
>         >>= (\(a, s2) -> return (s2, s2))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\_ -> return ((), u)) s1
>         >>= (\(a, s2) -> return (s2, s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return ((), u)
>         >>= (\(a, s2) -> return (s2, s2))
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> return (s2, s2)) ((), u)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return (u, u)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\s -> return (u, s)) u
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (u, s)) u
> 
>   -- (condense return)
> 
>   , StateT $ \s1 ->
>       unStateT (return u) u
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> unStateT (return u) s2) ((), u)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       return ((), u)
>         >>= (\(a, s2) -> unStateT (return u) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\_ -> (return ((), u))) s1
>         >>= (\(a, s2) -> unStateT (return u) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \_ -> return ((), u)) s1
>         >>= (\(a, s2) -> unStateT (return u) s2)
> 
>   -- (condense put)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a,s2) -> unStateT (return u) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       unStateT (put u) s1
>         >>= (\(a,s2) -> unStateT ((\_ -> return u) a) s2)
> 
>   -- (condense >>=)
> 
>   , put u >>= (\_ -> return u)
> 
>   -- (condense >>)
> 
>   , put u >> return u
>   ]

:::::::::::

Third: `StateT s m` satisfies the get/put law with state type `s`.

::: example

> _StateT_get_put_law_natural
>   :: forall s m
>    . ( Monad m )
>   => [StateT s m ()]
> _StateT_get_put_law_natural =
> 
>   [ get @s >>= put
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (get @s) s1
>         >>= (\(a, s2) -> unStateT (put a) s2)
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (put a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (put a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStateT (put a) s2)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> unStateT (put a) s2) (s1, s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (put s1) s1
> 
>   -- (expand put)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return ((), s)) s1
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> return ((), s)) s1
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return ((), s1)
> 
>   -- (condense return)
> 
>   , return ()
>   ]

:::::::::::

Fourth: `StateT s m` satisfies the get/get law with state type `s`.

::: example

> _StateT_get_get_law_natural
>   :: forall s m a
>    . ( Monad m )
>   => (s -> s -> StateT s m a)
>   -> [StateT s m a]
> _StateT_get_get_law_natural k =
> 
>   [ get >>= (\u -> get >>= k u)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT (get >>= k a) s2)
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (get >>= k a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (get >>= k a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStateT (get >>= k a) s2)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> unStateT (get >>= k a) s2) (s1, s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (get >>= k s1) s1
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s ->
>         unStateT get s >>= (\(a, s2) -> unStateT (k s1 a) s2)) s1
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> unStateT get s >>= (\(a, s2) -> unStateT (k s1 a) s2)) s1
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT (k s1 a) s2)
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (k s1 a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT (k s1 a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStateT (k s1 a) s2)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> unStateT (k s1 a) s2) (s1, s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       unStateT (k s1 s1) s1
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       unStateT ((\u -> k u u) s1) s1
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\(a, s2) -> unStateT ((\u -> k u u) a) s2) (s1, s1)
> 
>   -- (left identity law for m)
> 
>   , StateT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense get)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , get >>= (\u -> k u u)
>   ]

:::::::::::

Next we show that if `m` is a lawful instance of `MonadReadWrite` with state type `t` different from `s`, then so is `StateT s m`.

First: `StateT s m` satisfies the put/put law with state type `t`:

::: example

> _StateT_put_put_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m, Compare s t ~ NotEqual )
>   => t -> t
>   -> [StateT s m ()]
> _StateT_put_put_law_lifted u1 u2 =
> 
>   [ put u1 >> put u2
> 
>   -- (expand >>)
> 
>   , put u1 >>= (\_ -> put u2)
> 
>   -- (condense const)
> 
>   , put u1 >>= const (put u2)
> 
>   -- (expand put)
> 
>   , lift (put u1) >>= const (lift (put u2))
> 
>   -- (property of const)
> 
>   , lift (put u1) >>= lift . const (put u2)
> 
>   -- (bind law)
> 
>   , lift (put u1 >>= const (put u2))
> 
>   -- (expand const)
> 
>   , lift (put u1 >>= (\_ -> put u2))
> 
>   -- (condense >>)
> 
>   , lift (put u1 >> put u2)
> 
>   -- (put/put law in m)
> 
>   , lift (put u2)
> 
>   -- (condense put)
> 
>   , put u2
>   ]

:::::::::::

Second: `StateT s m` satisfies the put/get law with state type `t`:

::: example

> _StateT_put_get_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m, Compare s t ~ NotEqual )
>   => t
>   -> [StateT s m t]
> _StateT_put_get_law_lifted u =
> 
>   [ put u >> get
> 
>   -- (expand >>)
> 
>   , put u >>= (\_ -> get)
> 
>   -- (condense const)
> 
>   , put u >>= const get
> 
>   -- (expand put and get)
> 
>   , lift (put u) >>= const (lift get)
> 
>   -- (property of const)
> 
>   , lift (put u) >>= lift . const get
> 
>   -- (bind law)
> 
>   , lift (put u >>= const get)
> 
>   -- (expand const)
> 
>   , lift (put u >>= (\_ -> get))
> 
>   -- (condense >>)
> 
>   , lift (put u >> get)
> 
>   -- (put/get law in m)
> 
>   , lift (return u)
> 
>   -- (unit law)
> 
>   , return u
>   ]

:::::::::::

Third: `StateT s m` satisfies the get/put law.

::: example

> _StateT_get_put_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m, Compare s t ~ NotEqual )
>   => [StateT s m ()]
> _StateT_get_put_law_lifted =
> 
>   [ get @t >>= put
> 
>   -- (expand put and get)
> 
>   , lift (get @t) >>= lift . put
> 
>   -- (bind law)
> 
>   , lift (get @t >>= put)
> 
>   -- (get/put law in m)
> 
>   , lift (return ())
> 
>   -- (unit law)
> 
>   , return ()
>   ]

:::::::::::

Fourth: `StateT s m` satisfies the get/get law with state type `t`.

::: example

> _StateT_get_get_law_lifted
>   :: forall s t m a
>    . ( MonadReadWrite t m, Compare s t ~ NotEqual )
>   => (t -> t -> StateT s m a)
>   -> [StateT s m a]
> _StateT_get_get_law_lifted k =
> 
>   [ get >>= (\u -> get >>= k u)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       unStateT (lift get) s1
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (expand lift)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       (get >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2)
> 
>   -- (associative law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\(a, s2) -> unStateT ((\u -> get >>= k u) a) s2) (c, s1))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT ((\u -> get >>= k u) c) s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT (get >>= k c) s1)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT (StateT $ \s -> unStateT get s
>           >>= (\(a, s2) -> unStateT (k c a) s2)) s1)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\s -> unStateT get s >>= (\(a, s2) -> unStateT (k c a) s2)) s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT get s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (expand get)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT (lift get) s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (expand lift)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT (StateT $ \s -> get
>           >>= (\b -> return (b, s))) s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\s -> get >>= (\b -> return (b, s))) s1
>           >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>= (\b -> return (b, s1))
>           >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (associative law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\b -> return (b, s1)) d >>= (\(a, s2) -> unStateT (k c a) s2)))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> return (d, s1) >>= (\(a, s2) -> unStateT (k c a) s2)))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\(a, s2) -> unStateT (k c a) s2) (d, s1)))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> unStateT (k c d) s1))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\u v -> unStateT (k u v) s1) c d))
> 
>   -- (η-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> get >>= (\u v -> unStateT (k u v) s1) c)
> 
>   -- (get/get law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\u v -> unStateT (k u v) s1) c c)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT (k c c) s1)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> unStateT ((\u -> k u u) c) s1)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c ->
>           (\(a, s2) -> unStateT ((\u -> k u u) a) s2) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       get >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2))
> 
>   -- (associativity law in m)
> 
>   , StateT $ \s1 ->
>       (get >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense lift)
> 
>   , StateT $ \s1 ->
>       unStateT (lift get) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense get)
> 
>   , StateT $ \s1 ->
>       unStateT get s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , get >>= (\u -> k u u)
>   ]

:::::::::::

There has to be a shorter way to do that!



MonadReadOnly Laws
------------------

[@statet-monad-read-only-lawful]()

There is a `MonadReadOnly` instance for `StateT s m` that we need to verify. Recall the laws for [`MonadReadOnly`](@monad-read-only):

Local/Ask Law
  : `local u ask` is equivalent to `fmap u ask`

Local/Local Law
  : `local u (local v x)` is equivalent to `local (v . u) x`

Interchange Law
  : `local u x >> ask` is equivalent to `ask >>= (\r -> local u x >> return r)`

Local Unit Law
  : `local u (return a)` is equivalent to `return a`

Local Bind Law
  : `local u (x >>= f)` is equivalent to `local u x >>= (local u . f)`

Here we show that if `m` is a lawful instance of `MonadReadOnly` with environment type `t`, then so is `StateT s m`.

First: `StateT s m` satisfies the local/ask law with environment type `t`:

::: example

> _StateT_local_ask_law_lifted
>   :: forall s t m
>    . ( MonadReadOnly t m )
>   => (t -> t)
>   -> [StateT s m t]
> _StateT_local_ask_law_lifted u =
> 
>   [ local u ask
> 
>   -- (expand local)
> 
>   , StateT $ \s ->
>       local u (unStateT ask s)
> 
>   -- (expand ask)
> 
>   , StateT $ \s ->
>       local u (unStateT (lift ask) s)
> 
>   -- (expand lift)
> 
>   , StateT $ \s ->
>       local u (unStateT (StateT $ \s1 -> ask
>         >>= (\a -> return (a, s1))) s)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       local u ((\s1 -> ask
>         >>= (\a -> return (a, s1))) s)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (ask >>= (\a -> return (a, s)))
> 
>   -- (local/bind law in m)
> 
>   , StateT $ \s ->
>       local u ask >>= local u . (\a -> return (a, s))
> 
>   -- (compose lambda)
> 
>   , StateT $ \s ->
>       local u ask >>= (\a -> local u (return (a, s)))
> 
>   -- (local unit law in m)
> 
>   , StateT $ \s ->
>       local u ask >>= (\a -> return (a, s))
> 
>   -- (local/ask law in m)
> 
>   , StateT $ \s ->
>       fmap u ask >>= (\a -> return (a, s))
> 
>   -- (condense lift)
> 
>   , lift (fmap u ask)
> 
>   -- (lift/fmap)
> 
>   , fmap u (lift ask)
> 
>   -- (condense ask)
> 
>   , fmap u ask
>   ]

:::::::::::

Second: `StateT s m` satisfies the local/local law.

::: example

> _StateT_local_local_law_lifted
>   :: forall s t m a
>    . ( MonadReadOnly t m )
>   => (t -> t) -> (t -> t) -> StateT s m a
>   -> [StateT s m a]
> _StateT_local_local_law_lifted u v x =
> 
>   [ local u (local v x)
> 
>   -- (expand local)
> 
>   , StateT $ \s ->
>       local u (unStateT (local v x) s)
> 
>   -- (expand local)
> 
>   , StateT $ \s ->
>       local u (unStateT (StateT $ \s1 -> local v (unStateT x s1)) s)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       local u ((\s1 -> local v (unStateT x s1)) s)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (local v (unStateT x s))
> 
>   -- (local/local law in m)
> 
>   , StateT $ \s ->
>       local (v . u) (unStateT x s)
> 
>   -- (condense local)
> 
>   , local (v . u) x
>   ]

:::::::::::

Third: `StateT s m` satisfies the interchange law.

::: example

> _StateT_interchange_law_lifted
>   :: forall s t m a
>    . ( MonadReadOnly t m )
>   => (t -> t) -> StateT s m a
>   -> [StateT s m t]
> _StateT_interchange_law_lifted u x =
> 
>   [ local u x >> ask
> 
>   -- (expand >>)
> 
>   , local u x >>= (\_ -> ask)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s ->
>       unStateT (local u x) s
>         >>= (\(a, s2) -> unStateT ((\_ -> ask) a) s2)
> 
>   -- (expand local)
> 
>   , StateT $ \s ->
>       unStateT (StateT $ \s1 -> local u (unStateT x s1)) s
>         >>= (\(a, s2) -> unStateT ((\_ -> ask) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       (\s1 -> local u (unStateT x s1)) s
>         >>= (\(a, s2) -> unStateT ((\_ -> ask) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (unStateT x s)
>         >>= (\(a, s2) -> unStateT ((\_ -> ask) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (unStateT x s)
>         >>= (\(a, s2) -> unStateT ask s2)
> 
>   -- (expand ask)
> 
>   , StateT $ \s ->
>       local u (unStateT x s)
>         >>= (\(a, s2) -> unStateT (lift ask) s2)
> 
>   -- (expand lift)
> 
>   , StateT $ \s ->
>       local u (unStateT x s)
>         >>= (\(a, s2) -> unStateT (StateT $ \s1 -> ask >>= (\b -> return (b, s1))) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       local u (unStateT x s)
>         >>= (\(a, s2) -> (\s1 -> ask >>= (\b -> return (b, s1))) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (unStateT x s) >>= (\(a, s2) -> ask >>= (\b -> return (b, s2)))






> 
>   , StateT $ \s ->
>       ask >>= (\d -> local u (unStateT x s) >>= (\(a, s3) -> return (d, s3)))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           local u (unStateT x s)
>             >>= (\(a, s3) -> (\s1 -> return (d, s1)) s3))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           local u (unStateT x s)
>             >>= (\(a, s3) -> unStateT (StateT $ \s1 -> return (d, s1)) s3))
> 
>   -- (condense return)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           local u (unStateT x s)
>             >>= (\(a, s3) -> unStateT (return d) s3))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           (\s2 -> local u (unStateT x s2)) s
>             >>= (\(a, s3) -> unStateT (return d) s3))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           unStateT (StateT $ \s2 -> local u (unStateT x s2)) s
>             >>= (\(a, s3) -> unStateT (return d) s3))
> 
>   -- (condense local)
> 
>   , StateT $ \s ->
>       ask
>         >>= (\d ->
>           unStateT (local u x) s
>             >>= (\(a, s3) -> unStateT (return d) s3))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       ask >>= (\d -> unStateT (local u x) s
>         >>= (\(a, s3) -> unStateT ((\_ -> return d) a) s3))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       ask >>= (\d -> (\(c, s1) ->
>         unStateT (local u x) s1
>           >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3)) (d, s))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s ->
>       ask >>= (\d -> return (d, s)
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3)))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       ask >>= (\d -> (\b -> return (b, s)) d
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3)))
> 
>   -- (associative law in m)
> 
>   , StateT $ \s ->
>       (ask >>= (\b -> return (b, s)))
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       (\s2 -> ask >>= (\b -> return (b, s2))) s
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       unStateT (StateT $ \s2 -> ask >>= (\b -> return (b, s2))) s
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3))
> 
>   -- (condense lift)
> 
>   , StateT $ \s ->
>       unStateT (lift ask) s
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3))
> 
>   -- (condense ask)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) ->
>           unStateT (local u x) s1
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) -> (\s2 ->
>           unStateT (local u x) s2
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3)) s1)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) -> unStateT (StateT $ \s2 ->
>           unStateT (local u x) s2
>             >>= (\(a, s3) -> unStateT ((\_ -> return c) a) s3)) s1)
> 
>   -- (condense >>=)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) -> unStateT (local u x >>= (\_ -> return c)) s1)
> 
>   -- (condense >>)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) -> unStateT (local u x >> return c) s1)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s ->
>       unStateT ask s
>         >>= (\(c, s1) -> unStateT ((\a -> local u x >> return a) c) s1)
> 
>   -- (condense >>=)
> 
>   , ask >>= (\a -> local u x >> return a)
>   ]

:::::::::::

Fourth: `StateT s m` satisfies the local unit law.

::: example

> _StateT_local_unit_law_lifted
>   :: forall s t m a
>    . ( MonadReadOnly t m )
>   => (t -> t) -> a
>   -> [StateT s m a]
> _StateT_local_unit_law_lifted u a =
> 
>   [ local u (return a)
> 
>   -- (expand local)
> 
>   , StateT $ \s ->
>       local u (unStateT (return a) s)
> 
>   -- (expand return)
> 
>   , StateT $ \s ->
>       local u (unStateT (StateT $ \s1 -> return (a, s1)) s)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s ->
>       local u ((\s1 -> return (a, s1)) s)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s ->
>       local u (return (a, s))
> 
>   -- (local unit law in m)
> 
>   , StateT $ \s ->
>       return (a, s)
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Fifth: `StateT s m` satisfies the local bind law.

::: example

> _StateT_local_bind_law_lifted
>   :: forall s t m a b
>    . ( MonadReadOnly t m )
>   => (t -> t) -> StateT s m a -> (a -> StateT s m b)
>   -> [StateT s m b]
> _StateT_local_bind_law_lifted u x f =
> 
>   [ local u (x >>= f)
> {- TODO
>   -- (expand local)
> 
>   , StateT $ \r ->
>       local u (unStateT (x >>= f) r)
> 
>   -- (expand >>=)
> 
>   , StateT $ \r ->
>       local u (unStateT (StateT $ \r1 -> unStateT x r1
>         >>= (\a -> unStateT (f a) r1)) r)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \r ->
>       local u ((\r1 -> unStateT x r1
>         >>= (\a -> unStateT (f a) r1)) r)
> 
>   -- (β-reduction)
> 
>   , StateT $ \r ->
>       local u (unStateT x r
>         >>= (\a -> unStateT (f a) r))
> 
>   -- (local bind law in m)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (local u . (\a -> unStateT (f a) r))
> 
>   -- (α-conversion)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (local u . (\c -> unStateT (f c) r))
> 
>   -- (η-expansion)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> (local u . (\c -> unStateT (f c) r)) a)
> 
>   -- (expand .)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> local u ((\c -> unStateT (f c) r) a))
> 
>   -- (β-reduction)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> local u (unStateT (f a) r))
> 
>   -- (β-expansion)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> (\r1 -> local u (unStateT (f a) r1)) r)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> unStateT (StateT $ \r1 -> local u (unStateT (f a) r1)) r)
> 
>   -- (condense local)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> unStateT (local u (f a)) r)
> 
>   -- (condense .)
> 
>   , StateT $ \r ->
>       local u (unStateT x r)
>         >>= (\a -> unStateT ((local u . f) a) r)
> 
>   -- (β-expansion)
> 
>   , StateT $ \r ->
>       (\r1 -> local u (unStateT x r1)) r
>         >>= (\a -> unStateT ((local u . f) a) r)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \r ->
>       unStateT (StateT $ \r1 -> local u (unStateT x r1)) r
>         >>= (\a -> unStateT ((local u . f) a) r)
> 
>   -- (condense local)
> 
>   , StateT $ \r ->
>       unStateT (local u x) r
>         >>= (\a -> unStateT ((local u . f) a) r)
> 
>   -- (condense >>=)
> -}
>   , local u x >>= (local u . f)
>   ]

:::::::::::



MonadStream Laws
----------------

[@statet-monad-stream-lawful]()

There is a `MonadStream` instance for `StateT s m` that we need to verify. Recall the laws for [`MonadStream`](@monad-stream):

Next/Rest Law
  : `(,) <$> next <*> rest` is equivalent to `advance <$> rest <* next`

Rest/Rest Law
  : `rest >>= (\u -> rest >>= k u)` is equivalent to `rest >>= (\u -> k u u)`

Fiddle/Id Law
  : `fiddle id` is equivalent to `return ()`

Fiddle/Compose Law
  : `fiddle u >> fiddle v` is equivalent to `fiddle (v . u)`

Fiddle/Rest Law
  : `fiddle u >> rest` is equivalent to `rest >>= (\a -> fiddle u >> return (u a))`

Here we show that if `m` is a lawful instance of `MonadStream` with stream type `t`, then so is `StateT s m`.

First: `StateT s m` satisfies the next/rest law:

::: example

> _StateT_next_rest_law_lifted
>   :: forall s t m
>    . (IsStream t, MonadStream t m)
>   => [StateT s m (StreamValue t, t)]
> _StateT_next_rest_law_lifted =
> 
>   [ (advance <$> rest) <* (next @t)
> 
>   -- (expand <*)
> 
>   , const <$> (advance <$> rest) <*> (next @t)
> 
>   -- (expand <$>)
> 
>   , (fmap const (advance <$> rest)) <*> (next @t)
> 
>   -- (expand <$>)
> 
>   , (fmap const (fmap advance rest)) <*> (next @t)
> 
>   -- (composite law for StateT s m)
> 
>   , (fmap (const . advance) rest) <*> (next @t)
> 
>   -- (expand <*>)
> 
>   , (fmap (const . advance) rest)
>       >>= (\f -> (next @t) >>= (\a -> return (f a)))
> 
>   -- (expand fmap)
> 
>   , (rest >>= (return . const . advance))
>       >>= (\f -> (next @t) >>= (\a -> return (f a)))
> 
>   -- (associative law in StateT s m)
> 
>   , rest >>= (\c -> (return . const . advance) c
>       >>= (\f -> (next @t) >>= (\a -> return (f a))))
> 
>   -- (expand .)
> 
>   , rest >>= (\c -> return (const (advance c))
>       >>= (\f -> (next @t) >>= (\a -> return (f a))))
> 
>   -- (left identity law in StateT s m)
> 
>   , rest >>= (\c ->
>       (\f -> (next @t) >>= (\a -> return (f a))) (const (advance c)))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       (next @t) >>= (\a -> return (const (advance c) a)))
> 
>   -- (expand const)
> 
>   , rest >>= (\c ->
>       (next @t) >>= (\a -> return (advance c)))
> 
>   -- (expand >>=)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (next @t) s2
>           >>= (\(b, s1) -> unStateT ((\a -> return (advance c)) b) s1))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (next @t) s2
>           >>= (\(b, s1) -> unStateT (return (advance c)) s1))
> 
>   -- (expand return)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (next @t) s2
>           >>= (\(b, s1) -> unStateT (StateT $ \s3 -> return (advance c, s3)) s1))
> 
>   -- (unStateT . StateT == id)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (next @t) s2
>           >>= (\(b, s1) -> (\s3 -> return (advance c, s3)) s1))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (next @t) s2
>           >>= (\(b, s1) -> return (advance c, s1)))
> 
>   -- (expand next)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (lift (next @t)) s2
>           >>= (\(b, s1) -> return (advance c, s1)))
> 
>   -- (expand lift)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         unStateT (StateT $ \s3 -> next @t >>= (\a -> return (a, s3))) s2
>           >>= (\(b, s1) -> return (advance c, s1)))
> 
>   -- (unStateT . StateT == id)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         (\s3 -> next @t >>= (\a -> return (a, s3))) s2
>           >>= (\(b, s1) -> return (advance c, s1)))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\a -> return (a, s2))
>           >>= (\(b, s1) -> return (advance c, s1)))
> 
>   -- (associativity law in m)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\d -> (\a -> return (a, s2)) d
>           >>= (\(b, s1) -> return (advance c, s1))))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\d -> return (d, s2)
>           >>= (\(b, s1) -> return (advance c, s1))))
> 
>   -- (left identity law in m)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\d ->
>           (\(b, s1) -> return (advance c, s1)) (d, s2)))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\d ->
>           return (advance c, s2)))
> 
>   -- (α-conversion)
> 
>   , rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\_ ->
>           return (advance c, s2)))
> 
>   -- (expand rest)
> 
>   , lift rest >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\_ ->
>           return (advance c, s2)))
> 
>   -- (expand lift)
> 
>   , (StateT $ \s -> rest >>= (\a -> return (a, s))) >>= (\c ->
>       StateT $ \s2 ->
>         next @t >>= (\_ ->
>           return (advance c, s2)))
>   ]

:::::::::::

Second: `StateT s m` satisfies the rest/rest law with stream type `t`:

::: example

> _StateT_rest_rest_law_lifted
>   :: forall s t m a
>    . ( MonadStream t m, IsStream t )
>   => (t -> t -> StateT s m a)
>   -> [StateT s m a]
> _StateT_rest_rest_law_lifted k =
> 
>   [ rest >>= (\u -> rest >>= k u)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       unStateT rest s1
>         >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2)
> 
>   -- (expand rest)
> 
>   , StateT $ \s1 ->
>       unStateT (lift rest) s1
>         >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2)
> 
>   -- (expand lift)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       (\s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       (rest >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2)
> 
>   -- (associative law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\(a, s2) -> unStateT ((\u -> rest >>= k u) a) s2) (c, s1))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT ((\u -> rest >>= k u) c) s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT (rest >>= k c) s1)
> 
>   -- (expand >>=)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT (StateT $ \s -> unStateT rest s
>           >>= (\(a, s2) -> unStateT (k c a) s2)) s1)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\s -> unStateT rest s >>= (\(a, s2) -> unStateT (k c a) s2)) s1)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT rest s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (expand rest)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT (lift rest) s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (expand lift)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT (StateT $ \s -> rest
>           >>= (\b -> return (b, s))) s1 >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\s -> rest >>= (\b -> return (b, s))) s1
>           >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>= (\b -> return (b, s1))
>           >>= (\(a, s2) -> unStateT (k c a) s2))
> 
>   -- (associative law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\b -> return (b, s1)) d >>= (\(a, s2) -> unStateT (k c a) s2)))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> return (d, s1) >>= (\(a, s2) -> unStateT (k c a) s2)))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\(a, s2) -> unStateT (k c a) s2) (d, s1)))
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> unStateT (k c d) s1))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\u v -> unStateT (k u v) s1) c d))
> 
>   -- (η-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> rest >>= (\u v -> unStateT (k u v) s1) c)
> 
>   -- (rest/rest law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\u v -> unStateT (k u v) s1) c c)
> 
>   -- (β-reduction)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT (k c c) s1)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> unStateT ((\u -> k u u) c) s1)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c ->
>           (\(a, s2) -> unStateT ((\u -> k u u) a) s2) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2))
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       rest >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2))
> 
>   -- (associativity law in m)
> 
>   , StateT $ \s1 ->
>       (rest >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StateT $ \s1 ->
>       (\s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (unStateT . StateT == id)
> 
>   , StateT $ \s1 ->
>       unStateT (StateT $ \s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense lift)
> 
>   , StateT $ \s1 ->
>       unStateT (lift rest) s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense rest)
> 
>   , StateT $ \s1 ->
>       unStateT rest s1
>         >>= (\(a, s2) -> unStateT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , rest >>= (\u -> k u u)
>   ]

:::::::::::

Third: `StateT s m` satisfies the fiddle/id law with stream type `t`:

::: example

> _StateT_fiddle_id_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t )
>   => [StateT s m ()]
> _StateT_fiddle_id_law_lifted =
> 
>   [ fiddle (id @t)
> 
>   -- (expand fiddle)
> 
>   , lift (fiddle (id @t))
> 
>   -- (fiddle/id law in m)
> 
>   , (lift (return ()))
> 
>   -- (unit law)
> 
>   , return ()
>   ]

:::::::::::

Fourth: `StateT s m` satisfies the fiddle/compose law with stream type `t`:

::: example

> _StateT_fiddle_compose_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t )
>   => (t -> t) -> (t -> t)
>   -> [StateT s m ()]
> _StateT_fiddle_compose_law_lifted u v =
> 
>   [ fiddle u >> fiddle v
> 
>   -- (expand fiddle)
> 
>   , lift (fiddle u) >> lift (fiddle v)
> 
>   -- (expand >>)
> 
>   , lift (fiddle u) >>= (\_ -> lift (fiddle v))
> 
>   -- (condense const)
> 
>   , lift (fiddle u) >>= const (lift (fiddle v))
> 
>   -- (const compose)
> 
>   , lift (fiddle u) >>= (lift . const (fiddle v))
> 
>   -- (expand const)
> 
>   , lift (fiddle u) >>= (lift . (\_ -> fiddle v))
> 
>   -- (bind law)
> 
>   , lift (fiddle u >>= (\_ -> fiddle v))
> 
>   -- (condense >>)
> 
>   , lift (fiddle u >> fiddle v)
> 
>   -- (fiddle/compose law in m)
> 
>   , lift (fiddle (v . u))
> 
>   -- (condense fiddle)
> 
>   , fiddle (v . u)
>   ]

:::::::::::

Fifth: `StateT s m` satisfies the fiddle/rest law:

::: example

> _StateT_fiddle_rest_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t )
>   => (t -> t)
>   -> [StateT s m t]
> _StateT_fiddle_rest_law_lifted u =
> 
>   [ fiddle u >> rest
> 
>   -- (expand fiddle)
> 
>   , lift (fiddle u) >> rest
> 
>   -- (expand rest)
> 
>   , lift (fiddle u) >> lift rest
> 
>   -- (lift/>>)
> 
>   , lift (fiddle u >> rest)
> 
>   -- (fiddle/rest law in m)
> 
>   , lift (rest >>= (\a -> fiddle u >> return (u a)))
> 
>   -- (bind law)
> 
>   , lift rest >>= lift . (\a -> fiddle u >> return (u a))
> 
>   -- (compose lambda)
> 
>   , lift rest >>= (\a -> lift (fiddle u >> return (u a)))
> 
>   -- (lift/>>)
> 
>   , lift rest >>= (\a -> lift (fiddle u) >> lift (return (u a)))
> 
>   -- (lift/unit law)
> 
>   , lift rest >>= (\a -> lift (fiddle u) >> return (u a))
> 
>   -- (condense rest)
> 
>   , rest >>= (\a -> lift (fiddle u) >> return (u a))
> 
>   -- (condense fiddle)
> 
>   , rest >>= (\a -> fiddle u >> return (u a))
>   ]

:::::::::::
