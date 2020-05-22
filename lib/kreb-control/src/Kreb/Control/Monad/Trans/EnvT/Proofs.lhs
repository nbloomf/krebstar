---
title: Lawfulness Proofs for EnvT
author: nbloomf
---

::: frontmatter

> module Kreb.Control.Monad.Trans.EnvT.Proofs where
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Trans.EnvT

:::



Introduction
------------

In this section we give lawfulness proofs for the various typeclass instances of `EnvT`. A nice side effect (!) of referential transparency is that we can write _equational proofs_ that program transformations are semantics-preserving, and that's what we're doing here. These can be safely ignored if all you want to know is how the instances are defined.



Monad Laws
----------

[@envt-monad-lawful]()

Recall the three _monad laws_:

Left Identity Law
  : `return a >>= f` is equivalent to `f a`

Right Identity Law
  : `x >>= return` is equivalent to `x`

Associative Law
  : `(x >>= f) >>= g` is equivalent to `x >>= (\a -> f a >>= g)`

Here we show that if `m` is a monad, then `EnvT s m` is also a monad. To that end suppose `m` is a monad.

First: `EnvT s m` satisfies the left identity law.

::: example

> _EnvT_left_identity_law
>   :: forall r m a b
>    . ( Monad m )
>   => a -> (a -> EnvT r m b)
>   -> [EnvT r m b]
> _EnvT_left_identity_law a f =
> 
>   [ return a >>= f
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (return a) r >>= (\c -> unEnvT (f c) r)
> 
>   -- (expand return)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> return a) r
>         >>= (\c -> unEnvT (f c) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\_ -> return a) r
>         >>= (\c -> unEnvT (f c) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       return a
>         >>= (\c -> unEnvT (f c) r)
> 
>   -- (left identity law in m)
> 
>   , EnvT $ \r ->
>       (\c -> unEnvT (f c) r) a
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT (f a) r
> 
>   -- (η-reduction)
> 
>   , EnvT $ unEnvT (f a)
> 
>   -- (EnvT . unEnvT == id)
> 
>   , f a
>   ]

:::::::::::

Second: `EnvT s m` satisfies the right identity law.

::: example

> _EnvT_right_identity_law
>   :: forall s m a
>    . ( Monad m )
>   => (EnvT s m a)
>   -> [EnvT s m a]
> _EnvT_right_identity_law x =
> 
>   [ x >>= return
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\a -> unEnvT (return a) r)
> 
>   -- (expand return)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\a -> unEnvT (EnvT $ \_ -> return a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\a -> (\_ -> return a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\a -> return a)
> 
>   -- (η-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x r >>= return
> 
>   -- (right identity law in m)
> 
>   , EnvT $ \r ->
>       unEnvT x r
> 
>   -- (η-reduction)
> 
>   , EnvT (unEnvT x)
> 
>   -- (EnvT . unEnvT == id)
> 
>   , x
>   ]

:::::::::::

Third: `EnvT s m` satisfies the associative law.

::: example

> _EnvT_associativity_law
>   :: forall s m a b c
>    . ( Monad m )
>   => EnvT s m a -> (a -> EnvT s m b) -> (b -> EnvT s m c)
>   -> [EnvT s m c]
> _EnvT_associativity_law x f g =
> 
>   [ (x >>= f) >>= g
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (x >>= f) r
>         >>= (\a -> unEnvT (g a) r)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> unEnvT x r2 >>= (\b -> unEnvT (f b) r2)) r
>         >>= (\a -> unEnvT (g a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r2 -> unEnvT x r2 >>= (\b -> unEnvT (f b) r2)) r
>         >>= (\a -> unEnvT (g a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       (unEnvT x r >>= (\b -> unEnvT (f b) r))
>         >>= (\a -> unEnvT (g a) r)
> 
>   -- (associativity law in m)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> (\b -> unEnvT (f b) r) c
>           >>= (\a -> unEnvT (g a) r))
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> unEnvT (f c) r
>           >>= (\a -> unEnvT (g a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> (\r2 -> unEnvT (f c) r2
>           >>= (\a -> unEnvT (g a) r2)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> unEnvT (EnvT $ \r2
>           -> unEnvT (f c) r2 >>= (\a -> unEnvT (g a) r2)) r)
> 
>   -- (condense >>=)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> unEnvT (f c >>= g) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT x r
>         >>= (\c -> unEnvT ((\a -> f a >>= g) c) r)
> 
>   -- (condense >>=)
> 
>   , x >>= (\a -> f a >>= g)
>   ]

:::::::::::



Monad Transformer Laws
----------------------

[@envt-monad-trans-lawful]()

Recall the two _monad transformer laws_:

Unit Law
  : `lift . return` is equivalent to `return`

Bind Law
  : `lift (x >>= f)` is equivalent to `lift x >>= (lift . f)`

Here we show that `EnvT s` is a monad transformer. First: `EnvT s` satisfies the unit law.

::: example

> _EnvT_unit_law
>   :: forall r m a
>    . ( Monad m )
>   => a
>   -> [EnvT r m a]
> _EnvT_unit_law a =
> 
>   [ lift (return a)
> 
>   -- (expand lift)
> 
>   , EnvT $ \_ -> return a
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Second: `EnvT s` satisfies the bind law.

::: example

> _EnvT_bind_law
>   :: forall r m a b
>    . ( Monad m )
>   => m a -> (a -> m b)
>   -> [EnvT r m b]
> _EnvT_bind_law x f =
> 
>   [ lift (x >>= f)
> 
>   -- (expand lift)
> 
>   , EnvT $ \_ ->
>       x >>= f
> 
>   -- (η-expansion)
> 
>   , EnvT $ \r ->
>       x >>= (\a -> f a)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       x
>         >>= (\a -> (\_ -> f a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       x
>         >>= (\a -> unEnvT (EnvT $ \_ -> f a) r)
> 
>   -- (condense lift)
> 
>   , EnvT $ \r ->
>       x
>         >>= (\a -> unEnvT (lift (f a)) r)
> 
>   -- (definition of .)
> 
>   , EnvT $ \r ->
>       x
>         >>= (\a -> unEnvT ((lift . f) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\_ -> x) r
>         >>= (\a -> unEnvT ((lift . f) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> x) r
>         >>= (\a -> unEnvT ((lift . f) a) r)
> 
>   -- (condense lift)
> 
>   , EnvT $ \r ->
>       unEnvT (lift x) r
>         >>= (\a -> unEnvT ((lift . f) a) r)
> 
>   -- (condense >>=)
> 
>   , lift x >>= (lift . f)
>   ]

:::::::::::



MonadReadOnly Laws
------------------

[@envt-monad-read-only-lawful]()

There are two different `MonadReadOnly` instances for `EnvT r m` that we need to verify. Recall the laws for [`MonadReadOnly`](@monad-read-only):

Local/Ask Law
  : `local u ask` is equivalent to `fmap u ask`

Local/Local Law
  : `local u (local v x)` is equivalent to `local (v . u) x`

Interchange Law
  : `local u x >> ask` is equivalent to `ask >>= (\r -> local u x >> return r)`
  : `local u x >>= (\a -> ask >>= (\b -> f a b))` is equivalent to `ask >>= (\b -> local u x >>= (\a -> f a b))`

Local Unit Law
  : `local u (return a)` is equivalent to `return a`

Local Bind Law
  : `local u (x >>= f)` is equivalent to `local u x >>= (local u . f)`

Here we show that if `m` is a monad then `EnvT r m` satisfies the MonadReadOnly laws with environment type `r`.

First: `EnvT r m` satisfies the local/ask law:

::: example

> _EnvT_local_ask_law_natural
>   :: forall r m
>    . ( Monad m )
>   => (r -> r)
>   -> [EnvT r m r]
> _EnvT_local_ask_law_natural u =
> 
>   [ local u ask
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT ask (u r)
> 
>   -- (expand ask)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> return r2) (u r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r2 -> return r2) (u r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       return (u r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\_ -> return (u r)) r
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> return (u r)) r
> 
>   -- (condense return)
> 
>   , EnvT $ \r ->
>       unEnvT (return (u r)) r
> 
>   -- (condense .)
> 
>   , EnvT $ \r ->
>       unEnvT ((return . u) r) r
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\a -> unEnvT ((return . u) a) r) r
> 
>   -- (left identity law in m)
> 
>   , EnvT $ \r ->
>       return r
>         >>= (\a -> unEnvT ((return . u) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\r2 -> return r2) r
>         >>= (\a -> unEnvT ((return . u) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> return r2) r
>         >>= (\a -> unEnvT ((return . u) a) r)
> 
>   -- (condense ask)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\a -> unEnvT ((return . u) a) r)
> 
>   -- (condense >>=)
> 
>   , ask >>= (return . u)
> 
>   -- (condense fmap)
> 
>   , fmap u ask
>   ]

:::::::::::

Second: `EnvT r m` satisfies the local/local law:

::: example

> _EnvT_local_local_law_natural
>   :: forall r m a
>    . ( Monad m )
>   => (r -> r) -> (r -> r) -> EnvT r m a
>   -> [EnvT r m a]
> _EnvT_local_local_law_natural u v x =
> 
>   [ local u (local v x)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (local v x) (u r)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> unEnvT x (v r2)) (u r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r2 -> unEnvT x (v r2)) (u r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (v (u r))
> 
>   -- (condense .)
> 
>   , EnvT $ \r ->
>       unEnvT x ((v . u) r)
> 
>   -- (condense local)
> 
>   , local (v . u) x
>   ]

:::::::::::

Third: `EnvT r m` satisfies the interchange law:

::: example

> _EnvT_interchange_law_natural
>   :: forall r m a b
>    . ( Monad m )
>   => (r -> r) -> EnvT r m a -> (a -> r -> EnvT r m b)
>   -> [EnvT r m b]
> _EnvT_interchange_law_natural u x f =
> 
>   [ local u x >>= (\a -> ask >>= (\b -> f a b))
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (local u x) r
>         >>= (\c -> unEnvT ((\a -> ask >>= (\b -> f a b)) c) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT (local u x) r
>         >>= (\c -> unEnvT (ask >>= (\b -> f c b)) r)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> unEnvT x (u r2)) r
>         >>= (\c -> unEnvT (ask >>= (\b -> f c b)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r2 -> unEnvT x (u r2)) r
>         >>= (\c -> unEnvT (ask >>= (\b -> f c b)) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\c -> unEnvT (ask >>= (\b -> f c b)) r)
> {-
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> unEnvT ask r)
> 
>   -- (expand ask)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> unEnvT (EnvT $ \r2 -> return r2) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> (\r2 -> return r2) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> return r)
> 
>   -- (α-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\c -> return r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\a -> unEnvT x (u r)
>           >>= (\c -> return a)) r
> 
>   -- (left identity law for r)
> 
>   , EnvT $ \r ->
>       return r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> return a))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\r2 -> return r2) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> return a))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> return r2) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> return a))
> 
>   -- (condense ask)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> return a))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> (\_ -> return a) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> unEnvT (EnvT $ \_ -> return a) r))
> 
>   -- (condense return)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT x (u r)
>           >>= (\c -> unEnvT (return a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> (\r2 -> unEnvT x (u r2)) r
>           >>= (\c -> unEnvT (return a) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (EnvT $ \r2 -> unEnvT x (u r2)) r
>           >>= (\c -> unEnvT (return a) r))
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (local u x) r
>           >>= (\c -> unEnvT (return a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> (\r2 -> unEnvT (local u x) r2
>           >>= (\c -> unEnvT (return a) r2)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (EnvT $ \r2 -> unEnvT (local u x) r2
>           >>= (\c -> unEnvT (return a) r2)) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (EnvT $ \r2 -> unEnvT (local u x) r2
>           >>= (\c -> unEnvT ((\_ -> return a) c) r2)) r)
> 
>   -- (condense >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (local u x >>= (\_ -> return a)) r)
> 
>   -- (condense >>)
> 
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\a -> unEnvT (local u x >> return a) r)
> 
>   -- (β-expansion)
> -}
>   , EnvT $ \r ->
>       unEnvT (ask @r) r
>         >>= (\c -> unEnvT ((\b -> local u x >>= (\a -> f a b)) c) r)
> 
>   -- (condense >>=)
> 
>   , ask >>= (\b -> local u x >>= (\a -> f a b))
>   ]

:::::::::::

Fourth: `EnvT r m` satisfies the local unit law.

::: example

> _EnvT_local_unit_law_natural
>   :: forall r m a
>    . ( Monad m )
>   => (r -> r) -> a
>   -> [EnvT r m a]
> _EnvT_local_unit_law_natural u a =
> 
>   [ local u (return a)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (return a) (u r)
> 
>   -- (expand return)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> return a) (u r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\_ -> return a) (u r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       return a
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Fifth: `EnvT r m` satisfies the local bind law.

::: example

> _EnvT_local_bind_law_natural
>   :: forall r m a b
>    . ( Monad m )
>   => (r -> r) -> EnvT r m a -> (a -> EnvT r m b)
>   -> [EnvT r m b]
> _EnvT_local_bind_law_natural u x f =
> 
>   [ local u (x >>= f)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (x >>= f) (u r)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> unEnvT x r2
>         >>= (\a -> unEnvT (f a) r2)) (u r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r2 -> unEnvT x r2 >>= (\a -> unEnvT (f a) r2)) (u r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r) >>= (\a -> unEnvT (f a) (u r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> (\r2 -> unEnvT (f a) (u r2)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> unEnvT (EnvT $ \r2 -> unEnvT (f a) (u r2)) r)
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> unEnvT (local u (f a)) r)
> 
>   -- (condense .)
> 
>   , EnvT $ \r ->
>       unEnvT x (u r)
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\r2 -> unEnvT x (u r2)) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r2 -> unEnvT x (u r2)) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       unEnvT (local u x) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (condense >>=)
> 
>   , local u x >>= (local u . f)
>   ]

:::::::::::

Next we show that if `m` is a lawful instance of `MonadReadOnly` with environment type `t` different from `r`, then so is `EnvT r m`.

First: `EnvT r m` satisfies the local/ask law:

::: example

> _EnvT_local_ask_law_lifted
>   :: forall r t m
>    . ( MonadReadOnly t m, Compare r t ~ NotEqual )
>   => (t -> t)
>   -> [EnvT r m t]
> _EnvT_local_ask_law_lifted u =
> 
>   [ local u ask
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT ask r)
> 
>   -- (expand ask)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (lift ask) r)
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (EnvT $ \_ -> ask) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u ((\_ -> ask) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u ask
> 
>   -- (local/ask law in m)
> 
>   , EnvT $ \r ->
>       fmap u ask
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

Second: `EnvT r m` satisfies the local/local law.

::: example

> _EnvT_local_local_law_lifted
>   :: forall r t m a
>    . ( MonadReadOnly t m, Compare r t ~ NotEqual )
>   => (t -> t) -> (t -> t) -> EnvT r m a
>   -> [EnvT r m a]
> _EnvT_local_local_law_lifted u v x =
> 
>   [ local u (local v x)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (local v x) r)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (EnvT $ \r1 -> local v (unEnvT x r1)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u ((\r1 -> local v (unEnvT x r1)) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (local v (unEnvT x r))
> 
>   -- (local/local law in m)
> 
>   , EnvT $ \r ->
>       local (v . u) (unEnvT x r)
> 
>   -- (condense local)
> 
>   , local (v . u) x
>   ]

:::::::::::

Third: `EnvT r m` satisfies the interchange law.

::: example

> _EnvT_interchange_law_lifted
>   :: forall r t m a
>    . ( MonadReadOnly t m, Compare r t ~ NotEqual )
>   => (t -> t) -> EnvT r m a
>   -> [EnvT r m t]
> _EnvT_interchange_law_lifted u x =
> 
>   [ local u x >> ask
> 
>   -- (expand >>)
> 
>   , local u x >>= (\_ -> ask)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT (local u x) r
>         >>= (\a -> unEnvT ((\_ -> ask) a) r)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r1 -> local u (unEnvT x r1)) r
>         >>= (\a -> unEnvT ((\_ -> ask) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\r1 -> local u (unEnvT x r1)) r
>         >>= (\a -> unEnvT ((\_ -> ask) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT ((\_ -> ask) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT ask r)
> 
>   -- (expand ask)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT (lift ask) r)
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT (EnvT $ \_ -> ask) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> (\_ -> ask) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> ask)
> 
>   -- (α-equivalence)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\_ -> ask)
> 
>   -- (condense >>)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r) >> ask
> 
>   -- (interchange law in m)
> 
>   , EnvT $ \r ->
>       ask >>= (\c -> local u (unEnvT x r) >> return c)
> 
>   -- (expand >>)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           local u (unEnvT x r)
>             >>= (\_ -> return c))
> 
>   -- (α-equivalence)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           local u (unEnvT x r)
>             >>= (\a -> return c))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           local u (unEnvT x r)
>             >>= (\a -> (\_ -> return c) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           local u (unEnvT x r)
>             >>= (\a -> unEnvT (EnvT $ \_ -> return c) r))
> 
>   -- (condense return)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           local u (unEnvT x r)
>             >>= (\a -> unEnvT (return c) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           (\r1 -> local u (unEnvT x r1)) r
>             >>= (\a -> unEnvT (return c) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           unEnvT (EnvT $ \r1 -> local u (unEnvT x r1)) r
>             >>= (\a -> unEnvT (return c) r))
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT (return c) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       ask
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT ((\_ -> return c) a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\_ -> ask) r
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT ((\_ -> return c) a) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> ask) r
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT ((\_ -> return c) a) r))
> 
>   -- (condense lift)
> 
>   , EnvT $ \r ->
>       unEnvT (lift ask) r
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT ((\_ -> return c) a) r))
> 
>   -- (condense ask)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c ->
>           unEnvT (local u x) r
>             >>= (\a -> unEnvT ((\_ -> return c) a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c -> (\r1 ->
>           unEnvT (local u x) r1
>             >>= (\a -> unEnvT ((\_ -> return c) a) r1)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c -> unEnvT (EnvT $ \r1 ->
>           unEnvT (local u x) r1
>             >>= (\a -> unEnvT ((\_ -> return c) a) r1)) r)
> 
>   -- (condense >>=)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c -> unEnvT (local u x >>= (\_ -> return c)) r)
> 
>   -- (condense >>)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c -> unEnvT (local u x >> return c) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       unEnvT ask r
>         >>= (\c -> unEnvT ((\a -> local u x >> return a) c) r)
> 
>   -- (condense >>=)
> 
>   , ask >>= (\a -> local u x >> return a)
>   ]

:::::::::::

Fourth: `EnvT r m` satisfies the local unit law.

::: example

> _EnvT_local_unit_law_lifted
>   :: forall r t m a
>    . ( MonadReadOnly t m, Compare r t ~ NotEqual )
>   => (t -> t) -> a
>   -> [EnvT r m a]
> _EnvT_local_unit_law_lifted u a =
> 
>   [ local u (return a)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (return a) r)
> 
>   -- (expand return)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (EnvT $ \_ -> return a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u ((\_ -> return a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (return a)
> 
>   -- (local unit law in m)
> 
>   , EnvT $ \r ->
>       return a
> 
>   -- (α-equivalence)
> 
>   , EnvT $ \_ ->
>       return a
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Fifth: `EnvT r m` satisfies the local bind law.

::: example

> _EnvT_local_bind_law_lifted
>   :: forall r t m a b
>    . ( MonadReadOnly t m, Compare r t ~ NotEqual )
>   => (t -> t) -> EnvT r m a -> (a -> EnvT r m b)
>   -> [EnvT r m b]
> _EnvT_local_bind_law_lifted u x f =
> 
>   [ local u (x >>= f)
> 
>   -- (expand local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (x >>= f) r)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       local u (unEnvT (EnvT $ \r1 -> unEnvT x r1
>         >>= (\a -> unEnvT (f a) r1)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u ((\r1 -> unEnvT x r1
>         >>= (\a -> unEnvT (f a) r1)) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r
>         >>= (\a -> unEnvT (f a) r))
> 
>   -- (local bind law in m)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (local u . (\a -> unEnvT (f a) r))
> 
>   -- (α-conversion)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (local u . (\c -> unEnvT (f c) r))
> 
>   -- (η-expansion)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> (local u . (\c -> unEnvT (f c) r)) a)
> 
>   -- (expand .)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> local u ((\c -> unEnvT (f c) r) a))
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> local u (unEnvT (f a) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> (\r1 -> local u (unEnvT (f a) r1)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT (EnvT $ \r1 -> local u (unEnvT (f a) r1)) r)
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT (local u (f a)) r)
> 
>   -- (condense .)
> 
>   , EnvT $ \r ->
>       local u (unEnvT x r)
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\r1 -> local u (unEnvT x r1)) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \r1 -> local u (unEnvT x r1)) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (condense local)
> 
>   , EnvT $ \r ->
>       unEnvT (local u x) r
>         >>= (\a -> unEnvT ((local u . f) a) r)
> 
>   -- (condense >>=)
> 
>   , local u x >>= (local u . f)
>   ]

:::::::::::



MonadReadWrite Laws
-------------------

[@envt-monad-read-write-lawful]()

There is a `MonadReadWrite` instance for `EnvT r m` that we need to verify. Recall the laws for [`MonadReadWrite`](@monad-read-write):

Put/Put Law
  : `put u1 >> put u2` is equivalent to `put u2`

Put/Get Law
  : `put u >> get` is equivalent to `put u >> return u`

Get/Put Law
  : `get >>= put` is equivalent to `return ()`

Get/Get Law
  : `get >>= (\u -> get >>= k u)` is equivalent to `get >>= (\u -> k u u)`

Here we show that if `m` is an instance of `MonadReadWrite` with state `s`, then so is `EnvT r m`.

First: `EnvT s m` satisfies the put/put law:

::: example

> _EnvT_put_put_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m )
>   => t -> t
>   -> [EnvT s m ()]
> _EnvT_put_put_law_lifted u1 u2 =
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

Second: `EnvT s m` satisfies the put/get law:

::: example

> _EnvT_put_get_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m )
>   => t
>   -> [EnvT s m t]
> _EnvT_put_get_law_lifted u =
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

Third: `EnvT s m` satisfies the get/put law.

::: example

> _EnvT_get_put_law_lifted
>   :: forall s t m
>    . ( MonadReadWrite t m )
>   => [EnvT s m ()]
> _EnvT_get_put_law_lifted =
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

Fourth: `EnvT s m` satisfies the get/get law.

::: example

> _EnvT_get_get_law_lifted
>   :: forall s t m a
>    . ( MonadReadWrite t m )
>   => (t -> t -> EnvT s m a)
>   -> [EnvT s m a]
> _EnvT_get_get_law_lifted k =
> 
>   [ get >>= (\u -> get >>= k u)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT get r
>         >>= (\a -> unEnvT ((\u -> get >>= k u) a) r)
> 
>   -- (expand get)
> 
>   , EnvT $ \r ->
>       unEnvT (lift get) r
>         >>= (\a -> unEnvT ((\u -> get >>= k u) a) r)
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> get) r
>         >>= (\a -> unEnvT ((\u -> get >>= k u) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\_ -> get) r
>         >>= (\a -> unEnvT ((\u -> get >>= k u) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       get
>         >>= (\a -> unEnvT ((\u -> get >>= k u) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       get
>         >>= (\a -> unEnvT (get >>= k a) r)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT (EnvT $ \s -> unEnvT get s
>           >>= (\c -> unEnvT (k a c) r)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> (\s -> unEnvT get s >>= (\c -> unEnvT (k a c) r)) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT get r >>= (\c -> unEnvT (k a c) r))
> 
>   -- (expand get)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT (lift get) r >>= (\c -> unEnvT (k a c) r))
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT (EnvT $ \_ -> get) r
>           >>= (\c -> unEnvT (k a c) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> (\_ -> get) r
>           >>= (\c -> unEnvT (k a c) r))
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> get >>=
>           (\c -> unEnvT (k a c) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> get >>=
>           (\c -> (\u v -> unEnvT (k u v) r) a c))
> 
>   -- (η-reduction)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> get >>= (\u v -> unEnvT (k u v) r) a)
> 
>   -- (get/get law in m)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> (\u v -> unEnvT (k u v) r) a a)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT (k a a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       get >>=
>         (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\_ -> get) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> get) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense lift)
> 
>   , EnvT $ \r ->
>       unEnvT (lift get) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense get)
> 
>   , EnvT $ \r ->
>       unEnvT get r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense >>=)
> 
>   , get >>= (\u -> k u u)
>   ]

:::::::::::



MonadStream Laws
-------------------

[@envt-monad-stream-lawful]()

There is a `MonadStream` instance for `EnvT r m` that we need to verify. Recall the laws for [`MonadStream`](@monad-stream):

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

Here we show that if `m` is a lawful instance of `MonadStream` with stream type `t` different from `s`, then so is `EnvT r m`.

First: `EnvT r m` satisfies the next/rest law:

::: example

> _EnvT_next_rest_law_lifted
>   :: forall r t m
>    . ( IsStream t, MonadStream t m )
>   => [EnvT r m (StreamValue t, t)]
> _EnvT_next_rest_law_lifted =
> 
>   [ (advance <$> rest) <* (next @t)
> 
>   -- (expand rest)
> 
>   , (advance <$> lift rest) <* (next @t)
> 
>   -- (expand next)
> 
>   , (advance <$> lift rest) <* lift (next @t)
> 
>   -- (lift/<$>)
> 
>   , lift (advance <$> rest) <* lift (next @t)
> 
>   -- (lift/<*)
> 
>   , lift ((advance <$> rest) <* (next @t))
> 
>   -- (next/rest law in m)
> 
>   , lift ((,) <$> next <*> rest)
> 
>   -- (lift/<*>)
> 
>   , lift ((,) <$> next) <*> lift rest
> 
>   -- (lift/<$>)
> 
>   , (,) <$> lift next <*> lift rest
> 
>   -- (condense next)
> 
>   , (,) <$> next <*> lift rest
> 
>   -- (condense rest)
> 
>   , (,) <$> next <*> rest
>   ]

:::::::::::

Second: `EnvT r m` satisfies the rest/rest law with stream type `t`:

::: example

> _EnvT_rest_rest_law_lifted
>   :: forall r t m a
>    . ( MonadStream t m, IsStream t )
>   => (t -> t -> EnvT r m a)
>   -> [EnvT r m a]
> _EnvT_rest_rest_law_lifted k =
> 
>   [ rest >>= (\u -> rest >>= k u)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       unEnvT rest r
>         >>= (\a -> unEnvT ((\u -> rest >>= k u) a) r)
> 
>   -- (expand rest)
> 
>   , EnvT $ \r ->
>       unEnvT (lift rest) r
>         >>= (\a -> unEnvT ((\u -> rest >>= k u) a) r)
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> rest) r
>         >>= (\a -> unEnvT ((\u -> rest >>= k u) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       (\_ -> rest) r
>         >>= (\a -> unEnvT ((\u -> rest >>= k u) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       rest
>         >>= (\a -> unEnvT ((\u -> rest >>= k u) a) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> unEnvT (rest >>= k a) r)
> 
>   -- (expand >>=)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\c -> unEnvT (EnvT $ \r1 -> unEnvT rest r1
>           >>= (\a -> unEnvT (k c a) r1)) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\c -> (\r1 -> unEnvT rest r1
>           >>= (\a -> unEnvT (k c a) r1)) r)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\c -> unEnvT rest r >>= (\a -> unEnvT (k c a) r))
> 
>   -- (α-conversion)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> unEnvT rest r >>= (\d -> unEnvT (k a d) r))
> 
>   -- (expand rest)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> unEnvT (lift rest) r >>= (\d -> unEnvT (k a d) r))
> 
>   -- (expand lift)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> unEnvT (EnvT $ \_ -> rest) r
>           >>= (\d -> unEnvT (k a d) r))
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> (\_ -> rest) r >>=
>           (\d -> unEnvT (k a d) r))
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> rest >>=
>           (\d -> unEnvT (k a d) r))
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> rest >>=
>           (\d -> (\u v -> unEnvT (k u v) r) a d))
> 
>   -- (η-reduction)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> rest >>= (\u v -> unEnvT (k u v) r) a)
> 
>   -- (rest/rest law in m)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> (\u v -> unEnvT (k u v) r) a a)
> 
>   -- (β-reduction)
> 
>   , EnvT $ \r ->
>       rest >>=
>         (\a -> unEnvT (k a a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       rest
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (β-expansion)
> 
>   , EnvT $ \r ->
>       (\_ -> rest) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (unEnvT . EnvT == id)
> 
>   , EnvT $ \r ->
>       unEnvT (EnvT $ \_ -> rest) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense lift)
> 
>   , EnvT $ \r ->
>       unEnvT (lift rest) r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense rest)
> 
>   , EnvT $ \r ->
>       unEnvT rest r
>         >>= (\a -> unEnvT ((\u -> k u u) a) r)
> 
>   -- (condense >>=)
> 
>   , rest >>= (\u -> k u u)
>   ]

:::::::::::

Third: `EnvT r m` satisfies the fiddle/id law with stream type `t`:

::: example

> _EnvT_fiddle_id_law_lifted
>   :: forall r t m
>    . ( MonadStream t m, IsStream t )
>   => [EnvT r m ()]
> _EnvT_fiddle_id_law_lifted =
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

Fourth: `EnvT r m` satisfies the fiddle/compose law with stream type `t`:

::: example

> _EnvT_fiddle_compose_law_lifted
>   :: forall r t m
>    . ( MonadStream t m, IsStream t )
>   => (t -> t) -> (t -> t)
>   -> [EnvT r m ()]
> _EnvT_fiddle_compose_law_lifted u v =
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

Fifth: `EnvT r m` satisfies the fiddle/rest law:

::: example

> _EnvT_fiddle_rest_law_lifted
>   :: forall r t m
>    . ( MonadStream t m, IsStream t )
>   => (t -> t)
>   -> [EnvT r m t]
> _EnvT_fiddle_rest_law_lifted u =
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
