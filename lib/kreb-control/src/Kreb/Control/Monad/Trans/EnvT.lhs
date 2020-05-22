---
title: The EnvT Monad Transformer
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE UndecidableInstances #-}
> 
> module Kreb.Control.Monad.Trans.EnvT (
>     EnvT(..)
>   , runEnvT
> 
>   , Env
>   , runEnv
> ) where
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Identity

:::



Introduction
------------

In this module we'll define a monad transformer called `EnvT`. This transformer takes a monad `m` and adds a new side effect to it: specifically the ability to read an immutable "environment" variable of some type `r` and to run subcomputations with a modified environment. Before writing down the definition of the type, let's think about what it _should_ look like.

We have:

- A monad `m` describing a class of side-effectful computations;
- A type `r` of possible _environments_ for our computation.

And we want:

- An `m` computation that may depend on the specific `r` we have, but which cannot alter it.

That is, an `EnvT r m` computation is a function taking in the initial environment `r` and producing an `m`-computation. In code:

> newtype EnvT r m a = EnvT
>   { unEnvT :: r -> m a }

There is only one way to evaluate an `EnvT` computation.

> runEnvT
>   :: ( Monad m )
>   => r -> EnvT r m a -> m a
> runEnvT r (EnvT x) = x r



Class Instances
---------------

The `Monad` instance for `EnvT` writes itself. We don't use `do` notation here to make the [proofs](@envt-monad-lawful) simpler.

> instance
>   ( Monad m
>   ) => Monad (EnvT r m)
>   where
>     return a =
>       EnvT $ \_ -> return a
> 
>     x >>= f =
>       EnvT $ \r ->
>         unEnvT x r >>= (\a -> unEnvT (f a) r)

With a `Monad` instance in hand we of course get `Functor` and `Applicative` for free.

> instance (Monad m) => Functor (EnvT r m) where
>   fmap f x = x >>= (return . f)
> 
> instance (Monad m) => Applicative (EnvT r m) where
>   pure = return
> 
>   f <*> x = do
>     f' <- f
>     x' <- x
>     return (f' x')

Finally, `EnvT r` is a monad transformer, meaning we can (lawfully) lift `m` computations up to `EnvT r m`.

> instance MonadTrans (EnvT r) where
>   lift x = EnvT $ \_ -> x

The [proofs](./EnvT/Proofs.html) that these instances are lawful are routine.



Specializing to Identity
------------------------

When we want an environment variable but don't need the full power of a transformer, we can instead use this specialization over the identity monad.

> type Env r a = EnvT r Identity a
> 
> runEnv
>   :: r -> Env r a -> a
> runEnv r = unIdentity . runEnvT r

Of course `ask`, `local`, and `asks` are still part of the API for `Env`.



The Read-Only Interface
-----------------------

`EnvT r m` was designed to be an instance of `MonadReadOnly`. However, if `m` is itself an instance of `MonadReadOnly`, we'd like to be able to lift this instance to `EnvT r m`. On its face this appears to require writing two different (and overlapping) `MonadReadOnly` instances for `EnvT r m`, but with some type hackery we can get around this. The trick works by having a copy of the `MonadReadOnly` class taking an extra type paramter.

> class HasReadOnlyState flag env m where
>   ask' :: m env
>   local' :: (env -> env) -> m a -> m a

Now we can define a `MonadReadOnly` instance for any type inhabiting `HasReadOnlyState`, using the `Compare` type-level function.

> instance
>   ( Monad m, Compare r t ~ flag
>   , HasReadOnlyState flag t (EnvT r m)
>   ) => MonadReadOnly t (EnvT r m)
>   where
>     ask = ask' @flag
>     local = local' @flag

Now we can write two different instances of `HasReadOnlyState` for `EnvT`, one for the "natural" instance and one for the "lifted" instance. These are not overlapping due to the extra type constraint (`Equal` or `NotEqual`).

> instance
>   ( Monad m
>   ) => HasReadOnlyState Equal r (EnvT r m)
>   where
>     ask' :: EnvT r m r
>     ask' = EnvT $ \r -> return r
> 
>     local' :: (r -> r) -> EnvT r m a -> EnvT r m a
>     local' f x =
>       EnvT $ \r -> unEnvT x (f r)
> 
> instance
>   ( MonadReadOnly t m, Compare r t ~ NotEqual
>   ) => HasReadOnlyState NotEqual t (EnvT r m)
>   where
>     ask' :: EnvT r m t
>     ask' = lift ask
> 
>     local' :: (t -> t) -> EnvT r m a -> EnvT r m a
>     local' f x = EnvT $ \r -> local f (unEnvT x r)

The [proofs](@envt-monad-read-only-lawful) that these instances are lawful are tedious but straightforward.



Lifted Instances
----------------

If `m` is a lawful instance of `MonadReadWrite`, we can lift this to a lawful instance for `EnvT r m` with [proof](@envt-monad-read-write-lawful).

> instance
>   ( MonadReadWrite s m
>   ) => MonadReadWrite s (EnvT t m)
>   where
>     get = lift get
>     put = lift . put

If `m` is a lawful instance of `MonadStream`, we can lift this to a lawful instance for `EnvT r m` with [proof](@envt-monad-stream-lawful).

> instance
>   ( MonadStream s m, IsStream s
>   ) => MonadStream s (EnvT t m)
>   where
>     rest = lift rest
>     fiddle f = lift (fiddle f)
>     next = lift (next @s)
