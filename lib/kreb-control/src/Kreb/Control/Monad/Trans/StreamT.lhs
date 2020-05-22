---
title: The StreamT Monad Transformer
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE UndecidableInstances #-}
> 
> module Kreb.Control.Monad.Trans.StreamT (
>     StreamT(..)
>   , unStreamT
>   , evalStreamT
>   , runStreamT
>   , execStreamT
> ) where
> 
> import Control.Applicative
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans

:::



Introduction
------------

In this module we'll define a monad transformer called `StreamT`. The definition of this transformer is very similar to `StateT` -- in fact with one minor exception they are identical. The difference between `StateT` and `StreamT` is in their "access pattern". Where `StateT` adds a mutable variable of arbitrary type to a monad, `StreamT` specifically adds a _stream_ variable.^[This is a handy way to define "new" monad classes -- start with `StateT s` and restrict `s` to a particular kind of data structure.]

> data StreamT s m a where
>   StreamT
>     :: ( IsStream s )
>     => (s -> m (a, s))
>     -> StreamT s m a
> 
> unStreamT
>   :: ( IsStream s )
>   => StreamT s m a -> s -> m (a, s)
> unStreamT (StreamT x) = x

We're using a GADT here to allow enforcing the `IsStream` constraint on the data constructor.

As with `StateT`, we can define three specialized evaluation functions for computations in `StreamT s m` depending on what information we want out of it. The first, `evalStreamT`, returns an `m`-computation that produces both normal input `a` and updated state `s`:

> evalStreamT
>   :: ( Monad m, IsStream s )
>   => s -> StreamT s m a -> m (a, s)
> evalStreamT s x = unStreamT x s

The second, `runStreamT`, throws away the updated stream `s`:

> runStreamT
>   :: ( Monad m, IsStream s )
>   => s -> StreamT s m a -> m a
> runStreamT s =
>   fmap fst . evalStreamT s

While the third, `execStreamT`, throws away the normal output, returning only the updated stream:

> execStreamT
>   :: ( Monad m, IsStream s )
>   => s -> StreamT s m a -> m s
> execStreamT s =
>   fmap snd . evalStreamT s

Which of these is more appropriate will depend on the situation.



Class Instances
---------------

The `Monad` instance for `StreamT` is identical to that for `StateT`; we also [prove](@streamt-monad-lawful) this instance is lawful.

> instance (Monad m, IsStream s) => Monad (StreamT s m) where
>   return
>     :: a -> StreamT s m a
>   return a = StreamT $ \s ->
>     return (a, s)
> 
>   (>>=)
>     :: StreamT s m a
>     -> (a -> StreamT s m b)
>     -> StreamT s m b
>   x >>= f = StreamT $ \s1 ->
>     unStreamT x s1
>       >>= (\(a, s2) -> unStreamT (f a) s2)

And `Functor` and `Applicative` are determined by `Monad`.

> instance (Monad m, IsStream s) => Functor (StreamT s m) where
>   fmap f x = x >>= (return . f)
> 
> instance (Monad m, IsStream s) => Applicative (StreamT s m) where
>   pure = return
> 
>   f <*> x = do
>     f' <- f
>     x' <- x
>     return (f' x')

If `m` is an alternative functor, then so is `StreamT s m`.

> instance (Monad m, Alternative m, IsStream s) => Alternative (StreamT s m) where
>   empty = StreamT $ \_ -> empty
> 
>   x <|> y = StreamT $ \s ->
>     (unStreamT x s) <|> (unStreamT y s)

Finally, `StreamT` is a monad transformer; again we can [prove](@streamt-monad-trans-lawful) this instance is lawful.

> instance (IsStream s) => MonadTrans (StreamT s) where
>   lift x = StreamT $ \s ->
>     x >>= (\a -> return (a, s))



The Stream Interface
--------------------

`StreamT s m` was designed to be an instance of [`MonadStream`](@monad-stream), but much like our other concrete transformers, it could (maybe) be an instance of that class in two ways -- the "natural" instance with stream type `s`, and a "lifted" instance from `m` with stream type `t` if one exists. We can define both unambiguously using the auxiliary class trick.

> class (IsStream st) => HasStreamState flag st m where
>   rest' :: m st
>   fiddle' :: (st -> st) -> m ()
>   next' :: m (StreamValue st)

Now any instance of `HasStreamState` is an instance of `MonadStream`:

> instance
>   ( Monad m, Compare s t ~ flag
>   , IsStream s, IsStream t
>   , HasStreamState flag t (StreamT s m)
>   ) => MonadStream t (StreamT s m)
>   where
>     rest = rest' @flag
>     fiddle = fiddle' @flag
>     next = next' @flag

And the natural and lifted instance of `HasStreamState` for `StreamT` do not overlap due to the extra `flag` parameter.

> instance
>   ( Monad m, IsStream s
>   ) => HasStreamState Equal s (StreamT s m)
>   where
>     rest' = StreamT $ \s ->
>       return (s, s)
> 
>     fiddle' f = StreamT $ \s ->
>       return ((), f s)
> 
>     next' = StreamT $ \s ->
>       return (advance s)
> 
> instance
>   ( MonadStream t m, IsStream s
>   ) => HasStreamState NotEqual t (StreamT s m)
>   where
>     rest' = lift rest
>     fiddle' f = lift (fiddle f)
>     next' = lift next

We also can [prove](@streamt-monad-stream-lawful) the lawfulness of both instances.



Lifted Instances
----------------

If `m` is a lawful instance of `MonadReadOnly`, we can lift this to a lawful instance for `StreamT s m` with [proof](@streamt-monad-read-only-lawful).

> instance
>   ( MonadReadOnly r m, IsStream s
>   ) => MonadReadOnly r (StreamT s m)
>   where
>     ask = lift ask
>     local f x = StreamT $ \s -> local f (unStreamT x s)

If `m` is a lawful instance of `MonadReadWrite`, we can lift this to a lawful instance for `StreamT s m` with [proof](@streamt-monad-read-write-lawful).

> instance
>   ( MonadReadWrite u m, IsStream s
>   ) => MonadReadWrite u (StreamT s m)
>   where
>     get = lift get
>     put = lift . put
