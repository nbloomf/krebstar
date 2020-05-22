---
title: The StateT Monad Transformer
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE UndecidableInstances #-}
> 
> module Kreb.Control.Monad.Trans.StateT (
>     StateT(..)
>   , evalStateT
>   , runStateT
>   , execStateT
> 
>   , State
>   , evalState
>   , runState
>   , execState
> ) where
> 
> import Control.Applicative
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Identity

:::



Introduction
------------

In this module we'll define a monad transformer called `StateT`. This transformer takes a monad `m` and adds a new side effect to it: specifically the ability to read and write a "mutable" variable of some type `s`. Before we write down the definition of this type, let's think about what it _should_ look like.

We have:

- A monad `m` describing a class of side-effectful computations;
- A type `s` of possible _states_ for our variable.

And we want:

- An `m` computation that may depend on the specific `s` we have, and which may change the `s`.

That is, a `StateT s m` computation is a function taking in the initial `s`, and producing an `m`-computation which, when executed, produces an updated `s` along with the regular output `a`. In code:

> newtype StateT s m a = StateT
>   { unStateT :: s -> m (a, s) }

And we can define three specialized evaluation functions for computations in `StateT s m`, depending on what information we want out of it. The first, `evalStateT`, returns an `m`-computation that produces both normal input `a` and updated state `s`:

> evalStateT
>   :: ( Monad m )
>   => s -> StateT s m a -> m (a, s)
> evalStateT s x = unStateT x s

The second, `runStateT`, throws away the updated state `s`:

> runStateT
>   :: ( Monad m )
>   => s -> StateT s m a -> m a
> runStateT s =
>   fmap fst . evalStateT s

While the third, `execStateT`, throws away the normal output, returning only the updated state:

> execStateT
>   :: ( Monad m )
>   => s -> StateT s m a -> m s
> execStateT s =
>   fmap snd . evalStateT s

Which evaluator is most convenient will depend on the problem at hand.



Class Instances
---------------

We still haven't established that `StateT s` is a lawful monad transformer -- if not then it isn't of much use. First we give a `Monad` instance for `StateT s m` when `m` is a monad. As difficult as they are to wrap our heads around at first, in practice writing monad instances is often routine. We can get pretty far letting the types guide the code and keeping our desired semantics in mind.

> instance (Monad m) => Monad (StateT s m) where

First for `return`. Recall that this function "injects" a pure value `a` into a monadic computation that produces only `a`. Polymorphism requires the implementation of `return` to look like this:

>   return
>     :: a -> StateT s m a
>   return a = StateT $ \s ->
>     return (a, s)

(We haven't shown that this implementation is _lawful_, just that it is the only possibility.)

Taking the type-driven approach, bind is just a bit more complex -- the only time we have a choice to make is when running the computation `f a`. Note that we're defining `>>=` here without using `do` notation -- this will make the lawfulness proofs a little shorter, since we won't need to translate between sugared and desugared notation.

>   (>>=)
>     :: StateT s m a
>     -> (a -> StateT s m b)
>     -> StateT s m b
>   x >>= f = StateT $ \s1 ->
>     unStateT x s1
>       >>= (\(a, s2) -> unStateT (f a) s2)

Either `s1` or `s2` would have fit in the very last argument position of the last line. But placing `s1` there means that the updated state `s2` after evaluating `x` is never used, violating the intention that state updates be passed to the future of the computation.

With a `Monad` instance in hand we of course get `Functor` and `Applicative` for free.

> instance (Monad m) => Functor (StateT s m) where
>   fmap f x = x >>= (return . f)
> 
> instance (Monad m) => Applicative (StateT s m) where
>   pure = return
> 
>   f <*> x = do
>     f' <- f
>     x' <- x
>     return (f' x')

If `m` is an alternative functor, then so is `StateT s m`.

> instance (Monad m, Alternative m) => Alternative (StateT s m) where
>   empty = StateT $ \_ -> empty
> 
>   x <|> y = StateT $ \s ->
>     (unStateT x s) <|> (unStateT y s)

Finally, `StateT s` is a monad transformer, meaning we can (lawfully) lift `m` computations up to `StateT s m`. We're not using `do` notation again here to make proofs a little shorter.

> instance MonadTrans (StateT s) where
>   lift x = StateT $ \s ->
>     x >>= (\a -> return (a, s))

We also need to establish that these instances are lawful; the proofs of this are somewhat lengthy and not terribly enlightening, so they are in a [separate section](./StateT/Proofs.html).



Specializing to Identity
------------------------

There are times when we don't need a state monad _transformer_, but just a state _monad_; it is simple enough to provide that here by specializing `StateT s m` to the case `m ~ Identity`.

> type State s a = StateT s Identity a

Now `State s` is automatically a monad. We can also specialize the evaluation functions for this case, hiding the fact that there is an `Identity` lurking underneath.

> evalState
>   :: s -> State s a -> (a, s)
> evalState s =
>   unIdentity . evalStateT s
> 
> runState
>   :: s -> State s a -> a
> runState s =
>   unIdentity . runStateT s
> 
> execState
>   :: s -> State s a -> s
> execState s =
>   unIdentity . execStateT s

Note that `get`, `put`, `mutate`, etc. are still part of the API for `State`. This specialization is useful if we only want to use the state side effect, and can also be used as the base of a transformer stack.



The Read/Write Interface
------------------------

We have a type class, [`MonadReadWrite`](@monad-read-write), representing monads with access to a mutable variable of some type -- and `StateT` was tailor made to be an instance of this class. Indeed, filling in the blanks of

::: example
~~~ haskell
instance (Monad m) => MonadReadWrite s (StateT s m) where
  get :: StateT s m s
  get = ...

  put :: s -> StateT s m ()
  put = ...
~~~~~~~~~~~
:::::::::::

is straightforward. Recall, however, that the type of the mutable variable is a class parameter, and that `StateT` is a monad _transformer_, so making this the only instance for `StateT s m` may be too restrictive. What happens if `m` is itself an instance of `MonadReadWrite t` for some other state type t? If possible we'd like to lift this instance to `StateT s m` too, so we can use both.

That is to say, if `t ~ s` then the constraint `MonadReadWrite t (StateT s m)` should use the "natural" instance, but if not it should attempt to use a "lifted" instance from `MonadReadWrite t m` instance. Naively writing two different instances causes GHC to complain about overlapping instances; however with a little type hackery we can (legitimately!) get around this.

The trick is to use an auxiliary type class with the same signature as `MonadReadWrite`, but with an extra type parameter:

> class HasReadWriteState flag st m where
>   get' :: m st
>   put' :: st -> m ()

Then we write an instance of `MonadReadWrite` in terms of the auxiliary class, where the `flag` is used to _dispatch_ on the `HasReadWriteState` instance. Specifically, we use a type equality constraint on `flag` against a type family `Compare` to decide which (nonoverlapping!) `HasReadWriteState` instance to use.

> instance
>   ( Monad m, Compare s t ~ flag
>   , HasReadWriteState flag t (StateT s m)
>   ) => MonadReadWrite t (StateT s m)
>   where
>     get = get' @flag
>     put = put' @flag

Now we just write the appropriate `HasReadWriteState` instances with appropriate constraints.

> instance
>   ( Monad m
>   ) => HasReadWriteState Equal s (StateT s m)
>   where
>     get' :: StateT s m s
>     get' = StateT $ \s -> return (s,s)
> 
>     put' :: s -> StateT s m ()
>     put' s = StateT $ \_ -> return ((),s)
> 
> instance
>   ( MonadReadWrite t m, Compare s t ~ NotEqual
>   ) => HasReadWriteState NotEqual t (StateT s m)
>   where
>     get' :: StateT s m t
>     get' = lift get
> 
>     put' :: t -> StateT s m ()
>     put' u = lift (put u)

This seemed a little magical to me the first time I saw it working. But we can have a monad stack like

::: example
~~~ haskell
foo :: a -> StateT Int (StateT Bool Identity) a
foo a = do
  put False
  put (2 :: Int)
  return a
~~~~~~~~~~~
:::::::::::

and everything works as expected. We may sometimes need to add type signatures to help GHC figure out which state type to use, but that's a small price to pay for the ability to stack transformers of the same type like this.



Lifted Instances
----------------

If `m` is a lawful instance of `MonadReadOnly`, we can lift this to a lawful instance for `StateT s m` with [proof](@statet-monad-read-only-lawful).

> instance
>   ( MonadReadOnly r m
>   ) => MonadReadOnly r (StateT s m)
>   where
>     ask = lift ask
>     local f x = StateT $ \s -> local f (unStateT x s)

If `m` is a lawful instance of `MonadStream`, we can lift this to a lawful instance for `StateT s m` with [proof](@statet-monad-stream-lawful).

> instance
>   ( MonadStream t m
>   ) => MonadStream t (StateT s m)
>   where
>     rest = lift rest
>     fiddle f = lift (fiddle f)
>     next = lift (next @t)
