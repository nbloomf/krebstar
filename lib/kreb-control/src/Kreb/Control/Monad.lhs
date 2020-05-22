---
title: Monads and Side Effects
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE MultiParamTypeClasses, QuantifiedConstraints, FunctionalDependencies, UndecidableInstances #-}
> 
> module Kreb.Control.Monad where

:::



Introduction
------------

The monad interface is a powerful abstraction for building and reasoning about sequenced actions. Each specific instance of a monad corresponds to a particular kind of permitted computation (possibly with side effects), and moreover these permitted computations are reflected in the monad's type. Monad transformers are a powerful abstraction on top of monads which allow us to "chain together" different effects. Together, these ideas allow us to have type-safe side effects built up from standardized parts. The `MonadTrans` class is defined like so:

> class
>   ( forall m. ( Monad m ) => Monad (t m)
>   ) => MonadTrans t
>   where
>     lift :: ( Monad m ) => m a -> t m a

Instances of this class must satisfy the following laws:

Unit Law
  : `lift (return a)` is equivalent to `return a`

Bind Law
  : `lift (x >>= f)` is equivalent to `lift x >>= (lift . f)`

Among abstractions for modelling chained actions, the monad interface is also extremely general -- and being general means it can express quite a bit about a lot of instances, but not much about any particular instance. In this module we define some useful classes of monads. These are families of monads with additional machinery, and additional laws, representing more specific features of computation.



Read/Write State
----------------

[@monad-read-write]()

Monads with access to a "state" variable which must be initialized to some value and which can be arbitrarily read and overwritten inhabit the `MonadReadWrite` class. Such monads have an associated type representing the state -- given here as a parameter to the class -- and two primitives: `get`, which reads the "current" value of the state, and `put`, which overwrites the state.

> class (Monad m) => MonadReadWrite s m where
>   -- Retrieve the current state
>   get :: m s
> 
>   -- Replace the current state
>   put :: s -> m ()

Instances of this class are required to obey four additional laws which capture the interaction between `get` and `put`.

Put/Put Law
  : `put u1 >> put u2` is equivalent to `put u2`

Put/Get Law
  : `put u >> get` is equivalent to `put u >> return u`

Get/Put Law
  : `get >>= put` is equivalent to `return ()`

Get/Get Law
  : `get >>= (\u -> get >>= k u)` is equivalent to `get >>= (\u -> k u u)`

"Equivalence" is a very powerful statement here; it means that in _any_ program, one expression can be replaced by another without changing either its outcome or its side effects. For instance, the put/put law says that if we overwrite the state, and then overwrite it again, the first overwrite may as well not have happened -- in particular, `put` cannot have any side effects other than overwriting the state.

`get` and `put` are the primitive operations on mutable state, but we can define some helper functions in terms of these which capture common usage patterns. `gets` acts like `get`, but applies a function to the state; `mutate` alters the state in place.

> gets
>   :: ( MonadReadWrite s m )
>   => (s -> u) -> m u
> gets f = fmap f get
> 
> mutate
>   :: ( MonadReadWrite s m )
>   => (s -> s) -> m ()
> mutate f = get >>= (put . f)



Read-Only State
---------------

[@monad-read-only]()

Monads with access to a _read-only_ environment variable which must be initialized inhabit the `MonadReadOnly` class. Such monads have an associated type representing the environment -- given here as a parameter to the class -- and two primitives: `ask`, which gets the value of the environment, and `local`, which runs a subcomputation with a modified environment.

> class (Monad m) => MonadReadOnly r m where
>   -- Retrieve the environment
>   ask :: m r
> 
>   -- | Run a subcomputation with a modified environment
>   local :: (r -> r) -> m a -> m a

Instances of this class are required to obey five additional laws which capture the interaction between `ask` and `local`.

Local/Ask Law
  : `local u ask` is equivalent to `fmap u ask`

Local/Local Law
  : `local u (local v x)` is equivalent to `local (v . u) x`

Interchange Law
  : `local u x >>= (\a -> ask >>= (\b -> f a b))` is equivalent to `ask >>= (\b -> local u x >>= (\a -> f a b))`

Local Unit Law
  : `local u (return a)` is equivalent to `return a`

Local Bind Law
  : `local u (x >>= f)` is equivalent to `local u x >>= (local u . f)`

As usual, equivalence means that two program fragments can always be substituted. The local unit and local bind laws together state that `local u` is a monad homomorphism. The interchange law guarantees that `ask` has no side effects other than making the environment value available.

> asks
>   :: ( MonadReadOnly r m )
>   => (r -> a) -> m a
> asks f = fmap f ask



Stream State
------------

[@monad-stream]()

A _stream_ is a data type from which we can extract arbitrarily many values. `MonadStream` is inhabited by monads with access to a stream. These monads must provide primitives for getting and mutating the current stream, as well as for extracting the next value from the stream.

> class IsStream s where
>   data StreamValue s :: *
>   advance :: s -> (StreamValue s, s)
> 
> class (Monad m, IsStream s) => MonadStream s m where
>   -- See the stream
>   rest :: m s
> 
>   -- Adjust the stream
>   fiddle :: (s -> s) -> m ()
> 
>   -- Advance the stream
>   next :: m (StreamValue s)

Instances of this class are required to satisfy the following law governing the interaction between `advance`, `rest`, and `next`:

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
