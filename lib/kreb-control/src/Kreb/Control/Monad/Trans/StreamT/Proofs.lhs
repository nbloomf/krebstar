---
title: Lawfulness Proofs for StreamT
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE TupleSections #-}
> 
> module Kreb.Control.Monad.Trans.StreamT.Proofs where
> 
> import Kreb.Control.Compare
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Trans.StreamT

:::



Introduction
------------

In this section we give lawfulness proofs for the class instances of `StreamT`.



Monad Laws
----------

[@streamt-monad-lawful]()

Recall the three _monad laws_:

Left Identity Law
  : `return a >>= f` is equivalent to `f a`

Right Identity Law
  : `x >>= return` is equivalent to `x`

Associative Law
  : `(x >>= f) >>= g` is equivalent to `x >>= (\a -> f a >>= g)`

Here we show that if `m` is a monad, then `StreamT s m` is also a monad. To that end suppose `m` is a monad.

First: `StreamT s m` satisfies the left identity law.

::: example

> _StreamT_left_identity_law
>   :: forall s m a b
>    . ( Monad m, IsStream s )
>   => a -> (a -> StreamT s m b)
>   -> [StreamT s m b]
> _StreamT_left_identity_law a f =
> 
>   [ return a >>= f
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT (return a) s1
>         >>= (\(b, s2) -> unStreamT (f b) s2)
> 
>   -- (expand return)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> return (a,s)) s1
>         >>= (\(b, s2) -> unStreamT (f b) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> return (a,s)) s1
>         >>= (\(b, s2) -> unStreamT (f b) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       return (a,s1)
>         >>= (\(b, s2) -> unStreamT (f b) s2)
> 
>   -- (left identity law for m)
> 
>   , StreamT $ \s1 ->
>       (\(b, s2) -> unStreamT (f b) s2) (a, s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT (f a) s1
> 
>   -- (η-reduction)
> 
>   , StreamT $ (unStreamT (f a))
> 
>   -- (StreamT . unStreamT == id)
> 
>   , f a
>   ]

:::::::::::

Second: `StreamT s m` satisfies the right identity law.

::: example

> _StreamT_right_identity_law
>   :: forall s m a
>    . ( Monad m, IsStream s )
>   => (StreamT s m a)
>   -> [StreamT s m a]
> _StreamT_right_identity_law x =
> 
>   [ x >>= return
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1
>         >>= (\(a, s2) -> unStreamT (return a) s2)
> 
>   -- (expand return)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1
>         >>= (\(a, s2) -> unStreamT (StreamT $ \s -> return (a,s)) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1
>         >>= (\(a, s2) -> (\s -> return (a,s)) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1
>         >>= (\(a, s2) -> return (a, s2))
> 
>   -- (η-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1
>         >>= return
> 
>   -- (right identity law for m)
> 
>   , StreamT $ \s1 -> unStreamT x s1
> 
>   -- (η-reduction)
> 
>   , StreamT $ unStreamT x
> 
>   -- (StreamT . unStreamT == id)
> 
>   , x
>   ]

:::::::::::

Third: `StreamT s m` satisfies the associativity law.

::: example

> _StreamT_associativity_law
>   :: forall s m a b c
>    . ( Monad m, IsStream s )
>   => StreamT s m a -> (a -> StreamT s m b) -> (b -> StreamT s m c)
>   -> [StreamT s m c]
> _StreamT_associativity_law x f g =
> 
>   [ (x >>= f) >>= g
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT (x >>= f) s1
>         >>= (\(b, s2) -> unStreamT (g b) s2)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s2 ->
>         unStreamT x s2 >>= (\(a, s3) -> unStreamT (f a) s3)) s1
>         >>= (\(b, s2) -> unStreamT (g b) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s2 ->
>         unStreamT x s2 >>= (\(a, s3) -> unStreamT (f a) s3)) s1
>         >>= (\(b, s2) -> unStreamT (g b) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>= (\(a, s3) -> unStreamT (f a) s3)
>         >>= (\(b, s2) -> unStreamT (g b) s2)
> 
>   -- (associativity law in m)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>= (\(c, s4) ->
>         (\(a, s3) -> unStreamT (f a) s3) (c, s4)
>           >>= (\(b, s2) -> unStreamT (g b) s2))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>= (\(c, s4) ->
>         unStreamT (f c) s4
>           >>= (\(b, s2) -> unStreamT (g b) s2))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>=
>         (\(c, s4) -> (\s5 ->
>           unStreamT (f c) s5 >>= (\(b, s2) -> unStreamT (g b) s2)) s4)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>=
>         (\(c, s4) -> unStreamT (StreamT $ \s5 ->
>           unStreamT (f c) s5 >>= (\(b, s2) -> unStreamT (g b) s2)) s4)
> 
>   -- (condense >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>=
>         (\(c, s4) -> unStreamT (f c >>= g) s4)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       unStreamT x s1 >>=
>         (\(c, s4) -> unStreamT ((\a -> f a >>= g) c) s4)
> 
>   -- (condense >>=)
> 
>   , x >>= (\a -> f a >>= g)
>   ]

:::::::::::



Monad Transformer Laws
----------------------

[@streamt-monad-trans-lawful]()

Recall the two _monad transformer laws_:

Unit Law
  : `lift . return` is equivalent to `return`

Bind Law
  : `lift (x >>= f)` is equivalent to `lift x >>= (lift . f)`

Here we show that `StreamT s` is a monad transformer. First: `StreamT s` satisfies the unit law.

::: example

> _StreamT_unit_law
>   :: forall s m a
>    . ( Monad m, IsStream s )
>   => a
>   -> [StreamT s m a]
> _StreamT_unit_law a =
> 
>   [ lift (return a)
> 
>   -- (expand lift)
> 
>   , StreamT $ \s ->
>       return a
>         >>= (\b -> return (b, s))
> 
>   -- (left identity law for m)
> 
>   , StreamT $ \s ->
>       (\b -> return (b, s)) a
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return (a, s)
> 
>   -- (condense return)
> 
>   , return a
>   ]

:::::::::::

Second: `StreamT s` satisfies the bind law.

::: example

> _StreamT_bind_law
>   :: forall s m a b
>    . ( Monad m, IsStream s )
>   => m a -> (a -> m b)
>   -> [StreamT s m b]
> _StreamT_bind_law x f =
> 
>   [ lift (x >>= f)
> 
>   -- (expand lift)
> 
>   , StreamT $ \s ->
>       (x >>= f)
>         >>= (\a -> return (a, s))
> 
>   -- (associativity law for m)
> 
>   , StreamT $ \s ->
>       x
>         >>= (\c -> f c >>= (\a -> return (a, s)))
> 
>   -- (α-conversion)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\c -> f c >>= (\d -> return (d, s1)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\c ->
>           (\(a, s2) -> f a >>= (\d -> return (d, s2))) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\c -> return (c, s1)
>           >>= (\(a, s2) -> f a >>= (\d -> return (d, s2))))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> f a >>= (\d -> return (d, s2))))
> 
>   -- (associativity law in m)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> f a >>= (\d -> return (d, s2)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> (\s -> f a >>= (\d -> return (d, s))) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStreamT (StreamT $ \s -> f a >>= (\d -> return (d, s))) s2)
> 
>   -- (condense lift)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStreamT (lift (f a)) s2)
> 
>   -- (definition of .)
> 
>   , StreamT $ \s1 ->
>       x
>         >>= (\b -> return (b, s1))
>         >>= (\(a, s2) -> unStreamT ((lift . f) a) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       ((\s -> x >>= (\b -> return (b, s))) s1)
>         >>= (\(a, s2) -> unStreamT ((lift . f) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> x >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((lift . f) a) s2)
> 
>   -- (condense lift)
> 
>   , StreamT $ \s1 ->
>       unStreamT (lift x) s1
>         >>= (\(a, s2) -> unStreamT ((lift . f) a) s2)
> 
>   -- (condense >>=)
> 
>   , lift x >>= (lift . f)
>   ]

:::::::::::



MonadStream Laws
----------------

[@streamt-monad-stream-lawful]()

There are two different `MonadStream` instances for `StreamT s m` that we need to verify. Recall the `MonadStream` laws:

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

Here we show that if `m` is a monad then `StreamT s m` satisfies the `MonadStream` laws with stream type `s`.

First: `StreamT s m` satisfies the next/rest law with stream type `s`.

::: example

> _StreamT_next_rest_law_natural
>   :: forall s m
>    . ( Monad m, IsStream s )
>   => [ StreamT s m (StreamValue s, s) ]
> _StreamT_next_rest_law_natural =
> 
>   [ (advance <$> rest) <* (next @s)
> 
>   -- (expand <*)
> 
>   , const <$> (advance <$> rest) <*> (next @s)
> 
>   -- (expand <$>)
> 
>   , (fmap const (advance <$> rest)) <*> (next @s)
> 
>   -- (expand <$>)
> 
>   , (fmap const (fmap advance rest)) <*> (next @s)
> 
>   -- (composite law for StreamT s m)
> 
>   , (fmap (const . advance) rest) <*> (next @s)
> 
>   -- (expand <*>)
> 
>   , (fmap (const . advance) rest)
>       >>= (\f -> (next @s) >>= (\a -> return (f a)))
> 
>   -- (expand fmap)
> 
>   , (rest >>= (return . const . advance))
>       >>= (\f -> (next @s) >>= (\a -> return (f a)))
> 
>   -- (associative law in StreamT s m)
> 
>   , rest >>= (\c -> (return . const . advance) c
>       >>= (\f -> (next @s) >>= (\a -> return (f a))))
> 
>   -- (expand .)
> 
>   , rest >>= (\c -> return (const (advance c))
>       >>= (\f -> (next @s) >>= (\a -> return (f a))))
> 
>   -- (left identity law in StreamT s m)
> 
>   , rest >>= (\c ->
>       (\f -> (next @s) >>= (\a -> return (f a))) (const (advance c)))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       (next @s) >>= (\a -> return (const (advance c) a)))
> 
>   -- (expand const)
> 
>   , rest >>= (\c ->
>       (next @s) >>= (\a -> return (advance c)))
> 
>   -- (expand >>=)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         unStreamT (next @s) s2
>           >>= (\(b, s1) -> unStreamT ((\a -> return (advance c)) b) s1))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         unStreamT (next @s) s2
>           >>= (\(b, s1) -> unStreamT (return (advance c)) s1))
> 
>   -- (expand next)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         unStreamT (StreamT $ \s3 -> return (advance s3)) s2
>           >>= (\(b, s1) -> unStreamT (return (advance c)) s1))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         (\s3 -> return (advance s3)) s2
>           >>= (\(b, s1) -> unStreamT (return (advance c)) s1))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         return (advance s2)
>           >>= (\(b, s1) -> unStreamT (return (advance c)) s1))
> 
>   -- (left identity law in m)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         (\(b, s1) -> unStreamT (return (advance c)) s1) (advance s2))
> 
>   -- (expand return)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         (\(b, s1) -> unStreamT (StreamT $ \s2 ->
>           return (advance c, s2)) s1) (advance s2))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         (\(b, s1) -> (\s2 ->
>           return (advance c, s2)) s1) (advance s2))
> 
>   -- (β-reduction)
> 
>   , rest >>= (\c ->
>       StreamT $ \s2 ->
>         (\(b, s1) -> return (advance c, s1)) (advance s2))
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(a, s3) -> unStreamT ((\c ->
>           StreamT $ \s2 ->
>             (\(b, s1) -> return (advance c, s1)) (advance s2)) a) s3)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(a, s3) -> unStreamT (StreamT $ \s2 ->
>           (\(b, s1) -> return (advance s3, s1)) (advance s2)) a)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(a, s3) -> (\s2 ->
>           (\(b, s1) -> return (advance s3, s1)) (advance s2)) a)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(a, s3) ->
>           (\(b, s1) -> return (advance s3, s1)) (advance a))
> 
>   -- (expand rest)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s2 -> return (s2, s2)) s
>         >>= (\(a, s3) ->
>           (\(b, s1) -> return (advance s3, s1)) (advance a))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (\s2 -> return (s2, s2)) s
>         >>= (\(a, s3) ->
>           (\(b, s1) -> return (advance s3, s1)) (advance a))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return (s, s)
>         >>= (\(a, s3) ->
>           (\(b, s1) -> return (advance s3, s1)) (advance a))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       (\(a, s3) -> (\(b, s1) -> return (advance s3, s1)) (advance a)) (s, s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       (\(b, s1) -> return (advance s, s1)) (advance s)
> 
>   -- (condense snd)
> 
>   , StreamT $ \s ->
>       (\(b, s1) -> return (advance s, snd (b, s1))) (advance s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return (advance s, snd (advance s))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\(c, s3) -> return ((c, s3), snd (c, s3))) (advance s)
> 
>   -- (expand snd)
> 
>   , StreamT $ \s ->
>       (\(c, s3) -> return ((c, s3), s3)) (advance s)
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       return (advance s) >>=
>         (\(c, s3) -> return ((c, s3), s3))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\s1 -> return (advance s1)) s >>=
>         (\(c, s3) -> return ((c, s3), s3))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return (advance s1)) s >>=
>         (\(c, s3) -> return ((c, s3), s3))
> 
>   -- (condense next)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) -> return ((c, s3), s3))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) -> (\(b, s4) -> return ((c, b), s4)) (s3, s3))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) ->
>           return (s3, s3)
>             >>= (\(b, s4) -> return ((c, b), s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) ->
>           (\s2 -> return (s2, s2)) s3
>             >>= (\(b, s4) -> return ((c, b), s4)))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) ->
>           unStreamT (StreamT $ \s2 -> return (s2, s2)) s3
>             >>= (\(b, s4) -> return ((c, b), s4)))
> 
>   -- (condense rest)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) ->
>           unStreamT rest s3
>             >>= (\(b, s4) -> return ((c, b), s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) ->
>           unStreamT rest s3
>             >>= (\(b, s4) -> return ((c,) b, s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) -> (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> return (g b, s4))) ((c,), s3))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) -> return ((c,), s3)
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4))))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT next s >>=
>         (\(c, s3) -> (\(a, s5) -> return ((a,), s5)) (c, s3)
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4))))
> 
>   -- (associative law in m)
> 
>   , StreamT $ \s ->
>       (unStreamT next s
>         >>= (\(a, s5) -> return ((a,), s5)))
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (unStreamT next s
>         >>= (\(a, s5) -> (\s3 -> return ((a,), s3)) s5))
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (unStreamT next s
>         >>= (\(a, s5) -> unStreamT (StreamT $ \s3 -> return ((a,), s3)) s5))
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (condense return)
> 
>   , StreamT $ \s ->
>       (unStreamT next s
>         >>= (\(a, s5) -> unStreamT (return (a,)) s5))
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (condense .)
> 
>   , StreamT $ \s ->
>       (unStreamT next s
>         >>= (\(a, s5) -> unStreamT ((return . (,)) a) s5))
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\s3 -> unStreamT next s3
>         >>= (\(a, s5) -> unStreamT ((return . (,)) a) s5)) s
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s3 -> unStreamT next s3
>         >>= (\(a, s5) -> unStreamT ((return . (,)) a) s5)) s
>           >>= (\(g, s2) ->
>             unStreamT rest s2
>               >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (condense >>=)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> return (g b, s4)))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> (\s3 -> return (g b, s3)) s4))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> unStreamT (StreamT $ \s3 -> return (g b, s3)) s4))
> 
>   -- (condense return)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> unStreamT (return (g b)) s4))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) ->
>           unStreamT rest s2
>             >>= (\(b, s4) -> unStreamT ((\a -> return (g a)) b) s4))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) -> (\s3 ->
>           unStreamT rest s3
>             >>= (\(b, s4) -> unStreamT ((\a -> return (g a)) b) s4)) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) -> unStreamT (StreamT $ \s3 ->
>           unStreamT rest s3
>             >>= (\(b, s4) -> unStreamT ((\a -> return (g a)) b) s4)) s2)
> 
>   -- (condense >>=)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) -> unStreamT (rest >>= (\a -> return (g a))) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT (next >>= (return . (,))) s
>         >>= (\(g, s2) -> unStreamT ((\f -> rest >>= (\a -> return (f a))) g) s2)
> 
>   -- (condense fmap)
> 
>   , StreamT $ \s ->
>       unStreamT (fmap (,) next) s
>         >>= (\(g, s2) -> unStreamT ((\f -> rest >>= (\a -> return (f a))) g) s2)
> 
>   -- (condense <$>)
> 
>   , StreamT $ \s ->
>       unStreamT ((,) <$> next) s
>         >>= (\(g, s2) -> unStreamT ((\f -> rest >>= (\a -> return (f a))) g) s2)
> 
>   -- (condense >>=)
> 
>   , ((,) <$> next) >>= (\f -> rest >>= (\a -> return (f a)))
> 
>   -- (condense <*>)
> 
>   , (,) <$> next <*> rest
>   ]

:::::::::::

Second: `StreamT s m` satisfies the rest/rest law with stream type `s`.

::: example

> _StreamT_rest_rest_law_natural
>   :: forall s m a
>    . ( Monad m, IsStream s )
>   => (s -> s -> StreamT s m a)
>   -> [StreamT s m a]
> _StreamT_rest_rest_law_natural k =
> 
>   [ rest >>= (\u -> rest >>= k u)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT (rest >>= k a) s2)
> 
>   -- (expand rest)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT (rest >>= k a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT (rest >>= k a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStreamT (rest >>= k a) s2)
> 
>   -- (left identity law for m)
> 
>   , StreamT $ \s1 ->
>       (\(a, s2) -> unStreamT (rest >>= k a) s2) (s1, s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT (rest >>= k s1) s1
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s ->
>         unStreamT rest s >>= (\(a, s2) -> unStreamT (k s1 a) s2)) s1
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> unStreamT rest s >>= (\(a, s2) -> unStreamT (k s1 a) s2)) s1
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT (k s1 a) s2)
> 
>   -- (expand rest)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT (k s1 a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT (k s1 a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStreamT (k s1 a) s2)
> 
>   -- (left identity law for m)
> 
>   , StreamT $ \s1 ->
>       (\(a, s2) -> unStreamT (k s1 a) s2) (s1, s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       unStreamT (k s1 s1) s1
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       unStreamT ((\u -> k u u) s1) s1
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       (\(a, s2) -> unStreamT ((\u -> k u u) a) s2) (s1, s1)
> 
>   -- (left identity law for m)
> 
>   , StreamT $ \s1 ->
>       return (s1, s1)
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       (\s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> return (s, s)) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense rest)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , rest >>= (\u -> k u u)
>   ]

:::::::::::

Third: `StreamT s m` satisfies the fiddle/id law with stream type `s`.

::: example

> _StreamT_fiddle_id_law_natural
>   :: forall s m
>    . ( Monad m, IsStream s )
>   => [StreamT s m ()]
> _StreamT_fiddle_id_law_natural =
> 
>   [ fiddle (id @s)
> 
>   -- (expand fiddle)
> 
>   , StreamT $ \s ->
>       return ((), id s)
> 
>   -- (expand id)
> 
>   , StreamT $ \s ->
>       return ((), s)
> 
>   -- (condense return)
> 
>   , return ()
>   ]

:::::::::::

Fourth: `StreamT s m` satisfies the fiddle/compose law with stream type `s`.

::: example

> _StreamT_fiddle_compose_law_natural
>   :: forall s m
>    . ( Monad m, IsStream s )
>   => (s -> s) -> (s -> s)
>   -> [StreamT s m ()]
> _StreamT_fiddle_compose_law_natural u v =
> 
>   [ fiddle u >> fiddle v
> 
>   -- (expand >>)
> 
>   , fiddle u >>= (\_ -> fiddle v)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u) s
>         >>= (\(a, s2) -> unStreamT ((\_ -> fiddle v) a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u) s
>         >>= (\(a, s2) -> unStreamT (fiddle v) s2)
> 
>   -- (expand fiddle)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT (fiddle v) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (\s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT (fiddle v) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return ((), u s)
>         >>= (\(a, s2) -> unStreamT (fiddle v) s2)
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       (\(a, s2) -> unStreamT (fiddle v) s2) ((), u s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle v) (u s)
> 
>   -- (expand fiddle)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return ((), v s1)) (u s)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (\s1 -> return ((), v s1)) (u s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return ((), v (u s))
> 
>   -- (condense .)
> 
>   , StreamT $ \s ->
>       return ((), (v . u) s)
> 
>   -- (condense fiddle)
> 
>   , fiddle (v . u)
>   ]

:::::::::::

Fifth: `StreamT s m` satisfies the fiddle/rest law with stream type `s`.

::: example

> _StreamT_fiddle_rest_law_natural
>   :: forall s m
>    . ( Monad m, IsStream s )
>   => (s -> s)
>   -> [StreamT s m s]
> _StreamT_fiddle_rest_law_natural u =
> 
>   [ fiddle u >> rest
> 
>   -- (expand >>)
> 
>   , fiddle u >>= (\_ -> rest)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u) s
>         >>= (\(a, s2) -> unStreamT ((\_ -> rest) a) s2)
> 
>   -- (expand fiddle)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT ((\_ -> rest) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (\s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT ((\_ -> rest) a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return ((), u s)
>         >>= (\(a, s2) -> unStreamT ((\_ -> rest) a) s2)
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       (\(a, s2) -> unStreamT ((\_ -> rest) a) s2) ((), u s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT ((\_ -> rest) ()) (u s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       unStreamT rest (u s)
> 
>   -- (expand rest)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return (s1, s1)) (u s)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       (\s1 -> return (s1, s1)) (u s)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s ->
>       return (u s, u s)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\s1 -> return (u s, s1)) (u s)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return (u s, s1)) (u s)
> 
>   -- (condense return)
> 
>   , StreamT $ \s ->
>       unStreamT (return (u s)) (u s)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\(a, s2) -> unStreamT (return (u s)) s2) ((), u s)
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       return ((), u s)
>         >>= (\(a, s2) -> unStreamT (return (u s)) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT (return (u s)) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return ((), u s1)) s
>         >>= (\(a, s2) -> unStreamT (return (u s)) s2)
> 
>   -- (condense fiddle)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u) s
>         >>= (\(a, s2) -> unStreamT (return (u s)) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u) s
>         >>= (\(a, s2) -> unStreamT ((\_ -> return (u s)) a) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\s1 -> unStreamT (fiddle u) s1
>         >>= (\(a, s2) -> unStreamT ((\_ -> return (u s)) a) s2)) s
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> unStreamT (fiddle u) s1
>         >>= (\(a, s2) -> unStreamT ((\_ -> return (u s)) a) s2)) s
> 
>   -- (condense >>=)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u >>= (\_ ->return (u s))) s
> 
>   -- (condense >>)
> 
>   , StreamT $ \s ->
>       unStreamT (fiddle u >> return (u s)) s
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       (\(c, s2) -> unStreamT (fiddle u >> return (u c)) s2) (s, s)
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s ->
>       return (s, s)
>         >>= (\(c, s2) -> unStreamT (fiddle u >> return (u c)) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s ->
>       unStreamT (StreamT $ \s1 -> return (s1, s1)) s
>         >>= (\(c, s2) -> unStreamT (fiddle u >> return (u c)) s2)
> 
>   -- (condense rest)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(c, s2) -> unStreamT (fiddle u >> return (u c)) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s ->
>       unStreamT rest s
>         >>= (\(c, s2) -> unStreamT ((\a -> fiddle u >> return (u a)) c) s2)
> 
>   -- (condense >>=)
> 
>   , rest >>= (\a -> fiddle u >> return (u a))
>   ]

:::::::::::

Next we show that if `m` is a lawful instance of `MonadStream` with stream type `t` different from `s`, then so is `StreamT s m`.

First: `StreamT s m` satisfies the next/rest law:

::: example

> _StreamT_next_rest_law_lifted
>   :: forall s t m
>    . ( IsStream t, IsStream s, MonadStream t m, Compare s t ~ NotEqual )
>   => [StreamT s m (StreamValue t, t)]
> _StreamT_next_rest_law_lifted =
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

Second: `StreamT s m` satisfies the rest/rest law with stream type `t`:

::: example

> _StreamT_rest_rest_law_lifted
>   :: forall s t m a
>    . ( MonadStream t m, IsStream t, IsStream s, Compare s t ~ NotEqual )
>   => (t -> t -> StreamT s m a)
>   -> [StreamT s m a]
> _StreamT_rest_rest_law_lifted k =
> 
>   [ rest >>= (\u -> rest >>= k u)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (expand rest)
> 
>   , StreamT $ \s1 ->
>       unStreamT (lift rest) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (expand lift)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       (rest >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2)
> 
>   -- (associative law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\(a, s2) -> unStreamT ((\u -> rest >>= k u) a) s2) (c, s1))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT ((\u -> rest >>= k u) c) s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT (rest >>= k c) s1)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT (StreamT $ \s -> unStreamT rest s
>           >>= (\(a, s2) -> unStreamT (k c a) s2)) s1)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\s -> unStreamT rest s >>= (\(a, s2) -> unStreamT (k c a) s2)) s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT rest s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (expand rest)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT (lift rest) s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (expand lift)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT (StreamT $ \s -> rest
>           >>= (\b -> return (b, s))) s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\s -> rest >>= (\b -> return (b, s))) s1
>           >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>= (\b -> return (b, s1))
>           >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (associative law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\b -> return (b, s1)) d >>= (\(a, s2) -> unStreamT (k c a) s2)))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> return (d, s1) >>= (\(a, s2) -> unStreamT (k c a) s2)))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\(a, s2) -> unStreamT (k c a) s2) (d, s1)))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> unStreamT (k c d) s1))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>=
>           (\d -> (\u v -> unStreamT (k u v) s1) c d))
> 
>   -- (η-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> rest >>= (\u v -> unStreamT (k u v) s1) c)
> 
>   -- (rest/rest law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\u v -> unStreamT (k u v) s1) c c)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT (k c c) s1)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> unStreamT ((\u -> k u u) c) s1)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c ->
>           (\(a, s2) -> unStreamT ((\u -> k u u) a) s2) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       rest >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2))
> 
>   -- (associativity law in m)
> 
>   , StreamT $ \s1 ->
>       (rest >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       (\s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> rest >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense lift)
> 
>   , StreamT $ \s1 ->
>       unStreamT (lift rest) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense rest)
> 
>   , StreamT $ \s1 ->
>       unStreamT rest s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , rest >>= (\u -> k u u)
>   ]

:::::::::::

Third: `StreamT s m` satisfies the fiddle/id law with stream type `t`:

::: example

> _StreamT_fiddle_id_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t, IsStream s, Compare s t ~ NotEqual )
>   => [StreamT s m ()]
> _StreamT_fiddle_id_law_lifted =
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

Fourth: `StreamT s m` satisfies the fiddle/compose law with stream type `t`:

::: example

> _StreamT_fiddle_compose_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t, IsStream s, Compare s t ~ NotEqual )
>   => (t -> t) -> (t -> t)
>   -> [StreamT s m ()]
> _StreamT_fiddle_compose_law_lifted u v =
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

Fifth: `StreamT s m` satisfies the fiddle/rest law:

::: example

> _StreamT_fiddle_rest_law_lifted
>   :: forall s t m
>    . ( MonadStream t m, IsStream t, IsStream s, Compare s t ~ NotEqual )
>   => (t -> t)
>   -> [StreamT s m t]
> _StreamT_fiddle_rest_law_lifted u =
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



MonadReadOnly Laws
------------------

[@streamt-monad-read-only-lawful]()

TODO



MonadReadWrite Laws
-------------------

[@streamt-monad-read-write-lawful]()

There is a `MonadReadWrite` instance for `StreamT s m` that we need to verify. Recall the laws for [`MonadReadWrite`](@monad-read-write):

Put/Put Law
  : `put u1 >> put u2` is equivalent to `put u2`

Put/Get Law
  : `put u >> get` is equivalent to `put u >> return u`

Get/Put Law
  : `get >>= put` is equivalent to `return ()`

Get/Get Law
  : `get >>= (\u -> get >>= k u)` is equivalent to `get >>= (\u -> k u u)`

Here we show that if `m` is a lawful instance of `MonadReadWrite` with state type `t` different from `s`, then so is `StreamT s m`.

First: `StreamT s m` satisfies the put/put law:

::: example

> _StreamT_put_put_law_lifted
>   :: forall s t m
>    . ( IsStream s, MonadReadWrite t m )
>   => t -> t
>   -> [StreamT s m ()]
> _StreamT_put_put_law_lifted u1 u2 =
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

Second: `StreamT s m` satisfies the put/get law:

::: example

> _StreamT_put_get_law_lifted
>   :: forall s t m
>    . ( IsStream s, MonadReadWrite t m )
>   => t
>   -> [StreamT s m t]
> _StreamT_put_get_law_lifted u =
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

Third: `StreamT s m` satisfies the get/put law.

::: example

> _StreamT_get_put_law_lifted
>   :: forall s t m
>    . ( IsStream s, MonadReadWrite t m )
>   => [StreamT s m ()]
> _StreamT_get_put_law_lifted =
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

Fourth: `StreamT s m` satisfies the get/get law.

::: example

> _StreamT_get_get_law_lifted
>   :: forall s t m a
>    . ( IsStream s, MonadReadWrite t m )
>   => (t -> t -> StreamT s m a)
>   -> [StreamT s m a]
> _StreamT_get_get_law_lifted k =
> 
>   [ get >>= (\u -> get >>= k u)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       unStreamT get s1
>         >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2)
> 
>   -- (expand get)
> 
>   , StreamT $ \s1 ->
>       unStreamT (lift get) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2)
> 
>   -- (expand lift)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       (\s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       (get >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2)
> 
>   -- (associative law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\(a, s2) -> unStreamT ((\u -> get >>= k u) a) s2) (c, s1))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT ((\u -> get >>= k u) c) s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT (get >>= k c) s1)
> 
>   -- (expand >>=)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT (StreamT $ \s -> unStreamT get s
>           >>= (\(a, s2) -> unStreamT (k c a) s2)) s1)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\s -> unStreamT get s >>= (\(a, s2) -> unStreamT (k c a) s2)) s1)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT get s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (expand get)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT (lift get) s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (expand lift)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT (StreamT $ \s -> get
>           >>= (\b -> return (b, s))) s1 >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\s -> get >>= (\b -> return (b, s))) s1
>           >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>= (\b -> return (b, s1))
>           >>= (\(a, s2) -> unStreamT (k c a) s2))
> 
>   -- (associative law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\b -> return (b, s1)) d >>= (\(a, s2) -> unStreamT (k c a) s2)))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> return (d, s1) >>= (\(a, s2) -> unStreamT (k c a) s2)))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\(a, s2) -> unStreamT (k c a) s2) (d, s1)))
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> unStreamT (k c d) s1))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>=
>           (\d -> (\u v -> unStreamT (k u v) s1) c d))
> 
>   -- (η-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> get >>= (\u v -> unStreamT (k u v) s1) c)
> 
>   -- (get/get law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\u v -> unStreamT (k u v) s1) c c)
> 
>   -- (β-reduction)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT (k c c) s1)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> unStreamT ((\u -> k u u) c) s1)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c ->
>           (\(a, s2) -> unStreamT ((\u -> k u u) a) s2) (c, s1))
> 
>   -- (left identity law in m)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> return (c, s1)
>           >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2))
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       get >>=
>         (\c -> (\b -> return (b, s1)) c
>           >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2))
> 
>   -- (associativity law in m)
> 
>   , StreamT $ \s1 ->
>       (get >>= (\b -> return (b, s1)))
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (β-expansion)
> 
>   , StreamT $ \s1 ->
>       (\s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (unStreamT . StreamT == id)
> 
>   , StreamT $ \s1 ->
>       unStreamT (StreamT $ \s -> get >>= (\b -> return (b, s))) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense lift)
> 
>   , StreamT $ \s1 ->
>       unStreamT (lift get) s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense get)
> 
>   , StreamT $ \s1 ->
>       unStreamT get s1
>         >>= (\(a, s2) -> unStreamT ((\u -> k u u) a) s2)
> 
>   -- (condense >>=)
> 
>   , get >>= (\u -> k u u)
>   ]

:::::::::::
