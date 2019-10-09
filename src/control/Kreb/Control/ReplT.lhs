---
title: "The Read, Eval, Print Loop"
subtitle: "Kreb.Control.ReplT"
---



<div class='frontmatter'>

> module Kreb.Control.ReplT (
>     ReplParams(..)
>   , ReplT
>   , runReplT
>   , loopReplT
> ) where

</div>



Interactive Systems
-------------------

Text editors are meant to be used by people. In particular, text editors are meant to interact with, and take direction from, an agent many orders of magnitude slower than themselves. During typical usage the human will spend much more time thinking than the machine will spend working. This pattern of interaction lends itself well to a model called the _read, evaluate, print loop_, or "REPL" for short.

You are probably familiar with REPLs, if not necessarily by that name. _Command line environments_ such as shells and language interpreters are the classic example, but the model extends much more broadly than this. We can call a system a REPL if its operation can be divided into three phases which cycle one after the other indefinitely:

* The *Read* phase, where the system accepts _input_ via some agreed upon interface;
* The *Eval* phase, where the system _updates_ its internal state based on the input;
* The *Print* phase, where the system emits _output_ representing its internal state.

In a text editor, the Read phase will generally involve accepting keyboard or mouse events, the Eval phase will involve updating a text buffer in memory, and the Print phase will involve writing a buffer to the screen, followed by a (relatively) long wait for the next Read cycle.

Two properties of the REPL pattern make it especially handy as a tool for designing software. First, it is extremely general. Virtually all interactive software can be modeled with it, since it is basically a thin layer on top of a state machine. Second, and crucially, the REPL pattern naturally maintains hard boundaries between the three phases. The Eval phase should not be reading in-band input, and the Print phase should not be updating state. This division of labor among the phases helps us manage complexity.

Because it is so general we can express the REPL pattern in terms of just a few atomic pieces: the Read, Eval, and Print phases, along with two more added for practicality. We also have an Init phase, which runs before the main loop and sets up the environment, and the Exit phase, which cleans up when the loop needs to stop.

We can wrap these 5 functions in a data type for convenience.

> -- | The essential pieces of a REPL. Uses the following type parameters:
> --
> -- * @act@ : semantic /actions/ accepted by the evaluator
> -- * @sig@ : out-of-band /signals/ (e.g. errors)
> -- * @env@ : read-only state
> -- * @st@  : mutable state
> -- * @m@   : effect monad
> 
> data ReplParams act env sig st m = ReplParams
>   { _Init  :: env -> st -> m (Either sig st)        --  | Startup
>   , _Read  :: env -> st -> m act                    --  | Get input
>   , _Eval  :: env -> st -> act -> m (Either sig st) --  | Update state
>   , _Print :: env -> st -> m ()                     --  | Send output
>   , _Exit  :: sig -> m ()                           --  | Cleanup
>   }

Note that our REPL functions collectively use several type parameters which the application will have to specify. These include the type of _actions_ understood by the Read phase and accepted by the Eval phase, as well as a type of _signals_ for communicating out-of-band status such as exceptions. We also have read-only and mutable state to be used in the underlying monad, `m`.



The ReplT Transformer
---------------------

Given a set of REPL parameters, we can express the logic of the read, eval, print loop at a very high level. We'll implement this in the form of a monad transformer, `ReplT`. This type bundles up the concept of a REPL over an arbitrary effect monad `m`.

> newtype ReplT act env sig st m a = ReplT
>   { unReplT
>       :: ReplParams act env sig st m
>       -> env -> st
>       -> m (a, st)
>   }

Note that, as a monad transformer, `ReplT` essentially augments the monad `m` with an extra bit of mutable state and two bits of read-only state. As usual, we can evaluate a `ReplT` computation by providing inital values for this state.

> runReplT
>   :: ( Monad m )
>   => ReplParams act env sig st m -> env -> st
>   -> ReplT act env sig st m a -> m a
> runReplT next env st (ReplT x) =
>   fst <$> x next env st

We also need to specify `Functor`, `Applicative`, and `Monad` instances for `ReplT`; this code is standard stuff.

> instance
>   ( Monad m
>   ) => Functor (ReplT act env sig st m)
>   where
>     fmap f (ReplT x) =
>       ReplT $ \next env st1 -> do
>         (a, st2) <- x next env st1
>         return (f a, st2)
> 
> instance
>   ( Monad m
>   ) => Applicative (ReplT act env sig st m)
>   where
>     pure a = ReplT $ \_ _ st ->
>       pure (a, st)
> 
>     (ReplT f) <*> (ReplT x) =
>       ReplT $ \next env st1 -> do
>         (g, st2) <- f next env st1
>         (a, st3) <- x next env st2
>         return (g a, st3)
> 
> instance
>   ( Monad m
>   ) => Monad (ReplT act env sig st m)
>   where
>     return = pure
> 
>     (ReplT x) >>= f =
>       ReplT $ \next env st1 -> do
>         (a, st2) <- x next env st1
>         unReplT (f a) next env st2

And finally, we can lift a monadic value into `ReplT m`.

> liftReplT
>   :: ( Monad m )
>   => m a -> ReplT act env sig st m a
> liftReplT x = ReplT $ \_ _ st -> do
>   a <- x
>   return (a, st)

Effectively, `ReplT` will let us add REPL functionality to an arbitrary monad `m`. In a "real" program `m` will be `IO`. But we will also have the freedom to swap out `IO` for another type. In particular we can use it to run computations in a mock environment.



Expressing the Loop
-------------------

We're nearly prepared to express the L in REPL. The logic will be more clear if we first define some lifted versions of the parameter functions to handle threading state.

> _init
>   :: ( Monad m )
>   => ReplT act env sig st m (Maybe sig)
> _init = ReplT $ \params env st1 -> do
>   result <- _Init params env st1
>   case result of
>     Left sig -> return (Just sig, st1)
>     Right st2 -> return (Nothing, st2)
> 
> _read
>   :: ( Monad m )
>   => ReplT act env sig st m act
> _read = ReplT $ \params env st -> do
>   act <- _Read params env st
>   return (act, st)
> 
> _eval
>   :: ( Monad m )
>   => act -> ReplT act env sig st m (Maybe sig)
> _eval act = ReplT $ \params env st1 -> do
>   result <- _Eval params env st1 act
>   return $ case result of
>     Right st2 -> (Nothing, st2)
>     Left sig -> (Just sig, st1)
> 
> _print
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> _print = ReplT $ \params env st -> do
>   _Print params env st
>   return ((), st)
> 
> _exit
>   :: ( Monad m )
>   => sig -> ReplT act env sig st m ()
> _exit sig = ReplT $ \params _ st -> do
>   _Exit params sig
>   return ((), st)

Now for the main event. `loopReplT` is the top-level function we expose for running REPLs.

> loopReplT
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> loopReplT = do
>   result <- _init
>   case result of
>     Nothing -> _print >> _loop
>     Just sig -> _exit sig
> 
> _loop
>   :: ( Monad m )
>   => ReplT act env sig st m ()
> _loop = do
>   result <- _read >>= _eval
>   case result of
>     Nothing -> _print >> _loop
>     Just sig -> _exit sig
