---
title: Declarable Types
author: nbloomf
---

::: frontmatter

> module Kreb.Unit.Declarable where
> 
> import Kreb.Unit.Declare

:::



Declarable Types
----------------

The `Declaration` type is so simple that it barely warrants further comment. In practice it is handy to bundle the exception machinery behind more meaningful types, which we do with the `Declarable` class. These are types which can be converted into a `Declaration`, letting us selectively hide the machinery of IO and exception throwing.

> class Declarable t where
>   declare :: t -> Declaration

Let's describe some `Declarable` types. First of all, `IO` actions themselves are trivially declarable when we throw away the wrapped value:

> instance Declarable (IO a) where
>   declare x = x >> return ()

The unit type is also trivially declarable.

> instance Declarable () where
>   declare = return

Lists of declarable values are declarable; we check each declaration in order.

> instance
>   ( Declarable t
>   ) => Declarable [t]
>   where
>     declare = mapM_ declare

Booleans are also declarable in a sensible, albeit not very useful, way:

> instance Declarable Bool where
>   declare p = if p
>     then return ()
>     else raiseMsgIO "False =/= True"

Tuples are also declarable in a straightforward way. (Note that the order of the tuple values matters -- entries to the left are evaluated (and their effects observed) before those to the right.)

> instance
>   ( Declarable a1, Declarable a2
>   ) => Declarable (a1,a2)
>   where
>     declare (a1,a2) = sequence_
>       [ declare a1, declare a2 ]
> 
> instance
>   ( Declarable a1, Declarable a2, Declarable a3
>   ) => Declarable (a1,a2,a3)
>   where
>     declare (a1,a2,a3) = sequence_
>       [ declare a1, declare a2, declare a3 ]
> 
> instance
>   ( Declarable a1, Declarable a2, Declarable a3, Declarable a4
>   ) => Declarable (a1,a2,a3,a4)
>   where
>     declare (a1,a2,a3,a4) = sequence_
>       [ declare a1, declare a2, declare a3, declare a4 ]
> 
> instance
>   ( Declarable a1, Declarable a2, Declarable a3, Declarable a4
>   , Declarable a5
>   ) => Declarable (a1,a2,a3,a4,a5)
>   where
>     declare (a1,a2,a3,a4,a5) = sequence_
>       [ declare a1, declare a2, declare a3, declare a4
>       , declare a5 ]
> 
> instance
>   ( Declarable a1, Declarable a2, Declarable a3, Declarable a4
>   , Declarable a5, Declarable a6
>   ) => Declarable (a1,a2,a3,a4,a5,a6)
>   where
>     declare (a1,a2,a3,a4,a5,a6) = sequence_
>       [ declare a1, declare a2, declare a3, declare a4
>       , declare a5, declare a6 ]

We can define declarable instances for base types all day. But the real power will come from custom types we define for individual use cases.
