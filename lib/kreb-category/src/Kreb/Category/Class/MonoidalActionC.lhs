---
title: Monoidal Actions
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.MonoidalActionC where
> 
> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC
> import Kreb.Category.Class.MonoidalCategoryC

:::



Introduction
------------

A monoidal category is the categorical analog of a monoid. Another useful concept is the _monoid action_; if $M$ is a monoid and $A$ a set, an action is a function $\ast : M \times A \rightarrow A$ that is "compatible" with the monoid structure. The categorical analog is a bifunctor $\ast : \mathcal{M} \times \mathcal{C} \rightarrow \mathcal{C}$ compatible with the monoidal structure on $\mathcal{M}$.

More precisely, suppose we have a monoidal category $(\mathcal{M}, \otimes, 1, \alpha, \lambda, \rho)$, a category $\mathcal{C}$, and a bifunctor $\ast : \mathcal{M} \times \mathcal{C} \rightarrow \mathcal{C}$. Suppose further we have two natural families of isomorphisms in $\mathcal{C}$:

- $\kappa_{M,N,A} : M \ast (N \ast A) \rightarrow (M \otimes N) \ast A$, called an _associator_
- $\mu_{A} : 1 \ast A \rightarrow A$, called a _unitor_

Then the structure $(\mathcal{M}, \otimes, 1, \alpha, \lambda, \rho, \mathcal{C}, \ast, \kappa, \mu)$ is called a _monoidal category action_ or just _monoidal action_ if these additional properties hold for all objects $M$, $N$, $P$ of $\mathcal{M}$ and $A$ of $\mathcal{C}$.

The following diagram commutes:

~~~ tikzcd
(1 \otimes M) \ast A
  \arrow[rr, "\kappa_{1,M,A}"]
  \arrow[rdd, "\lambda_M \ast 1_A"']
    & &
1 \ast (M \ast A)
  \arrow[ldd, "\mu_{M \ast A}"]
\\
 & & \\
 &
M \ast A
 &
~~~

That is, $$\mu_{M \ast A} \circ \kappa_{1,M,A} = \lambda_M \ast 1_A.$$

Likewise the following diagram commutes:

~~~ tikzcd
(M \otimes 1) \ast A
  \arrow[rr, "\kappa_{M,1,A}"]
  \arrow[rdd, "\rho_M \ast 1_A"']
    & &
M \ast (1 \ast A)
  \arrow[ldd, "1_M \ast \mu_A"]
\\
 & & \\
 &
M \ast A
 &
~~~

That is, $$1_M \ast \mu_A \circ \kappa_{M,1,A} = \rho_M \ast 1_A.$$

And finally, the following diagram commutes:

~~~ tikzcd
M \ast (N \ast (P \ast A))
  \arrow[rr, "\kappa_{M, N, P \ast A}"]
  \arrow[dd, "1_M \ast \kappa_{N,P,A}"']
 & &
(M \otimes N) \ast (P \ast A)
  \arrow[rr, "\kappa_{M \otimes N, P, A}"]
 & &
((M \otimes N) \otimes P) \ast A
\\
 & & & & \\
M \ast ((N \otimes P) \ast A)
  \arrow[rrrr, "\kappa_{M, N \otimes P, A}"']
 & & & &
(M \otimes (N \otimes P)) \ast A
  \arrow[uu, "\alpha_{M,N,P} \ast 1_A"']
~~~

That is, $$\kappa_{M \otimes N, P, A} \circ \kappa_{M, N, P \ast A} = \alpha_{M,N,P} \ast 1_A \circ \kappa_{M, N \otimes P, A} \circ 1_M \ast \kappa_{N,P,A}.$$



Monoidal Actions in Haskell
---------------------------

We again translate this concept using a type class -- in this case a class with 7 parameters. In the past type classes like this would be a pain to use, but the `TypeApplications` language extension makes them much more ergonomic.

> class
>   ( MonoidalCategoryC objm morm otimes unit
>   , CategoryC objc morc
>   , BifunctorC objm morm objc morc objc morc f
>   ) => MonoidalActionC objm morm otimes unit objc morc f
>   where
>     unitor
>       :: ( objc x, objm unit )
>       => morc (f unit x) x
> 
>     unitor'
>       :: ( objc x, objm unit )
>       => morc x (f unit x)
> 
>     compor
>       :: ( objm p, objm q, objc x )
>       => morc (f p (f q x)) (f (otimes p q) x)
> 
>     compor'
>       :: ( objm p, objm q, objc x )
>       => morc (f (otimes p q) x) (f p (f q x))
