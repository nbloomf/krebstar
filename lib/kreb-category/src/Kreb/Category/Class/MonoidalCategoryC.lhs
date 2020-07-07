---
title: Monoidal Categories
author: nbloomf
---

::: frontmatter

> module Kreb.Category.Class.MonoidalCategoryC where
> 
> import Kreb.Category.Class.CategoryC
> import Kreb.Category.Class.BifunctorC

:::



Introduction
------------

A bifunctor $\otimes : \mathcal{C} \times \mathcal{C} \rightarrow \mathcal{C}$ can be thought of as a kind of "multiplication" on the category $\mathcal{C}$. On the level of sets, when some space of things has a multiplication we can start asking whether that operation has nice algebraic properties (associativity, commutativity, idempotence etc.) as well as how it interacts with any ambient structure. One of the simplest examples is called a _monoid_; these arise when a multiplication is associative and has a neutral element.

With a little tweaking, we can steal this idea for categories. Roughly speaking, we want to talk about bifunctors that turn their host categories into monoids. How we do this is complicated by (1) the fact that _equality_ on categories is too strict a concept, so what really matters is _isomorphism_; and (2) categories have additional structure we need to take into account.



Monoidal Bifunctors
-------------------

Let $\mathcal{C}$ be a category and $\otimes : \mathcal{C} \times \mathcal{C} \rightarrow \mathcal{C}$ a bifunctor. Suppose further we have a distinguished object $I$ and three natural families^[In the sense that $\alpha$, $\lambda$, and $\rho$ are natural transformations.] of isomorphisms in $\mathcal{C}$:

- $\alpha_{A,B,C} : A \otimes (B \otimes C) \rightarrow (A \otimes B) \otimes C$, called an _associator_
- $\lambda_{A} : 1 \otimes A \rightarrow A$, called the _left unitor_
- $\rho_{A} : A \otimes 1 \rightarrow A$, called the _right unitor_

Then the structure $(\mathcal{C}, \otimes, 1, \alpha, \lambda, \rho)$ is called a _monoidal category_ if these additional properties hold for all objects $A$, $B$, $C$, and $D$.

The following diagram commutes:

~~~ tikzcd
A \otimes (1 \otimes B)
  \arrow[rr, "\alpha_{A,I,B}"]
  \arrow[rdd, "1_A \otimes \lambda_B"']
    & &
(A \otimes 1) \otimes B
  \arrow[ldd, "\rho_A \otimes 1_B"]
\\
 & & \\
 &
A \otimes B
 &
~~~

That is, we have $$\rho_A \otimes 1_B \circ \alpha_{A,I,B} = 1_A \otimes \lambda_B;$$ and likewise the following diagram commutes:

~~~ tikzcd
A \otimes (B \otimes (C \otimes D))
  \arrow[rr, "\alpha_{A, B, C \otimes D}"]
  \arrow[dd, "1_A \otimes \alpha_{B,C,D}"']
 & &
(A \otimes B) \otimes (C \otimes D)
  \arrow[rr, "\alpha_{A \otimes B, C, D}"]
 & &
((A \otimes B) \otimes C) \otimes D
\\
 & & & & \\
A \otimes ((B \otimes C) \otimes D)
  \arrow[rrrr, "\alpha_{A, B \otimes C, D}"']
 & & & &
(A \otimes (B \otimes C)) \otimes D
  \arrow[uu, "\alpha_{A,B,C} \otimes 1_D"']
~~~

That is, we have $$\alpha_{A \otimes B, C, D} \circ \alpha_{A, B, C \otimes D} = \alpha_{A,B,C} \otimes 1_D \circ \alpha_{A, B \otimes C, D} \circ 1_A \otimes \alpha_{B,C,D}.$$



Translating to Code
-------------------

We can model monoidal categories in Haskell as a multi-parameter type class; the category, bifunctor, and unit (type) are parameters of the class, and the associator and unitors are functions on the class.

> class
>   ( CategoryC obj mor
>   , BifunctorC obj mor obj mor obj mor otimes
>   ) => MonoidalCategoryC obj mor otimes unit
>   where
>     assocL
>       :: ( obj x, obj y, obj z )
>       => mor
>           (otimes x (otimes y z))
>           (otimes (otimes x y) z)
> 
>     assocR
>       :: ( obj x, obj y, obj z )
>       => mor
>           (otimes (otimes x y) z)
>           (otimes x (otimes y z))
> 
>     unitL
>       :: ( obj x, obj unit )
>       => mor (otimes x unit) x
> 
>     unitL'
>       :: ( obj x, obj unit )
>       => mor x (otimes x unit)
> 
>     unitR
>       :: ( obj x, obj unit )
>       => mor (otimes unit x) x
> 
>     unitR'
>       :: ( obj x, obj unit )
>       => mor x (otimes unit x)
