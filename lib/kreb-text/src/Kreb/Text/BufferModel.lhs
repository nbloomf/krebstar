---
title: A Semantic Model of Text Buffers
author: nbloomf
---

::: contents
* [Algebraic Design](#algebraic-design): APIs are universal algebras
* [A Concrete Model](#a-concrete-model): Math? In _my_ editor?
:::



::: frontmatter

> module Kreb.Text.BufferModel where

:::



Algebraic Design
----------------

One of the most essential pieces of a text editor is the structure that "holds" the text being edited and the collection of editing operations it allows us to perform: I will call this structure a _buffer_. It's vital that this component be powerful enough to perform all the actions we want -- like navigation and manipulating text -- but also do them correctly and efficiently.

Before we try to implement a real buffer structure it will be useful to think about _how it should behave_ at an abstract level. We'll do this in two phases:

* First we'll build a concrete mathematical description of what a text buffer is. We will also implement this description in code without worrying too much about operational characteristics -- the description can be hideously inefficient. What matters is that the description be _complete_ and _easy to reason about_.
* Second we'll try to extract from our concrete description a minimal set of _essential behaviors_ -- these will form a kind of axioms for text editing. What matters here is that the essential behaviors capture how the text editing operations interact with each other without depending on the "internal" details of our concrete description, and thus are generalizable to other implementations.

It may seem like overkill to put this effort into building what is essentially a prototype, and maintaining it in parallel with the real implementation. But doing this algebraic design work will have some useful side effects. Ideally the model will be _obviously correct_ such that we can test the real implementation by comparing its output to that of the model. The abstract behaviors will also serve as useful property tests of the real implementation, and be simpler to reason about without getting bogged down in unnecessary details.

Let's start with a (very abstract) list of features we want out of our text buffers -- we'll call these the **Buffer Criteria**.

1. To represent a sequence of unicode characters;
2. To track one or more disjoint, contiguous subsequences, called "regions";
3. To designate one special region called the "focus";
4. To adjust the position and size of the focus in the sequence;
5. To change which of the regions is designated as the focus;
6. To facilitate "edits" at or near the focus, such as "insert" and "delete".

There are plenty of useful text editor functions this doesn't cover, but that is ok: we're looking for the barest minimum. The idea is to model the most basic behavior required for interactive text editing.

If we think of the focus as a "highlighted" region of text, note that this list of requirements points to an editor with multiple selections. We're doing this on purpose because having multiple selection support built in will make some more sophisticated operations simpler to implement. For instance the result of a text search might be naturally represented as a multiple selection, as would a two-dimensional or box style selection mode.



A Concrete Model
----------------

A _prebuffer_ is a finite sequence of _characters_ from some _character set_. More formally:

::: definition
Given a (fixed) character set $C$, a _prebuffer_ is a function $\beta$ with signature $[1,L] \rightarrow C$ for some natural number $L$. We call this (unique) $L$ the _length_ of the prebuffer.
:::

As a special case, note what happens when $L = 0$; the interval $[1,0]$ is empty, and $\beta$ is the (unique) empty function.

Since a prebuffer $\beta$ is just a function we can extract the character at a given index position $i$ by evaluating $\beta(i)$ (as long as $i$ is in the interval $[1,L]$).

Prebuffers are a decent enough solution to Buffer Criterion 1. Next we deal with the region business. Roughly speaking, a region is a contiguous subsequence of a prebuffer.

::: definition
A _region_ is a triple $\rho = (d_\rho, w_\rho, e_\rho)$ where $d_\rho$ and $w_\rho$ are natural numbers and $e_\rho = \pm 1$. (We will drop the suffixes if $\rho$ is clear in context.) $d_\rho$ is the _index_ of the region, $w_\rho$ is the _width_, and $e_\rho$ is the _parity_. The set of all regions is denoted $\mathrm{Reg}$.
:::

Note that a region by itself has nothing to do with any particular prebuffer -- a region is a kind of subsequence address that can refer to _any_ prebuffer. We nail this down by defining a derived prebuffer like so.

::: definition
Given a prebuffer $\beta : [1,L] \rightarrow C$ and a region $\rho = (d,w,e)$, we define a prebuffer $$\beta_\rho : [1,\mathrm{min}(w,L-d+1)] \rightarrow C$$ by $\beta_\rho(i) = \beta(i+d-1)$. This $\beta_\rho$ is called the $\rho$-_extract_ of $\beta$. Note that if $d > L$, then $\beta_\rho$ is the empty prebuffer.
:::

Regions capture contiguous subsequences of a prebuffer. But Buffer Criterion 2 needs a little more than this; we want to be able to tell whether two regions "overlap". Intuitively, two regions $\rho_1$ and $\rho_2$ overlap if the $\rho_1$- and $\rho_2$-extracts of $\beta$ have any indices in common. Intuitively, the indices of a region are the integers from $d$ to $d+w$ inclusive, so detecting an overlap is straightforward.

::: definition
Let $\rho_1 = (d_1, w_1, e_1)$ and $\rho_2 = (d_2, w_2, e_2)$ be regions. We say that $\rho_1$ _precedes_ $\rho_2$, denoted $\rho_1 \prec \rho_2$, if $d_1 + w_1 < d_2$.
:::

It's pretty straightforward to show that $\prec$ on the set of regions is transitive: if $\rho_1 \prec \rho_2$ and $\rho_2 \prec \rho_3$, then $$d_1 + w_1 < d_2 \leq d_2 + w_2 < d_3$$ as needed. This relation is also irreflexive; note that $d + w \nless d$ for any natural numbers $d$ and $w$. Suppose now that $\rho_1 \prec \rho_2$ and $\rho_2 \prec \rho_1$. We then have $$d_1 \leq d_1 + w_1 < d_2 \leq d_2 + w_2 < d_1,$$ which is absurd. Letting $\preccurlyeq$ denote the reflexive closure of $\prec$, and waving our hands around a bit, we have that $\preccurlyeq$ is a partial order.

We can now give a workable definition for Buffer Criterion 2.

::: definition
Let $t$ be a positive natural number, $\sigma : [1,t] \rightarrow \mathrm{Reg}$ an order preserving map, and $f \in [1,t]$. Then $\langle t, \sigma, f \rangle$ is called a _selection_, of which $f$ is called the _focus_.
:::



::: definition
A _buffer_ is a tuple $B = \langle L, \beta, t, \sigma, f \rangle$ such that the following hold.

1. $L$ is a natural number;
2. $\beta : [1,L] \rightarrow C$ is a map;
3. $t$ is a positive natural number;
4. $\sigma : [1,t] \rightarrow \mathrm{Reg}$ is an order homomorphism;
5. $f$ is a natural number in $[1,t]$.
:::


> data PreBuffer a
>   = PreBuffer [a]
>   deriving (Eq, Show)

> data Region
>   = Region Int Int Sign
>   deriving (Eq, Show)
> 
> data Sign
>   = Pos | Neg
>   deriving (Eq, Show)

> regionPrec :: Region -> Region -> Bool
> regionPrec (Region m1 k1 _) (Region m2 k2 _) =
>   (m1 + k1) <= m2

> data Selection
>   = Selection [Region]
>   deriving (Eq, Show)

> data ModelBuffer a = ModelBuffer
>   { prebuffer :: PreBuffer a
>   , selection :: Selection
>   , focus     :: Int
>   } deriving (Eq, Show)

> class BufferAlgebra buf where
>   type CharSet buf :: *
> 
>   toPreBuffer :: buf -> PreBuffer (CharSet buf)
