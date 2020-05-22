---
title: The Kreb Editor
author: nbloomf
---

Introduction
------------

This is the user manual, design documentation, and source code of the `krebedit` text editor.

That description is more than a little generous -- let's call it _aspirational_. `krebedit` only just barely exists at this point. It started out as a project just to see if I could do it, and since the world isn't exactly crying out for yet another text editor, I have decided to lean waaay in to the yak-shaving aspect of this as an ongoing learning project. In lieu of the usual pressure to ship a product I am putting it on the web for others to learn from, even if that learning is mostly of the "cautionary tale" variety.

To that end we've pressed hard into doing things you probably shouldn't do in a "real" software project, like using literate programming techniques and writing proofs and rewriting code you'd normally use a library for, just to make sure we understand how it works. We're also doing things we probably _should_ do in real projects but usually don't, like interacting with the literature and aggressively refactoring.

So this is more like a lab notebook -- we're running an experiment on how to design and build a midsized Haskell project, and these are the notes.

I've tried to avoid pulling in dependencies when it's at all reasonable (and even sometimes when it's not). This means reimplementing some standard functionality, working either from research papers or by cloning existing (permissively licensed) libraries. It also means the code here doesn't have the benefit of years of tweaks and efficiency improvements that a mature library has, but in exchange we don't have to make any compromises about APIs or poke shims into `.Extras` modules and can easily inspect the whole stack. Perhaps surprisingly, starting out by writing our own testing frameworks doesn't add _that_ much code.



1. **Monads and Monad Transformers**
    1. [Monads and Side Effects](../html/lib/kreb-control/src/Kreb/Control/Monad.html)
    1. [EnvT](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/EnvT.html) and [proofs of lawfulness](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/EnvT/Proofs.html)
    1. [StateT](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/StateT.html) and [proofs of lawfulness](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/StateT/Proofs.html)
    1. [StreamT](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/StreamT.html) and [proofs of lawfulness](../html/lib/kreb-control/src/Kreb/Control/Monad/Trans/StreamT/Proofs.html)
1. **Pretty Printing**
    1. [Abstract Documents](../html/lib/kreb-format/src/Kreb/Format/Doc.html)
    1. [A Document Algebra](../html/lib/kreb-format/src/Kreb/Format/Combinator.html)
    1. [The Display Class](../html/lib/kreb-format/src/Kreb/Format/Display.html)
1. **Unit Testing**
    1. [Dirt Cheap Unit Tests](../html/lib/kreb-unit/src/Kreb/Unit/Declare.html)
    1. [Declarable Types](../html/lib/kreb-unit/src/Kreb/Unit/Declarable.html)
    1. [Test Helpers and Patterns](../html/lib/kreb-unit/src/Kreb/Unit/Helpers.html)
1. **Property Testing**
    1. [Pseudorandom Data](../html/lib/kreb-prop/src/Kreb/Prop/Sample.html)
    1. [Generatable Types](../html/lib/kreb-prop/src/Kreb/Prop/Arb.html)
    1. [Test Outcomes](../html/lib/kreb-prop/src/Kreb/Prop/Check.html)
    1. [Running Tests](../html/lib/kreb-prop/src/Kreb/Prop/Tests.html)
    1. [Shrinkable, Showable Functions](../html/lib/kreb-prop/src/Kreb/Prop/Fun.html)
1. **Just Enough Category Theory**
    1. [Categories in Haskell](../html/lib/kreb-category/src/Kreb/Category/Class/CategoryC.html)
    1. [Functors](../html/lib/kreb-category/src/Kreb/Category/Class/FunctorC.html)
