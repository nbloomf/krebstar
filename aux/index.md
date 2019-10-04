---
title: The Kreb Editor
author: nbloomf
---

Introduction
------------

This text is the user manual, design documentation, and source code of the `krebedit` text editor. As of this writing the project is very much Not Ready for Prime Time, as it supports exactly zero users. It is under rapid development though, and I hope for it to be "self-hosting" -- featureful enough that I can develop krebedit _in_ krebedit -- very soon.

In lieu of practically usable software, and to give a taste of the plans for the project, here is a very rough overview of the components and their goals.

* At the base is a _minimal core_ supporting the essential functions of an editor: modeling and manipulating text buffers. This component is minimally concerned with user input, display, and file IO, and the API it exposes should be as compact and orthogonal as possible.
* Facing the user we have one or more _frontends_ for interacting with the core. A terminal interface is the simplest (and currently only) one of these, but the design is meant to allow, for instance, a native GUI or web-based frontend.
* For extensibility we have a built-in _command language_ that can also drive the core. This language is statically typed and concatenative, and designed as a separate component suitable for inclusion in other projects.

Even at this early stage the core of each of these pieces is mostly in place. But it turns out that the hard part of building a text editor is not the core components -- drawing to the screen, editing buffers -- but rather the dozens or hundreds of policy level decisions about API design. It is very important to me that interfaces be thoughtful and, to the extent possible, _natural_; achieving this is a fun puzzle.

Of course the main goal of this project is to produce a text editor I can use in my daily work, and I'll mainly judge my success on this metric. (I write and edit a _lot_ of text.) But I have some auxilliary goals as well.

* To learn more about the design and structure of mid-sized projects
* To 



User Manual
-----------

1. What to Expect
1. Getting Started
1. Editing
1. Navigation
1. Scripting



Design and Implementation
-------------------------

1. Project Organization
1. The Editor Core
    1. [The Read, Eval, Print Loop](html/ReplT.html)
    1. Buffers
    1. Text Boxes
1. The Language



Appendices
----------

1. Data Structures
    1. Finger Trees
    1. Zippered Finger Trees
1. Testing Library
