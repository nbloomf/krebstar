---
title: The Kreb Editor
author: nbloomf
---

<section>

This is the user manual, design documentation, and source code of the `krebedit` text editor. As of this writing the project is very much Not Ready for Prime Time and supports exactly zero users. It is under rapid development though, and I hope for it to be "self-hosting" -- featureful enough that I can develop krebedit _in_ krebedit -- very soon.

The table of contents below is _aspirational_; most of it doesn't exist yet.

</section>



User Manual
-----------

1. Overview
1. Installation
1. Getting Started
1. Reference
    1. Editing
    1. Navigation
    1. Scripting
1. Release Notes



Design and Implementation
-------------------------

1. [Project Rationale](html/Rationale.html)
1. The Editor Core
    1. [The Read-Eval-Print Loop](html/ReplT.html)
    1. [Buffers](html/Buffer.html)
    1. [Sized Buffers](html/SizedBuffer.html)
    1. [Text Boxes](html/TextBox.html)
1. The Language
    1. Virtual Machine
    1. Expressions
    1. Type System
1. The Terminal Frontend



Appendices
----------

1. Data Structures
    1. [Finger Trees](html/FingerTree.html)
    1. [One-Pointed Lists](html/OnePointedList.html)
    1. [Sequences](html/Sequence.html)
    1. [Run Length Encoding](html/RunLengthEncoding.html)
    1. [Two-Pointed Lists](html/TwoPointedList.html)
    1. [Red-Black Trees](html/RedBlackTree.html)
1. Testing Library
1. Bibliography
1. Colophon
