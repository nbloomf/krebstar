---
title: The Kreb Editor
author: nbloomf
---

<section>

This is the user manual, design documentation, and source code of the `krebedit` text editor. As of this writing the project is very much Not Ready for Prime Time and supports exactly zero users. It is under rapid development though, and I hope for it to be "self-hosting" -- featureful enough that I can develop krebedit _in_ krebedit -- very soon.

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
1. Requirements
1. Design
1. The Editor Core
    1. [The Read-Eval-Print Loop](html/ReplT.html)
    1. Buffers
    1. Text Boxes
1. The Language
    1. Virtual Machine
    1. Expressions
    1. Type System



Appendices
----------

1. Data Structures
    1. [Finger Trees](html/FingerTree.html)
    1. [One-Pointed Lists](html/OnePointedList.html)
    1. Seq
    1. Two-Pointed Lists
1. Testing Library
1. Bibliography
1. Colophon
