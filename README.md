krebstar
========

krebstar is a few things.

1. An extremely primitive console text editor with no useful features
2. An extremely primitive embeddable statically type-inferred concatenative scripting language based on the BCI calculus, also with no useful features
3. A constellation of smaller single purpose libraries to keep external dependencies minimal

but mostly it is a place for me to play.

Why?
====

I'm not really sure, this just sort of happened



~~~ tikz
\node (P) {$P$};
\node (B) [right of=P] {$B$};
\node (A) [below of=P] {$A$};
\node (C) [below of=B] {$C$};
\node (P1) [node distance=2cm, left of=P, above of=P] {$\overline{P}$};
\draw[->] (P) to node {$f$} (B);
\draw[->] (P) to node [swap] {$g$} (A);
\draw[->] (A) to node [swap] {$f$} (C);
\draw[->] (B) to node {$g$} (C);
\draw[->, bend right] (P1) to node [swap] {$\overline{g}$} (A);
\draw[->, bend left] (P1) to node {$\overline{f}$} (B);
\draw[->, dashed] (P1) to node {$k$} (P);
~~~~~~~~
