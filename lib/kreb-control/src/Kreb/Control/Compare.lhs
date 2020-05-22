> module Kreb.Control.Compare where


> data Equal
> data NotEqual

> type family Compare a b where
>   Compare a a = Equal
>   Compare a b = NotEqual
