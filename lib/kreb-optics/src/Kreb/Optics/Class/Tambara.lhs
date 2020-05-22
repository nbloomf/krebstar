> module Kreb.Optics.Class.Tambara where
> 
> import Kreb.Category



Tambara Module
--------------

Laws:
- natural in x and y
- dimapC unitor unitor' . tambara == unit
- tambara == dimap compor' compor . tambara . tambara
- ???

> class
>   ( MonoidalActionC objm morm otimes unit objc morc f
>   , MonoidalActionC objm morm otimes unit objd mord g
>   , ProfunctorC objc morc objd mord Hask (->) p
>   ) => Tambara objc morc objd mord objm morm otimes unit f g p
>   where
>     tambara
>       :: ( objc x, objd y, objm w )
>       => p x y -> p (f w x) (g w y)





Existential Optic
-----------------

> data
>   Optic objc morc objd mord objm morm otimes unit f g a b s t
>   where
>     Optic
>       :: ( MonoidalActionC objm morm otimes unit objc morc f
>          , MonoidalActionC objm morm otimes unit objd mord g
>          , objc a, objc s, objd b, objd t, objm x
>          )
>       => morc s (f x a) -> mord (g x b) t
>       -> Optic objc morc objd mord objm morm otimes unit f g a b s t

> instance
>   ( MonoidalActionC objm morm otimes unit objc morc f
>   , MonoidalActionC objm morm otimes unit objd mord g
>   , objc a, objd b
>   ) => ProfunctorC objc morc objd mord Hask (->)
>     (Optic objc morc objd mord objm morm otimes unit f g a b)
>   where
>     dimapC f g (Optic l r) = Optic
>       (comp @objc @morc l f)
>       (comp @objd @mord g r)

> instance
>   ( MonoidalActionC objm morm otimes unit objc morc f
>   , MonoidalActionC objm morm otimes unit objd mord g
>   , objc a, objd b
>   ) => Tambara objc morc objd mord objm morm otimes unit f g
>     (Optic objc morc objd mord objm morm otimes unit f g a b)
>   where
>     tambara (Optic l r) = Optic
>       (comp @objc @morc
>         (compor @objm @morm @otimes @unit @objc @morc)
>         (bimapC @objm @morm @objc @morc @objc @morc
>           (unit @objm @morm) l))
>       (comp @objd @mord
>         (bimapC @objm @morm @objd @mord @objd @mord
>           (unit @objm @morm) r)
>         (compor' @objm @morm @otimes @unit @objd @mord))





Profunctor Optic
----------------

> type
>   ProfOptic objc morc objd mord objm morm otimes unit f g a b s t
>      = forall p
>      . ( Tambara objc morc objd mord objm morm otimes unit f g p
>        , objc a, objc s, objd b, objd t )
>     => p a b -> p s t

> toProfOptic
>   :: forall    objc morc objd mord objm morm otimes unit f g a b s t
>    . Optic     objc morc objd mord objm morm otimes unit f g a b s t
>   -> ProfOptic objc morc objd mord objm morm otimes unit f g a b s t
> toProfOptic (Optic l r) =
>   dimapC @objc @morc @objd @mord @Hask @(->) l r
>     . tambara @objc @morc @objd @mord @objm @morm @otimes @unit

> toOptic
>   :: forall objc morc objd mord objm morm otimes unit f g a b s t
>    . ( MonoidalActionC objm morm otimes unit objc morc f
>      , MonoidalActionC objm morm otimes unit objd mord g
>      , objm unit, objc a, objc s, objc unit, objd b, objd t, objd unit
>      )
>   => ProfOptic objc morc objd mord objm morm otimes unit f g a b s t
>   -> Optic     objc morc objd mord objm morm otimes unit f g a b s t
> toOptic p = p $ Optic
>   (unitor' @objm @morm @otimes @unit @objc @morc @f)
>   (unitor  @objm @morm @otimes @unit @objd @mord @g)
