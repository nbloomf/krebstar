> {-# LANGUAGE
>     TypeFamilies
>   , FlexibleContexts
>   , EmptyDataDeriving
>   , StandaloneDeriving
>   , FlexibleInstances
> #-}

> module Kreb.Prop.Build where
> 
> import Kreb.Prop.Sample
> import Kreb.Prop.Arb
> import Kreb.Prop.Fun



> class
>   ( Show (Plan t), Arb (Plan t), Prune (Plan t)
>   ) => Build t
>   where
>     data Plan t :: *
> 
>     build :: Plan t -> t





Unit
----

> instance Build () where
>   data Plan ()
>     = PlanUnit
>     deriving (Eq, Show)
> 
>   build z = case z of
>     PlanUnit -> ()
> 
> instance Arb (Plan ()) where
>   arb = return PlanUnit
> 
> instance Prune (Plan ()) where
>   prune _ = []



Bool
----

> instance Build Bool where
>   data Plan Bool
>     = PlanBool Bool
>     deriving (Eq, Show)
> 
>   build z = case z of
>     PlanBool p -> p
> 
> instance Arb (Plan Bool) where
>   arb = PlanBool <$> arb
> 
> instance Prune (Plan Bool) where
>   prune _ = []



Int
---

> instance Build Int where
>   data Plan Int
>     = PlanInt Int
>     deriving (Eq, Show)
> 
>   build z = case z of
>     PlanInt k -> k
> 
> instance Arb (Plan Int) where
>   arb = PlanInt <$> arb
> 
> instance Prune (Plan Int) where
>   prune z = case z of
>     PlanInt k ->
>       map PlanInt $ prune k



Maybe
-----

> instance
>   ( Build a
>   ) => Build (Maybe a)
>   where
>     data Plan (Maybe a)
>       = PlanMaybe_Nothing
>       | PlanMaybe_Just (Plan a)
> 
>     build z = case z of
>       PlanMaybe_Nothing -> Nothing
>       PlanMaybe_Just u  -> Just $ build u
> 
> deriving instance
>   ( Show (Plan a)
>   ) => Show (Plan (Maybe a))
> 
> instance
>   ( Arb (Plan a)
>   ) => Arb (Plan (Maybe a))
>   where
>     arb = freq
>       [ (1, return PlanMaybe_Nothing)
>       , (5, PlanMaybe_Just <$> arb)
>       ]
> 
> instance
>   ( Prune (Plan a)
>   ) => Prune (Plan (Maybe a))
>   where
>     prune z = case z of
>       PlanMaybe_Nothing -> []
>       PlanMaybe_Just a ->
>         map PlanMaybe_Just $ prune a




> {-
> 




> instance
>   ( Build a, Build b
>   ) => Build (Either a b)
>   where
>     data Plan (Either a b)
>       = PlanEither_Left (Plan a)
>       | PlanEither_Right (Plan b)
> 
>     data Prob (Either a b)
>       = ProbEither_Left (Prob a)
>       | ProbEither_Right (Prob b)
> 
>     pick = pickFrom2
>       ( fmap PlanEither_Left pick
>       , fmap PlanEither_Right pick
>       )
> 
>     prune z = case z of
>       PlanEither_Left x ->
>         map PlanEither_Left $ prune x
>       PlanEither_Right x ->
>         map PlanEither_Right $ prune x
> 
>     produce z = case z of
>       PlanEither_Left a ->
>         case produce a of
>           Left prob -> Left $ ProbEither_Left prob
>           Right val -> Right $ Left val
>       PlanEither_Right b ->
>         case produce b of
>           Left prob -> Left $ ProbEither_Right prob
> 
>     analyze z = case z of
>       Left a -> PlanEither_Left $ analyze a
>       Right b -> PlanEither_Right $ analyze b
> 
> deriving instance
>   ( Show (Plan a), Show (Plan b)
>   ) => Show (Plan (Either a b))
> 
> deriving instance
>   ( Show (Prob a), Show (Prob b)
>   ) => Show (Prob (Either a b))



> instance
>   ( Build a, Build b
>   ) => Build (a,b)
>   where
>     data Plan (a,b)
>       = PlanTup2 (Plan a) (Plan b)
> 
>     data Prob (a,b)
>       = ProbTup2_1 (Prob a)
>       | ProbTup2_2 (Prob b)
> 
>     pick = PlanTup2
>       <$> pick
>       <*> pick
> 
>     prune (PlanTup2 a b) =
>       [ PlanTup2 a v | v <- prune b ] ++
>       [ PlanTup2 u b | u <- prune a ]
> 
>     produce (PlanTup2 a b) =
>       case produce a of
>         Left prob1 -> Left $ ProbTup2_1 prob1
>         Right val1 -> case produce b of
>           Left prob2 -> Left $ ProbTup2_2 prob2
>           Right val2 -> Right (val1, val2)
> 
>     analyze (a,b) =
>       PlanTup2 (analyze a) (analyze b)
> 
> deriving instance
>   ( Show (Plan a), Show (Plan b)
>   ) => Show (Plan (a,b))
> 
> deriving instance
>   ( Show (Prob a), Show (Prob b)
>   ) => Show (Prob (a,b))





> instance
>   ( Build a, Build b, CoBuild a, Function a
>   ) => Build (To a b)
>   where
>     data Plan (To a b)
>       = PlanTo (To a (Plan b))
> 
>     data Prob (To a b)
> 
>     --pick = fmap PlanFunc $ promoteFun $ \a -> copick (analyze a) pick
> 
>     --prune (PlanTo f) = map (PlanFunc . totalize example) $ pruneTo prune $ function f
> 
> instance Show (Plan (a -> b)) where
>   show _ = "Plan <function>"
> 
> instance Show (Prob (a -> b)) where
>   show _ = "Prob <function>"

> promoteTo :: To a (Arb b) -> Arb (To a b)
> promoteTo z = case z of
>   

> 
> instance
>   ( CoBuild a, CoBuild b
>   ) => CoBuild (Either a b)
>   where
>     copick z = case z of
>       PlanEither_Left a  -> twiddle 0 . copick a
>       PlanEither_Right b -> twiddle 1 . copick b
> 

instance CoArbitrary a => CoArbitrary [a] where
  coarbitrary []     = variant 0
  coarbitrary (x:xs) = variant 1 . coarbitrary (x,xs)


instance (Arbitrary a, CoArbitrary b) => CoArbitrary (a -> b) where
  coarbitrary f gen =
    do xs <- arbitrary
       coarbitrary (map f xs) gen


> -}
