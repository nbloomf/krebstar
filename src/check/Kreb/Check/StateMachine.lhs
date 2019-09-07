> {-# LANGUAGE
>     TypeFamilies
>   , ExistentialQuantification
>   , FlexibleContexts
>   , RankNTypes
> #-}

> module Kreb.Check.StateMachine where

> import Data.List (unlines)

> import Kreb.Check.Arb
> import Kreb.Check.Check
> import Kreb.Check.Tests

> class
>   ( Arb (Transition t)
>   , Show (Transition t)
>   , Prune (Transition t)
>   ) => StateMachine t
>   where
>     data Transition t :: *
> 
>     transition :: Transition t -> t -> t

> data Impl t = forall a. Impl
>   { elucidate  :: String
>   , antecedent :: t -> Maybe (a, String)
>   , consequent :: a -> t -> Maybe String
>   }
> 
> data Cond t = Cond
>   (Transition t -> [Impl t])

> instance Semigroup (Cond t) where
>   (Cond f) <> (Cond g) =
>     Cond $ \x -> (f x) ++ (g x)
> 
> instance Monoid (Cond t) where
>   mempty = Cond $ const []

> data Analysis
>   = AsExpected
>   | Exceptional [String] [(String, String, String)]
>   deriving (Eq, Show)
> 
> instance Semigroup Analysis where
>   x <> y = case x of
>     AsExpected -> y
>     Exceptional vs1 ws1 ->
>       case y of
>         AsExpected -> x
>         Exceptional vs2 ws2 ->
>           Exceptional (vs1 ++ vs2) (ws1 ++ ws2)
> 
> instance Monoid Analysis where
>   mempty = AsExpected

> checkImpl
>   :: ( StateMachine t )
>   => Transition t -> t -> Impl t -> Analysis
> checkImpl t st (Impl name p q) =
>   case p st of
>     Nothing -> Exceptional [name] []
>     Just (a, why) ->
>       let m = q a (transition t st) in
>       case m of
>         Just prob ->
>           Exceptional [] [(name, why, prob)]
>         Nothing -> AsExpected
> 
> checkImpls
>   :: ( StateMachine t )
>   => Transition t -> t -> [Impl t] -> Analysis
> checkImpls t st =
>   mconcat . map (checkImpl t st)

> data StateTrace t
>   = StateTrace [(Transition t, Analysis)]
> 
> instance
>   ( StateMachine t
>   ) => Show (StateTrace t)
>   where
>     show (StateTrace xs) = unlines $
>       zipWith step [1..] xs
>       where
>         step :: (StateMachine t) => Int -> (Transition t, Analysis) -> String
>         step k (tr, a) = case a of
>           AsExpected -> concat
>             [ show k, ": ", show tr ]
>           Exceptional _ [] -> concat
>             [ show k, ": ", show tr ]
>           Exceptional _ zs -> concat
>             [ show k, ": ", show tr, "\n"
>             , concat $ map prob zs
>             ]
> 
>         prob :: (String, String, String) -> String
>         prob (name, why, bad) = concat $ filter (/= []) $
>           [ if null name then "" else "    check: " ++ name ++ "\n"
>           , if null why then "" else "    precondition: " ++ why ++ "\n"
>           , if null bad then "" else "    post violation: " ++ bad
>           ]
> 
> instance
>   ( StateMachine t
>   ) => Checkable (StateTrace t)
>   where
>     check tr@(StateTrace xs) =
>       let
>         isError :: Analysis -> Bool
>         isError z = case z of
>           AsExpected -> False
>           Exceptional vs ws -> not $ null ws
>       in if any (isError . snd) xs
>         then reject $ concat
>           [ "\nInvalid state trace\n"
>           , "-------------------\n"
>           , show tr
>           ]
>         else accept

> checkTransitions
>   :: ( StateMachine t )
>   => Cond t -> [Transition t] -> t
>   -> StateTrace t
> checkTransitions (Cond f) ts st0 = StateTrace $ h ts st0
>   where
>     h ws st = case ws of
>       [] -> []
>       u:us ->
>         let
>           this = checkImpls u st (f u)
>           rest = h us (transition u st)
>         in (u, this) : rest




















