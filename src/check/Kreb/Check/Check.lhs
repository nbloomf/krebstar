> {-# LANGUAGE
>     ScopedTypeVariables
> #-}

> module Kreb.Check.Check where

> import System.Random (split, newStdGen, StdGen)
> import Data.List (find)

> import Kreb.Check.Seeded
> import Kreb.Check.Arb





Outcome of a single test case

> data Outcome
>   = Accept
>   | Discard Reason
>   | Reject Reason [ShowArg]
>   deriving Show
> 
> type Reason = String
> type ShowArg = String
> 
> noteArg :: ShowArg -> Outcome -> Outcome
> noteArg arg z = case z of
>   Accept -> Accept
>   Discard msg -> Discard msg
>   Reject msg args -> Reject msg (arg:args)
> 
> isFailure :: Rose Outcome -> Bool
> isFailure (Rose z _) = case z of
>   Reject _ _ -> True
>   _ -> False





> data Rose a
>   = Rose a [Rose a]
>   deriving (Eq, Show)
> 
> instance Functor Rose where
>   fmap f (Rose a as) =
>     Rose (f a) (map (fmap f) as)
> 
> unfurl
>   :: (a -> [a]) -> a -> Rose a
> unfurl f x =
>   Rose x (map (unfurl f) (f x))
> 
> joinRose :: Rose (Rose a) -> Rose a
> joinRose (Rose (Rose x xs) ys) =
>   Rose x (map joinRose ys ++ xs)





> newtype Check = Check
>   { unCheck :: (Seeded (Rose Outcome)) }
> 
> runCheck
>   :: Check -> Seed -> Rose Outcome
> runCheck (Check x) s =
>   runSeeded s x
> 
> minimizeFailure
>   :: [Rose Outcome] -> Int
>   -> (Int, Int, Reason, [ShowArg])
>   -> (Int, Int, Reason, [ShowArg])
> minimizeFailure next m (prunes, pruneAttempts, msg, args) =
>   case find isFailure $ take m next of
>     Nothing -> (prunes, pruneAttempts + length next, msg, args)
>     Just (Rose (Reject msg0 args0) ts) ->
>       minimizeFailure ts m (1 + prunes, 0, msg0, args0)





> class Checkable t where
>   check :: t -> Check



> instance Checkable Check where
>   check = id



> instance Checkable Outcome where
>   check x = Check $ return $ Rose x []
> 
> accept :: Check
> accept = check Accept
> 
> discard :: Reason -> Check
> discard msg = check $ Discard msg
> 
> reject :: Reason -> Check
> reject msg = check $ Reject msg []

> claimEqual
>   :: ( Eq a, Show a )
>   => a -> a -> Check
> claimEqual x y =
>   if x == y
>     then accept
>     else reject $ concat
>       [ "expecting ", show x
>       , " to equal ", show y ]
> 
> claimLT
>   :: ( Ord a, Show a )
>   => a -> a -> Check
> claimLT x y =
>   if x < y
>     then accept
>     else reject $ concat
>       [ "expecting ", show x
>       , " to be less than ", show y ]
> 
> claimLEQ
>   :: ( Ord a, Show a )
>   => a -> a -> Check
> claimLEQ x y =
>   if x <= y
>     then accept
>     else reject $ concat
>       [ "expecting ", show x
>       , " to be less than or equal to ", show y ]

> (.&&.)
>   :: ( Checkable check1, Checkable check2 )
>   => check1 -> check2 -> Check
> u .&&. v =
>   let
>     Check x = check u
>     Check y = check v
>   in Check $ do
>     Rose a _ <- x
>     case a of
>       Accept -> y
>       _ -> x
> 
> checkAll
>   :: (Checkable check)
>   => [check] -> Check
> checkAll = foldr (.&&.) accept
> 
> (.||.) :: Check -> Check -> Check
> (Check x) .||. (Check y) = Check $ do
>   Rose a _ <- x
>   case a of
>     Accept -> x
>     _ -> y
> 
> checkAny :: [Check] -> Check
> checkAny = foldr (.||.) accept



> provisio
>   :: ( Checkable check )
>   => [(String, Bool)] -> check -> Check
> provisio ps ch =
>   case fst <$> find (not . snd) ps of
>     Nothing -> check ch
>     Just msg -> discard msg



> instance Checkable Bool where
>   check p = Check $ return $
>     case p of
>       True ->
>         Rose Accept []
>       False ->
>         Rose (Reject "False =/= True" []) []



> instance
>   ( Show a, Arb a, Prune a, Checkable check
>   ) => Checkable (a -> check)
>   where
>     check = forEach arb prune
> 
> forEach
>   :: ( Show a, Checkable check )
>   => Seeded a -> (a -> [a]) -> (a -> check) -> Check
> forEach gen sh f = Check $ Seeded $ \env ->
>   let
>     (seed1, seed2) = split $ _stdgen env
>     arg = runSeeded (env { _stdgen = seed1 }) gen
>   in runCheck (results sh arg f) (env { _stdgen = seed2 })
> 
> results
>   :: forall a t
>    . ( Show a, Checkable t )
>   => (a -> [a]) -> a -> (a -> t) -> Check
> results f arg ch = Check $ Seeded $ \env ->
>   let
>     result :: a -> Rose Outcome
>     result x = fmap (noteArg $ show x) $
>       runSeeded env (unCheck $ check $ ch x)
>   in joinRose $ fmap result $ unfurl f arg
