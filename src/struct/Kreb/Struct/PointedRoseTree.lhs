> module Kreb.Struct.PointedRoseTree (
>     PointedRoseTree()
>   , rewind
> ) where

> import Kreb.Check
> import qualified Kreb.Struct.Sequence as S
> import qualified Kreb.Struct.RoseTree as RT

> data PointedRoseTree a = PointedRoseTree
>   { thorn
>       :: a
>   , root
>       :: Maybe (a, S.Sequence a)
>   , stem
>       :: S.Sequence a
>   , fork
>       :: S.Sequence (RT.RoseTree a)
>   , bush
>       :: S.Sequence
>           ( a
>           , S.Sequence a
>           , S.Sequence (RT.RoseTree a)
>           , S.Sequence (RT.RoseTree a) )
>   } deriving (Eq, Show)

> -- to the head of the most recent branch
> rewind
>   :: PointedRoseTree a -> PointedRoseTree a
> rewind t@(PointedRoseTree a r s f b) = case r of
>   Nothing ->
>     t
>   Just (u, us) ->
>     PointedRoseTree u Nothing
>       (S.moveToInit $ S.prepend us $ S.insertInit a s) f b

> instance
>   ( Arb a
>   ) => Arb (PointedRoseTree a)
>   where
>     arb = do
>       NonNegative sS <- arb
>       NonNegative fS <- arb
>       NonNegative bS <- arb
>       PointedRoseTree
>         <$> arb
>         <*> arb
>         <*> (fmap S.fromList $ vectOf sS $
>               adjustSize (`div` (sS + 2)) arb)
>         <*> (fmap S.fromList $ vectOf fS $
>               adjustSize (`div` (fS + 2)) arb)
>         <*> (fmap S.fromList $ vectOf bS $
>               adjustSize (`div` (bS + 2)) arb)

> instance
>   ( Prune a
>   ) => Prune (PointedRoseTree a)
>   where
>     prune (PointedRoseTree a r s f b) = concat
>       [ [ PointedRoseTree a r s f b' | b' <- prune b ]
>       , [ PointedRoseTree a r s f' b | f' <- prune f ]
>       , [ PointedRoseTree a r s' f b | s' <- prune s ]
>       , [ PointedRoseTree a r' s f b | r' <- prune r ]
>       , [ PointedRoseTree a' r s f b | a' <- prune a ]
>       ]
