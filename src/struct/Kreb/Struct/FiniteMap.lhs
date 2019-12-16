> module Kreb.Struct.FiniteMap (
>     FiniteMap()
> 
>   , empty
>   , isEmpty
>   , singleton
> 
>   , toList
>   , fromList
> 
>   , lookup
>   , isDefinedAt

>   , defineAt
>   , undefineAt
>   , redefineAt
>   , upsertAt
> ) where

> import Prelude hiding (lookup)

> import Kreb.Check
> import qualified Kreb.Struct.RedBlackTree as RBT



> newtype FiniteMap k v = FiniteMap
>   { unFiniteMap :: RBT.RedBlackTree (Image k v)
>   }
> 
> newtype Image k v = Image
>   { unImage :: (k, v)
>   } deriving Show
> 
> instance (Eq k) => Eq (Image k v) where
>   (Image (k1, _)) == (Image (k2, _)) = k1 == k2
> 
> instance (Ord k) => Ord (Image k v) where
>   compare (Image (k1, _)) (Image (k2, _)) = compare k1 k2
> 
> instance Functor (Image k) where
>   fmap f (Image (k, v)) = Image (k, f v)



> empty
>   :: FiniteMap k v
> empty =
>   FiniteMap RBT.empty
> 
> isEmpty
>   :: FiniteMap k v -> Bool
> isEmpty (FiniteMap x) = RBT.isEmpty x
> 
> singleton
>   :: ( Ord k )
>   => k -> v -> FiniteMap k v
> singleton k v =
>   FiniteMap (RBT.singleton (Image (k,v)))

> toList
>   :: ( Ord k )
>   => FiniteMap k v -> [(k,v)]
> toList =
>   map unImage . RBT.toList . unFiniteMap
> 
> fromList
>   :: ( Ord k )
>   => [(k,v)] -> FiniteMap k v
> fromList =
>   FiniteMap . RBT.fromList . map Image

> instance
>   ( Ord k, Eq v
>   ) => Eq (FiniteMap k v)
>   where
>     xs == ys =
>       (toList xs) == (toList ys)
> 
> instance
>   ( Ord k, Show k, Show v
>   ) => Show (FiniteMap k v)
>   where
>     show xs = "fromList " ++ show (toList xs)

> lookup
>   :: ( Ord k )
>   => k -> FiniteMap k v -> Maybe v
> lookup key (FiniteMap xs) = do
>   let k = Image (key, undefined)
>   Image (_, v) <- RBT.lookup k xs
>   return v



> isDefinedAt
>   :: ( Ord k )
>   => k -> FiniteMap k v -> Bool
> isDefinedAt k m =
>   case lookup k m of
>     Nothing -> False
>     Just _ -> True

> defineAt
>   :: ( Ord k )
>   => k -> v -> FiniteMap k v -> Maybe (FiniteMap k v)
> defineAt k v m =
>   if isDefinedAt k m
>     then Nothing
>     else Just $ FiniteMap $ RBT.insert (Image (k,v)) $ unFiniteMap m
> 
> undefineAt
>   :: ( Ord k )
>   => k -> FiniteMap k v -> Maybe (FiniteMap k v)
> undefineAt k m =
>   if isDefinedAt k m
>     then Just $ FiniteMap $ RBT.delete (Image (k,undefined)) $ unFiniteMap m
>     else Nothing
> 
> redefineAt
>   :: ( Ord k )
>   => k -> v -> FiniteMap k v -> Maybe (FiniteMap k v)
> redefineAt k v m =
>   undefineAt k m >>= defineAt k v

> upsertAt
>   :: ( Ord k )
>   => k -> v -> FiniteMap k v -> FiniteMap k v
> upsertAt k v =
>   FiniteMap . RBT.upsert (Image (k,v)) . unFiniteMap

> instance
>   ( Ord k
>   ) => Functor (FiniteMap k)
>   where
>     fmap f =
>       FiniteMap . fmap (fmap f) . unFiniteMap



> instance
>   ( Arb k, Arb v, Ord k
>   ) => Arb (Image k v)
>   where
>     arb = do
>       k <- arb
>       v <- arb
>       return (Image (k,v))
> 
> instance
>   ( Arb k, Arb v, Ord k
>   ) => Arb (FiniteMap k v)
>   where
>     arb = FiniteMap <$> arb

> instance
>   ( Prune k, Prune v, Ord k
>   ) => Prune (Image k v)
>   where
>     prune (Image (k,v)) =
>       [ Image (k', v) | k' <- prune k ] ++
>       [ Image (k, v') | v' <- prune v ]
> 
> instance
>   ( Prune k, Prune v, Ord k
>   ) => Prune (FiniteMap k v)
>   where
>     prune (FiniteMap x) =
>       fmap FiniteMap (prune x)
