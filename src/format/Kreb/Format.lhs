> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

> module Kreb.Format where

> import Prelude hiding (Word)
> import Data.Function
> import qualified Data.List as L
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

> class DisplayNeat t where
>   displayNeat :: t -> String

> data L = L [T.Text]
>   deriving (Eq, Show)

> instance Semigroup L where
>   (L as) <> (L (b:bs)) =
>     L $ xs0 <> [x <> b] <> map (indent <>) bs
>     where
>       (xs0, x) = (init as, last as)
>       indent = T.pack $ replicate (T.length x) ' '
> 
> instance Monoid L where
>   mempty = L [mempty]


> class
>   ( Monoid a
>   ) => Layout a
>   where
>     text :: T.Text -> a
>     flush :: a -> a
>     render :: a -> T.Text

> instance Layout L where
>   text s = L [s]
>   flush (L xs) = L $ xs ++ [mempty]
>   render (L xs) = T.intercalate (T.singleton ' ') xs

> class
>   ( Layout a
>   ) => Doc a
>   where
>     opt :: a -> a -> a
>     bad :: a

> data Ls = Ls [L]
>   deriving (Eq, Show)

> instance Semigroup Ls where
>   (Ls as) <> (Ls bs) = Ls (as <> bs)
> 
> instance Monoid Ls where
>   mempty = Ls [mempty]

> instance Doc Ls where
>   opt (Ls as) (Ls bs) = Ls (as ++ bs)
>   bad = Ls []
> 
> instance Layout Ls where
>   text s = Ls [text s]
>   flush (Ls as) = Ls $ map flush as
>   render (Ls as) = mconcat $ map render as

> instance Semigroup M where
>   a <> b = M
>     { maxWidth = max (maxWidth a) (lastWidth a+maxWidth b)
>     , height = height a + height b
>     , lastWidth = lastWidth a + lastWidth b
>     }
> 
> instance Monoid M where
>   mempty = M
>     { height = 0
>     , lastWidth = 0
>     , maxWidth = 0
>     }
> 
> instance Layout M where
>   text s = M
>     { height = 0
>     , maxWidth = T.length s
>     , lastWidth = T.length s
>     }
> 
>   flush a = M
>     { maxWidth = maxWidth a
>     , height = height a + 1
>     , lastWidth = 0
>     }
> 
>   render m = T.intercalate (T.singleton '\n')
>     (replicate (height m) (T.pack $ replicate (maxWidth m) 'x') ++
>     [ T.pack $ replicate (lastWidth m) 'x' ])


> measure :: L -> M
> measure (L xs) = M
>   { maxWidth = maximum $ map T.length xs
>   , height = length xs - 1
>   , lastWidth = T.length $ last xs
>   }

> data M = M
>   { height :: Int
>   , lastWidth :: Int
>   , maxWidth :: Int
>   } deriving (Show, Eq, Ord)

> class Poset a where
>   prec :: a -> a -> Bool

> instance Poset M where
>   prec m1 m2 = and
>     [ height m1 <= height m2
>     , maxWidth m1 <= maxWidth m2
>     , lastWidth m1 <= lastWidth m2
>     ]

> pareto :: ( Poset a ) => [a] -> [a]
> pareto = loop []
>   where
>     loop acc [] = acc
>     loop acc (x:xs) = if any(`prec` x) acc
>       then loop acc xs
>       else loop (x:filter (not . (prec x)) acc) xs


> data DM = DM [M]
>   deriving (Eq, Show)

> instance Semigroup DM where
>   (DM xs) <> (DM ys) = DM $ pareto $
>     concat [ filter valid [ x <> y | y <- ys ] | x <- xs ]
> 
> instance Monoid DM where
>   mempty = DM []

> instance Layout DM where
>   flush (DM xs) = DM $ pareto (map flush xs)
>   text s = DM $ filter valid [text s]
>   render (DM xs) = render $ minimum xs

> instance Doc DM where
>   bad = DM []
>   opt (DM xs) (DM ys) = DM $ pareto (xs++ys)

> valid' :: M -> Bool
> valid' x = maxWidth x <= pageWidth

> valid :: M -> Bool
> valid x = (valid' x) && (fitRibbon x)

> fitRibbon :: M -> Bool
> fitRibbon m = (height m > 0) || maxWidth m < ribbonLength

> pageWidth = 20
> ribbonLength = 10


> foobar :: (Layout a) => a
> foobar = text "foobar"


> instance Poset (M,L) where
>   prec (a,_) (b,_) = prec a b

> instance Layout (M,L) where
>   flush (x,y) = (flush x, flush y)
>   text s = (text s,text s)
>   render = render . snd

> data MLS = MLS [(M,L)]

> instance Semigroup MLS where
>   (MLS xs) <> (MLS ys) = MLS $ pareto $ concat [ filter(valid.fst)[x<>y|y<-ys]|x<-xs]
> 
> instance Monoid MLS where
>   mempty = MLS []

> instance Layout MLS where
>   flush (MLS xs) = MLS $ pareto $ (map flush xs)
>   text s = MLS $ filter(valid.fst)[text s]
>   render (MLS xs) = render $ L.minimumBy (compare `on` fst) xs

> instance Doc MLS where
>   bad = MLS []
>   opt (MLS xs) (MLS ys) = MLS $ pareto(xs++ys)

> x $$ y = flush x <> y

> hang :: (Doc d) => Int -> d -> d-> d
> hang n x y = opt (x<>y) (x $$ (nest n y))

> nest :: Layout d => Int -> d -> d
> nest n y = spaces n <> y
>   where
>     spaces n = text (T.pack $ replicate n ' ')


