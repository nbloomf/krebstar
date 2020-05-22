> {-# LANGUAGE
>     ScopedTypeVariables
> #-}
> 
> module Kreb.Text.Rune.Test (
>     test_Rune
> ) where
> 
> import Data.Proxy
> import Data.List
> 
> import Test.Tasty
> 
> import Kreb.Prop
> import Kreb.Reflect
> import Kreb.Text.Rune


> test_Rune :: TestTree
> test_Rune =
>   testGroup "Rune"
>     [ test_Rune_properties "2" nat2
>     , test_Rune_properties "3" nat3
>     , test_Rune_properties "6" nat6
>     , test_Rune_properties "11" nat11
>     , test_Rune_properties "20" nat20
>     ]

> test_Rune_properties
>   :: forall d
>    . ( IsBase d )
>   => String -> Proxy d -> TestTree
> test_Rune_properties title _ =
>   testGroup ("Rune properties: " ++ title)
>     [ krebProp
>         "a == a" $
>         \(a :: ZZFrac d) ->
>           claimTrue (a == a)
> 
>     , krebProp
>         "(a == b) == (b == a)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimEqual
>             (a == b)
>             (b == a)
> 
>     , krebProp
>         "(a == b) == (compare a b == EQ)" $
>          \(a :: ZZFrac d) (b :: ZZFrac d) ->
>            claimEqual
>              (a == b)
>              (compare a b == EQ)
> 
>     , krebProp
>         "(0 + a == a) && (a + 0 == a)" $
>         \(a :: ZZFrac d) ->
>           claimAll
>             [ claimEqual a (0 + a)
>             , claimEqual a (a + 0)
>             ]
> 
>     , krebProp
>         "(a + b) == (b + a)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimEqual (a + b) (b + a)
> 
>     , krebProp
>         "(a + (b + c)) == ((a + b) + c)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimEqual (a + (b + c)) ((a + b) + c)
> 
>     , krebProp
>         "abs a >= 0" $
>         \(a :: ZZFrac d) ->
>           claimTrue (0 <= abs a)
> 
>     , krebProp
>         "a <= a + (abs b)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimTrue (a <= a + (abs b))
> 
>     , krebProp
>         "(a * 1 == a) && (1 * a == a)" $
>         \(a :: ZZFrac d) ->
>           claimAll
>             [ claimEqual a (1 * a)
>             , claimEqual a (a * 1)
>             ]
> 
>     , krebProp
>         "(a * 0 == 0) && (0 * a == 0)" $
>         \(a :: ZZFrac d) ->
>           claimAll
>             [ claimEqual 0 (0 * a)
>             , claimEqual 0 (a * 0)
>             ]
> 
>     , krebProp
>         "(a * b) == (b * a)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimEqual (a * b) (b * a)
> 
>     , krebProp
>         "(a * (b * c)) == ((a * b) * c)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimEqual (a * (b * c)) ((a * b) * c)
> 
>     , krebProp
>         "(a * (b + c)) == ((a * b) + (a * c))" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimEqual (a * (b + c)) ((a * b) + (a * c))
> 
>     , krebProp
>         "((a + b) * c) == ((a * c) + (b * c))" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimEqual ((a + b) * c) ((a * c) + (b * c))
> 
>     , krebProp
>         "a * a >= 0" $
>         \(a :: ZZFrac d) ->
>           claimTrue (a*a >= 0)
> 
>     , krebProp
>         "compare (a + c) (b + c) == compare a b" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimEqual
>             (compare (a + c) (b + c))
>             (compare a b)
> 
>     , krebProp
>         "(c == 0) || (compare (a * abs c) (b * abs c) == compare a b)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) (c :: ZZFrac d) ->
>           claimAny
>             [ claimEqual c 0
>             , claimEqual
>                 (compare (a * abs c) (b * abs c))
>                 (compare a b)
>             ]
> 
>     , krebProp
>         "complexity (a + b) <= max (complexity a) (complexity b)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimTrue
>             (complexity (a + b) <= max (complexity a) (complexity b))
> 
>     , krebProp
>         "complexity (a * b) <= complexity a + complexity b" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimTrue
>             ((complexity (a * b)) <= (complexity a + complexity b))
> 
>     , krebProp
>         "(a == b) || ((min a b < chooseBetween a b) && (chooseBetween a b < max a b))" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimAny
>             [ claimEqual a b
>             , claimAll
>                 [ claimTrue (min a b < chooseBetween a b)
>                 , claimTrue (chooseBetween a b < max a b)
>                 ]
>             ]
> 
>     , krebProp
>         "(a == b) || (complexity (chooseBetween a b) <= max (complexity a) (complexity b) + 1)" $
>         \(a :: ZZFrac d) (b :: ZZFrac d) ->
>           claimAny
>             [ claimEqual a b
>             , claimTrue
>                 ((complexity (chooseBetween a b)) <= 1 + (max (complexity a) (complexity b)))
>             ]
>     ]
