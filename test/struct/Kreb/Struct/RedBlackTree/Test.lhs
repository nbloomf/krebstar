---
title: Kreb.Struct.RedBlackTree.Test
---

> {-# LANGUAGE
>     MultiParamTypeClasses
>   , ScopedTypeVariables
>   , KindSignatures
>   , DeriveGeneric
> #-}
> 
> module Kreb.Struct.RedBlackTree.Test (
>     test_RedBlackTree
> ) where
> 
> import Data.Proxy
> import Data.List (sort, nub)
> 
> import Test.Tasty
> 
> import Kreb.Check
> import Kreb.Struct.RedBlackTree

> test_RedBlackTree :: TestTree
> test_RedBlackTree = testGroup "Red-Black Tree"
>   [ test_RedBlackTree_properties "Bool" (Proxy :: Proxy Bool)
>   , test_RedBlackTree_properties "Int" (Proxy :: Proxy Int)
>   ]

> test_RedBlackTree_properties
>   :: forall a
>    . ( Ord a, Arb a, Show a, Prune a )
>   => String -> Proxy a -> TestTree
> test_RedBlackTree_properties name _ =
>   testGroup ("RedBlackTree: " ++ name)
>     [ testKreb
>         "member a empty == False" $
>         \(a :: a) ->
>           claimFalse (member a (empty :: RedBlackTree a))
> 
>     , testKreb
>         "member a (insert a empty) == true" $
>         \(a :: a) ->
>           claimTrue (member a (insert a empty :: RedBlackTree a))
> 
>     , testKreb
>         "toList (fromList (toList as)) == toList as" $
>         \(as :: RedBlackTree a) ->
>           claimEqual (toList as) (toList (fromList (toList as)))
> 
>     , testKreb
>         "toList (fromList as) == nub (sort as)" $
>         \(as :: [a]) ->
>           claimEqual (nub (sort as)) (toList (fromList as :: RedBlackTree a))
> 
>     , testKreb
>         "delete a (insert a (delete a as)) == delete a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList $ delete a as)
>             (toList $ delete a (insert a (delete a as)))
> 
>     , testKreb
>         "insert a (delete a as) == insert a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList $ insert a as)
>             (toList $ insert a (delete a as))
> 
>     , testKreb
>         "insert a (insert a as) == insert a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (insert a as)
>             (insert a (insert a as))
> 
>     , testKreb
>         "delete a (delete a as) == delete a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (delete a as)
>             (delete a (delete a as))
> 
>     , testKreb
>         "unique and sorted" $
>         \(as :: RedBlackTree a) ->
>           claimEqual
>             (toList as)
>             (nub (sort (toList as)))
> 
>     , testKreb
>         "not (member a (delete a as))" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimFalse (member a (delete a as))
> 
>     , testKreb
>         "not (member x (delete x as)) if member x as" $
>         \(as :: RedBlackTree a) ->
>           claimAll
>             [ claimFalse (member x (delete x as)) | x <- toList as ]
> 
>     , testKreb
>         "member y (delete x as) if member x as and member y as and x /= y" $
>         \(as :: RedBlackTree a) ->
>           let ps = [(x,y) | x <- toList as, y <- toList as, x /= y]
>           in claimAll
>             [ claimTrue (member y (delete x as)) | (x,y) <- ps ]
> 
>     , testKreb
>         "(member a as) || (as == delete a as)" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimAny
>             [ claimTrue (member a as)
>             , claimEqual as (delete a as)
>             ]
> 
>     , testKreb
>         "toList (fromList (delete a as)) == nub (sort (delete a as))" $
>         \(as :: RedBlackTree a) ->
>           claimAll
>             [ claimEqual (nub (sort (toList $ delete a as))) (toList (delete a as)) | a <- toList as ]
>     ]
