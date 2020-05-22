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
> import Prelude hiding (lookup)
> import Data.Proxy
> import Data.List (sort, nub)
> 
> import Test.Tasty
> 
> import qualified Kreb.Format as Fmt
> import           Kreb.Prop
> import           Kreb.Struct.Data.RedBlackTree

> test_RedBlackTree :: TestTree
> test_RedBlackTree = testGroup "Red-Black Tree"
>   [ test_RedBlackTree_properties "Bool" (Proxy :: Proxy Bool)
>   , test_RedBlackTree_properties "Int" (Proxy :: Proxy Int)
>   ]

> test_RedBlackTree_properties
>   :: forall a
>    . ( Ord a, Arb a, Fmt.Display a, Prune a )
>   => String -> Proxy a -> TestTree
> test_RedBlackTree_properties name _ =
>   testGroup ("RedBlackTree: " ++ name)
>     [ krebProp
>         "isEmpty empty == True" $
>         claimTrue (isEmpty (empty :: RedBlackTree a))
> 
>     , krebProp
>         "isEmpty (singleton a) == False" $
>         \(a :: a) ->
>           claimFalse (isEmpty (singleton a))
> 
>     , krebProp
>         "member a empty == False" $
>         \(a :: a) ->
>           claimFalse (member a (empty :: RedBlackTree a))
> 
>     , krebProp
>         "member a (insert a empty) == true" $
>         \(a :: a) ->
>           claimTrue (member a (insert a empty :: RedBlackTree a))
> 
>     , krebProp
>         "toList (fromList (toList as)) == toList as" $
>         \(as :: RedBlackTree a) ->
>           claimEqual (toList as) (toList (fromList (toList as)))
> 
>     , krebProp
>         "toList (fromList as) == nub (sort as)" $
>         \(as :: [a]) ->
>           claimEqual (nub (sort as)) (toList (fromList as :: RedBlackTree a))
> 
>     , krebProp
>         "delete a (insert a (delete a as)) == delete a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList $ delete a as)
>             (toList $ delete a (insert a (delete a as)))
> 
>     , krebProp
>         "insert a (delete a as) == insert a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList $ insert a as)
>             (toList $ insert a (delete a as))
> 
>     , krebProp
>         "insert a (insert a as) == insert a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (insert a as)
>             (insert a (insert a as))
> 
>     , krebProp
>         "insert a (insert b as) == insert b (insert a as)" $
>         \(a :: a) (b :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList (insert a (insert b as)))
>             (toList (insert b (insert a as)))
> 
>     , krebProp
>         "delete a (delete a as) == delete a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (delete a as)
>             (delete a (delete a as))
> 
>     , krebProp
>         "delete a (delete b as) == delete b (delete a as)" $
>         \(a :: a) (b :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList (delete a (delete b as)))
>             (toList (delete b (delete a as)))
> 
>     , krebProp
>         "unique and sorted" $
>         \(as :: RedBlackTree a) ->
>           claimEqual
>             (toList as)
>             (nub (sort (toList as)))
> 
>     , krebProp
>         "not (member a (delete a as))" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimFalse (member a (delete a as))
> 
>     , krebProp
>         "not (member x (delete x as)) if member x as" $
>         \(as :: RedBlackTree a) ->
>           claimAll
>             [ claimFalse (member x (delete x as)) | x <- toList as ]
> 
>     , krebProp
>         "member y (delete x as) if member x as and member y as and x /= y" $
>         \(as :: RedBlackTree a) ->
>           let ps = [(x,y) | x <- toList as, y <- toList as, x /= y]
>           in claimAll
>             [ claimTrue (member y (delete x as)) | (x,y) <- ps ]
> 
>     , krebProp
>         "(member a as) || (as == delete a as)" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimAny
>             [ claimTrue (member a as)
>             , claimEqual as (delete a as)
>             ]
> 
>     , krebProp
>         "toList (fromList (delete a as)) == nub (sort (delete a as))" $
>         \(as :: RedBlackTree a) ->
>           claimAll
>             [ claimEqual (nub (sort (toList $ delete a as))) (toList (delete a as)) | a <- toList as ]
> 
>     , krebProp
>         "lookup a empty == Nothing" $
>         \(a :: a) ->
>           claimEqual (lookup a (empty :: RedBlackTree a)) Nothing
> 
>     , krebProp
>         "lookup a (singleton a) == Just a" $
>         \(a :: a) ->
>           claimEqual (lookup a (singleton a)) (Just a)
> 
>     , krebProp
>         "lookup a (delete a as) == Nothing" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual (lookup a (delete a as)) Nothing
> 
>     , krebProp
>         "lookup a (upsert a as) == Just a" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual (lookup a (upsert a as)) (Just a)
> 
>     , krebProp
>         "upsert a (upsert b as) == upsert b (upsert a as)" $
>         \(a :: a) (b :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList (upsert a (upsert b as)))
>             (toList (upsert b (upsert a as)))
> 
>     , krebProp
>         "upsert a (insert a as) == upsert a as" $
>         \(a :: a) (as :: RedBlackTree a) ->
>           claimEqual
>             (toList (upsert a (insert a as)))
>             (toList (upsert a as))
>     ]
