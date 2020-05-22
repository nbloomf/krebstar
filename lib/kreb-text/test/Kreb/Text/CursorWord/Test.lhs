> {-# LANGUAGE ScopedTypeVariables #-}

> module Kreb.Text.CursorWord.Test where

> import Test.Tasty

> import           Kreb.Prop
> import           Kreb.Text.CursorWord



> test_CursorWord :: TestTree
> test_CursorWord = testGroup "CursorWord"
>   [ krebProp
>       "mempty <> x == x" $
>       \(x :: CursorWord) ->
>         claimEqual (mempty <> x) x
> 
>   , krebProp
>       "x <> mempty == x" $
>       \(x :: CursorWord) ->
>         claimEqual (x <> mempty) x
> 
>   , krebProp
>       "u <> (v <> w) == (u <> v) <> w" $
>       \(u :: CursorWord) (v :: CursorWord) (w :: CursorWord) ->
>         claimEqual (u <> (v <> w)) ((u <> v) <> w)
> 
>   , krebProp
>       "mempty <> x == x" $
>       \(x :: CursorData) ->
>         claimEqual (mempty <> x) x
> 
>   , krebProp
>       "x <> mempty == x" $
>       \(x :: CursorData) ->
>         claimEqual (x <> mempty) x
> 
>   , krebProp
>       "u <> (v <> w) == (u <> v) <> w" $
>       \(u :: CursorData) (v :: CursorData) (w :: CursorData) ->
>         claimEqual (u <> (v <> w)) ((u <> v) <> w)
>   ]
