> {-# LANGUAGE OverloadedStrings, TypeSynonymInstances, FlexibleInstances #-}

> module Kreb.Format where

> import Prelude hiding (Word)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

> data TextParams = TextParams
>   { _textIndent :: Int
>   } deriving (Eq, Show)
> 
> defaultTextParams :: TextParams
> defaultTextParams = TextParams
>   { _textIndent = 2
>   }

> class PrettyPrint t where
>   pretty :: t -> Docs


> prettyText
>   :: ( PrettyPrint t )
>   => Maybe Int -> t -> T.Text
> prettyText =
>   prettyTextWith defaultTextParams

> prettyTextIO
>   :: ( PrettyPrint t )
>   => Maybe Int -> t -> IO ()
> prettyTextIO w = T.putStrLn . prettyText w

> prettyTextWith
>   :: ( PrettyPrint t )
>   => TextParams -> Maybe Int -> t -> T.Text
> prettyTextWith p width x =
>   layout $ case width of
>     Nothing -> oneline $ pretty x
>     Just w -> best (_textIndent p) w 0 $ pretty x

> best :: Int -> Int -> Int -> Docs -> Doc
> best tab w k x = be w k [(0,x)]
>   where
>     be :: Int -> Int -> [(Int, Docs)] -> Doc
>     be w k zs = case zs of
>       [] -> Empty
>       (i,x):xs -> case x of
>         Nil -> be w k xs
>         Cat u v -> be w k ((i,u):(i,v):xs)
>         Nest j u -> be w k $ (i+j, u):xs
>         Text s -> Chars s $ be w (k + T.length s) xs
>         Line s -> Indent i $ be w i xs
>         Dec _ u -> be w k $ (i,u):xs
>         Opt u v -> better w k (be w k ((i,u):xs)) (be w k ((i,v):xs))
> 
>     better :: Int -> Int -> Doc -> Doc -> Doc
>     better w k x y = if fits (w-k) x then x else y
> 
>     fits :: Int -> Doc -> Bool
>     fits w x =
>       if w < 0
>         then False
>         else case x of
>           Empty -> True
>           Chars s y -> fits (w - T.length s) y
>           Indent i y -> True

> data Doc
>   = Empty
>   | Chars T.Text Doc
>   | Indent Int Doc
>   deriving (Eq, Show)

> instance Semigroup Doc where
>   x <> y = case y of
>     Empty -> x
>     Chars a z -> Chars a (x <> z)
>     Indent k z -> Indent k (x <> z)
> 
> instance Monoid Doc where
>   mempty = Empty

> layout :: Doc -> T.Text
> layout z = case z of
>   Empty -> ""
>   Chars x doc -> x <> layout doc
>   Indent k doc -> mconcat
>     [ T.singleton '\n'
>     , T.replicate k $ T.singleton ' '
>     , layout doc
>     ]



> data Docs
>   = Nil
>   | Cat Docs Docs
>   | Nest Int Docs
>   | Text T.Text
>   | Line T.Text
>   | Opt Docs Docs
>   | Dec Attr Docs
>   deriving Show

> data Attr
>   = Attr
>   deriving (Eq, Show)

> strictEq :: Docs -> Docs -> Bool
> strictEq x y = case (x,y) of
>   (Nil, Nil) ->
>     True
>   (Cat a1 b1, Cat a2 b2) ->
>     (strictEq a1 a2) && (strictEq b1 b2)
>   (Nest k1 a1, Nest k2 a2) ->
>     (k1 == k2) && (strictEq a1 a2)
>   (Text s1, Text s2) ->
>     s1 == s2
>   (Line s1, Line s2) ->
>     s1 == s2
>   (Opt x1 _, Opt x2 _) ->
>     strictEq x1 x2
>   (Dec _ x1, Dec _ x2) ->
>     strictEq x1 x2
>   _ -> False

> group :: Docs -> Docs
> group x = Opt (flatten x) x

> flatten :: Docs -> Docs
> flatten z = case z of
>   Nil -> Nil
>   Cat x y -> Cat (flatten x) (flatten y)
>   Nest _ x -> flatten x
>   Text x -> Text x
>   Line x -> Text x
>   Dec a x -> Dec a $ flatten x
>   Opt x _ -> flatten x

> oneline :: Docs -> Doc
> oneline z = case z of
>   Nil -> Empty
>   Cat x y -> (oneline x) <> (oneline y)
>   Nest _ x -> oneline x
>   Text x -> Chars x Empty
>   Line x -> Chars x Empty
>   Dec a x -> oneline x
>   Opt x _ -> oneline x

> instance Eq Docs where
>   x == y =
>     strictEq (flatten x) (flatten y)
> 
> instance Semigroup Docs where
>   x <> y = Cat x y
> 
> instance Monoid Docs where
>   mempty = Nil

> rep :: [(Int, Docs)] -> Docs
> rep = mconcat . map (uncurry nest)

> nest :: Int -> Docs -> Docs
> nest = Nest

> text :: T.Text -> Docs
> text = Text

> line :: T.Text -> Docs
> line = Line

> newline :: Docs
> newline = Line " "


> (<+>) :: Docs -> Docs -> Docs
> x <+> y = x <> text " " <> y

> (</>) :: Docs -> Docs -> Docs
> x </> y = x <> newline <> y

> (<+/>) :: Docs -> Docs -> Docs
> x <+/> y = x <> (Opt (text " ") newline) <> y


> spread :: [Docs] -> Docs
> spread = foldl (<+>) mempty

> stack :: [Docs] -> Docs
> stack = foldl (</>) mempty

> bracket :: T.Text -> Docs -> T.Text -> Docs
> bracket l x r = group $ text l <> nest 2 (newline <> x) <> newline <> text r

> instance PrettyPrint Docs where
>   pretty = id
> 
> instance PrettyPrint () where
>   pretty _ = text "()"



> instance PrettyPrint String where
>   pretty = text . T.pack . show

> fill :: [Docs] -> Docs
> fill z = case z of
>   [] -> mempty
>   [x] -> x
>   x:y:xs -> Opt ((flatten x) <+> (fill ((flatten y) : xs))) (x </> (fill (y:xs)))



fillwords                =  folddoc (<+/>) . map text . words


