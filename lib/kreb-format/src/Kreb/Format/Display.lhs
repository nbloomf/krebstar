---
title: The Display Class
author: nbloomf
---

::: frontmatter

> {-# LANGUAGE DefaultSignatures #-}
> 
> module Kreb.Format.Display where
> 
> import           Control.Exception
> import qualified Data.Text    as T
> import qualified Data.Text.IO as T
> import           Data.Void
> import           Data.Int
> import           Data.Word
> import           Data.Monoid
> import           System.IO
> 
> import Kreb.Control
> 
> import Kreb.Format.Util
> import Kreb.Format.Doc
> import Kreb.Format.Combinator

:::



Introduction
------------

So far we've defined a class of abstract documents, with the machinery to render them nicely, and a family of combinators for building well-formed documents. Here we define a class of pretty-printable types.

> class Display t where
>   display :: t -> Doc a
> 
>   default display :: (Show t) => t -> Doc a
>   display = string . show
> 
>   displayList :: [t] -> Doc a
>   displayList = align . brackList . map display
> 
> instance Display (Doc b) where
>   display = unAnnotate

The `Display` class is analogous to the standard `Show` class. In the code to come will be used as a constraint where we might otherwise use `Show`, notably in tests.



Rendering
---------

For convenience, we also define a handful of rendering functions generic on `Display`able types. We group these into two families: first are the generic renderers targeting a monoid, in increasing order of specialization:

> renderWithA
>   :: ( Display a, Applicative f, Monoid t )
>   => Layout
>   -> (T.Text -> f t) -- render plain Text
>   -> a -> f t
> renderWithA opt text =
>   renderDocSA text (\_ -> pure mempty) (\_ -> pure mempty)
>     . layout opt . display
> 
> renderWith
>   :: ( Display a, Monoid t )
>   => Layout
>   -> (T.Text -> t) -- render plain text
>   -> a -> t
> renderWith opt text = unIdentity
>   . renderWithA opt (Identity . text)
> 
> renderA
>   :: ( Display a, Applicative f, Monoid t )
>   => (T.Text -> f t) -- render plain Text
>   -> a -> f t
> renderA =
>   renderWithA (Pretty defaultLayoutOptions)
> 
> render
>   :: ( Display a, Monoid t )
>   => (T.Text -> t) -- render plain Text
>   -> a -> t
> render =
>   renderWith (Pretty defaultLayoutOptions)
> 
> renderTextWith
>   :: ( Display a )
>   => Layout
>   -> a -> T.Text
> renderTextWith opts =
>   renderWith opts id
> 
> renderText
>   :: ( Display a )
>   => a -> T.Text
> renderText = renderTextWith
>   (Pretty defaultLayoutOptions)
> 
> renderStringWith
>   :: ( Display a )
>   => Layout
>   -> a -> String
> renderStringWith opts =
>   T.unpack . renderTextWith opts
> 
> renderString
>   :: ( Display a )
>   => a -> String
> renderString =
>   T.unpack . renderText

Second we have the printing functions, which output directly to a handle (or stdout).

> hPrettyPrintWith
>   :: ( Display a )
>   => Handle -> Layout -> a -> IO ()
> hPrettyPrintWith h opts =
>   hPrintDocS h . layout opts . display
> 
> prettyPrintWith
>   :: ( Display a )
>   => Layout -> a -> IO ()
> prettyPrintWith opts =
>   hPrettyPrintWith stdout opts
> 
> hPrettyPrint
>   :: ( Display a )
>   => Handle -> a -> IO ()
> hPrettyPrint h =
>   hPrettyPrintWith h (Pretty defaultLayoutOptions)
> 
> prettyPrint
>   :: ( Display a )
>   => a -> IO ()
> prettyPrint =
>   prettyPrintWith (Pretty defaultLayoutOptions)



Instances
---------

To avoid having orphan instances for `Display` this is where we dump the instance declarations for standard library types. The default implementation of `display` allows a lot of these to be trivial.

> instance Display ()
> instance Display Void
> instance Display Bool
> instance Display Int
> instance Display Int8
> instance Display Int16
> instance Display Int32
> instance Display Int64
> instance Display Word
> instance Display Word8
> instance Display Word16
> instance Display Word32
> instance Display Word64
> instance Display Integer
> instance Display Float
> instance Display Double

`Char` needs special treatment to allow for double quoting strings.

> instance Display Char where
>   display = squote . char
>   displayList = dquote . string

The basic type constructors are straightforward:

> instance (Display a) => Display (Maybe a) where
>   display x = case x of
>     Nothing -> text "Nothing"
>     Just a  -> text "Just" <+> display a
> 
> instance (Display a) => Display [a] where
>   display = displayList
> 
> instance
>   ( Display a, Display b
>   ) => Display (Either a b)
>   where
>     display x = case x of
>       Left a  -> "Left" <+> display a
>       Right b -> "Right" <+> display b

Everyone loves tuples!

> instance
>   ( Display a1, Display a2
>   ) => Display (a1,a2)
>   where
>     display (a1,a2) = parenList
>       [ display a1, display a2 ]
> 
> instance
>   ( Display a1, Display a2, Display a3
>   ) => Display (a1,a2,a3)
>   where
>     display (a1,a2,a3) = parenList
>       [ display a1, display a2, display a3 ]
> 
> instance
>   ( Display a1, Display a2, Display a3, Display a4
>   ) => Display (a1,a2,a3,a4)
>   where
>     display (a1,a2,a3,a4) = parenList
>       [ display a1, display a2, display a3, display a4 ]
> 
> instance
>   ( Display a1, Display a2, Display a3, Display a4, Display a5
>   ) => Display (a1,a2,a3,a4,a5)
>   where
>     display (a1,a2,a3,a4,a5) = parenList
>       [ display a1, display a2, display a3, display a4, display a5 ]
> 
> instance
>   ( Display a1, Display a2, Display a3, Display a4, Display a5
>   , Display a6
>   ) => Display (a1,a2,a3,a4,a5,a6)
>   where
>     display (a1,a2,a3,a4,a5,a6) = parenList
>       [ display a1, display a2, display a3, display a4, display a5
>       , display a6 ]
> 
> instance
>   ( Display a1, Display a2, Display a3, Display a4, Display a5
>   , Display a6, Display a7
>   ) => Display (a1,a2,a3,a4,a5,a6,a7)
>   where
>     display (a1,a2,a3,a4,a5,a6,a7) = parenList
>       [ display a1, display a2, display a3, display a4, display a5
>       , display a6, display a7 ]
> 
> instance
>   ( Display a1, Display a2, Display a3, Display a4, Display a5
>   , Display a6, Display a7, Display a8
>   ) => Display (a1,a2,a3,a4,a5,a6,a7,a8)
>   where
>     display (a1,a2,a3,a4,a5,a6,a7,a8) = parenList
>       [ display a1, display a2, display a3, display a4, display a5
>       , display a6, display a7, display a8 ]

We've also got the odd wrapper types.

> instance (Display a) => Display (Sum a) where
>   display (Sum a) = string "Sum" <+> display a

This is also where we need to define instances for the data types defined in our control package.

> instance
>   ( Display a
>   ) => Display (Identity a)
>   where
>     display (Identity a) = "Identity" <+> display a
> 
> instance
>   ( Display (f (g a))
>   ) => Display (Compose f g a)
>   where
>     display (Compose x) = "Compose" <+> display x
