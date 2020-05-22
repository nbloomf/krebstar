> module Kreb.Control (
>     module Kreb.Control.EqIn
>   , module Kreb.Control.Compare
>   , module Kreb.Control.Functor.Compose
>   , module Kreb.Control.Monad
>   , module Kreb.Control.Monad.Flow
>   , module Kreb.Control.Monad.Identity
>   , module Kreb.Control.Monad.LiftIO
>   , module Kreb.Control.Monad.Trans
>   , module Kreb.Control.Monad.Trans.EnvT
>   , module Kreb.Control.Monad.Trans.StateT
>   , module Kreb.Control.Monad.Trans.StreamT
>   , module Kreb.Control.Monad.Trans.ReplT
>   , module Kreb.Control.Tuple
> ) where

> import Kreb.Control.EqIn
> import Kreb.Control.Compare
> import Kreb.Control.Functor.Compose
> import Kreb.Control.Monad
> import Kreb.Control.Monad.Flow
> import Kreb.Control.Monad.Identity
> import Kreb.Control.Monad.LiftIO
> import Kreb.Control.Monad.Trans
> import Kreb.Control.Monad.Trans.EnvT
> import Kreb.Control.Monad.Trans.EnvT.Proofs
> import Kreb.Control.Monad.Trans.StateT
> import Kreb.Control.Monad.Trans.StateT.Proofs
> import Kreb.Control.Monad.Trans.StreamT
> import Kreb.Control.Monad.Trans.StreamT.Proofs
> import Kreb.Control.Monad.Trans.ReplT
> import Kreb.Control.Tuple





















> {-









> data FunList a b t
>   = Done t
>   | More a (FunList a b (b -> t))

> out
>   :: FunList a b t
>   -> Either t (a, FunList a b (b -> t))
> out x = case x of
>   Done t -> Left t
>   More a x -> Right (a, x)
> 
> inn
>   :: Either t (a, FunList a b (b -> t))
>   -> FunList a b t
> inn x = case x of
>   Left t -> Done t
>   Right (a, x) -> More a x

> instance Functor (FunList a b) where
>   fmap f z = case z of
>     Done t -> Done (f t)
>     More a x -> More a (fmap (f .) x)
> 
> instance Applicative (FunList a b) where
>   pure = Done
> 
>   g <*> x = case g of
>     Done f -> fmap f x
>     More a y -> More a (fmap flip y <*> x)

> single :: a -> FunList a b b
> single x = More x (Done id)

> fuse :: FunList b b t -> t
> fuse z = case z of
>   Done t -> t
>   More a x -> fuse x a





Laws:
- dimap id id == id
- dimap f1 g1 . dimap f2 g2 == dimap (f2 . f1) (g1 . g2)

> class Profunctor (p :: * -> * -> *) where
>   dimap :: (a2 -> a1) -> (b1 -> b2) -> p a1 b1 -> p a2 b2

> instance Profunctor (->) where
>   dimap f g h = g . h . f

> data UpStar
>   (f :: * -> *) (a :: *) (b :: *)
>   where
>     UpStar
>       :: ( Functor f )
>       => (a -> f b)
>       -> UpStar f a b
> 
> instance
>   ( Functor f
>   ) => Profunctor (UpStar f)
>   where
>     dimap f g (UpStar h) =
>       UpStar (fmap g . h . f)

Laws:
- dimap unitR unitR' = prod1
- dimap assocL assocR . prod1 . prod1 = prod1
- dimap unitL' unitL = prod2
- dimap assocR assocL . prod2 . prod2 = prod2
where
- unitR :: (a,()) -> a, unitR' :: a -> (a,())
- unitL :: ((),a) -> a, unitL' :: a -> ((),a)
- assocL :: (a,(b,c)) -> ((a,b),c)
- assocR :: ((a,b),c) -> (a,(b,c))

> class (Profunctor p) => Cartesian p where
>   prod1 :: p a b -> p (a,c) (b,c)
>   prod2 :: p a b -> p (c,a) (c,b)

> instance Cartesian (->) where
>   prod1 h = cross h id
>   prod2 h = cross id h

> cross
>   :: (a1 -> a2) -> (b1 -> b2) -> (a1,b1) -> (a2,b2)
> cross f g (x,y) = (f x, g y)

> instance (Functor f) => Cartesian (UpStar f) where
>   prod1 (UpStar f) = UpStar (rstrength . cross f id)
>   prod2 (UpStar f) = UpStar (lstrength . cross id f)

> rstrength :: Functor f => (f a, b) -> f (a, b)
> rstrength (z, y) = fmap (\x -> (x, y)) z

> lstrength :: Functor f => (a, f b) -> f (a, b)
> lstrength (x, z) = fmap (\y -> (x, y)) z

Laws:
- dimap zeroR zeroR' = ins1
- dimap coassocL coassocR . ins1 . ins1 = ins1
- dimap zeroL' zeroL = prod2
- dimap coassocR coassocL . ins2 . ins2 = ins2
where
- zeroR :: Either a Void -> a, zeroR' :: a -> Either a Void
- unitL :: Either Void a -> a, unitL' :: a -> Either Void a
- coassocL :: Either a (Either b c)) -> Either (Either a b) c
- coassocR :: Either (Either a b) c) -> Either a (Either b c)

> class (Profunctor p) => Cocartesian p where
>   ins1 :: p a b -> p (Either a c) (Either b c)
>   ins2 :: p a b -> p (Either c a) (Either c b)

> instance Cocartesian (->) where
>   ins1 h = plus h id
>   ins2 h = plus id h

> plus
>   :: (a1 -> a2) -> (b1 -> b2) -> Either a1 b1 -> Either a2 b2
> plus f g x = case x of
>   Left z -> Left $ f z
>   Right z -> Right $ g z

> instance (Applicative f) => Cocartesian (UpStar f) where
>   ins1 (UpStar h) = UpStar (either (fmap Left . h) (pure . Right))
>   ins2 (UpStar h) = UpStar (either (pure . Left) (fmap Right . h))

Laws
- dimap assocL assocR (both (both h j) k) == both h (both j k)
- dimap unitR unitR' h == both h triv
- dimap unitL unitL' h == both triv h

> class (Profunctor p) => Monoidal p where
>   both :: p a1 b1 -> p a2 b2 -> p (a1,a2) (b1,b2)
>   triv :: p () ()

> instance Monoidal (->) where
>   both = cross
>   triv = id

> instance (Applicative f) => Monoidal (UpStar f) where
>   triv = UpStar pure
>   both (UpStar h) (UpStar k) = UpStar (pair h k)

> pair :: (Applicative f) => (a -> f b) -> (c -> f d) -> (a,c) -> f (b, d)
> pair h k (x, y) = pure (\u v -> (u,v)) <*> h x <*> k y

> fork :: (a -> b) -> (a -> c) -> a -> (b,c)
> fork f g a = (f a, g a)



> {-

> data Optic a b s t where
>   Optic
>     :: (Profunctor p)
>     => (p a b -> p s t)
>     -> Optic a b s t

> class IsOptic u where
>   optic :: u a b s t -> Optic a b s t

> (<&)
>   :: forall u v a b h k s t
>    . ( IsOptic u, IsOptic v )
>   => u h k s t -> v a b h k -> Optic a b h k
> u <& v =
>   let
>     Optic u' = optic u
>     Optic v' = optic v
>   in Optic (u' . v' :: forall p. (Profunctor p) => p a b -> p h k)

> -}

> {-





> data C u v p a b s t =
>   forall h k. C (u p s t h k) (v p a b s t)



> data Adapter p a b s t where
>   Adapter
>     :: ( Profunctor p )
>     => (p a b -> p s t)
>     -> Adapter p a b s t
> 
> instance Profunctor (Adapter p a b) where
>   dimap f g (Adapter x) = Adapter (dimap f g . x)
> 
> instance IsOptic Adapter where
>   optic (Adapter x) = Optic x




> data Lens p a b s t where
>   Lens
>     :: ( Cartesian p )
>     => (p a b -> p s t)
>     -> Lens p a b s t
> 
> instance Profunctor (Lens p a b) where
>   dimap f g (Lens x) = Lens (dimap f g . x)
> 
> instance Cartesian (Lens p a b) where
>   prod1 (Lens x) = Lens (prod1 . x)
>   prod2 (Lens x) = Lens (prod2 . x)
> 
> instance IsOptic Lens where
>   optic (Lens x) = Optic x



> data Prism p a b s t where
>   Prism
>     :: ( Cocartesian p )
>     => (p a b -> p s t)
>     -> Prism p a b s t
> 
> instance Profunctor (Prism p a b) where
>   dimap f g (Prism x) = Prism (dimap f g . x)
> 
> instance Cocartesian (Prism p a b) where
>   ins1 (Prism x) = Prism (ins1 . x)
>   ins2 (Prism x) = Prism (ins2 . x)
> 
> instance IsOptic Prism where
>   optic (Prism x) = Optic x



> data Traversal p a b s t where
>   Traversal
>     :: ( Cartesian p, Cocartesian p, Monoidal p )
>     => (p a b -> p s t)
>     -> Traversal p a b s t
> 
> instance Profunctor (Traversal p a b) where
>   dimap f g (Traversal x) = Traversal (dimap f g . x)
> 
> instance Cartesian (Traversal p a b) where
>   prod1 (Traversal x) = Traversal (prod1 . x)
>   prod2 (Traversal x) = Traversal (prod2 . x)
> 
> instance Cocartesian (Traversal p a b) where
>   ins1 (Traversal x) = Traversal (ins1 . x)
>   ins2 (Traversal x) = Traversal (ins2 . x)
> 
> instance
>   ( Cartesian p, Cocartesian p, Monoidal p
>   ) => Monoidal (Traversal p a b)
>   where
>     both (Traversal x) (Traversal y) =
>       Traversal (uncurry both . fork x y)
>     triv = Traversal (const triv)
> 
> instance IsOptic Traversal where
>   optic (Traversal x) = Optic x



> fst_ :: (Cartesian p) => Lens p a b (a,c) (b,c)
> fst_ = Lens (dimap (fork fst id) (cross id snd) . prod1)

> -}

> -}
