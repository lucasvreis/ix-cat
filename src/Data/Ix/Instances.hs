{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.Instances (Generically (..)) where

import Control.Category.Endofunctor qualified as E
import Control.Category.Natural (type (~>) (..))
import Data.Coerce (coerce)
import Data.Ix.Foldable (IFoldable (..))
import Data.Ix.Traversable (ITraversable (..), itraverse)
import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy (..))
import Generics.Kind

type IxEndoK k = (k -> Type) -> k -> Type

newtype Generically (f :: IxEndoK k) g ix = Generically {getGenerically :: f g ix}

generically :: (f1 g1 ix1 -> f2 g2 ix2) -> Generically f1 g1 ix1 -> Generically f2 g2 ix2
generically = coerce

-- * Endofunctor

instance (GFunctor (RepK f), GenericK f) => E.Endofunctor (~>) (Generically f) where
  fmap f = NT $ generically $ toK . gfmap f Proxy . fromK

type GFunctor :: (LoT (IxEndoK k) -> Type) -> Constraint
class GFunctor f where
  gfmap :: forall a b. (a ~> b) -> (forall ix. Proxy ix -> f (a :&&: ix :&&: LoT0) -> f (b :&&: ix :&&: LoT0))

instance GFunctor U1 where
  gfmap _ _ U1 = U1

instance (GFunctor f) => GFunctor (M1 i c f) where
  gfmap f p (M1 x) = M1 (gfmap f p x)

instance
  ( GFunctor f
  , GFunctor g
  ) =>
  GFunctor (f :+: g)
  where
  gfmap f p (L1 x) = L1 (gfmap f p x)
  gfmap f p (R1 x) = R1 (gfmap f p x)

instance
  ( GFunctor f
  , GFunctor g
  ) =>
  GFunctor (f :*: g)
  where
  gfmap f p (x :*: y) = (:*:) (gfmap f p x) (gfmap f p y)

instance (GFunctor f) => GFunctor (Kon c :=>: f) where
  gfmap f p (SuchThat x) = SuchThat (gfmap f p x)

instance (GFunctor f) => GFunctor (Var1 :~~: Kon c :=>: f) where
  gfmap f p (SuchThat x) = SuchThat (gfmap f p x)

instance (GFunctorArg t) => GFunctor (Field t) where
  gfmap f p (Field x) = Field (gfmapf @t f p x)

type GFunctorArg :: forall {k}. Atom (IxEndoK k) Type -> Constraint
class GFunctorArg (t :: Atom (IxEndoK l) Type) where
  gfmapf :: (a ~> b) -> (forall (ix :: l). Proxy ix -> Interpret t (a :&&: ix :&&: LoT0) -> Interpret t (b :&&: ix :&&: LoT0))

-- constant fields should be left unchanged

instance GFunctorArg (Kon t) where
  gfmapf _ _ = id

-- instances for fields (rec K) and (rec ix) where we are instantiating (MyEndo rec ix).
-- more elaborate indices for (rec _) are possible but will be implement on request.

instance GFunctorArg (Var0 :@: Kon ix) where
  gfmapf (NT f) _ = f

instance GFunctorArg (Var0 :@: Var1) where
  gfmapf (NT f) _ = f

-- instances for applications of ordinary functors

instance (Functor f, GFunctorArg x) => GFunctorArg (f :$: x) where
  gfmapf f p = Prelude.fmap (gfmapf @x f p)

-- instances for applications of indexed functor. same observations as above about indices.

instance (E.Endofunctor (~>) f) => GFunctorArg (f :$: Var0 :@: Kon ix) where
  gfmapf f _ = (E.fmap f #)

instance (E.Endofunctor (~>) f) => GFunctorArg (f :$: Var0 :@: Var1) where
  gfmapf f _ = (E.fmap f #)

-- * Foldable

instance (GFoldable (RepK f), GenericK f) => IFoldable (Generically f) where
  ifoldMap f = gfoldMap f Proxy . fromK . getGenerically
  ifoldr f c = gfoldr f c Proxy . fromK . getGenerically

type GFoldable :: (LoT (IxEndoK k) -> Type) -> Constraint
class GFoldable f where
  gfoldMap :: (Monoid m) => (forall ix. a ix -> m) -> forall ix. Proxy ix -> f (a :&&: ix :&&: LoT0) -> m
  gfoldr :: (forall ix. a ix -> c -> c) -> c -> forall ix. Proxy ix -> f (a :&&: ix :&&: LoT0) -> c

instance GFoldable U1 where
  gfoldMap _ _ U1 = mempty
  gfoldr _ c _ U1 = c

instance (GFoldable f) => GFoldable (M1 i c f) where
  gfoldMap f p (M1 x) = gfoldMap f p x
  gfoldr f c p (M1 x) = gfoldr f c p x

instance
  ( GFoldable f
  , GFoldable g
  ) =>
  GFoldable (f :+: g)
  where
  gfoldMap f p (L1 x) = gfoldMap f p x
  gfoldMap f p (R1 x) = gfoldMap f p x
  gfoldr f c p (L1 x) = gfoldr f c p x
  gfoldr f c p (R1 x) = gfoldr f c p x

instance
  ( GFoldable f
  , GFoldable g
  ) =>
  GFoldable (f :*: g)
  where
  gfoldMap f p (x :*: y) = gfoldMap f p x <> gfoldMap f p y
  gfoldr f c p (x :*: y) = gfoldr f (gfoldr f c p x) p y

instance (GFoldable f) => GFoldable (Kon c :=>: f) where
  gfoldMap f p (SuchThat x) = gfoldMap f p x
  gfoldr f c p (SuchThat x) = gfoldr f c p x

instance (GFoldable f) => GFoldable (Var1 :~~: Kon c :=>: f) where
  gfoldMap f p (SuchThat x) = gfoldMap f p x
  gfoldr f c p (SuchThat x) = gfoldr f c p x

instance (GFoldableArg t) => GFoldable (Field t) where
  gfoldMap f p (Field x) = gfoldMapf @t f p x
  gfoldr f c p (Field x) = gfoldrf @t f c p x

type GFoldableArg :: forall {k}. Atom (IxEndoK k) Type -> Constraint
class GFoldableArg t where
  gfoldMapf :: (Monoid m) => (forall ix. a ix -> m) -> forall ix. Proxy ix -> Interpret t (a :&&: ix :&&: LoT0) -> m
  gfoldrf :: (forall ix. a ix -> c -> c) -> c -> forall ix. Proxy ix -> Interpret t (a :&&: ix :&&: LoT0) -> c

instance GFoldableArg (Kon t) where
  gfoldMapf _ _ _ = mempty
  gfoldrf _ c _ _ = c

instance GFoldableArg (Var0 :@: Kon ix) where
  gfoldMapf f _ = f
  gfoldrf f c _ x = x `f` c

instance GFoldableArg (Var0 :@: Var1) where
  gfoldMapf f _ = f
  gfoldrf f c _ x = x `f` c

instance (Foldable f, GFoldableArg x) => GFoldableArg (f :$: x) where
  gfoldMapf f p = foldMap (gfoldMapf @x f p)
  gfoldrf f c p = foldr (\x cc -> gfoldrf @x f cc p x) c

instance (IFoldable f) => GFoldableArg (f :$: Var0 :@: Kon ix) where
  gfoldMapf f _ = ifoldMap f
  gfoldrf f c _ = ifoldr f c

instance (IFoldable f) => GFoldableArg (f :$: Var0 :@: Var1) where
  gfoldMapf f _ = ifoldMap f
  gfoldrf f c _ = ifoldr f c

-- * Traversable

instance (GTraversable (RepK f), GFoldable (RepK f), GFunctor (RepK f), GenericK f) => ITraversable (Generically f) where
  ifmapTraverse f g = fmap (f . Generically . toK) . gtraverse g Proxy . fromK . getGenerically

type GTraversable :: (LoT (IxEndoK k) -> Type) -> Constraint
class GTraversable t where
  gtraverse :: (Applicative f) => (forall ix. a ix -> f (b ix)) -> forall ix. Proxy ix -> t (a :&&: ix :&&: LoT0) -> f (t (b :&&: ix :&&: LoT0))

instance GTraversable U1 where
  gtraverse _ _ U1 = pure U1

instance (GTraversable f) => GTraversable (M1 i c f) where
  gtraverse f p (M1 x) = M1 <$> gtraverse f p x

instance
  ( GTraversable f
  , GTraversable g
  ) =>
  GTraversable (f :+: g)
  where
  gtraverse f p (L1 x) = L1 <$> gtraverse f p x
  gtraverse f p (R1 x) = R1 <$> gtraverse f p x

instance
  ( GTraversable f
  , GTraversable g
  ) =>
  GTraversable (f :*: g)
  where
  gtraverse f p (x :*: y) = liftA2 (:*:) (gtraverse f p x) (gtraverse f p y)

instance (GTraversable f) => GTraversable (Kon c :=>: f) where
  gtraverse f p (SuchThat x) = SuchThat <$> gtraverse f p x

instance (GTraversable f) => GTraversable (Var1 :~~: Kon c :=>: f) where
  gtraverse f p (SuchThat x) = SuchThat <$> gtraverse f p x

instance (GTraversableArg t) => GTraversable (Field t) where
  gtraverse f p (Field x) = Field <$> gtraversef @t f p x

type GTraversableArg :: forall {k}. Atom (IxEndoK k) Type -> Constraint
class GTraversableArg t where
  gtraversef :: (Applicative f) => (forall ix. a ix -> f (b ix)) -> forall ix. Proxy ix -> Interpret t (a :&&: ix :&&: LoT0) -> f (Interpret t (b :&&: ix :&&: LoT0))

instance GTraversableArg (Kon t) where
  gtraversef _ _ = pure

instance GTraversableArg (Var0 :@: Kon ix) where
  gtraversef f _ = f

instance GTraversableArg (Var0 :@: Var1) where
  gtraversef f _ = f

instance (Traversable f, GTraversableArg x) => GTraversableArg (f :$: x) where
  gtraversef f p = traverse (gtraversef @x f p)

instance (ITraversable f) => GTraversableArg (f :$: Var0 :@: Kon ix) where
  gtraversef f _ = itraverse f

instance (ITraversable f) => GTraversableArg (f :$: Var0 :@: Var1) where
  gtraversef f _ = itraverse f
