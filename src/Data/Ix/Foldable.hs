{-# LANGUAGE QuantifiedConstraints #-}

module Data.Ix.Foldable (
  IFoldable (..),
  itraverse_,
) where

import Data.Coerce (Coercible, coerce)
import Data.Functor.Const (Const (..))
import Data.Kind (Type)
import Data.Monoid (Endo (..))

(#.) :: (Coercible b c) => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}

class IFoldable (f :: (k -> Type) -> k -> Type) where
  ifold :: (Monoid m) => forall ix. f (Const m) ix -> m
  {-# INLINE ifold #-}
  ifold = ifoldMap getConst

  ifoldMap :: (Monoid m) => (forall ix. a ix -> m) -> forall ix. f a ix -> m
  {-# INLINE ifoldMap #-}
  ifoldMap f = ifoldr (mappend . f) mempty

  ifoldr :: (forall ix. a ix -> c -> c) -> c -> forall ix. f a ix -> c
  ifoldr f z t = appEndo (ifoldMap (Endo #. f) t) z

  {-# MINIMAL ifoldMap | ifoldr #-}

itraverse_ :: (IFoldable t, Applicative f) => (forall ix. a ix -> f (b ix)) -> forall ix. t a ix -> f ()
itraverse_ f = ifoldr c (pure ())
  where
    c x k = f x *> k
    {-# INLINE c #-}
