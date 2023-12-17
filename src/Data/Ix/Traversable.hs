{-# LANGUAGE ExplicitNamespaces #-}

module Data.Ix.Traversable (ITraversable (..)) where

import Data.Functor.Compose (Compose (..))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Functor (IFunctor, ifmap)

class (IFunctor t, IFoldable t) => ITraversable t where
  itraverse :: (Applicative f) => (forall ix. a ix -> f (b ix)) -> forall ix. t a ix -> f (t b ix)
  itraverse f = isequenceA . ifmap (Compose . f)

  isequenceA :: (Applicative f) => forall ix. t (Compose f a) ix -> f (t a ix)
  isequenceA = itraverse getCompose

  {-# MINIMAL itraverse | isequenceA #-}
