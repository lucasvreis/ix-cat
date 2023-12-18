{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Data.Ix.Traversable (ITraversable (..), itraverse, isequenceA) where

import Control.Category.Natural (type (~~>))
import Data.Functor.Compose (Compose (..))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Functor (IFunctor)

class (IFunctor t, IFoldable t) => ITraversable t where
  ifmapTraverse :: (Applicative f) => (t b ~~> r) -> (forall ix. a ix -> f (b ix)) -> forall ix. t a ix -> f (r ix)

itraverse :: (ITraversable t, Applicative f) => (forall ix. a ix -> f (b ix)) -> forall ix. t a ix -> f (t b ix)
itraverse = ifmapTraverse id

isequenceA :: (ITraversable t, Applicative f) => t (Compose f a) ~~> Compose f (t a)
isequenceA = Compose . ifmapTraverse id getCompose
