{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.Cofree where

import Control.Category.Comonad (Comonad (..))
import Control.Category.Functor (Endofunctor (..))
import Control.Category.Natural (type (~>) (..))
import Data.Kind (Type)
import Prelude hiding (fmap)

type CofreeIx :: ((k -> Type) -> k -> Type) -> (k -> Type) -> k -> Type
data CofreeIx f k ix = k ix :< f (CofreeIx f k) ix

instance (Endofunctor (~>) f) => Endofunctor (~>) (CofreeIx f) where
  fmap f = NT $ \ ~(x :< rec) -> f # x :< (fmap @(~>) (cfmap f) # rec)

instance (Endofunctor (~>) f) => Comonad (~>) (CofreeIx f) where
  extract = NT $ \ ~(x :< _) -> x
  duplicate = NT $ \ ~w@(_ :< rec) -> w :< (cfmap @(~>) duplicate # rec)
