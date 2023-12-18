{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Category.Endofunctor where

import Control.Category (Category)
import Data.Functor.Compose (Compose (..))
import Control.Category.Natural (type (~>) (..))

class (Category c) => Endofunctor c f where
  fmap :: a `c` b -> f a `c` f b

instance (Prelude.Functor f) => Endofunctor (->) f where
  fmap = Prelude.fmap

instance (Prelude.Functor f) => Endofunctor (~>) (Compose f) where
  fmap (NT f) = NT $ Compose . Prelude.fmap f . getCompose
