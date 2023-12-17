{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Category.Endofunctor where

import Control.Category (Category)

class (Category c) => Endofunctor c f where
  fmap :: a `c` b -> f a `c` f b

instance (Prelude.Functor f) => Endofunctor (->) f where
  fmap = Prelude.fmap
