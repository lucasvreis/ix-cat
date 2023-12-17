{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.Functor where

import Control.Category.Endofunctor qualified as E
import Control.Category.Natural (type (~>) (..))

type IFunctor f = E.Endofunctor (~>) f

ifmap :: (IFunctor f) => (forall ix. a ix -> b ix) -> forall ix. f a ix -> f b ix
ifmap f x = E.fmap (NT f) # x
