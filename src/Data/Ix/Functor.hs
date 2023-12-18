{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.Functor where

import Control.Category.Endofunctor qualified as E
import Control.Category.Natural (type (~>) (..), type (~~>))

type IFunctor f = E.Endofunctor (~>) f

ifmap :: (IFunctor f) => (a ~~> b) -> f a ~~> f b
ifmap f x = E.fmap (NT f) # x
