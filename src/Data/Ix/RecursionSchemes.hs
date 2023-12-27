{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.RecursionSchemes (
  Fix (..),
  module Control.Category.RecursionSchemes,
) where

import Control.Category.Natural (type (~>) (..))
import Control.Category.RecursionSchemes
import Control.DeepSeq (NFData)
import Data.Ix.Functor (IFunctor)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype Fix (f :: (k -> Type) -> k -> Type) (ix :: k) = Fix {unFix :: f (Fix f) ix}
  deriving (Generic, Typeable)

deriving instance (Eq (f (Fix f) ix)) => (Eq (Fix f ix))
deriving instance (Ord (f (Fix f) ix)) => (Ord (Fix f ix))
deriving instance (Read (f (Fix f) ix)) => (Read (Fix f ix))
deriving instance (Show (f (Fix f) ix)) => (Show (Fix f ix))
deriving instance (NFData (f (Fix f) ix)) => (NFData (Fix f ix))

type instance Base (Fix f) = f

instance (IFunctor f) => Recursive (~>) (Fix f) where
  project = NT unFix

instance (IFunctor f) => Corecursive (~>) (Fix f) where
  embed = NT Fix
