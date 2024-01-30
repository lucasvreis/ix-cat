{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Ix.RecursionSchemes (
  Fix (..),
  distHisto,
  histo,
  module Control.Category.RecursionSchemes,
) where

import Control.Category.Natural (type (~>) (..), type (~~>))
import Control.Category.RecursionSchemes
import Control.DeepSeq (NFData)
import Data.Ix.Functor (IFunctor, ifmap)
import Data.Kind (Type)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Data.Ix.Cofree (CofreeIx (..), unwrap)
import Control.Category.Comonad (Comonad(..))
import Data.Type.Equality (TestEquality (..))

newtype Fix (f :: (k -> Type) -> k -> Type) (ix :: k) = Fix {unFix :: f (Fix f) ix}
  deriving (Generic, Typeable)

deriving instance (Eq (f (Fix f) ix)) => (Eq (Fix f ix))
deriving instance (Ord (f (Fix f) ix)) => (Ord (Fix f ix))
deriving instance (Read (f (Fix f) ix)) => (Read (Fix f ix))
deriving instance (Show (f (Fix f) ix)) => (Show (Fix f ix))
deriving instance (NFData (f (Fix f) ix)) => (NFData (Fix f ix))

instance (TestEquality (f (Fix f))) => TestEquality (Fix f) where
  testEquality (Fix x) (Fix y) = testEquality x y

type instance Base (Fix f) = f

instance (IFunctor f) => Recursive (~>) (Fix f) where
  project = NT unFix
  {-# INLINE project #-}

instance (IFunctor f) => Corecursive (~>) (Fix f) where
  embed = NT Fix
  {-# INLINE embed #-}

distHisto :: IFunctor f => f (CofreeIx f a) ~~> CofreeIx f (f a)
distHisto fc = ifmap (extract #) fc :< ifmap (distHisto . unwrap) fc

histo :: (Recursive (~>) t) => (Base t (CofreeIx (Base t) a) ~> a) -> t ~> a
histo = gfold (NT distHisto)
