{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | Some instances for the category of natural transformations.
module Control.Category.Natural (
  type (~~>),
  type (~>) (..),
  NProd (..),
  NSum (..),
) where

import Control.Category qualified as C (Category (..))
import Control.Category.Biendofunctor
import Data.Kind (Type)

type a ~~> b = forall ix. a ix -> b ix

newtype a ~> b = NT {(#) :: a ~~> b}

instance C.Category (~>) where
  id = NT id
  NT f . NT g = NT $ f . g

-- Cartesian structure

type NProd :: forall {k}. (k -> Type) -> (k -> Type) -> k -> Type
data NProd f g ix = (:.:) (f ix) (g ix)

instance Biendofunctor (~>) NProd where
  bimap (NT f) (NT g) = NT $ \(x :.: y) -> f x :.: g y

instance Cartesian (~>) where
  type Product (~>) = NProd
  fst = NT $ \(x :.: _) -> x
  snd = NT $ \(_ :.: y) -> y
  diag = NT $ \x -> x :.: x
  NT f &&& NT g = NT $ \x -> f x :.: g x

-- Cocartesian structure

type NSum :: forall {k}. (k -> Type) -> (k -> Type) -> k -> Type
data NSum f g ix = IxLeft (f ix) | IxRight (g ix)

instance Biendofunctor (~>) NSum where
  bimap (NT f) (NT g) = NT $ \case
    IxLeft x -> IxLeft (f x)
    IxRight x -> IxRight (g x)

instance CoCartesian (~>) where
  type Sum (~>) = NSum
  inl = NT IxLeft
  inr = NT IxRight
  codiag = NT $ \case
    IxLeft x -> x
    IxRight x -> x
  NT f ||| NT g = NT $ \case
    IxLeft x -> f x
    IxRight x -> g x
