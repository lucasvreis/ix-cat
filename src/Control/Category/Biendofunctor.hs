{-# LANGUAGE TypeFamilies #-}

{- | Unfortunately, I couldn't find any kind-polymorphic Bifunctor classes in
Hackage.
-}
module Control.Category.Biendofunctor where

import Control.Arrow qualified as A
import Control.Category (Category (..))
import Data.Bifunctor qualified as B
import Data.Kind (Constraint, Type)
import Prelude hiding (id, (.))

type Biendofunctor :: forall {k}. (k -> k -> Type) -> (k -> k -> k) -> Constraint
class Biendofunctor r p where
  bimap :: a `r` b -> c `r` d -> (a `p` c) `r` (b `p` d)

instance {-# OVERLAPPING #-} (B.Bifunctor f) => Biendofunctor (->) f where
  bimap = B.bimap

instance (A.Arrow a) => Biendofunctor a (,) where
  bimap = (A.***)

instance (A.ArrowChoice a) => Biendofunctor a Either where
  bimap = (A.+++)

type Cartesian :: forall {k}. (k -> k -> Type) -> Constraint
class (Category r, Biendofunctor r (Product r)) => Cartesian (r :: k -> k -> Type) where
  type Product r :: k -> k -> k
  fst :: Product r a b `r` a
  snd :: Product r a b `r` b
  diag :: a `r` Product r a a
  (&&&) :: (a `r` b) -> (a `r` c) -> a `r` Product r b c

  diag = id &&& id
  f &&& g = bimap f g . diag
  {-# MINIMAL fst, snd, diag | fst, snd, (&&&) #-}

instance (A.Arrow a, Category a) => Cartesian a where
  type Product a = (,)
  fst = A.arr Prelude.fst
  snd = A.arr Prelude.snd
  (&&&) = (A.&&&)

type CoCartesian :: forall {k}. (k -> k -> Type) -> Constraint
class (Category r, Biendofunctor r (Sum r)) => CoCartesian (r :: k -> k -> Type) where
  type Sum r :: k -> k -> k
  inl :: a `r` Sum r a b
  inr :: b `r` Sum r a b
  codiag :: Sum r a a `r` a
  (|||) :: (b `r` a) -> (c `r` a) -> Sum r b c `r` a

  codiag = id ||| id
  f ||| g = codiag . bimap f g

instance (A.ArrowChoice a, Category a) => CoCartesian a where
  type Sum a = Either
  inl = A.arr Left
  inr = A.arr Right
  (|||) = (A.|||)
