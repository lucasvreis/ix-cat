{-# LANGUAGE TypeFamilies #-}

module Control.Category.RecursionSchemes where

import Control.Category (Category (..))
import Control.Category.Biendofunctor (Cartesian (..), CoCartesian (..))
import Control.Category.Comonad (Comonad (..))
import Control.Category.Endofunctor (Endofunctor (..))
import Control.Category.Monad (Monad (..))
import Prelude hiding (Monad (..), fmap, id, (.))

type family Base (b :: k) :: k -> k

class (Endofunctor c (Base b)) => Recursive c b where
  project :: b `c` Base b b

fold :: (Recursive c t) => (Base t a `c` a) -> t `c` a
fold f = c where c = f . fmap c . project

para :: (Cartesian c, Recursive c t) => (Base t (Product c t a) `c` a) -> t `c` a
para f = p where p = f . fmap (id &&& p) . project

gfold ::
  (Recursive c t, Comonad c w) =>
  -- | a distributive law
  (forall b. Base t (w b) `c` w (Base t b)) ->
  -- | a (Base t)-w-algebra
  (Base t (w a) `c` a) ->
  -- | fixed point
  t `c` a
gfold k g = g . extract . c
  where
    c = k . fmap (duplicate . fmap g . c) . project

class (Endofunctor c (Base b)) => Corecursive c b where
  embed :: Base b b `c` b

unfold :: (Corecursive c t) => (a `c` Base t a) -> a `c` t
unfold f = c where c = embed . fmap c . f

apo :: (CoCartesian c, Corecursive c t) => (a `c` Base t (Sum c t a)) -> a `c` t
apo f = c where c = embed . fmap (id ||| c) . f

gunfold ::
  (Corecursive c t, Monad c m) =>
  -- | a distributive law
  (forall b. m (Base t b) `c` Base t (m b)) ->
  -- | a (Base t)-w-coalgebra
  (a `c` Base t (m a)) ->
  -- | fixed point
  a `c` t
gunfold k f = a . return . f
  where
    a = embed . fmap (a . fmap f . join) . k

refold :: (Endofunctor c f) => (f b `c` b) -> (a `c` f a) -> a `c` b
refold f g = h where h = f . fmap h . g

hoist :: (Recursive c s, Corecursive c t) => (Base s t `c` Base t t) -> s `c` t
hoist f = fold (embed . f)

transverse :: (Recursive c s, Corecursive c t, Endofunctor c f) => (Base s (f t) `c` f (Base t t)) -> s `c` f t
transverse f = fold (fmap embed . f)
