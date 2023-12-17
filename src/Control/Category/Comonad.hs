module Control.Category.Comonad where

import Control.Category (Category (..))
import Control.Category.Endofunctor (Endofunctor (..))
import Prelude hiding (fmap, id, (.))

class (Endofunctor c w) => Comonad c w where
  extract :: w a `c` a

  duplicate :: w a `c` w (w a)
  duplicate = extend id

  extend :: (w a `c` b) -> w a `c` w b
  extend f = fmap f . duplicate

  {-# MINIMAL extract, (duplicate | extend) #-}
