{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use =<<" #-}
module Control.Category.Monad where

import Control.Category (Category (..))
import Control.Category.Endofunctor (Endofunctor (..))
import Prelude hiding (fmap, id, (.))

class (Endofunctor c w) => Monad c w where
  return :: a `c` w a

  join :: w (w a) `c` w a
  join = bind id

  bind :: (a `c` w b) -> w a `c` w b
  bind f = join . fmap f

  {-# MINIMAL return, (join | bind) #-}
