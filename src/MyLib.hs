{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module MyLib where

import Control.Natural
import Data.Kind (Type)

data MyIx = Ob | El | Se

data ObjData (rec :: MyIx -> Type)
  = Plain String
  | Italic [rec Ob]

data ElData (rec :: MyIx -> Type)
  = Para [rec Ob]
  | Block [rec El]

data SecData (rec :: MyIx -> Type) = SecData [rec Ob] [rec El]

data StuffF (rec :: MyIx -> Type) (ix :: MyIx) where
  ObjF :: ObjData rec -> StuffF rec Ob
  ElmF :: ElData rec -> StuffF rec El
  SecF :: SecData rec -> StuffF rec Se

newtype Fix (f :: (k -> Type) -> k -> Type) (ix :: k) = Fix {unFix :: f (Fix f) ix}

type Stuff = Fix StuffF

type Obj = Stuff Ob
type Elm = Stuff El
type Sec = Stuff Se

pattern Obj a = Fix (ObjF a)
pattern Elm a = Fix (ElmF a)
pattern Sec a = Fix (SecF a)
{-# COMPLETE Obj, Elm, Sec #-}

foo :: Obj
foo = Obj (Italic [Obj (Plain "foo")])

bar :: Stuff :~> Stuff
bar = NT $ \case
  Obj (Plain s) -> Obj (Plain (tail s))
  Obj a -> Obj a
  Elm a -> Elm a
  Sec a -> Sec a
