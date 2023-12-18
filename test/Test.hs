{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Test where

import Control.Category.Endofunctor (Endofunctor)
import Control.Category.Natural (type (~>) (..), type (~~>))
import Data.Functor.Compose (Compose (..))
import Data.Ix.Foldable (IFoldable)
import Data.Ix.Instances
import Data.Ix.RecursionSchemes
import Data.Ix.Traversable (ITraversable, isequenceA)
import Data.Kind (Type)
import Generics.Kind.TH

data MyIx = Ob | El | Se

data ObjData (r :: MyIx -> Type) _i
  = Plain String
  | Italic [r Ob]

$(deriveGenericK ''ObjData)
deriving instance (Show (r Ob)) => Show (ObjData r a)
deriving via (Generically ObjData) instance (Endofunctor (~>) ObjData)
deriving via (Generically ObjData) instance (IFoldable ObjData)
deriving via (Generically ObjData) instance (ITraversable ObjData)

data ElData (rec :: MyIx -> Type) ix
  = Para [rec Ob]
  | Block [rec El]

$(deriveGenericK ''ElData)
deriving instance (Show (r Ob), Show (r El)) => Show (ElData r a)
deriving via (Generically ElData) instance (Endofunctor (~>) ElData)
deriving via (Generically ElData) instance (IFoldable ElData)
deriving via (Generically ElData) instance (ITraversable ElData)

data SecData (rec :: MyIx -> Type) ix = SecData [rec Ob] [rec El]

$(deriveGenericK ''SecData)
deriving instance (Show (r Ob), Show (r El)) => Show (SecData r a)
deriving via (Generically SecData) instance (Endofunctor (~>) SecData)
deriving via (Generically SecData) instance (IFoldable SecData)
deriving via (Generically SecData) instance (ITraversable SecData)

data StuffF (rec :: MyIx -> Type) (ix :: MyIx) where
  ObjF :: ObjData rec Ob -> StuffF rec Ob
  ElmF :: ElData rec El -> StuffF rec El
  SecF :: SecData rec Se -> StuffF rec Se

$(deriveGenericK ''StuffF)
deriving instance (Show (r El), Show (r Ob)) => (Show (StuffF r a))
deriving via (Generically StuffF) instance (Endofunctor (~>) StuffF)
deriving via (Generically StuffF) instance (IFoldable StuffF)
deriving via (Generically StuffF) instance (ITraversable StuffF)

type Stuff = Fix StuffF

type Obj = Stuff Ob
type Elm = Stuff El
type Sec = Stuff Se

pattern Obj a = Fix (ObjF a)
pattern Elm a = Fix (ElmF a)
pattern Sec a = Fix (SecF a)
{-# COMPLETE Obj, Elm, Sec #-}

foo :: Sec
foo = Sec (SecData [Obj (Italic [Obj (Plain "foo")])] [Elm (Para [Obj (Plain "hello")])])

bar :: StuffF (Compose IO r) ~~> Compose IO (StuffF r)
bar = \case
  ObjF (Plain s) -> Compose $ do
    putStrLn s
    return $ ObjF (Plain ('a' : s))
  a -> isequenceA a

foo' :: IO Sec
foo' = getCompose $ transverse (NT bar) # foo

-- >>> foo'
-- Fix {unFix = SecF (SecData [Fix {unFix = ObjF (Italic [Fix {unFix = ObjF (Plain "afoo")}])}] [Fix {unFix = ElmF (Para [Fix {unFix = ObjF (Plain "ahello")}])}])}
