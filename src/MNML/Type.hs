module MNML.Type
    ( ConcreteType (..)
    , Type (..)
    ) where

import           Data.Text

type VarId = Integer
type TraitId = Integer

data Type
  = ConcreteType ConcreteType
  | NamedType Text -- "User"
  | Var Text VarId -- "a"
  | Generic
  | Trait TraitId -- A type that implements a trait (concrete type unknown)
  deriving (Eq, Show)

data ConcreteType
  = Int
  | Float
  | Char
  | String
  | List Type
  | Fun [Type] Type
  | Record [(Text, Type)]
  deriving (Eq, Show)
