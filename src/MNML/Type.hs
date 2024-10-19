module MNML.Type
    ( ConcreteType (..)
    , TraitId
    , Type (..)
    , VarId
    ) where

import           Data.Text
import           MNML.AST  (NodeId)

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
